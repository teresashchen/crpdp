library(rio)
library(here)
library(tidyverse)
library(janitor)
library(knitr)
library(fs)
library(signal)
library(glue) 
library(colorblindr) 
library(DT)
library(shiny)
library(rsconnect)


knitr::opts_chunk$set(warning = FALSE,
                      message = FALSE,
                      error = TRUE,
                      fig.width = 10,
                      fig.height = 8
)
theme_set(theme_minimal(base_size = 16))


files <- dir_ls(here("data"), glob = "*.txt")
batch <- map_df(files, import, setclass = "tbl_df", .id = "file")

tidy_for_v <- function(v){
    v <- v %>% 
        mutate(var = value[2],
               trial = value[1],
               trial = str_replace_all(trial, ".c3d", ""),
               trial = str_replace_all(trial, "(......)_", ""),
               var = tolower(var)) %>%
        rename(frame = V1) %>% 
        slice(-1:-5) %>% 
        dplyr::select(trial, var, frame, value) 
}

tidy_for_subject <- function(data){
    data_from_one_subject <- data %>% 
        gather(temvar, value, V2:V31) %>% 
        group_by(temvar) %>% 
        nest() %>% 
        mutate(tidy = map(data, tidy_for_v)) %>% 
        dplyr::select(-data) %>%
        unnest() 
}


tidy <- batch %>% 
    mutate(file = str_replace_all(file, here("data"), ""),
           id = parse_number(file)) %>% 
    select(id, everything(), -file) %>% 
    group_by(id) %>%
    nest() %>% 
    mutate(new_data = map(data, tidy_for_subject)) %>% 
    dplyr::select(-data) %>%
    unnest() %>%
    dplyr::select(-temvar) %>%
    dplyr::filter(!is.na(value)) %>% 
    mutate(id = as.factor(id),
           trial = as.factor(trial),
           var = factor(var, levels = c("right_hip_angle", "right_hip_velocity", 
                                        "right_knee_angle", "right_knee_velocity", 
                                        "right_ankle_angle", "right_ankle_velocity"),
                        labels = c("hip angle", "hip velocity",
                                   "knee angle", "knee velocity",
                                   "ankle angle", "ankle velocity")),
           frame = as.factor(frame),
           value = as.numeric(value))

normalize_angle <- function(angle){
    
    norm_angle_data <- angle %>% 
        mutate(norm_angle = (2* (value - min(value))/(max(value) - min(value))) - 1,
               max_angle = max(norm_angle),
               min_angle = min(norm_angle)) %>% 
        rename(raw_angle = value)
}

normalize_velocity <- function(velocity){
    
    norm_velocity_data <- velocity %>% 
        mutate(norm_velocity = value /max(abs(value)),
               max_velocity = max(norm_velocity),
               min_velocity = min(norm_velocity)) %>% 
        rename(raw_velocity = value)
    
}

normalize <- tidy %>% 
    separate(var, into = c("joint", "measure")) %>% 
    group_by(id, trial, joint, measure) %>% 
    nest() %>% 
    spread(measure, data) %>% 
    mutate(norm_angle = map(angle, normalize_angle),
           norm_velocity = map(velocity, normalize_velocity)) %>% 
    dplyr::select(-angle, -velocity) %>% 
    unnest() %>% 
    dplyr::select(-frame1) %>% 
    mutate(joint = factor(joint, levels = c("ankle", "knee", "hip")))

phase_portrait <- normalize %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(phase_portrait = map2(id, data, function(id, data){
        
        data %>% ggplot(aes(norm_angle, norm_velocity, color = trial)) +
            geom_point() +
            geom_path() +
            geom_point(data = dplyr::filter(data, frame == 1), 
                       color = "#000000", size = 3) +
            geom_vline(xintercept = 0, color = "gray50") +
            geom_hline(yintercept = 0, color = "gray50") +
            facet_wrap(~joint) +
            scale_color_OkabeIto() +
            labs(title = "Phase Portrait of Joint",
                 subtitle = glue("Subjet #{id}"),
                 caption = "The black dot represents the start of cycle", 
                 x = "Normalized Angle",
                 y = "Normalized Velocity")}))

phase_angle <- phase_portrait %>% 
    select(-phase_portrait) %>% 
    unnest() %>% 
    select(id, trial, joint, norm_angle, norm_velocity) %>% 
    group_by(id, trial, joint) %>% 
    nest() %>% 
    mutate(phase_angle = map(data, function(data){
        
        tibble(frame = seq(1, 101, 1),
               phase_angle = signal::unwrap((atan2(data$norm_velocity, data$norm_angle))) * 180 / pi)
        
    })) %>% 
    select(-data) %>% 
    unnest() 

phase_angle_plot <- phase_angle %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(phase_angle_plot = map2(id, data, function(id, data){
        
        data %>%
            ggplot(aes(x = frame, y = phase_angle, color = trial)) +
            geom_point() +
            scale_color_OkabeIto() +
            facet_wrap(~joint) +
            labs(title = "Phase Angle of Joint During A Gait Cycle",
                 subtitle = glue("Subjet #{id}"),
                 y = "Phase Angle",
                 x = "% of Gait Cycle")
    }))

crp_hipknee <- phase_angle %>% 
    dplyr::filter(joint == "hip" | joint == "knee") %>% 
    spread(joint, phase_angle) %>% 
    mutate(crp_hipknee = hip - knee)

crp_hipknee_plot <- crp_hipknee %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(crp_hipknee_plot = pmap(list(id, data), 
                                   function(id, data){
                                       data %>% 
                                           ggplot(aes(x = frame, y = crp_hipknee, color = trial)) +
                                           geom_point() +
                                           geom_path() +                               
                                           scale_color_OkabeIto() +                               
                                           labs(title = "Hip-Knee Continuous Relative Phase During A Gait Cycle",
                                                subtitle = glue("Subject #{id}"),
                                                y = "CRP",
                                                x = "% of Gait Cycle")
                                       
                                   }))

crp_kneeankle <- phase_angle %>% 
    dplyr::filter(joint == "knee" | joint == "ankle") %>% 
    spread(joint, phase_angle) %>% 
    mutate(crp_kneeankle = knee - ankle)

crp_kneeankle_plot <- crp_kneeankle %>% 
    group_by(id) %>% 
    nest() %>% 
    mutate(crp_kneeankle_plot = pmap(list(id, data), 
                                     function(id, data){
                                         data %>% 
                                             ggplot(aes(x = frame, y = crp_kneeankle, color = trial)) +
                                             geom_point() +
                                             geom_path() +                                 
                                             scale_color_OkabeIto() +                                 
                                             labs(title = "Knee-Ankle Continuous Relative Phase During A Gait Cycle",
                                                  subtitle = glue("Subject #{id}"),
                                                  y = "CRP",
                                                  x = "% of Gait Cycle")
                                         
                                     }))


dp <- crp_hipknee %>% 
    left_join(crp_kneeankle) %>% 
    group_by(id, frame) %>% 
    summarise(crp_hipknee_sd = sd(crp_hipknee),
              crp_kneeankle_sd = sd(crp_kneeankle)) %>% 
    group_by(id) %>% 
    summarise(dp_hipknee = mean(crp_hipknee_sd),
              dp_kneeankle = mean(crp_kneeankle_sd))

###############################################

# Define UI for random distribution app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Inter-Joint Coordination"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
    # Sidebar panel for inputs ----
    sidebarPanel(
            
    # Input: Select the random distribution type ----
    selectInput("id", "Choose a Particpant:",
                         c("Participant1" = "1",
                           "Participant2" = "2",
                           "Participant5" = "5",
                           "Participant6" = "6",
                           "Participant7" = "7")),
            
 hr(),
 helpText("The main purpose of the project is to calculate two outcome variables indicating human joint-joint coordination and variability of coordination during body movement. These two variables ared called inter-joint continuous relative phase (CRP) and deviation phase (DP), respectively.")
 ),
        
    # Main panel for displaying outputs ----
    mainPanel(
        
    # Output: Tabset w/ different movements ----
        tabsetPanel(type = "tabs",
            tabPanel("Phase Portrait", plotOutput("phase_portrait")), #phase_portrait
            tabPanel("Phase Angle Plot", plotOutput("phase_angle_plot")), #phase_angle_plot
            tabPanel("CRP Hip-Knee Plot", plotOutput("crp_hipknee_plot")), #crp_hipknee_plot
            tabPanel("CRP Knee-Ankle Plot", plotOutput("crp_kneeankle_plot")) #crp_kneeankle_plot 
            ),
    dataTableOutput("tbl")
        )
         
    )
)

    

# Define server logic for participant----
server <- function(input, output) {
    
    # Reactive expression to generate the requested participant ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
        id <- switch(input$id,
                       phase_portrait = rphase_portrait,
                       phase_angle_plot = rphase_angle_plot,
                       crp_hipknee_plot = rcrp_hipknee_plot,
                       crp_kneeankle_plot = rcrp_kneeankle_plot)
        # 
        # id(input$id)
    })
    
    # Generate a plot of phase_portrait ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$phase_portrait <- renderPlot({
        id <- input$id
        #n <- input$n
        
        phase_portrait %>% 
            dplyr::filter(id == input$id) %>% 
            pull(phase_portrait)
        
        #phase_portrait$phase_portrait[[1]]
        
        # normalize %>% 
        #     dplyr::filter(id == input$id) %>% 
        #     #group_by(id) %>% 
        #     #nest() %>% 
        #     #mutate(phase_portrait = map2(id, data, function(id, data){
        #         
        #         #data %>% 
        #             ggplot(aes(norm_angle, norm_velocity, color = trial)) +
        #             geom_point() +
        #             geom_path() +
        #             geom_point(data = dplyr::filter(normalize, frame == 1), 
        #                        color = "#000000", size = 3) +
        #             geom_vline(xintercept = 0, color = "gray50") +
        #             geom_hline(yintercept = 0, color = "gray50") +
        #             facet_wrap(~joint) +
        #             scale_color_OkabeIto() +
        #             labs(title = "Phase Portrait of Joint",
        #                  subtitle = glue("Subjet #{input$id}"),
        #                  caption = "The red dot represents the start of cycle", 
        #                  x = "Normalized Angle",
        #                  y = "Normalized Velocity")
                    })
    #)
        #phase_portrait$phase_portrait[[1]]
   # })

    # Generate a plot of phase_angle_plot ----
    # Also uses the inputs to build the plot label. Note that the
    # dependencies on the inputs and the data reactive expression are
    # both tracked, and all expressions are called in the sequence
    # implied by the dependency graph.
    output$phase_angle_plot <- renderPlot({
        id <- input$id
        #n <- input$n
        
        # normalize %>% 
        phase_angle_plot %>%
            dplyr::filter(id == input$id) %>%
            pull(phase_angle_plot)
        
         # phase_angle_plot <- phase_angle %>%
         #      group_by(id) %>%
         #      nest() %>%
         #      mutate(phase_angle_plot = map2(id, data, function(id, data){
         # 
         #         data %>%
                    # ggplot(aes(x = frame, y = phase_angle, color = trial)) +
                    # geom_point() +
                    # scale_color_OkabeIto() +
                    # facet_wrap(~joint) +
                    # labs(title = "Phase Angle of Joint During A Gait Cycle",
                    #      subtitle = glue("Subjet #{id}"),
                    #      y = "Phase Angle",
                    #      x = "% of Gait Cycle")
                    #      })
                #phase_angle_plot$phase_angle_plot[[1]]
            })
    

        # Generate a plot of crp_hipknee_plot ----
        # Also uses the inputs to build the plot label. Note that the
        # dependencies on the inputs and the data reactive expression are
        # both tracked, and all expressions are called in the sequence
        # implied by the dependency graph.
        output$crp_hipknee_plot <- renderPlot({
            id <- input$id
            #n <- input$n
                
            # normalize %>% 
            #     dplyr::filter(id == input$id) %>% 
            
            crp_hipknee_plot %>% 
                dplyr::filter(id == input$id) %>% 
                pull(crp_hipknee_plot)
            
            # crp_hipknee_plot <- crp_hipknee %>% 
            #     group_by(id) %>% 
            #     nest() %>% 
            #     mutate(crp_hipknee_plot = pmap(list(id, data), 
            #                                    function(id, data){
            #    data %>% 
               # ggplot(aes(x = frame, y = crp_hipknee, color = trial)) +
               #        geom_point() +
               #        geom_path() +                               
               #        scale_color_OkabeIto() +                               
               #        labs(title = "Hip-Knee Continuous Relative Phase During A Gait Cycle",
               #           subtitle = glue("Subject #{id}"),
               #           y = "CRP",
               #           x = "% of Gait Cycle")
                                                   #crp_hipknee_plot$crp_hipknee_plot[[1]]
                                              # }))
       
        })
            
            # Generate a plot of crp_kneeankle_plot ----
            # Also uses the inputs to build the plot label. Note that the
            # dependencies on the inputs and the data reactive expression are
            # both tracked, and all expressions are called in the sequence
            # implied by the dependency graph.
            output$crp_kneeankle_plot <- renderPlot({
                id <- input$id
                #n <- input$n
            
                # normalize %>% 
                #     dplyr::filter(id == input$id) %>% 
                
                crp_kneeankle_plot %>% 
                    dplyr::filter(id == input$id) %>% 
                    pull(crp_kneeankle_plot)
                
                #crp_kneeankle_plot <- crp_kneeankle %>% 
                    #group_by(id) %>% 
                    #nest() %>% 
                    #mutate(crp_kneeankle_plot = pmap(list(id, data), 
                     #function(id, data){
                     #data %>% 
                    # ggplot(aes(x = frame, y = crp_kneeankle, color = trial)) +
                    #           geom_point() +
                    #           geom_path() +                                 
                    #           scale_color_OkabeIto() +                                 
                    #           labs(title = "Knee-Ankle Continuous Relative Phase During A Gait Cycle",
                    #                subtitle = glue("Subject #{id}"),
                    #                y = "CRP",
                    #                x = "% of Gait Cycle")
                                                         #crp_kneeankle_plot$crp_kneeankle_plot[[1]] 
                                                     #}))
            })
                

   # Generate an HTML table view of the data ----
                output$tbl <- renderDataTable({
     
                    
                    dp %>% 
                        mutate(`Deviation Phase:Hip-Knee` = dp_hipknee,
                               `Deviation Phase:Knee-Ankle` = dp_kneeankle) %>% 
                        rename(Participant = id) %>% 
                        select(-dp_hipknee, -dp_kneeankle) %>% 
                        DT::datatable() %>% 
                        formatRound(columns=c('Deviation Phase:Hip-Knee', 'Deviation Phase:Knee-Ankle'), digits=3)
                })
                
}
   
   # Run the app ----
shinyApp(ui = ui, server = server)
