
library("rio")
library("here")
library("tidyverse")
library("janitor")
library("knitr")
library("fs")
library("glue")
library(shiny)
library(ggplot2)
theme_set(theme_minimal(15))


files <- dir_ls(here("data"), glob = "*.txt")
batch <- map_df(files, import, setclass = "tbl_df", .id = "file")
head(batch, 20L)



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
    filter(!is.na(value)) %>% 
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
    dplyr::select(-frame1)


phase_portrait <- normalize %>% 
    group_by(id, trial, joint) %>% 
    nest() %>% 
    mutate(phase_portrait = pmap(list(id, trial, joint, data), 
                                 function(id, trial, joint, data){
                                     data %>% ggplot(aes(norm_angle, norm_velocity)) +
                                         geom_point() +
                                         geom_path() +
                                         geom_point(data = filter(data, frame == 1), 
                                                    color = "red", size = 3) +
                                         geom_vline(xintercept = 0, color = "gray50") +
                                         geom_hline(yintercept = 0, color = "gray50") +
                                         labs(title = glue("Phase Portrait of {str_to_title(joint)} Joint"),
                                              subtitle = glue("Subjet #{id}, Trial #{trial}"),
                                              caption = "The red dot represents the start of cycle", 
                                              x = "Normalized Angle",
                                              y = "Normalized Velocity")}))


phase_angle <- phase_portrait %>% 
    mutate(phase_angle = map(data, function(data){
        
        tibble(frame = seq(1, 101, 1),
               phase_angle = signal::unwrap((atan2(data$norm_velocity, data$norm_angle))) * 180 / pi)
        
        
    })) %>% 
    select(-phase_portrait, -data) %>% 
    unnest() 

crp_hipknee <- phase_angle %>% 
    filter(joint == "hip" | joint == "knee") %>% 
    spread(joint, phase_angle) %>% 
    mutate(crp_hipknee = hip - knee)

crp_kneeankle <- phase_angle %>% 
    filter(joint == "knee" | joint == "ankle") %>% 
    spread(joint, phase_angle) %>% 
    mutate(crp_kneeankle = knee - ankle)

# 1 Define UI for application that draws plot
ui <- fluidPage(

    # Application title
    titlePanel("Human Body Movement"),

    # Sidebar with a slider input for participants 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", "Choose a movement", 
                        choices = c("Phase Portrait" = "phase_portrait",
                                    "Phase Angle" = "phase_angle_plot",
                                    "Hip-Knee" = "crp_hipknee_plot",
                                    "Knee-Ankle" = "crp_kneeankle_plot"))),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("phase_portrait", width = "100%", height = "400px")
         )
     )
 )

# Define server logic required to draw a plot1
server <- function(input, output) {
    
#Fill in the spot created for a plot
output$phase_portrait <- renderPlot({

#Render a plot
        phase_portrait <- normalize %>% 
            group_by(id, trial, joint) %>% 
            nest() %>% 
            mutate(phase_portrait = pmap(list(id, trial, joint, data), 
                                         function(id, trial, joint, data){
                                             data %>% ggplot(aes(norm_angle, norm_velocity)) +
                                                 geom_point() +
                                                 geom_path() +
                                                 geom_point(data = filter(data, frame == 1), 
                                                            color = "red", size = 3) +
                                                 geom_vline(xintercept = 0, color = "gray50") +
                                                 geom_hline(yintercept = 0, color = "gray50") +
                                                 labs(title = glue("Phase Portrait of {str_to_title(joint)} Joint"),
                                                      subtitle = glue("Subjet #{id}, Trial #{trial}"),
                                                      caption = "The red dot represents the start of cycle", 
                                                      x = "Normalized Angle",
                                                      y = "Normalized Velocity")}))
        
        phase_portrait$phase_portrait[[1]]
        })
}

# Run the application 
shinyApp(ui = ui, server = server)


# 2 Define UI for application that draws plot
ui <- fluidPage(
    
    # Application title
    titlePanel("Human Body Movement"),
    
    # Sidebar with a slider input for participants 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", "Choose a movement", 
                        choices = c("Phase Portrait" = "phase_portrait",
                                    "Phase Angle" = "phase_angle_plot",
                                    "Hip-Knee" = "crp_hipknee_plot",
                                    "Knee-Ankle" = "crp_kneeankle_plot"))),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("phase_angle_plot", width = "100%", height = "400px")
        )
    )
)

# Define server logic required to draw a plot2
server <- function(input, output) {
    
#Fill in the spot created for a plot
output$phase_angle_plot <- renderPlot({
        
#Render a plot
    phase_angle_plot <- phase_angle %>% 
        group_by(id, trial, joint) %>% 
        nest() %>% 
        mutate(phase_angle_plot = pmap(list(id, trial, joint, data), 
                                       function(id, trial, joint, data){
                                           data %>%
                                               ggplot(aes(x = frame, y = phase_angle)) +
                                               geom_point() +
                                               labs(title = glue("Phase Angle of {str_to_title(joint)} Joint During A Gait Cycle"),
                                                    subtitle = glue("Subjet #{id}, Trial #{trial}"),
                                                    y = "Phase Angle",
                                                    x = "% of Gait Cycle")}))
    
    phase_angle_plot$phase_angle_plot[[1]]
})
}

# Run the application 
shinyApp(ui = ui, server = server)



# 3 Define UI for application that draws plot
ui <- fluidPage(
    
    # Application title
    titlePanel("Human Body Movement"),
    
    # Sidebar with a slider input for participants 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", "Choose a movement", 
                        choices = c("Phase Portrait" = "phase_portrait",
                                    "Phase Angle" = "phase_angle_plot",
                                    "Hip-Knee" = "crp_hipknee_plot",
                                    "Knee-Ankle" = "crp_kneeankle_plot"))),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("crp_hipknee_plot", width = "100%", height = "400px")
        )
    )
)

# Define server logic required to draw a plot3
server <- function(input, output) {
    
    #Fill in the spot created for a plot
    output$crp_hipknee_plot <- renderPlot({
        
        #Render a plot
        crp_hipknee_plot <- crp_hipknee %>% 
            group_by(id, trial) %>% 
            nest() %>% 
            mutate(crp_hipknee_plot = pmap(list(id, trial, data), 
                                           function(id, trial, data){
                                               data %>% 
                                                   ggplot(aes(x = frame, y = crp_hipknee)) +
                                                   geom_point() +
                                                   labs(title = "Hip-Knee Continuous Relative Phase During A Gait Cycle",
                                                        subtitle = glue("Subject #{id}, Trial #{trial}"),
                                                        y = "CRP",
                                                        x = "% of Gait Cycle")
                                               
                                           }))
        
        crp_hipknee_plot$crp_hipknee_plot[[1]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# 4 Define UI for application that draws plot
ui <- fluidPage(
    
    # Application title
    titlePanel("Human Body Movement"),
    
    # Sidebar with a slider input for participants 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", "Choose a movement", 
                        choices = c("Phase Portrait" = "phase_portrait",
                                    "Phase Angle" = "phase_angle_plot",
                                    "Hip-Knee" = "crp_hipknee_plot",
                                    "Knee-Ankle" = "crp_kneeankle_plot"))),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("crp_kneeankle_plot", width = "100%", height = "400px")
        )
    )
)

# Define server logic required to draw a plot4
server <- function(input, output) {
    
    #Fill in the spot created for a plot
    output$crp_kneeankle_plot <- renderPlot({
        
        #Render a plot
        crp_kneeankle_plot <- crp_kneeankle %>% 
            group_by(id, trial) %>% 
            nest() %>% 
            mutate(crp_kneeankle_plot = pmap(list(id, trial, data), 
                                             function(id, trial, data){
                                                 data %>% 
                                                     ggplot(aes(x = frame, y = crp_kneeankle)) +
                                                     geom_point() +
                                                     labs(title = "Knee-Ankle Continuous Relative Phase During A Gait Cycle",
                                                          subtitle = glue("Subject #{id}, Trial #{trial}"),
                                                          y = "CRP",
                                                          x = "% of Gait Cycle")
                                                 
                                             }))
        
        crp_kneeankle_plot$crp_kneeankle_plot[[1]]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

