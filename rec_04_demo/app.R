
library(tidyverse)
library(primer.data)
library(shinythemes)
library(shiny)

# Sourcing files (.R files "Rscripts" only) allows you access to those files and
# their objects / plots etc. Bear in mind that unless data is stored locally and
# graphics are stored as images, outputs will still be re-generated when the app
# compiles.

source(file = "clean_pov_map.R")

data1 <- read_rds("poverty_map.Rds")

ui <- navbarPage(
    
    tabsetPanel(
        tabPanel("Interactivity",
                 fluidPage(
                     titlePanel("Harvard Qscores Data"),
                     sidebarLayout(
                         sidebarPanel(
                             
                             selectInput(
                                 
                                 # The first arg. here sets the local variable
                                 # that you use to toggle selections from the UI
                                 # within the server
                                 
                                 "var_plot",
                                 
                                 # The second position labels the selection pane
                                 # / area
                                 
                                 "Choose a Response Category",
                                 
                                 # These equations link the displayed options in
                                 # the UI as the user will see them to the value
                                 # taken on by the local variable. User selects
                                 # "Enrollment", local var becomes =
                                 # "enrollment"
                                 
                                 choices = c("Enrollment" = "enrollment", 
                                   "Instructor Rating" = "rating")
                             ),
                             width = 300),
                         
                         plotOutput("line_plot",
                                    width = 550,
                                    height = 500)))),
        
        tabPanel("Map",
                 titlePanel("Map of % HH Below Poverty Line"),
                 plotOutput("pov_plot2",
                            width = 450,
                            height = 400)),
        
        tabPanel("Other Map",
                 titlePanel("Another way to bring in a Map"),
                 plotOutput("map2")),
        
        tabPanel("Working Map",
                 titlePanel("Yet Another way to bring in a Map"),
                 img(src = "img01", align = "center", 
                     height = "80%", width = "80%")),
        
        tabPanel("Discussion",
                 titlePanel("Discussion Title"),
                 p("Show the iterations you walked through in model construction
                   and choosing your covariates, often this will look like tables
                   and distribution plots.")),
        
        tabPanel("Table",
                 titlePanel("You can include tables too!"),
                 gt_output("table1")),
        
        tabPanel("About", 
                 titlePanel("About"),
                 h3("Project Background and Motivations"),
                 p("Here you tell the story of your project, acknowledge sources,
                   and leave GH link and whatever contact(s) you feel comfortable with."),
                 uiOutput("link"))
        
    ))

server <- function(input, output) {
    
    output$link <- renderUI({
        tags$a(href="https://github.com/BeauMeche/Shiny_app_demo_1005", "Here is the link to this repo")
    })
    
    
    # This is the interactive portion, all else is more or less static
        
    output$line_plot <- renderPlot({
        
        ifelse(input$var_plot == "enrollment",
               z <- qscores$enrollment,
               z <- qscores$rating)
        
            qscores %>% 
                ggplot(aes(x = hours,
                           y = z,
                           color = term)) +
                geom_point(alpha = 0.5) +
                labs(title = "Student Reports from Courses at Harvard",
                     y = str_to_title(input$var_plot),
                     x = "Expected Workload / Week",
                     color = "Term",
                     caption = "Source: Harvard Registrar's Office")
    })
    
    output$pov_plot2 <- renderPlot({
        
        data1 %>%
            ggplot(aes(fill = pov_ratio)) +
            geom_sf() +
            scale_fill_viridis_c(option = "viridis") +
            labs(title = "Impoverished Households - 2015",
                 subtitle = "Poverty Line in 2015 was $24,250 for a family of 4",
                 caption = "Sources: ACS 2015, ASPE",
                 fill = "% HH") +
            theme_few()
    
    # This makes the image for the 3rd (working) map. 
    # ggsave("img01.png", plot = last_plot())
        
    })
    
    output$table1 <- render_gt({
        table
    })
    
    output$map2 <- renderPlot({
        map2
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
