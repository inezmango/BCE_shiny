library(shiny)
library(tidyverse)
library(palmerpenguins)
library(here)
library(janitor)


bce_data <- read.csv(here("Data","BCE_SURVEYS_All_Years_20230811.csv")) %>%
  clean_names() 


### Create the user interface:
ui <- fluidPage(
  titlePanel("Benthic Competition Experiment"),
  sidebarLayout(
    sidebarPanel("Choices",
                 radioButtons(
                   inputId = "bce_data",
                   label = "Kelp treatment", 
                   choices = c("Control", "Removal")
                 ),
                 
                 selectInput(
                   inputId = "pt_color",
                   label = "Select group",
                   choices = c("Algae" = "olivedrab",
                               "Invertebrate" = "brown3",
                               "Fish" = "grey")
                 )
    ), ### end sidebar layout
    mainPanel("Map of Coastal Sites",
              plotOutput(outputId = "bce_plot"),
              h3("Summary table"),
              tableOutput((outputId ="bce_table"))
    ) ### end main Panel
  ) ### end sidebar Layout
)

server <- function(input, output) {
  bce_select <- reactive({
    bce_groups <- bce_data %>%
      filter(group == input$bce_data)
    
    return(bce_groups)
  }) ### end bce_select
  
  output$bce_plot <- renderPlot({
    ggplot(data = bce_select()) +
      geom_col(aes(x = site, y = count, position = "dodge"),
               color = input$pt_color)
  }) ### end bce_plot
  
  bce_sum_table <- reactive({
    bce_summary_df <- bce_data %>%
      filter(group == input$group) %>%
      group_by(treatment) %>%
      summarise(density = sum(count/area, na.rm = TRUE))
    return(bce_summary_df)
  }) ### end of group select reactive function
  
  output$bce_table <- renderTable({
    bce_sum_table()
  })
} ### end server

shinyApp(ui = ui, server = server)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#### edits 3/2/2014

ui <- fluidPage(
  titlePanel("Benthic Competition Experiment"),
  sidebarLayout(
    sidebarPanel("Choices",
                 radioButtons(
                   inputId = "bce_data",
                   label = "Kelp treatment", 
                   choices = c("Control", "Removal"),
                   selected = "Control"
                 ),
                 
                 selectInput(
                   inputId = "pt_color",
                   label = "Select group",
                   choices = c("Algae" = "olivedrab",
                               "Invertebrate" = "brown3",
                               "Fish" = "grey"),
                   selected = "olivedrab"
                 )
    ), ### end sidebar layout
    mainPanel("Map of Coastal Sites",
              plotOutput(outputId = "bce_plot"),
              h3("Summary table"),
              tableOutput(outputId = "bce_table")
    ) ### end main Panel
  ) ### end sidebar Layout
)

server <- function(input, output) {
  bce_select <- reactive({
    bce_groups <- bce_data %>%
      filter(group == input$bce_data)
    
    return(bce_groups)
  }) ### end bce_select
  
  output$bce_plot <- renderPlot({
    ggplot(data = bce_select(), aes(x = site, y = count, fill = group)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = input$pt_color)
  }) ### end bce_plot
  
  bce_sum_table <- reactive({
    bce_summary_df <- bce_data %>%
      filter(group == input$bce_data) %>%
      group_by(site) %>%
      summarise(density = sum(count/area, na.rm = TRUE))
    return(bce_summary_df)
  }) ### end of group select reactive function
  
  output$bce_table <- renderTable({
    bce_sum_table()
  })
} ### end server

shinyApp(ui = ui, server = server)



#~~~~~~~~~~~~~~~~~~~~~~~~~~

site_summary_plot1 <- bce_data %>%
  filter(count > 0) %>% 
  filter(plot == "1") %>% 
  group_by(year, site) %>%
  summarize(
    species_richness = n_distinct(common_name), # Count the number of distinct species
    most_abundant_species = names(which.max(table(common_name))) # Find the most abundant species
  )

site_summary_plot2 <- bce_data %>%
  filter(count > 0) %>% 
  filter(plot == "2") %>% 
  group_by(year, site) %>%
  summarize(
    species_richness = n_distinct(common_name), # Count the number of distinct species
    most_abundant_species = names(which.max(table(common_name))) # Find the most abundant species
  )

coastal_locations <- data.frame(
  lat = c(34.409, 34.415, 34.421),
  lon = c(-119.844, -119.842, -119.846),
  species_richness = c(5, 8, 3),
  most_abundant_species = c("Species A", "Species B", "Species C"),
  image_path = c("path_to_image_A.jpg", "path_to_image_B.jpg", "path_to_image_C.jpg")
)

island_locations <- data.frame(
  lat = c(34.426, 34.430, 34.435),
  lon = c(-119.849, -119.852, -119.846),
  species_richness = c(6, 4, 7),
  most_abundant_species = c("Species D", "Species E", "Species F"),
  image_path = c("path_to_image_D.jpg", "path_to_image_E.jpg", "path_to_image_F.jpg")
)



