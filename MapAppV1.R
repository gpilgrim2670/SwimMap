library(ggplot2)
library(shiny)
library(tools)
library(grid)
library(sf)
library(mapview)
library(rgeos)
library(maps)
library(ggmap)
library(DT)

#Import Data
#setwd("/Users/Greg/Documents/R Programming Course/Swimming Project")
BigTop100 <- read.csv("BigTop100.csv")
states <- read.csv("states.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("School Browser"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs(s)
    sidebarPanel(
      
      # Select variable for y-axis
      #selectInput(inputId = "y", 
      #            label = "Y-axis:",
      #            choices = c("IMDB rating" = "imdb_rating", 
      #                        "IMDB number of votes" = "imdb_num_votes", 
      #                        "Critics Score" = "critics_score", 
      #                        "Audience Score" = "audience_score", 
      #                        "Runtime" = "runtime"), 
      #            selected = "audience_score"),
      
      # Select variable for x-axis
      #selectInput(inputId = "x", 
      #            label = "X-axis:",
      #            choices = c("IMDB rating" = "imdb_rating", 
      #                        "IMDB number of votes" = "imdb_num_votes", 
      #                        "Critics Score" = "critics_score", 
      #                        "Audience Score" = "audience_score", 
      #                        "Runtime" = "runtime"), 
      #            selected = "critics_score"),
      
      # Select variable for color
      #selectInput(inputId = "z", 
      #            label = "Color by:",
      #            choices = c("Division" = "Division"),
      #            selected = "Division"),
      
      # Enter text for plot title
      #textInput(inputId = "plot_title", 
      #          label = "Plot title", 
      #          placeholder = "Enter text for plot title"),
 
      # Select which Gender(s) to plot
      checkboxGroupInput(inputId = "Gender",
                         label = "Select Gender(s):",
                         choices = c("Male" = "M", "Female" = "F"),
                         selected = "M"),
           
      # Select which Division(s) to plot
      checkboxGroupInput(inputId = "Division",
                         label = "Select Divisions(s):",
                         choices = c("DIII", "DII", "DI"),
                         selected = "DI"),
 
      # Select which Region(s) to plot
      checkboxGroupInput(inputId = "Region",
                         label = "Select Region:",
                         choices = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific"),
                         selected = "NewEngland"),
      # Set Top X Rank
      sliderInput(inputId = "Rank", 
                  label = "Top:", 
                  min = 1, max = 1000, 
                  value = 25)
    ),
    
    # Output(s)
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "brush_plot"),
      br(),
      dataTableOutput(outputId = "schoolstable"),
      br(),
      textOutput(outputId = "test")
      #textOutput(outputId = "description")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


  # Create a subset of data filtering for selected Divisions
  BigTop100_subset <- reactive({
    req(input$Division)
    req(input$Region)
    req(input$Rank)
    filter(BigTop100, Division %in% input$Division) %>%
    filter(Region %in% input$Region) %>%
    filter(Rank <= input$Rank) %>%
    filter(Sex %in% input$Gender)
  })

  #Create a subset of data filtering for selected Region
  #Consider using SF based map to include Alaska and Hawaii
  #Might also need Canada map for Simon Fraser
  #This will help sort Name_1 value into regions
  #https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r
  
  states_subset <- reactive({
    req(input$Region)
    filter(states, GeoRegion %in% input$Region)
  })
  
  
  # Convert plot_title toTitleCase
  #pretty_plot_title <- reactive ({toTitleCase(input$plot_title)
  #})
  
  # Create scatterplot object the plotOutput function is expecting
  output$test <- renderText(paste0(BigTop100[4,6]))
  
  output$scatterplot <- renderPlot({

      ggplot() +
      geom_polygon(data = states_subset(), aes(x = long, y = lat, group = group), color = "white", fill = "gray") +
      coord_quickmap() +
      guides(fill = FALSE) +
      geom_count(data = BigTop100_subset(), aes(x = lon, y = lat, size = ..n.., color = Division, shape = Sex), alpha = 0.5) +
      theme_void() +
      scale_color_manual(values = c("DI" = "blue", "DII" = "red", "DIII" = "green")) +
      labs(size = "Count", color = "Division", shape = "Gender" 
           #, title = pretty_plot_title()
           ) +
      theme(axis.text = element_blank(), axis.ticks = element_blank()) +
      theme(plot.title = element_text(hjust=0.5, face = "bold")) +
      theme(plot.background = element_rect(fill = "white"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  })
  
  # Create data table
  #https://stackoverflow.com/questions/31445367/r-shiny-datatableoutput-not-displaying-brushed-points
  brushd <- reactive({
    user_brush <- input$brush_plot
    brushedPoints(BigTop100_subset(), user_brush, xvar = "lon", yvar = 
                    "lat")
  })
  
  output$schoolstable<-DT::renderDataTable({DT::datatable(unique(brushd()[,c("Team", "City", "State", "Division")]), rownames = FALSE)}) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

