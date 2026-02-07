#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

### REACTIVE ####

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



### GENERAL ####

library(shiny)

# define UI
ui <- fluidPage(
  
)

# define server logic 
server <- function(input, output) {
  
}

# run the app
shinyApp(ui = ui, server = server)





### WIDGETS ####


## add elements inside 
# define UI and elements
ui <- fluidPage(
  
  # general page 
  titlePanel('Title Panel'), 
  
  # sidebar and "content" 
  sidebarLayout(
    
  # sidebar title  
  sidebarPanel('Sidebar Panel'),
  
  # content title 
  # possible to change following HTML tags
  mainPanel(
    h1('H1 Main Page Panel'), 
    h3('H3 Main Page Title'), 
    
    p('a main page paragraph of text.'), 
    strong("strong() makes bold text."),
    em("em() creates italicized (i.e, emphasized) text."),
    br(),
    code("code displays your text similar to computer code"),
    div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", 
        style = "color:blue"),
    br(),
    p("span does the same thing as div, but it works with",
      span("groups of words", style = "color:blue"), 
      "that appear inside a paragraph."),
  
  # default on the left
  position = 'left', 
  
  # first row of buttons
  fluidRow(
    
    # first column of buttons 
    # ** actually order L --> R 
    column(3,
           h3("Buttons"),
           actionButton("action", "Action"),
           br(),
           br(),
           submitButton("Submit")),
    
    column(3,
           h3("Single checkbox"),
           checkboxInput("checkbox", "Choice A", value = TRUE)),
    
    column(3,
           checkboxGroupInput("checkGroup",
                              h3("Checkbox group"),
                              choices = list("Choice 1" = 1,
                                             "Choice 2" = 2,
                                             "Choice 3" = 3),
                              selected = 1)),
    
    column(3,
           dateInput("date",
                     h3("Date input"),
                     value = "2014-01-01"))
  ),
  
  fluidRow(
    
    column(3,
           dateRangeInput("dates", h3("Date range"))),
    
    column(3,
           fileInput("file", h3("File input"))),
    
    column(3,
           h3("Help text"),
           helpText("Note: help text isn't a true widget,",
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),
    
    column(3,
           numericInput("num",
                        h3("Numeric input"),
                        value = 1))
  ),
  
  fluidRow(
    
    column(3,
           radioButtons("radio", h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)),
    
    column(3,
           selectInput("select", h3("Select box"),
                       choices = list("Choice 1" = 1, "Choice 2" = 2,
                                      "Choice 3" = 3), selected = 1)),
    
    column(3,
           sliderInput("slider1", h3("Sliders"),
                       min = 0, max = 100, value = 50),
           sliderInput("slider2", "",
                       min = 0, max = 100, value = c(25, 75))
    ),
    
    column(3,
           textInput("text", h3("Text input"),
                     value = "Enter text..."))
  )
  
  )))


# define server logic 
server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)




### COOL EXAMPLE/INSPO ####



library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"


df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric), -Year)

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Bill Length (mm)"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Bill Depth (mm)"),
    checkboxGroupInput(
      "species", "Filter by species",
      choices = unique(df$Species), 
      selected = unique(df$Species)
    ),
    hr(), # Add a horizontal rule
    checkboxInput("by_species", "Show species", TRUE),
    checkboxInput("show_margins", "Show marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  plotOutput("scatter")
)


server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    df |> filter(Species %in% input$species)
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_species) aes(color = Species),
      geom_point(),
      if (input$smooth) geom_smooth()
    )
    
    if (input$show_margins) {
      margin_type <- if (input$by_species) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                               size = 8, groupColour = input$by_species, groupFill = input$by_species)
    }
    
    p
  }, res = 100)
}

shinyApp(ui, server)



library(shiny)
runExample("01_hello")



#------------------


# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load data --------------------------------------------------------------------

load("movies.RData")
n_total <- nrow(movies)


# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  
  # create layout with sidebar and main area
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      # Drop-down selector
      selectInput(
        # input name 
        inputId = "y",
        # displayed label 
        label = "Y-axis:",
        # drop-down choices
        choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
        # selected choice
        selected = "audience_score"),
      
      # Select variable for x-axis
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "critics_score"),
      
      # Select variable for color
      selectInput(inputId = "z",
                  label = "Color by:",
                  choices = c("title_type", "genre", "mpaa_rating", "critics_rating", "audience_rating"),
                  selected = "mpaa_rating"), 
      
      # command that regulates transparency of plotted points
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ), 
      
      # Show data table
      checkboxInput(inputId = "show_table",
                    label = "Show data table",
                    value = TRUE)
      
    ),
    
    # Output: Show scatterplot
    mainPanel(
      
      # here use plotOutput to show the plot 
      # connected to renderPlot used in the server 
      plotOutput(outputId = "scatterplot"), 
      
      dataTableOutput(outputId = 'movietable')
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  # define element with info under output
  # add render* depending on object displayed
  # here renderPlot displays obj created in UI thru plotOutput
  output$scatterplot <- renderPlot({
    
    # normal ggplot code but with *input* from UI
    ggplot(data = movies, aes_string(x = input$x, y = input$y,
                                     color = input$z)) +
      
      # takes transparency selector of UI
      geom_point(alpha = input$alpha)
  }) 
  
  # Print data table if checked
  output$movietable <- renderDataTable({
    
    
    if(input$show_table == TRUE){
      
      datatable(data = movies %>% select(1:7),
                options = list(pageLength = 15),
                rownames = FALSE)
    }
  })
  
  
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)







### APP ####
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- fluidPage(
  fluidRow(
    column(width=12,
           withSpinner(tableOutput('tb'), type = 2)
    ))
)

server <- function(input, output, session) {
  
  output$tb <- renderTable({
    Sys.sleep(3) # system sleeping for 3 seconds for demo purpose
    iris[1:5,]
  })
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)





library(shiny)

ui <- fluidPage(
  actionButton("go", "Go"),
  shinycssloaders::withSpinner(
    plotOutput("plot"), 
    type = 4, 
    color = 'firebrick', 
    size = 4
  )
)
server <- function(input, output) {
  output$plot <- renderPlot({
    input$go
    Sys.sleep(1.5)
    plot(runif(10))
  })
}
shinyApp(ui, server)


server <- function(input, output) {
  
  bs_themer()
  
  ...
}

shinyApp(ui, server)

