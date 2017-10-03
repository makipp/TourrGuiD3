

shinyUI(fluidPage(
  includeScript(path = "js-checkbox.js"),
  titlePanel("Welcome to the TourR Shiny app powered by D3.js"),
  conditionalPanel(
    "input.type == 'Guided'",
    selectInput(
      "guidedIndex",
      "Index function",
      c("Holes", "Centre Mass", "LDA", "PDA")
      ,
      selected = "LDA"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        
        "type",
        label = "Select tour type",
        choices = c("Guided", "Little", "Grand"),
        selected = "Grand"
      ),
      radioButtons(
        "density",
        label = "Density Plots",
        choices = c("On", "Off"),
        selected = "Off"
      ),
      radioButtons(
        "dataset",
        label ="Select Example Dataset",
        choices = c("Gaussian", "Geozoo", "Cognostics"),
        selected = "Geozoo"
      ),
      #radioButtons(
      #  "axes",
      #  label = "Select axis locations",
      #  choices = c("Centre", "Bottom Left", "Off")
      #),
      # radioButtons(
      #   "type",
      #   label = "Select visualisation type",
      #   choices = c("2D Scatter", "2D Density"),
      #   selected = "2D Scatter"
      # ),
      sliderInput(
        "speed",
        label =  "Tour speed",
        min = 0,
        max = 5,
        value = 1,
        step = 0.1
      ) ,
      actionButton("restart_random", "Restart tour with random basis"),
      selectInput("class",
                  choices = vector('character'), label = "Select class variable to colour the points"),
      checkboxGroupInput(
        "variables",
        label = "Choose variables for the 2D tour",
        choices = vector('character'),
        selected = vector('character')
      )
    ),
    mainPanel(
      tags$div(tags$p(" "),
               ggvisOutput("ggvis")),
      tags$div(tags$p(textOutput("type"))),
      tags$script(src = "https://d3js.org/d3.v4.min.js"),
      tags$script(src = "https://d3js.org/d3-contour.v1.min.js"),
      tags$script(src = "https://d3js.org/d3-scale-chromatic.v1.min.js"),
      tags$div(id = "d3_output"),
      tags$div(id = "d3_output_2"),
      tags$div(id = "info"),
      tags$div(id = "info_2"),
      tags$script(src = "d3anim.js"))
  )
))
