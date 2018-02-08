#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/temp-windows/urban_ggplot_theme.R")

latoCSS <- "http://fonts.googleapis.com/css?family=Lato:300,400,700,900,300italic,400italic,700italic,900italic"

source("life-exp-lib.R", local=T)

getERA <- make.getERA("data/per0014.h17", "data/per1500.217")
getLFP <- make.getLFP("data/lfp.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = latoCSS)),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  tags$head(tags$base(target = "_blank")),  
  tags$head(tags$script(src = "pym.min.js")),
  
  
  theme = "shiny.css",
  
   # Application title
   titlePanel("Equivalent Retirement Age"),

   radioButtons("chart", 
                "Chart", 
                choices=c('Labor Force Participation Rate','Equivalent Retirement Age'),
                selected = 'Labor Force Participation Rate',
                inline=T),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("refyear",
                    "Reference Year:",
                    min = 1900,
                    max = 2100,
                    value = 2017,
                    sep=""),
        sliderInput("refage",
                    "Reference Age:",
                    min = 55,
                    max = 70,
                    value = 65,
                    sep=""),
        radioButtons("sex", 
                     "Sex", 
                     choices = c("Female","Male"),
                     selected = "Male",
                     inline = TRUE
        ),
        conditionalPanel(
          condition="input.chart=='Labor Force Participation Rate'",
          checkboxInput("showage", "Show Ages", value = FALSE)
        )
      ),
      
      # Show a plot
      mainPanel(
         plotOutput("eraPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$eraPlot <- renderPlot({
    if( input$chart == 'Labor Force Participation Rate' ) {
      plotERALFP(from=1963,to=2016,refage=input$refage,refyear=input$refyear,sex=input$sex, text=input$showage)
    }
    else {
      plotERA(refage=input$refage,refyear=input$refyear,sex=input$sex, from=1963, to=2016)
    }
  },
  height=800)
}

# Run the application
shinyApp(ui = ui, server = server)

