library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinycssloaders)
library(markdown)

# load distance model functions 
source("socialDistanceModel.R")
# load shiny module for creating each scenario 
source("scenarioModule.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Social Distancing Scenarios",
    disable = TRUE
  ),
  dashboardSidebar(disable = TRUE),
  
  # change the status color to match plot colors
  dashboardBody(
    tags$style(HTML("
      .box.box-solid.box-primary>.box-header {
         color:#fff;
         background:#829AE1
      }

      .box.box-solid.box-primary{
        border-bottom-color:#829AE1;
        border-left-color:#829AE1;
        border-right-color:#829AE1;
        border-top-color:#829AE1;
      }

      .box.box-solid.box-warning>.box-header {
        color:#fff;
        background:#929292
      }
      
      .box.box-solid.box-warning{
        border-bottom-color:#929292;
        border-left-color:#929292;
        border-right-color:#929292;
        border-top-color:#929292;
      }

      .box.box-solid.box-info>.box-header {
        color:#fff;
        background:#46A546
      }
      
      .box.box-solid.box-info{
        border-bottom-color:#46A546;
        border-left-color:#46A546;
        border-right-color:#46A546;
        border-top-color:#46A546;
      }
      
      .box.box-solid.box-danger>.box-header {
        color:#fff;
        background:#3C8DBC
      }
      
      .box.box-solid.box-danger{
        border-bottom-color:#3C8DBC;
        border-left-color:#3C8DBC;
        border-right-color:#3C8DBC;
        border-top-color:#3C8DBC;
      }
      
      .box.box-solid.box-success{
        border-bottom-color:#3C8DBC;
        border-left-color:#3C8DBC;
        border-right-color:#3C8DBC;
        border-top-color:#3C8DBC;
      }
      
      .box.box-solid.box-success>.box-header h3.box-title {
         font-size: 25px;
         font-family: 'Source Sans Pro';
        font-weight; bold;
      }


      .box.box-solid.box-success>.box-header{
        background: #3C8DBC;
        color: #ffffff;
      }
      
      .box.box-solid.box-success{
        color: #000000;
      }
      
      
      #inputs-table {
        border-collapse: collapse;
      }

      #inputs-table td {
        padding: 10px;
        vertical-align: bottom;
      }

  ")),
    
    fluidRow(
      box(
        title = "Social Distancing Scenarios", status = "success", width = 12, solidHeader = TRUE,
        includeMarkdown("about.md")
      ),
      
      box(
        title = "Initial Scenario", status = "danger", width = 12, solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        column(
          6,
          dateInput("plotsEnd", "Plot End Date:", value = Sys.Date() + 200),
          numericInput("R0",
                       strong("R0:"),
                       value = 2.5,
                       min = 0.1
          ),
          
          numericInput("N",
                       strong("Population Size (million):"),
                       value = 2.4,
                       min = 0.1
          ),
          
          numericInput("i0Init",
                       strong("Initial Infectious Individuals:"),
                       value = 700,
                       min = 1
          ),
          dateInput("startDate", "Start Date:"),
          
          numericInput("reps",
                       strong("Repetitions:"),
                       value = 25
          )
        ),
        column(
          6,
          h4("Division of Contact"),
          sliderInput("communityContact", label = "Community:", min = 0, max = 100, post = " %", value = 35),
          sliderInput("schoolContact", label = "School:", min = 0, max = 100, post = " %", value = 10),
          sliderInput("workContact", label = "Work:", min = 0, max = 100, post = " %", value = 30),
          sliderInput("householdContact", label = "Household:", min = 0, max = 100, post = " %", value = 25)
        ),
        div(style = "text-align: center;", actionButton("applyInit", label = "Apply Changes", icon = icon("check"), width = "100%"))
      ),
      
      column(
        4,
        scenarioModuleUI("a","A","warning",0,0,0,100) # last four inputs are the reduction amounts 
      ),
      
      column(
        4,
        scenarioModuleUI("b","B","primary",25,10,35,100)
      ),
      
      column(
        4,
        scenarioModuleUI("c","C","info",65,90,65,145)
      ),
      
      tabBox(
        title = "Scenario Comparisons ", width = 12,
        tabPanel("Long Term", withSpinner(plotOutput("compareLong"))),
        tabPanel("Short Term", withSpinner(plotOutput("compareShort"))),
        
        fluidRow(
          align = "center",
          
          radioButtons("typeCompare", "Type:",
                       c("total symptomatic" = "symp",
                         "total infectious, symptomatic and pre-symptomatic cases" = "all",
                         "total cases throughout simulation" = "ever"), width = "100%", inline = TRUE, selected = "all"),
          
          
          checkboxInput("popScale","Scale by Population", value = TRUE)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  aData <- callModule(scenarioModule, "a",reactive({ input$plotsEnd }),reactive({ input$R0 }),reactive({ input$N }),reactive({ input$i0Init }),reactive({ input$startDate }),reactive({ input$reps }),reactive({ input$communityContact }),reactive({ input$schoolContact }),reactive({ input$workContact }),reactive({ input$householdContact }),reactive({ input$applyInit }),"grey") 
  bData <- callModule(scenarioModule, "b",reactive({ input$plotsEnd }),reactive({ input$R0 }),reactive({ input$N }),reactive({ input$i0Init }),reactive({ input$startDate }),reactive({ input$reps }),reactive({ input$communityContact }),reactive({ input$schoolContact }),reactive({ input$workContact }),reactive({ input$householdContact }),reactive({ input$applyInit }),"blue")
  cData <- callModule(scenarioModule, "c",reactive({ input$plotsEnd }),reactive({ input$R0 }),reactive({ input$N }),reactive({ input$i0Init }),reactive({ input$startDate }),reactive({ input$reps }),reactive({ input$communityContact }),reactive({ input$schoolContact }),reactive({ input$workContact }),reactive({ input$householdContact }),reactive({ input$applyInit }),"green")
  
  
  comparePlots <- eventReactive(c(input$applyInit,input$typeCompare,input$popScale,aData(),bData(),cData()),{
        allScenarioData <- list(aData(), bData(), cData())
        
        names(allScenarioData) <- c("A", "B", "C")
        
        makePlots(allScenarioData, 0:(as.numeric(input$plotsEnd - Sys.Date())), type = input$typeCompare, PopScale = input$popScale, popSize = input$N * 10^6, startDate = ymd(input$startDate))
    },
    ignoreNULL = FALSE
  )
  
  output$compareLong <- renderPlot({
    res <- comparePlots()
    res[[1]]
  })
  
  
  output$compareShort <- renderPlot({
    res <- comparePlots()
    res[[2]]
  })
  
}


shinyApp(ui = ui, server = server)
