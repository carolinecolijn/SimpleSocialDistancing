library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinycssloaders)
library(markdown)


source("socialDistanceModel.R")


ui <- dashboardPage(
  dashboardHeader(
    title = "Social Distancing Scenarios",
    disable = TRUE,
    titleWidth = 500
  ),
  dashboardSidebar(disable = TRUE),

  # change the status colors
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
        background:#3C8DBC
      }
      
      .box.box-solid.box-warning{
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
      
    

  ")),

    fluidRow(
      box(
        status = "success", width = 12,
        solidHeader = TRUE, title = "Interactive Social Distance Modelling",

        column(
          6,

          img(
            src = "plots.svg",
            height = "100%", width = "100%", align = "center"
          )
        ),

        column(
          6,
          includeMarkdown("about.md")
        )
      ),

      box(
        height = "100%",
        title = "Initial Settings", width = 12, solidHeader = TRUE, status = "warning",
        collapsible = TRUE, collapsed = TRUE,

        column(
          6,

          dateInput("startDate", "Plot Start Date:", value = Sys.Date()),

          dateInput("plotsEnd", "Plot End Date:", value = Sys.Date() + 240),

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
            value = 1000,
            min = 1
          ),

          numericInput("reps",
            strong("Repetitions:"),
            min = 2,
            value = 20
          )
        ),
        column(
          6,
          h4("Division of Contact"),
          sliderInput("communityContact", label = "Community:", min = 0, max = 100, post = " %", value = 35),
          sliderInput("schoolContact", label = "School:", min = 0, max = 100, post = " %", value = 10),
          sliderInput("workContact", label = "Work:", min = 0, max = 100, post = " %", value = 30),
          sliderInput("householdContact", label = "Household:", min = 0, max = 100, post = " %", value = 25),
        ),
        column(
          12,
          div(style = "text-align: center;", actionButton("applyInit", label = "Apply Changes", icon = icon("check"), width = "100%"))
        )
      )
    ),

    fluidRow(
      ##### Section 1 UI #####
      box(
        status = "success", width = 12,
        title = "COVID19 dynamics with simple assumptions", solidHeader = TRUE,
        p("Change the sliders to explore how the model reacts to different levels of remaining contacts and fractions of the population practicing distancing. This section has the fraction of remaining contacts fixed throughout the simulation.")
      ),

      column(
        4,

        box(
          width = 13, status = "success", solidHeader = TRUE,
          h4("Social Distancing Contacts"),
          sliderInput("f", label = "Fraction Remaining Contacts (f):", min = 1, max = 100, post = " %", value = 55),
          sliderInput("u", label = "Fraction Practicing Distancing:", min = 1, max = 100, post = " %", value = 65),
        ),

        box(
          width = 13, status = "success", solidHeader = TRUE, collapsible = FALSE,
          div(style = "text-align: center;", actionButton("run1", label = "Run Model", icon = icon("play"), width = "100%"))
        )
      ),

      tabBox(
        title = " ", width = 8,
        tabPanel("Fraction infectious", withSpinner(plotOutput("fractionInfectious1", height = "273px"))),
        tabPanel("Cumulative infected", withSpinner(plotOutput("cumlativeInfected1", height = "273px")))
      ),

      ##### Section 2 UI #####
      box(
        status = "success", width = 12,
        title = "Modelling distancing with simple assumptions", solidHeader = TRUE, 
        p("Here the fraction of contacts remaining is calculated from reduction in different areas.",
          tags$b("This assumes a certain division of contact between community, school, work and household contacts. 
                 We do not know these percentages, to change the assumptions see the initial settings."),
          "The social distancing also ends at a fixed time, after which 100% of contacts will remain.")
      ),

      column(
        4,

        box(
          width = 13, status = "success", solidHeader = TRUE,
          h4("Contact Reduction"),
          sliderInput("communityReduction2", label = "Community:", min = 0, max = 100, post = " %", value = 20),
          sliderInput("schoolReduction2", label = "School:", min = 0, max = 100, post = " %", value = 80),
          sliderInput("workReduction2", label = "Work:", min = 0, max = 100, post = " %", value = 50),
        ),
        box(
          width = 13, status = "success", solidHeader = TRUE,
          sliderInput("hEff2", label = "Household Contact Efficiency:", min = 0, max = 200, post = " %", value = 100),
        ),
        box(
          width = 13, status = "success", solidHeader = TRUE,
          sliderInput("lengthDistancing", "Length of social distancing (weeks)", min = 1, max = 52, value = 20),
        ),
        valueBoxOutput("endDateDist2", width = 13)
      ),

      column(
        8,

        valueBoxOutput("f2", width = 13),

        box(
          width = 13, status = "success", solidHeader = TRUE, collapsible = FALSE,
          div(style = "text-align: center;", actionButton("run2", label = "Run Model", icon = icon("play"), width = "100%"))
        ),

        tabBox(
          title = " ", width = 13,
          tabPanel("Fraction infectious", withSpinner(plotOutput("fractionInfectious2", height = "505px"))),
          tabPanel("Cumulative infected", withSpinner(plotOutput("cumlativeInfected2", height = "505px")))
        )
      ),

      ##### Section 3 UI #####
      box(
        status = "success", width = 12,
        title = "What happens when distancing is itermittent?", solidHeader = TRUE, 
        p("This section has two values for the fraction of remaining contacts (f). One with a scenario of full distancing and the second a more relaxed scenario. 
          Each scenario takes place for an input number of weeks and then repeats, starting with the full distancing.",
          tags$b("Again this assumes a certain division of contact between community, school, work and household contacts, to change the assumptions see the initial settings."))
      ),

      column(
        6,
        box(
          width = 13, status = "success", solidHeader = TRUE,
          h4("Full Distancing Contact Reduction"),
          sliderInput("communityReduction3a", label = "Community:", min = 0, max = 100, post = " %", value = 65),
          sliderInput("schoolReduction3a", label = "School:", min = 0, max = 100, post = " %", value = 90),
          sliderInput("workReduction3a", label = "Work:", min = 0, max = 100, post = " %", value = 75)
        ),
        box(
          width = 13, status = "success", solidHeader = TRUE,
          sliderInput("hEff3a", label = "Full Household Contact Efficiency:", min = 0, max = 200, post = " %", value = 150),
        ),
        valueBoxOutput("f3a", width = 13)
      ),
      column(
        6,
        box(
          width = 13, status = "success", solidHeader = TRUE,
          h4("Relaxed Distancing Contact Reduction"),
          sliderInput("communityReduction3b", label = "Community:", min = 0, max = 100, post = " %", value = 0),
          sliderInput("schoolReduction3b", label = "School:", min = 0, max = 100, post = " %", value = 0),
          sliderInput("workReduction3b", label = "Work:", min = 0, max = 100, post = " %", value = 0),

          
        ),
        box(
          width = 13, status = "success", solidHeader = TRUE,
          sliderInput("hEff3b", label = "Relaxed Household Contact Efficiency:", min = 0, max = 200, post = " %", value = 100),
        ),
        valueBoxOutput("f3b", width = 13)
      ),

      box(
        height = "225px",
        width = 8, status = "success", solidHeader = TRUE,


        sliderInput("dateIntFull", "Full Social Distance Interval (weeks)", min = 1, max = 52, value = 5),
        sliderInput("dateIntRelax", "Relaxed Social Distance Interval (weeks)", min = 1, max = 52, value = 4)
      ),

      valueBoxOutput("endInterval3a", width = 4),

      valueBoxOutput("endInterval3b", width = 4),


      box(
        width = 12, status = "success", solidHeader = TRUE, collapsible = FALSE,
        div(style = "text-align: center;", actionButton("run3", label = "Run Model", icon = icon("play"), width = "100%"))
      ),


      tabBox(
        title = " ", width = 12,
        tabPanel("Fraction infectious", withSpinner(plotOutput("fractionInfectious3"))),
        tabPanel("Cumulative infected", withSpinner(plotOutput("cumlativeInfected3")))
      )
    )
  )
)

server <- function(input, output, session) {

  ##### Section 1 Server #####
  data1 <- eventReactive(c(input$run1, input$applyInit),{
      f <- (input$f) * 0.01

      N <- input$N * 10^6

      times <- 0:(as.numeric(input$plotsEnd - input$startDate))

      i0 <- input$i0Init
      nReps <- input$reps

      pars <- list(N = input$N * 10^6, D = 5, R0 = input$R0, k1 = 1 / 4, k2 = 1, q = 0, r = 1, ur = (1 - (input$u * 0.01)) / ((input$u * 0.01)), f = f)

      # fraction social isolating
      fsi <- with(pars, r / (r + ur))

      # not isolating
      nsi <- 1 - fsi

      # calculate states
      state <- c(
        S = nsi * (N - i0), E1 = 0.4 * nsi * i0, E2 = 0.1 * nsi * i0, I = 0.5 * nsi * i0, Q = 0, R = 0,
        Sd = fsi * (N - i0), E1d = 0.4 * fsi * i0, E2d = 0.1 * fsi * i0, Id = 0.5 * fsi * i0, Qd = 0, Rd = 0
      )

      # determine when social distance is happening
      timing <- function(t) {
        1
      }


      multisolve(params = pars, timing = timing, state, times, nReps = nReps)
    },
    ignoreNULL = FALSE
  )



  output$cumlativeInfected1 <- renderPlot({
    input$applyInit
    input$run1

    isolate({
      N <- input$N * 10^6
      times <- 0:(as.numeric(input$plotsEnd - input$startDate))
      mydf <- getEverInfbyDay(data1(), times, nReps, startDate = ymd(input$startDate))

      ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
        geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "grey") +
        theme_bw() + ylab("Cumulative infected") + ylim(c(0, 1)) + theme(text = element_text(size = 15))
    })
  })

  output$fractionInfectious1 <- renderPlot({
    input$applyInit
    input$run1

    isolate({
      N <- input$N * 10^6
      times <- 0:(as.numeric(input$plotsEnd - input$startDate))
      mydf <- getAllCasesbyDay2(data1(), times, nReps, startDate = ymd(input$startDate))

      if (max(mydf$median / N) > 0.1) {
        ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
          geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "grey") +
          theme_bw() + ylab("Fraction infectious") + ylim(c(0, 0.3))
      } else {
        ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
          geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "grey") +
          theme_bw() + ylab("Fraction infectious")
      }
    })
  })


  ##### Section 2 Server #####
  data2 <- eventReactive(c(input$run2, input$applyInit),{
      f <- (input$householdContact * 0.01) * (input$hEff2  * 0.01) + (input$schoolContact * 0.01) * (1 - (input$schoolReduction2 * 0.01)) + (input$workContact * 0.01) * (1 - (input$workReduction2 * 0.01)) + (input$communityContact * 0.01) * (1 - (input$communityReduction2 * 0.01))

      N <- input$N * 10^6

      times <- 0:(as.numeric(input$plotsEnd - input$startDate))

      i0 <- input$i0Init
      nReps <- input$reps

      pars <- list(N = input$N * 10^6, D = 5, R0 = input$R0, k1 = 1 / 4, k2 = 1, q = 0, r = 1, ur = 1/3, f = f)

      # fraction social isolating
      fsi <- with(pars, r / (r + ur))

      # not isolating
      nsi <- 1 - fsi

      # calculate states
      state <- c(
        S = nsi * (N - i0), E1 = 0.4 * nsi * i0, E2 = 0.1 * nsi * i0, I = 0.5 * nsi * i0, Q = 0, R = 0,
        Sd = fsi * (N - i0), E1d = 0.4 * fsi * i0, E2d = 0.1 * fsi * i0, Id = 0.5 * fsi * i0, Qd = 0, Rd = 0
      )

      # determine when social distance is happening
      timing <- function(t) {
        ifelse(t > 0 & t < (input$lengthDistancing * 7), 1, 0)
      }


      multisolve(params = pars, timing = timing, state, times, nReps = nReps)
    },
    ignoreNULL = FALSE
  )



  output$cumlativeInfected2 <- renderPlot({
    input$applyInit
    input$run2

    isolate({
      N <- input$N * 10^6
      times <- 0:(as.numeric(input$plotsEnd - input$startDate))
      mydf <- getEverInfbyDay(data2(), times, nReps, startDate = ymd(input$startDate))

      ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
        geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "blue") +
        theme_bw() + ylab("Cumulative infected") + ylim(c(0, 1)) + theme(text = element_text(size = 15))
    })
  })

  output$fractionInfectious2 <- renderPlot({
    input$applyInit
    input$run2

    isolate({
      N <- input$N * 10^6
      times <- 0:(as.numeric(input$plotsEnd - input$startDate))
      mydf <- getAllCasesbyDay2(data2(), times, nReps, startDate = ymd(input$startDate))

      if (max(mydf$median / N) > 0.1) {
        ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
          geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "blue") +
          theme_bw() + ylab("Fraction infectious") + ylim(c(0, 0.3))
      } else {
        ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
          geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "blue") +
          theme_bw() + ylab("Fraction infectious")
      }
    })
  })


  output$endDateDist2 <- renderValueBox({
    valueBox(
      format(input$startDate + input$lengthDistancing * 7, "%b %d, %Y"),
      width = 13,
      "End date of social distancing", icon = icon("calendar-times")
    )
  })


  output$f2 <- renderValueBox({
    valueBox(
      paste(((input$householdContact * 0.01)  * (input$hEff2  * 0.01) + (input$schoolContact * 0.01) * (1 - (input$schoolReduction2 * 0.01)) + (input$workContact * 0.01) * (1 - (input$workReduction2 * 0.01)) + (input$communityContact * 0.01) * (1 - (input$communityReduction2 * 0.01))) * 100, "%"),
      "Fraction Remaining Contacts (f)",
      icon = icon("user-friends")
    )
  })

  ##### Section 3 Server #####
  data3 <- eventReactive(c(input$run3, input$applyInit),{
      fFull <- ((input$householdContact * 0.01)  * (input$hEff3a  * 0.01) + (input$schoolContact * 0.01) * (1 - (input$schoolReduction3a * 0.01)) + (input$workContact * 0.01) * (1 - (input$workReduction3a * 0.01)) + (input$communityContact * 0.01) * (1 - (input$communityReduction3a * 0.01)))
      fRelax <- ((input$householdContact * 0.01)  * (input$hEff3b  * 0.01) + (input$schoolContact * 0.01) * (1 - (input$schoolReduction3b * 0.01)) + (input$workContact * 0.01) * (1 - (input$workReduction3b * 0.01)) + (input$communityContact * 0.01) * (1 - (input$communityReduction3b * 0.01)))

      N <- input$N * 10^6

      times <- 0:(as.numeric(input$plotsEnd - input$startDate))

      i0 <- input$i0Init
      nReps <- input$reps

      pars <- list(N = input$N * 10^6, D = 5, R0 = input$R0, k1 = 1 / 4, k2 = 1, q = 0, r = 1, ur = 1/3, f = fFull)

      # fraction social isolating
      fsi <- with(pars, r / (r + ur))

      # not isolating
      nsi <- 1 - fsi

      # calculate states
      state <- c(
        S = nsi * (N - i0), E1 = 0.4 * nsi * i0, E2 = 0.1 * nsi * i0, I = 0.5 * nsi * i0, Q = 0, R = 0,
        Sd = fsi * (N - i0), E1d = 0.4 * fsi * i0, E2d = 0.1 * fsi * i0, Id = 0.5 * fsi * i0, Qd = 0, Rd = 0
      )

      # determine when social distance is happening
      timing <- function(t) {

        # make t between 0 and full interval
        tTemp <- t %% (input$dateIntFull * 7 + input$dateIntRelax * 7)

        if (tTemp < input$dateIntFull * 7) {
          fFull
        } else {
          fRelax
        }
     
      }

      multisolve2(params = pars, timing = timing, state, times, nReps = nReps)
    },
    ignoreNULL = FALSE
  )



  output$cumlativeInfected3 <- renderPlot({
    input$applyInit
    input$run3

    isolate({
      N <- input$N * 10^6
      times <- 0:(as.numeric(input$plotsEnd - input$startDate))
      mydf <- getEverInfbyDay(data1(), times, nReps, startDate = ymd(input$startDate))

      ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
        geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "green") +
        theme_bw() + ylab("Cumulative infected") + ylim(c(0, 1)) + theme(text = element_text(size = 15))
    })
  })

  output$fractionInfectious3 <- renderPlot({
    input$applyInit
    input$run3

    isolate({
      N <- input$N * 10^6
      times <- 0:(as.numeric(input$plotsEnd - input$startDate))
      mydf <- getAllCasesbyDay2(data3(), times, nReps, startDate = ymd(input$startDate))

      if (max(mydf$median / N) > 0.1) {
        ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
          geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "green") +
          theme_bw() + ylab("Fraction infectious") + ylim(c(0, 0.3))
      } else {
        ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
          geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = "green") +
          theme_bw() + ylab("Fraction infectious")
      }
    })
  })

  output$f3a <- renderValueBox({
    valueBox(
      paste(((input$householdContact * 0.01) * (input$hEff3a  * 0.01) + (input$schoolContact * 0.01) * (1 - (input$schoolReduction3a * 0.01)) + (input$workContact * 0.01) * (1 - (input$workReduction3a * 0.01)) + (input$communityContact * 0.01) * (1 - (input$communityReduction3a * 0.01))) * 100, "%"),
      "Fraction Remaining Contacts Full (f)",
      icon = icon("user-friends")
    )
  })

  output$f3b <- renderValueBox({
    valueBox(
      paste(((input$householdContact * 0.01) * (input$hEff3b  * 0.01) + (input$schoolContact * 0.01) * (1 - (input$schoolReduction3b * 0.01)) + (input$workContact * 0.01) * (1 - (input$workReduction3b * 0.01)) + (input$communityContact * 0.01) * (1 - (input$communityReduction3b * 0.01))) * 100, "%"),
      "Fraction Remaining Contacts Relaxed (f)",
      icon = icon("user-friends")
    )
  })


  output$endInterval3a <- renderValueBox({
    valueBox(
      format(input$startDate + input$dateIntFull * 7, "%b %d, %Y"),
      width = 13,
      "End date of first full interval", icon = icon("calendar-plus")
    )
  })

  output$endInterval3b <- renderValueBox({
    valueBox(
      format(input$startDate + input$dateIntFull * 7 + input$dateIntRelax * 7, "%b %d, %Y"),
      width = 13,
      "End date of first relaxed interval", icon = icon("calendar-minus")
    )
  })
}


shinyApp(ui = ui, server = server)
