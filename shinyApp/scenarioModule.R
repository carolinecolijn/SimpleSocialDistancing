library(shiny)
library(ggplot2)

# Module UI function for one scenario and the initial values 
scenarioModuleUI <- function(id, scenarioLetter, status, communityInit, schoolInit, workInit, hhEffInit) {
  ns <- NS(id)

  tagList(
    box(
      title = paste("Scenario", scenarioLetter), width = NULL, status = status, solidHeader = TRUE, collapsible = TRUE,
      h4("Contact Reduction"),
      sliderInput(ns("communityReduction"), label = "Community:", min = 0, max = 100, post = " %", value = communityInit),
      sliderInput(ns("schoolReduction"), label = "School:", min = 0, max = 100, post = " %", value = schoolInit),
      sliderInput(ns("workReduction"), label = "Work:", min = 0, max = 100, post = " %", value = workInit),
      h4("Household Contact Efficiency"),
      sliderInput(ns("hhEff"), "", min = 0, max = 200, post = " %", value = hhEffInit),
      h4("Remaining Contacts (f)"),
      verbatimTextOutput(ns("f"), placeholder = TRUE)
    ),

    box(
      title = "Distancing Intervals", width = NULL, status = status, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      div(style = "text-align: center;", actionButton(ns("addInterval"), " Add Social Distance Interval", icon = icon("plus", class = NULL, lib = "font-awesome"), width = "100%")),


      uiOutput(ns("socInterval1"))
    ),

    box(
      width = NULL, status = status, solidHeader = TRUE, collapsible = FALSE,
      div(style = "text-align: center;", actionButton(ns("apply"), label = paste("Run Scenario", scenarioLetter), icon = icon("play"), width = "100%"))
    ),

    box(
      status = status, width = NULL, solidHeader = TRUE,
      withSpinner(plotOutput(ns("fractionInfectious")))
    ),

    box(
      status = status, width = NULL, solidHeader = TRUE,
      withSpinner(plotOutput(ns("cumlativeInfected")))
    )
  )
}

scenarioModule <- function(input, output, session, plotsEnd, R0, N, i0Init, startDate, reps, communityContact, schoolContact, workContact, householdContact, applyInit, plotColor) {
  currIntervals <- c()
  i <- 0

  ns <- session$ns


  output$f <- renderText({
    c(((householdContact() * 0.01) * (input$hhEff  * 0.01) + (schoolContact() * 0.01) * (1 - (input$schoolReduction  * 0.01)) + (workContact() * 0.01) * (1 - (input$workReduction  * 0.01)) + (communityContact() * 0.01) * (1 - (input$communityReduction  * 0.01))) * 100, "%")
  })


  data <- eventReactive(c(input$apply , applyInit()),
    {
      f <- (householdContact() * 0.01) * (input$hhEff  * 0.01) + (schoolContact() * 0.01) * (1 - (input$schoolReduction  * 0.01)) + (workContact() * 0.01) * (1 - (input$workReduction  * 0.01)) + (communityContact() * 0.01) * (1 - (input$communityReduction  * 0.01))

      N <- N() * 10^6

      times <- 0:(as.numeric(plotsEnd() - Sys.Date()))


      i0 <- i0Init()
      nReps <- reps()

      pars <- list(N = N, D = 5, R0 = R0(), k1 = 1 / 4, k2 = 1, q = 0, r = 1, ur = 0.8, f = f)
      fsi <- with(pars, r / (r + ur))
      nsi <- 1 - fsi
      state <- c(
        S = nsi * (N - i0), E1 = 0.4 * nsi * i0, E2 = 0.1 * nsi * i0, I = 0.5 * nsi * i0, Q = 0, R = 0,
        Sd = fsi * (N - i0), E1d = 0.4 * fsi * i0, E2d = 0.1 * fsi * i0, Id = 0.5 * fsi * i0, Qd = 0, Rd = 0
      )

      # prepare social distance intervals
      start <- sapply(1:length(currIntervals), function(i) {
        start <- input[[paste("dateRange", i, sep = "")]][1]
        ifelse(is.null(start), 0, as.numeric(as.Date.character(start) - Sys.Date()))
      })

      end <- sapply(1:length(currIntervals), function(i) {
        end <- input[[paste("dateRange", i, sep = "")]][2]
        ifelse(is.null(end), 200, as.numeric(as.Date.character(end) - Sys.Date()))
      })


      # determine when social distance is happening
      timing <- function(t) {
        ifelse(any(t > start & t < end), 1, 0)
      }

      tt2 <- multisolve(params = pars, timing = timing, state, times, nReps = nReps)

      tt2
    },
    ignoreNULL = FALSE
  )

  output$fractionInfectious <- renderPlot({
    applyInit()
    input$apply 
    
    isolate({

      N <- N() * 10^6
      times <- 0:(as.numeric(plotsEnd() - Sys.Date()))
      mydf <- getAllCasesbyDay2(data (), times, nReps, startDate = ymd(startDate()))
  
      ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
        geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = plotColor) +
        theme_bw() + ylab("Fraction infectious") + ylim(c(0, 0.3)) + theme(text = element_text(size = 15))
      
    })
  })

  output$cumlativeInfected <- renderPlot({
    applyInit()
    input$apply
    
    isolate({

      N <- N() * 10^6
      times <- 0:(as.numeric(plotsEnd() - Sys.Date()))
      mydf <- getEverInfbyDay(data (), times, nReps, startDate = ymd(startDate()))
  
      ggplot(data = mydf) + geom_line(aes(x = dates, y = median / N)) +
        geom_ribbon(aes(x = dates, ymin = lower25 / N, ymax = upper75 / N), alpha = 0.5, fill = plotColor) +
        theme_bw() + ylab("Cumulative infected") + ylim(c(0, 1)) + theme(text = element_text(size = 15))
      
    })
  })

  observeEvent(input$addInterval,{
      i <<- i + 1
      currIntervals[length(currIntervals) + 1] <<- i
      output[[paste("socInterval", i, sep = "")]] <- renderUI({
        list(
          fluidPage(
            fluidRow(
              tags$table(
                id = "inputs-table",
                style = "width: 100%",
                tags$tr(
                  tags$td(
                    style = "width: 90%",
                    dateRangeInput(ns(paste("dateRange", i, sep = "")),
                      label = paste("Interval", i, "Dates"),
                      start = Sys.Date(), end = Sys.Date() + 200, min = Sys.Date()
                    )
                  ),
                  tags$td(
                    style = "width: 10%; text-align: right",
                    if (i != 1) {
                      div(
                        class = "form-group shiny-input-container",
                        actionButton(ns(paste("removeFactor", i, sep = "")), "",
                          icon = icon("times", class = NULL, lib = "font-awesome"),
                          onclick = sprintf('Shiny.onInputChange("%s", %d)', session$ns("remove"), i)
                        )
                      )
                    }
                  )
                )
              )
            )
          ),
          uiOutput(ns(paste("socInterval", i + 1, sep = "")))
        )
      })
    },
    ignoreNULL = FALSE,
    priority = 0
  )

  observeEvent(input$remove, {
    i <- input$remove

    currIntervals <<- currIntervals[-which(currIntervals == i, arr.ind = TRUE)]

    output[[paste("socInterval", i, sep = "")]] <- renderUI({
      uiOutput(ns(paste("socInterval", i + 1, sep = "")))
    })
  })

  return(reactive({
    data ()
  }))
}
