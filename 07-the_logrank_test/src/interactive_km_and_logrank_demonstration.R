# Interactive Graphical Demonstrations for Survival Analysis

# Init ------------------------------------------------------------

library(shiny)
library(ggplot2)
library(patchwork)
library(flexsurv)
library(survival)
library(rgl)
library(shinyRGL)

# Data ------------------------------------------------------------

bc <-
  data.frame(
    positive = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    weeks = c(19, 25, 30, 
              34, 37, 46, 47, 51, 56, 57, 61, 66, 67, 74, 78, 86, 122, 123, 
              130, 130, 133, 134, 136, 141, 143, 148, 151, 152, 153, 154, 156, 
              162, 164, 165, 182, 189, 22, 23, 38, 42, 73, 77, 89, 115, 144),
    censored = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 0, 0, 0, 0, 0, 0, 0, 0, 1)
  )

# Functions -------------------------------------------------------

GompertzHazard <- function (x, a, b) {
  a*exp(b*x)
}
GompertzSurvival <- function (x, a, b, lg = FALSE) {
  if (b != 0) {
    if (lg)  { Sx <- -a/b*(exp(b*x)-1) }
    if (!lg) { Sx <- exp(-a/b*(exp(b*x)-1)) }
  }
  if (b == 0) {
    if (lg)  { Sx <- -a*x }
    if (!lg) { Sx <- exp(-a*x) }
  }
  return(Sx)
}
GompertzQuantile <- function (p, a, b) {
  flexsurv::qgompertz(p, shape = b, rate = a)
}
GompertzDensity <- function (x, a, b) {
  flexsurv::dgompertz(x, shape = b, rate = a)
}
GompertzLogDensity <- function (x, a, b) {
  flexsurv::dgompertz(x, shape = b, rate = a, log = TRUE)
}
GompertzSample <- function (n, a, b) {
  flexsurv::rgompertz(n, shape = b, rate = a)
}

# UI --------------------------------------------------------------

# define user interface for plotting survival quantities
ui <- fluidPage(
  
    # Application title
    titlePanel("Kaplan-Meier Estimator and Logrank test"),

    sidebarLayout(
        sidebarPanel(
          checkboxInput("showtrue", label = 'Show truth', value = TRUE),
          conditionalPanel(
            "input.showtrue == true",
            sliderInput("true_rate",
                        "Baseline rate (x 100,000)",
                        min = 0,
                        max = 3000,
                        value = 1500),
            sliderInput("true_slope",
                        "Baseline log-derivative",
                        min = -0.05,
                        max = 0.05,
                        value = 0),
            sliderInput("true_hr",
                        "Hazard ratio blue vs. red",
                        min = 0.5,
                        max = 2,
                        value = 1)
          ),
          actionButton('randomize', 'Randomize Parameters!', width = '100%'),
          actionButton('sample', 'Draw Sample!', width = '100%'),
          sliderInput("sample_size",
                      "Sample size",
                      min = 1,
                      max = 100,
                      value = 10),
          checkboxInput("censoring",
                      "Enable Censoring",
                      value = FALSE),
          checkboxInput("showguess", label = 'Show guess', value = FALSE),
          conditionalPanel(
            "input.showguess == true",
            sliderInput("guess_rate",
                        "Guess rate",
                        min = 0,
                        max = 3000,
                        value = 1500),
            sliderInput("guess_b",
                        "Guess derivative",
                        min = -0.05,
                        max = 0.05,
                        value = 0)
          ),
          checkboxInput("showkm", label = 'Show Kaplan-Meier', value = FALSE),
          checkboxInput('showll', label = 'Show Log-likelihood', value = FALSE),
          checkboxInput("showlr", label = 'Show Logrank test', value = FALSE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "tabset",
            tabPanel("Distribution", plotOutput("distPlot", height = '600px')),
            tabPanel("Kaplan Meier", tableOutput('km_table')),
            tabPanel("Likelihood contributions", tableOutput('table')),
            tabPanel("Likelihood surface",
                     webGLOutput('llsurface', width = '400px',
                                 height = '400px'))
          ),
          conditionalPanel(
            "input.showll == true",
            h5('Log-likelihood'),
            verbatimTextOutput('loglike', placeholder = TRUE)
          ),
          conditionalPanel(
            "input.showlr == true",
            verbatimTextOutput('logrank', placeholder = TRUE)
          )
        )
    )
)

# Server ----------------------------------------------------------

server <- function(input, output, session) {

  cnst <- list(
    rate_scale = 1e5,
    min_rate = 0,
    max_rate = 3000,
    min_b = -0.05,
    max_b = 0.05
  )
  
  v <- reactiveValues()

  observeEvent(input$randomize, {
    updateSliderInput(session, 'true_rate',
                      value = runif(1, cnst$min_rate, cnst$max_rate))
    updateSliderInput(session, 'true_slope',
                      value = runif(1, cnst$min_b, cnst$max_b))
  })
  
  observeEvent(input$sample, {
    v$samples <-
      c(
        GompertzSample(input$sample_size,
                       input$true_rate/cnst$rate_scale,
                       input$true_slope),
        GompertzSample(input$sample_size,
                       input$true_rate/cnst$rate_scale*input$true_hr,
                       input$true_slope)
      )

    v$censored <- rep(0, input$sample_size*2)
    if (input$censoring == TRUE) {
      cens <- runif(input$sample_size*2, min = 0, max = 100)
      v$censored <- ifelse(cens < v$samples, 1, 0)
      v$samples[v$censored==1] <- cens[v$censored == 1]      
    }

    v$strata <- rep(c(0, 1), each = input$sample_size)
      
    km <- survfit(Surv(v$samples, event = 1-v$censored, type = 'right')~v$strata)
    v$km <- km[['surv']]
    v$x <- km[['time']]
    v$atrisk <- km[['n.risk']]
    v$event <- km[['n.event']]
    v$logrank <- survdiff(Surv(v$samples, event = 1-v$censored, type = 'right')~v$strata)
  })
  
  params <- reactive({
    l <- list(
      true_a = input$true_rate/cnst$rate_scale,
      true_b = input$true_slope,
      guess_a = input$guess_rate/cnst$rate_scale,
      guess_b = input$guess_b,
      true_hr = input$true_hr
    )
    l$samples <- v$samples
    l$censored <- as.integer(v$censored)
    l$strata <- v$strata
    l$x <- v$x
    l$km <- v$km
    l$atrisk <- v$atrisk
    l$event <- v$event
    l$logrank <- v$logrank
    
    if (is.null(v$samples)) {
      l$samples <- as.numeric(NA)
      l$censored <- as.numeric(NA)
    }
    l$logdensity <-
      GompertzLogDensity(
        l$samples, l$guess_a, l$guess_b
      )
    l$logsurvival <- GompertzSurvival(l$samples, l$guess_a, l$guess_b, lg = TRUE)
    l$loglikelihood <-
      (1-l$censored)*l$logdensity + (l$censored)*l$logsurvival
    rate = seq(cnst$min_rate, cnst$max_rate, length.out = 100)/cnst$rate_scale
    b = seq(cnst$min_b, cnst$max_b, length.out = 100)
    l$loglikelihoodsurface <-
      matrix(NA, nrow = 100, ncol = 100)
    for (i in 1:100) {
      for (j in 1:100) {
        l$loglikelihoodsurface[i,j] <-
          sum(GompertzLogDensity(l$samples, rate[i], b[j]))
        l$loglikelihoodsurface[is.infinite(l$loglikelihoodsurface)] <- NA
      }
    }
    l
  })
  
  output$distPlot <- renderPlot({
    
    plot_fx <-
      ggplot() +
      list(
        if (input$showtrue) {
          list(
            geom_function(
              fun = GompertzDensity, color = 'red',
              args = list(a = params()$true_a, b = params()$true_b)
            ),
            geom_function(
              fun = GompertzDensity, color = 'blue',
              args = list(a = params()$true_a*params()$true_hr, b = params()$true_b)
            )
          )
        } else { NULL },
        if (input$showguess) {
        geom_function(
          fun = GompertzDensity,
          args = list(a = params()$guess_a, b = params()$guess_b),
          color = 'blue', lty = 2
        )} else { NULL },
        annotate(
          'point', shape = 1, color = 'red',
          x = params()$samples[params()$censored==0&params()$strata==0],
          y = 0.001
        ),
        annotate(
          'point', shape = 3, color = 'red',
          x = params()$samples[params()$censored==1&params()$strata==0],
          y = 0.001
        ),
        annotate(
          'point', shape = 1, color = 'blue',
          x = params()$samples[params()$censored==0&params()$strata==1],
          y = 0.001
        ),
        annotate(
          'point', shape = 3, color = 'blue',
          x = params()$samples[params()$censored==1&params()$strata==1],
          y = 0.001
        ),
        scale_x_continuous(limits = c(0, 100), expand = c(0,0)),
        scale_y_continuous(limits = c(0, 0.03), expand = c(0,0)),
        labs(x = 'Time x', y = 'f(x)'),
        theme_classic(),
        coord_cartesian(clip = 'off')
      )
    
    plot_hx <-
      ggplot() +
      list(
        if (input$showtrue) {
          list(
            geom_function(
              fun = GompertzHazard, color = 'red',
              args = list(a = params()$true_a, b = params()$true_b)
            ),
            geom_function(
              fun = GompertzHazard, color = 'blue',
              args = list(a = params()$true_a*params()$true_hr, b = params()$true_b)
            )
          )
        } else { NULL },
        if (input$showguess) {
        geom_function(
          fun = GompertzHazard,
          args = list(a = params()$guess_a, b = params()$guess_b),
          color = 'blue', lty = 2
        )} else { NULL },
        scale_x_continuous(limits = c(0, 100),
                           expand = c(0,0)),
        scale_y_continuous(limits = c(0, 0.1), expand = c(0,0),
                           labels = scales::label_number(scale = 1e5)),
        labs(x = 'Time x', y = 'h(x) (x 100,000)'),
        theme_classic(),
        coord_cartesian(clip = 'off')
      )
    
    plot_sx <-
      ggplot() +
      list(
        if (input$showtrue) {
          list(
            geom_function(
              fun = GompertzSurvival, color = 'red',
              args = list(a = params()$true_a, b = params()$true_b)
            ),
            geom_function(
              fun = GompertzSurvival, color = 'blue',
              args = list(a = params()$true_a*params()$true_hr, b = params()$true_b)
            )
          )
        } else { NULL },
        if (input$showkm) {
          list(
            annotate(
              'step', x = params()$x[params()$strata == 0],
              y = params()$km[params()$strata == 0], color = 'red'
            ),
            annotate(
              'step', x = params()$x[params()$strata == 1],
              y = params()$km[params()$strata == 1], color = 'blue'
            )
          )
        } else { NULL },
        if (input$showguess) {
          geom_function(
            fun = GompertzSurvival,
            args = list(a = params()$guess_a, b = params()$guess_b),
            color = 'blue', lty = 2
          )} else { NULL },
        scale_x_continuous(limits = c(0, 100),
                           expand = c(0,0)),
        scale_y_continuous(limits = c(0, 1), expand = c(0,0)),
        labs(x = 'Time x', y = 'S(x)'),
        theme_classic(),
        coord_cartesian(clip = 'off')
      )
    
    plot_hx + plot_fx + plot_sx +
      patchwork::plot_layout(nrow = 3)
    
  })
  
  output$likelihood <- renderPrint({
    
    loglikelihood <- sum(params()$loglikelihood)
    loglikelihood
    
  })
  
  output$table <- renderTable({
    
    data.frame(
      `Stratum` = ifelse(params()$strata == 0, 'Red', 'Blue'),
      `Observation x` = params()$samples,
      `Censored` = params()$censored,
      `Log-density` = params()$logdensity,
      `Log-survival` = params()$logsurvival,
      `Log-likelihood contribution` = params()$loglikelihood
    )
    
  })
  
  
  output$km_table <- renderTable({
    
    data.frame(
      `Stratum` = ifelse(params()$strata == 0, 'Red', 'Blue'),
      `Time x` = params()$x,
      `Event` = params()$event,
      `At risk` = params()$atrisk,
      `Survival` = params()$km
    )
    
  })
  
  output$llsurface <- renderWebGL({
    
    persp3d(
      x = params()$loglikelihoodsurface,
      zlim = c(-1000, max(params()$loglikelihoodsurface, na.rm = TRUE)),
      col = 'lightblue', box = FALSE, 
    )
    
  })
  
  output$logrank <- renderPrint({
    
    params()$logrank
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
