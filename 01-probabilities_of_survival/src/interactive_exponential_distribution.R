#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(cowplot)
library(flexsurv)

GompertzHazard <- function (x, a, b) {
  a*exp(b*x)
}
GompertzSurvival <- function (x, a, b) {
  if (b != 0) {
    exp(-a/b*(exp(b*x)-1))  
  } else {
    exp(-a*x)
  }
}
GompertzQuantile <- function (p, a, b) {
  flexsurv::qgompertz(p, shape = b, rate = a)
}

# define user interface for plotting survival quantities
ui <- fluidPage(

    # Application title
    titlePanel("Exponential & Gompertz Distributions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("rate",
                        "Rate (x 100,000)",
                        min = 0,
                        max = 3000,
                        value = 1500),
            sliderInput("b",
                        "Relative derivatve",
                        min = -0.05,
                        max = 0.05,
                        value = 0)
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

      rate <- input$rate/1e5
      b <- input$b
      
      quantiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)
      quantile_values <- GompertzQuantile(quantiles, a = rate, b = b)
      names(quantile_values) <- quantiles
      quantile_values[is.infinite(quantile_values)] <- NA
      
      plot_Sx <-
        ggplot() +
        geom_function(
          fun = GompertzSurvival,
          args = list(a = rate, b = b)
        ) +
        annotate('point', x = quantile_values[2:4], y = 1-quantiles[2:4]) +
        annotate(
          'segment',
          x = quantile_values[2:4], xend = quantile_values[2:4],
          y = 0, yend = 1-quantiles[2:4],
          lty = 3
        ) +
        annotate(
          'text', x = quantile_values[2:4], y = 0,
          label = formatC(quantile_values[2:4], format = 'f', digits = 1),
          hjust = 1.1, vjust = -0.5, size = 3.5
        ) +
        scale_x_continuous(limits = c(0, 100), expand = c(0,0)) +
        scale_y_continuous(limits = c(0, 1), expand = c(0,0)) +
        labs(x = 'Time x', y = 'Survival probability') +
        theme_classic() +
        coord_cartesian(clip = 'off')
      
      plot_hx <-
        ggplot() +
        geom_function(
          fun = GompertzHazard,
          args = list(a = rate, b = b)
        ) +
        scale_x_continuous(limits = c(0, 100),
                           expand = c(0,0)) +
        scale_y_continuous(limits = c(0, 0.03), expand = c(0,0),
                           labels = scales::label_number(scale = 1e5)) +
        labs(x = 'Time x', y = 'Hazard rate (x 100,000)') +
        theme_classic() +
        coord_cartesian(clip = 'off')
      
      cowplot::plot_grid(plot_hx, plot_Sx, nrow = 2)
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
