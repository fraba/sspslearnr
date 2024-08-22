
library(shiny)
library(ggplot2)
library(MASS)

# Define UI for application that draws a histogram
ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(
            sliderInput("points",
                        "Number of points:",
                        min = 500,
                        max = 1000,
                        value = 750),
            sliderInput("mean_x",
                        "Mean of X:",
                        min = -100,
                        max = 100,
                        value = 0,
                        step = 1),
            sliderInput("mean_y",
                        "Mean of Y:",
                        min = -100,
                        max = 100,
                        value = 0,
                        step = 1),
            sliderInput("sd_x",
                        "Standard Deviation (SD) of X:",
                        min = -1,
                        max = 40,
                        value = 0,
                        step = 1),
            sliderInput("sd_y",
                        "Standard Deviation (SD) of Y:",
                        min = -1,
                        max = 40,
                        value = 0,
                        step = 1),
            sliderInput("cor",
                        "Correlation of X, Y:",
                        min = -1,
                        max = 1,
                        value = 0,
                        step = .1)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           htmlOutput('sdZero'),
           htmlOutput('sdError'),
           htmlOutput('cor_and_slope'),
           htmlOutput('means_and_sds'),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create a reactive expression to store the data
  shared_data <- reactive({
    # Generate random data based on input values
    cov_matrix <- matrix(c(1, input$cor, input$cor, 1), nrow = 2)
    
    cov_matrix <- 
      matrix(c(input$sd_x^2, input$cor * input$sd_x * input$sd_y, 
               input$cor * input$sd_x * input$sd_y, input$sd_y^2), 
             nrow = 2)
    
    simulated_data <-
      mvrnorm(n = input$points, 
              mu = c(input$mean_x, input$mean_y), 
              Sigma = cov_matrix)
    
    data <- as.data.frame(simulated_data)
    names(data) <- c("x", "y")
    
    return(data)
  })
  
  # Plot the data using ggplot
  output$distPlot <- renderPlot({
    
    model <- lm(data = shared_data(), y ~ x)
    slope <- coef(model)[2]
    intercept <- coef(model)[1]
    
    ggplot(shared_data(), aes(x = x, y = y)) +
      geom_point(size = 1, alpha = .5) +
      scale_x_continuous(limits = c(-200, +200)) +
      scale_y_continuous(limits = c(-200, +200)) +
      theme_minimal() +
      geom_vline(xintercept = 0) +
      geom_hline(yintercept = 0) +
      geom_abline(slope = slope, 
                  intercept = intercept, color = "blue", size = 1, 
                  linetype = "dashed")
  })
  
  # Display correlation between x and y
  output$cor_and_slope <- renderText({
    sprintf("<b>Correlation</b>: %s; <b>Slope</b>: %s", 
            round(cor(shared_data()$x, shared_data()$y), 3),
            round(coef(lm(data = shared_data(), y ~ x))[2], 3))
  })
  
  # Display means and SDs
  output$means_and_sds <- renderText({
    sprintf("<b>Mean X</b>: %s; <b>Mean Y</b>: %s;<br><b>SD X</b>: %s, <b>SD Y</b>: %s",
            round(mean(shared_data()$x), 3),
            round(mean(shared_data()$y), 3),
            round(sd(shared_data()$x), 3),
            round(sd(shared_data()$y), 3))
  })
    
    sdError_text <- reactive({
      ifelse(input$sd_x < 0 | input$sd_y < 0,
             "The SD can't be negative!",
             '') 
    })
    
    output$sdError <- renderText(sdError_text()) 
    
    sdZero_text <- reactive({
      ifelse(input$sd_x == 0 | input$sd_y == 0,
             "When the SD is zero all points have the same value (so your variable is not really a variable!).<br>Also if SD is zero, you can't calculate the correlation.<br><b>Try to change SD on both variables</b>.",
             '') 
    })
    
    output$sdZero <- renderText(sdZero_text()) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
