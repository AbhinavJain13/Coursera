library(shiny)
data(mtcars)



shinyServer(
  function(input, output, session) {
    #compute the formula for regressors, for example am + hp
    regressors <- reactive({
      do.call(paste, c(as.list(input$inRegressors), sep=" + "))
    })
    #compute the final linear model formula 
    formulaText <- reactive({
      paste(input$regressand,regressors() , sep=' ~ ')
    })
    #linear model
    fit <- reactive({ 
      lm(as.formula(formulaText()) , data = mtcars)
    })
    #display model fomrula
    output$model <- renderText({
      paste("Model: ",formulaText(), sep = " ")
    })
    # plot diagnostics
    output$plot1 <- renderPlot({
      plot(fit(), which = 1)
    })
    # plot diagnostics
    output$plot2 <- renderPlot({      
      plot(fit(), which = 2)
    })
    # plot diagnostics
    output$plot3 <- renderPlot({
      plot(fit(), which = 3)
      
    })
    # plot diagnostics
    output$plot4 <- renderPlot({
      plot(fit(), which = 4)
    })
    # display model summary
    output$summary <- renderPrint({
      summary(fit())
    })
    #display correlation between residuals and model variables
    output$correlation <- renderPrint({
      cor(fit()$residuals, fit()$model)
    })
    # dispaly shapiro test results
    output$shapiro <- renderPrint({
      shapiro.test(fit()$residuals)
    })
    #display mean of residuals
    output$mean <- renderPrint({
      mean(fit()$residuals)
    })
    
  }
)
