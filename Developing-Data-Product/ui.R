shinyUI(fluidPage(
  titlePanel("Regression model: visualisation / summary of the mtcars data set"),
  fluidRow(
    column(12,
           h3("Usage: Select the Regressand variable and one or more regressors. Various characteristisc about the created linear model will be displayed or plotted. ")    
    )
  ),
  fluidRow(
    column(12,
           h3(textOutput('model'))    
    )
  ),
  fluidRow(
    column(6,
           selectInput('inRegressors', 'Regressors', c("Miles/(US) gallon (mpg)" = "mpg",
                                                       "Number of cylinders (cyl)" = "cyl",
                                                       "Displacement (cu.in.)  (disp)" = "disp",
                                                       "Gross horsepower  (hp)" = "hp",
                                                       "Rear axle ratio  (drat)" = "drat",
                                                       "Weight (lb/1000)  (wt)" = "wt",
                                                       "1/4 mile time  (qsec)" = "qsec",
                                                       "V/S Engine  (vs)" = "vs",
                                                       "Transmission (0 = automatic, 1 = manual)  (am)" = "am",
                                                       "Number of forward gears  (gear)" = "gear",
                                                       "Number of carburetors  (carb)" = "carb"
           ), multiple=TRUE, selected = c("wt","hp"),selectize=FALSE)      
           
    ),
    column(6,
           selectInput('regressand', 'Regressand variable', c("Miles/(US) gallon (mpg)" = "mpg",
                                                              "Number of cylinders (cyl)" = "cyl",
                                                              "Displacement (cu.in.)  (disp)" = "disp",
                                                              "Gross horsepower  (hp)" = "hp",
                                                              "Rear axle ratio  (drat)" = "drat",
                                                              "Weight (lb/1000)  (wt)" = "wt",
                                                              "1/4 mile time  (qsec)" = "qsec",
                                                              "V/S Engine  (vs)" = "vs",
                                                              "Transmission (0 = automatic, 1 = manual)  (am)" = "am",
                                                              "Number of forward gears  (gear)" = "gear",
                                                              "Number of carburetors  (carb)" = "carb"
           ), multiple=FALSE, selectize=FALSE)
    )
  ),
  fluidRow(
    column(12,
           h3("Model details")      
    )
  ),
  fluidRow(
    column(6,
           h4("Summary"),
           verbatimTextOutput("summary")      
    ),
    column(6,
           h4("Residuals should be uncorelated to model variables"),
           verbatimTextOutput("correlation"),
           h4("Mean of residuals should be around 0"),
           verbatimTextOutput("mean"),
           h4("Shapiro test: are model residuals normal distrbuted?"),
           verbatimTextOutput("shapiro")
    )
  ),
  fluidRow(
    column(12,
           h3("Diagnostic plots")      
    )
  ), 
  fluidRow(
    column(6,
           plotOutput('plot1')      
    ),
    column(6,
           plotOutput('plot2')
    )
  ),
  fluidRow(
    column(6,
           plotOutput('plot3')      
    ),
    column(6,
           plotOutput('plot4')
    )
  )
  
)
)
