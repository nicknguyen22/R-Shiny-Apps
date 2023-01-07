shinyUI
(

  navbarPage("Assignment 2 - Hieu Nguyen", position = "fixed-top", theme = shinytheme("flatly"),
    navbarMenu("Data Preparation", icon = icon("database"),
        tabPanel("Raw data",
                 br(),br(),br(),br(),
          tabsetPanel(
            tabPanel("Loading data",
                sidebarLayout(
                  sidebarPanel(
                    width = 3,
                    br(),
                    h4("File loading options"),
                    "Assignment 2 dataset is preloaded", 
                    br(),br(),
                    fileInput('rawfile', 'Choose CSV File to load',
                              accept = c('.csv')),
                    checkboxInput(inputId = "header", "Load column names", value=T),
                    checkboxInput(inputId = "factor", "Load strings as factors", value=T),
                    br(),
                    "Select of enter missing value placeholder to be convert to NA",
                    selectizeInput(inputId = "navalue", label = NULL, 
                                choices=c("blank"=" ","NA","N/A","--","-1"= -1, "-99" = -99), 
                                selected = c("NA","N/A","--","",-1,-99),
                                multiple = T,
                                options = list(create = T)),
                    br(),
                    "Select ID variable to be converted to character type",
                     selectizeInput(inputId = "convrt", label = NULL, 
                                   choices=NULL, 
                                   selected = NULL,
                                   multiple = F)
                  ),
                       
                  mainPanel(width = 9,
                    br(),br(),
                    withSpinner(DTOutput("rdata"))
                    )
                       
                )
              ),
              
            
            tabPanel("Summary",
                     h4("Summary"),
                     verbatimTextOutput(outputId = "summary"),
                     h4("Structure of dataframe"),
                     verbatimTextOutput(outputId = "str"),
                     h4("Descriptive statistic"),
                     verbatimTextOutput(outputId = "desc")
            ),

            
            tabPanel("Overview dataframe",
                     h4("Overview"),
                     verbatimTextOutput(outputId = "overview")
            ),
            
            tabPanel("Overview chart",
                     h4("Overview chart"),
                     withSpinner(plotOutput("ovplot",height = 700))
            )
            
                  
          )
        ),
          


    tabPanel("Manual Imputation",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          br(),br(),br(),
          h4("Not Applicable Imputation"),
          br(),
          selectInput(inputId = "numVar", label = "Select numerical variables:", 
                      choices = NULL, selected = NULL,multiple = T
          ),
          "with missing values to create shadow variable and impute by 0",
          br(),br(),

          selectInput(inputId = "caVar", label = "Select factor variables:", 
                      choices = NULL, selected = NULL,multiple = T
          ),
          "with missing values to create shadow variable and impute by NONE",
          br(),br(),br(),
          "Impute factor variable with custom value",
          selectInput(inputId = "faVar", label = "Select factor variables:", 
                      choices = c("CODE","POLITICS","HEALTHCARE_BASIS"), 
                      selected = c("POLITICS"),multiple = F
          ),
          textInput(inputId = "cusVa", label = "Enter custom value:", value = "OTHER"
          )
        ),
                
                mainPanel(width = 9,
                          br(),br(),br(),br(),
                          h4("Review"),
                          br(),
                          withSpinner(DTOutput("impData"))
                )
                
      )
  ),
     
    tabPanel("Missingness Investigations",
      br(),br(),br(),br(),
      h4("Variable missing value correlation"),
      br(),
      withSpinner(plotOutput("corrChart",height = 500)),
      hr(),
      h4("Predicting the missingness"),
      br(),
      withSpinner(plotOutput("rpChart"))

    )
  ),
    
    navbarMenu("EDA", icon = icon("chart-simple"),
               
               tabPanel("Missing Values",
                        titlePanel("Missing Value Visualisation"),
                        fluidRow(
                          column(2,checkboxInput(inputId = "sortMiss", label = "Sort by missingness", value = F)),
                          column(2,checkboxInput(inputId = "scluster", label = "Cluster missingness", value = F))
                        ),
                        withSpinner(plotOutput("mvdChart",height = 500)),
                        hr(),
                        fluidRow(
                          column(3,sliderInput("set", "Number of sets:", min = 1, max = 15, value = 11)),
                          column(3,sliderInput("intsec", "Number of intersecs:", min = 1, max = 50, value = 10))
                        ),
                        withSpinner(plotOutput("usChart",height = 800))
               ),
               
               tabPanel("Boxplot Chart",
                        br(),br(),br(),
                        titlePanel("Boxplot of numeric variables"),
                        fluidRow(
                          column(6,selectizeInput(inputId = "VariablesBP", label = "Variables to show:", 
                                                  width = 500,
                                                  choices = NULL, multiple = T, 
                                                  selected =  NULL  )),
                          column(4,sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)),
                          column(2,checkboxInput(inputId = "standardise", label = "Show standardized", value = T),
                                 checkboxInput(inputId = "notch", label = "Show snotch", value = F),
                                 checkboxInput(inputId = "outliers", label = "Show outliers", value = T))
                        ),
                        withSpinner(
                          plotOutput(outputId = "Boxplot",height = 700)
                        )
               ),
               
               tabPanel("Correlation Matrix",
                        br(),br(),br(),
                        titlePanel("Correlation Matrix of numeric variables"),
                        fluidRow(
                          column(5,selectizeInput(inputId = "VariablesC", label = "Variables to show:",
                                                  width = 500,
                                                  choices = NULL, multiple = T,
                                                  selected = NULL)),
                          column(2,selectInput(inputId = "CorrMeth", label = "Correlation method",
                                               choices = c("Pearson"="pearson","Spearman"="spearman","Kendall"="kendall"),
                                               selected = "pearson")),
                          column(2,selectInput(inputId = "Group", label = "Grouping method",
                                               choices = list("none" = F,"OLO" = "OLO","GW" = "GW","HC" = "HC"),
                                               selected = "OLO")),
                          column(3,checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = T))
                        ),
                        withSpinner(
                          plotOutput(outputId = "cormat", height = 700)
                        )
               ),

               tabPanel("Histogram Chart",
                        br(),br(),br(),
                        titlePanel("Histogram of numeric variables"),
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       selectInput(inputId = "VariablesH", label = "Variables to plot:",
                                                   choices = NULL, multiple = F,
                                                   selected = NULL)
                          ),

                          mainPanel(width = 10,
                                    withSpinner(
                                      plotOutput(outputId = "Histogram",height = 800)

                                    ))
                        )
               ),



               tabPanel("Matplots Chart",
                        br(),br(),br(),
                        titlePanel("Matplots of numeric variables"),
                        fluidRow(
                          column(10,selectizeInput(inputId = "VariablesMP", label = "Variables to show:",
                                                   width = 900,
                                                   choices = NULL, multiple = T,
                                                   selected = NULL)),
                          column(2,checkboxInput(inputId = "centerMP", label = "Center", value = F),
                                 checkboxInput(inputId = "scaleMP", label = "Scale", value = F))
                        ),
                        withSpinner(
                          plotOutput(outputId = "Matplot", height = 700)
                        )
               ),

               tabPanel("Mosaic Chart",
                        br(),br(),br(),
                        titlePanel("Mosaic of categorical variables"),
                        selectizeInput(inputId = "VariablesM", label = "Variables to show:",
                                       width = 1200,
                                       choices = NULL, multiple = T,
                                       selected = NULL ),
                        withSpinner(
                          plotOutput(outputId = "Mosaic",height = 700)
                        )
               ),

               tabPanel("Pairs Chart",
                        br(),br(),br(),
                        titlePanel("Pairs chart of all variables"),
                        fluidRow(
                          column(10,selectizeInput(inputId = "VariablesP", label = "Variables to show:",
                                       width = 1200,
                                       choices = NULL, multiple = T,
                                       selected = NULL)),
                          column(2,selectizeInput(inputId = "VariablesP1", label = "Color by:",
                                                  choices = NULL, multiple = F,
                                                  selected = NULL))),
                        withSpinner(
                          plotOutput(outputId = "Pairs",height = 900)
                        )
               ),

               tabPanel("Rising Value Chart",
                        br(),br(),br(),
                        titlePanel("Rising Value of numeric variables"),
                        column(2,br(),br(),br(),
                               selectizeInput(inputId = "VariablesRV", label = "Variables to show:",
                                              choices = NULL, multiple = T,
                                              selected = NULL  ),
                               checkboxInput(inputId = "centerRV", label = "Center", value = F),
                               checkboxInput(inputId = "scaleRV", label = "Scale", value = F)),
                        column(10,
                               withSpinner(
                                 plotOutput(outputId = "rising", height = 800)
                               ))
               ),

               tabPanel("Tabplot Chart",
                        br(),br(),br(),
                        titlePanel("Tabplot of all variables"),
                        selectizeInput(inputId = "VariablesS", label = "Variables to show:",
                                       width = 1200,
                                       choices = NULL, multiple = TRUE,
                                       selected = NULL),
                        withSpinner(
                          plotOutput(outputId = "Sequence",height = 700)
                        )
               )
    ),

    navbarMenu("Analysis", icon = icon("cogs"),
      tabPanel("Dataset Imputation",
        shinyjs::useShinyjs(),
        sidebarLayout(
          sidebarPanel(width = 3,br(),br(),br(),
            h4("Imputation options"),
            br(),
            sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                        min = 1, max = 100, value = 50, post = "%"),
            br(),
            sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness", 
                        min = 1, max = 100, value = 50, post = "%"),
            br(),
            selectInput(inputId = "ImpMethod", label = "Imputation method", 
                        choices = c("None", "KNN", "Partial Del"), selected = "KNN"),
            br(),
            actionButton(inputId = "impute", label = "Impute dataset", icon = icon("play"))
          ),
          
          mainPanel(width = 9,br(),br(),br(),br(),
            withSpinner(plotOutput(outputId = "msChart", height = 500)),
            withSpinner(verbatimTextOutput(outputId = "msCount")),
            withSpinner(verbatimTextOutput(outputId = "msCount1")),
            withSpinner(verbatimTextOutput(outputId = "msCount2")),
            hr(),
            h4("Imputed Train Data Summary"),
            withSpinner(verbatimTextOutput(outputId = "sumTrain")),
            h4("Imputed Test Data Summary"),
            withSpinner(verbatimTextOutput(outputId = "sumTest"))
          )
        )
      ),
      
      tabPanel("Model Training",
               shinyjs::useShinyjs(),
               sidebarLayout(
                 sidebarPanel(width = 3,br(),br(),br(),
                              h4("Training options"),
                              br(),

                              selectInput(inputId = "trainModel", label = "Training method", 
                                          choices = c("None", "Glmnet" = "glmnet"), selected = "glmnet"),
                              br(),
                              actionButton(inputId = "train", label = "Training model", icon = icon("play"))
                 ),
                 
                 mainPanel(width = 9,br(),br(),br(),br(),
                           withSpinner(verbatimTextOutput(outputId = "modelSum")),
                           withSpinner(verbatimTextOutput(outputId = "trainRMSE")),
                           withSpinner(plotOutput(outputId = "modelplot"))
                 )
               )
      ),
      
      tabPanel("Model Prediction",
        sidebarLayout(
          sidebarPanel(width = 3,br(),br(),br(),br(),
            h4("RMSE (based on test data)"),
            withSpinner(verbatimTextOutput(outputId = "preResult")),
            br(),br(),
            h4("Data point detail"),
            "(click on scatter plot for detail)",
            withSpinner(verbatimTextOutput("info"))
          ),

        mainPanel(width = 8,br(),br(),br(),br(),
          withSpinner(plotOutput(outputId = "preplot", click = "plot_click",height = 800))
        )
        )  
      ),
      
      tabPanel("Residual Visualisation",
        sidebarLayout(
          sidebarPanel(width = 3,br(),br(),br(),br(),
          h4("Train data IQR"),
          sliderInput(inputId = "iqr", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
          br(),br(),br(),br(),br(),br(),br(),
          h4("Test data IQR"),
          sliderInput(inputId = "iqr1", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
          br(),br(),br(),br(),br(),br(),br(),
          h4("Train & Test data IQR"),
          sliderInput(inputId = "iqr2", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)
          ),
          
          mainPanel(width = 9,br(),br(),br(),br(),
            h4("Prediction residual boxplot of Train data"),
            withSpinner(plotOutput(outputId = "Rboxplot",height = "200")),
            br(),hr(),
            h4("Prediction residual boxplot of Test data"),
            withSpinner(plotOutput(outputId = "Rboxplot1",height = "200")),
            br(),hr(),
            h4("Prediction residual boxplot of Train & Test data"),
            withSpinner(plotOutput(outputId = "Rboxplot2",height = "200"))
          
          )
        )
      
      )
    )

  )
)


