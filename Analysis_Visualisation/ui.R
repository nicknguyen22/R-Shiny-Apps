shinyUI
(
  navbarPage("Assignment 1 - Hieu Nguyen", theme = shinytheme("flatly"),
     
      navbarMenu("Dataset", icon = icon("folder"),
          tabPanel("Data table",
              titlePanel("Ass1Data Data Table"),
                  sidebarLayout(
                      sidebarPanel(
                          br(),
                          h4("Data Table"),
                          br(),
                          checkboxInput(inputId = "Rownames", "Show row names", value=TRUE),
                          checkboxInput(inputId = "Order", "Column ordering", value=TRUE),
                          selectInput(inputId = "Selection", "Selection type", choices=c("None"="none","Single"="single","Multiple"="multiple"), selected = "none"),
                          selectInput(inputId = "Filter", "Filter type", choices=c("None"="none","Bottom"="bottom","Top"="top"), selected = "none"),
                          selectInput(inputId = "Dom", "DOM options", choices=domChoices, multiple = TRUE, selected=domChoices),
                          width = 3,
                          br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
                                  ),
              
                      mainPanel(width = 9,
                          withSpinner(DTOutput("rdata"))
                                )
                      
                                )
                      ),
          
          tabPanel("Missing Values",
              titlePanel("Ass1Data Missing Value Assesment "),
                  sidebarLayout(
                     sidebarPanel(
                        h4("Overall Chart"),
                        width = 3,
                        selectInput(inputId = "mvcolor", label = "Color palette selection", 
                                    choices = c("Default" = "default", "Qual" = "qual", "ColorBlindness Safe"="cb_safe"), 
                                    selected = "default"),
                        
                        br(),br(),
                        h4("Breakdown Chart"),
                        selectizeInput(inputId = "VariablesMV", label = "Variables to show:", 
                                       choices = choiAll, multiple = TRUE, 
                                       selected = c("Y","sensor1","sensor2","sensor3")),
                        checkboxInput(inputId = "sortMiss", label = "Sort by missingness", value = FALSE),
                        checkboxInput(inputId = "scluster", label = "Cluster missingness", value = FALSE),
                                  ),
                     
                     mainPanel(width = 9,
                          withSpinner(plotOutput("mvChart")),
                          br(),
                          withSpinner(plotOutput("mvdChart"))
                              )
                     
                                )
                  ),
          tabPanel("Summary",
              titlePanel("Ass1Data Summary"),
                     tabsetPanel(
                     tabPanel("All Variables",
                             h4("All Variables Summary"),
                              verbatimTextOutput(outputId = "SummaryA")
                     ),
                     
                     tabPanel("Categorical Variables",
                              h4("Categorical Variables Summary"),
                              verbatimTextOutput(outputId = "SummaryC1"),
                              verbatimTextOutput(outputId = "SummaryC")
                     ),
                     
                     tabPanel("Numeric Variables",
                              h4("Numeric Variables Summary"),
                              verbatimTextOutput(outputId = "SummaryN1"),
                              verbatimTextOutput(outputId = "SummaryN")
                     )
                    )
                  )
                  ),

      
      navbarMenu("Charts", icon = icon("chart-simple"),

          tabPanel("Boxplot Chart",
                   titlePanel("Boxplot of numeric variables"),
                   withSpinner(
                     plotOutput(outputId = "Boxplot",height = 700)
                   ),
                   fluidRow(
                     column(6,selectizeInput(inputId = "VariablesBP", label = "Variables to show:", 
                                            width = 500,
                                            choices = choiNum, multiple = TRUE, 
                                            selected =  choiNum  )),
                     column(4,sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)),
                     column(2,checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                              checkboxInput(inputId = "notch", label = "Show snotch", value = FALSE),
                              checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE))
                            )

                   ),
          
          tabPanel("Correlation Matrix",
                   titlePanel("Correlation Matrix of numeric variables"),
                   withSpinner(
                     plotOutput(outputId = "cormat", height = 700)
                   ),
                   fluidRow(
                      column(5,selectizeInput(inputId = "VariablesC", label = "Variables to show:", 
                                             width = 500,
                                             choices = choiNum, multiple = TRUE, 
                                             selected = choiNum  )),
                      column(2,selectInput(inputId = "CorrMeth", label = "Correlation method", 
                                           choices = c("Pearson"="pearson","Spearman"="spearman","Kendall"="kendall"), 
                                           selected = "pearson")),
                      column(2,selectInput(inputId = "Group", label = "Grouping method", 
                                           choices = list("none" = FALSE,"OLO" = "OLO","GW" = "GW","HC" = "HC"), 
                                           selected = "OLO")),
                      column(3,checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = TRUE))
                      ),

                   ),
          
          tabPanel("Histogram Chart",
                   titlePanel("Histogram of numeric variables"),
                   sidebarLayout(
                     sidebarPanel(width = 2,
                       selectInput(inputId = "VariablesH", label = "Variables to show:", 
                                   choices = choiNum, multiple = FALSE, 
                                   selected = c("Y"))
                       ),
                    
                       mainPanel(width = 10,
                         withSpinner(
                         plotOutput(outputId = "Histogram",height = 800)
                       
                     ))
                     )
                   ),

          
          
          tabPanel("Matplots Chart",
                   titlePanel("Matplots of numeric variables"),
                   withSpinner(
                     plotOutput(outputId = "Matplot", height = 700)
                   ),

                          fluidRow(
                            column(10,selectizeInput(inputId = "VariablesMP", label = "Variables to show:",
                                    width = 900,
                                    choices = choiNum, multiple = TRUE, 
                                    selected = choiNum  )),
                            column(2,checkboxInput(inputId = "centerMP", label = "Center", value = FALSE), 
                            checkboxInput(inputId = "scaleMP", label = "Scale", value = FALSE))
                          )

                  ),
          
          tabPanel("Mosaic Chart",
                   titlePanel("Mosaic of categorical variables"),
                   withSpinner(
                     plotOutput(outputId = "Mosaic",height = 700)
                   ),
                   selectizeInput(inputId = "VariablesM", label = "Variables to show:", 
                                  width = 1200,
                                  choices = choiFac, multiple = TRUE, 
                                  selected = c("Priority","Price","Speed")  )
                   

                   ),
          
          tabPanel("Pairs Chart",
                   titlePanel("Pairs of Ass1Data"),
                   withSpinner(
                     plotOutput(outputId = "Pairs",height = 700)
                   ),
                   selectizeInput(inputId = "VariablesP", label = "Variables to show:", 
                                  width = 1200,
                                  choices = choiAll, multiple = TRUE, 
                                  selected = c("Y","sensor1","sensor2","sensor3",
                                                       "sensor4","sensor5") )
                   

                   ),
          
          tabPanel("Rising Value Chart",
                   titlePanel("Rising Value of numeric variables"),

                   column(2,br(),br(),br(),
                          selectizeInput(inputId = "VariablesRV", label = "Variables to show:",
                                           choices = choiNum, multiple = TRUE, 
                                           selected = choiNum  ),
                          checkboxInput(inputId = "centerRV", label = "Center", value = FALSE), 
                          checkboxInput(inputId = "scaleRV", label = "Scale", value = FALSE)),
                   column(10,
                          withSpinner(
                     plotOutput(outputId = "rising", height = 800)
                   ))
          ),
          
          tabPanel("Tabplot Chart",
                   titlePanel("Tabplot of Ass1Data"),
                   withSpinner(
                     plotOutput(outputId = "Sequence",height = 700)
                   ),
                   selectizeInput(inputId = "VariablesS", label = "Variables to show:", 
                                  width = 1200,
                                  choices = choiAll, multiple = TRUE, 
                                  selected = c("Y","sensor1","sensor2","sensor3",
                                               "sensor4","sensor5") )
                   

          )
                  ),
      
            )
)
