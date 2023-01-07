shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Hieu Nguyen"),
  tabsetPanel(
    tabPanel("Data",
             htmlOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("RF Model",
                                  verbatimTextOutput(outputId = "rf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rf_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rf_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rf_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rf_ModelPlots"),
                                  verbatimTextOutput(outputId = "rf_Recipe"),
                                  verbatimTextOutput(outputId = "rf_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "rf_VIplot")  #  <- typically this is specific to RF
                                  )
                         ),
                         tabPanel("QRF Model",
                                  verbatimTextOutput(outputId = "qrf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "qrf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(qrf_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = qrf_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "qrf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "qrf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "qrf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "qrf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "qrf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "qrf_ModelPlots"),
                                  verbatimTextOutput(outputId = "qrf_Recipe"),
                                  verbatimTextOutput(outputId = "qrf_ModelSummary2"),
                                  wellPanel(
                                    h3("Variable Importance"),
                                    plotOutput(outputId = "qrf_VIplot")  #  <- typically this is specific to QRF
                                  )
                         ),
                         
                         tabPanel("LM Model",
                                  verbatimTextOutput(outputId = "lm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "lm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lm_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "lm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "lm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "lm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "lm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lm_Metrics"),
                                  # hr(),
                                  # plotOutput(outputId = "lm_ModelPlots"),
                                  verbatimTextOutput(outputId = "lm_Recipe"),
                                  verbatimTextOutput(outputId = "lm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "lm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("RLM Model",
                                  verbatimTextOutput(outputId = "rlm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "rlm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rlm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rlm_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rlm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rlm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rlm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rlm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rlm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rlm_ModelPlots"),
                                  verbatimTextOutput(outputId = "rlm_Recipe"),
                                  verbatimTextOutput(outputId = "rlm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "rlm_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         

                         tabPanel("KNN Model",
                                  verbatimTextOutput(outputId = "knn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "knn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(knn_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = knn_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "knn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "knn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "knn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "knn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "knn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "knn_ModelPlots"),
                                  verbatimTextOutput(outputId = "knn_Recipe"),
                                  verbatimTextOutput(outputId = "knn_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "knn_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         
                         tabPanel("BAG Model",
                                  verbatimTextOutput(outputId = "bag_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "bag_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bag_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bag_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "bag_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bag_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "bag_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bag_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bag_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bag_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bag_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bag_Metrics"),
                                  hr(),
                                  # plotOutput(outputId = "bag_ModelPlots"),
                                  verbatimTextOutput(outputId = "bag_Recipe"),
                                  verbatimTextOutput(outputId = "bag_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "bag_Coef")
                                  )
                         ),
                         
                         tabPanel("NNET Model",
                                  verbatimTextOutput(outputId = "nnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "nnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(nnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = nnet_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "nnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "nnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "nnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "nnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "nnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "nnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "nnet_Recipe"),
                                  verbatimTextOutput(outputId = "nnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "nnet_Coef")
                                  )
                         ),
                         
                         tabPanel("SVMLinear Model",
                                  verbatimTextOutput(outputId = "svml_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svml_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svml_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svml_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svml_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svml_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svml_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svml_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svml_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svml_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svml_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svml_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svml_ModelPlots"),
                                  verbatimTextOutput(outputId = "svml_Recipe"),
                                  verbatimTextOutput(outputId = "svml_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "svml_Coef")
                                  )
                         ),
                         
                         
                         tabPanel("SVMRadial Model",
                                  verbatimTextOutput(outputId = "svmr_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmr_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmr_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmr_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmr_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmr_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmr_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmr_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmr_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmr_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmr_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmr_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmr_Recipe"),
                                  verbatimTextOutput(outputId = "svmr_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "svmr_Coef")
                                  )
                         ),
                         
                         tabPanel("SVMRadialCost Model",
                                  verbatimTextOutput(outputId = "svmrc_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmrc_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmrc_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmrc_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmrc_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmrc_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmrc_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmrc_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmrc_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmrc_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmrc_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmrc_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmrc_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmrc_Recipe"),
                                  verbatimTextOutput(outputId = "svmrc_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "svmr_Coef")
                                  )
                         ),
                         
                         tabPanel("SVMRadialSigma Model",
                                  verbatimTextOutput(outputId = "svmrs_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmrs_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmrs_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmrs_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmrs_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmrs_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmrs_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmrs_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmrs_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmrs_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmrs_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmrs_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmrs_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmrs_Recipe"),
                                  verbatimTextOutput(outputId = "svmrs_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "svms_Coef")
                                  )
                         ),
                         
                         
                         tabPanel("SVMPoly Model",
                                  verbatimTextOutput(outputId = "svmp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "svmp_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmp_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmp_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "svmp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmp_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "svmp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmp_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmp_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmp_Metrics"),
                                  hr(),
                                  #plotOutput(outputId = "svmp_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmp_Recipe"),
                                  verbatimTextOutput(outputId = "svmp_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "svmp_Coef")
                                  )
                         ),
                         
                         tabPanel("KRLSRadial Model",
                                  verbatimTextOutput(outputId = "krlsr_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "krlsr_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(krlsr_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = krlsr_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "krlsr_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "krlsr_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsr_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "krlsr_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsr_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "krlsr_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "krlsr_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "krlsr_ModelPlots"),
                                  verbatimTextOutput(outputId = "krlsr_Recipe"),
                                  verbatimTextOutput(outputId = "krlsr_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "krlsr_Coef")
                                  )
                         ),
                         
                         tabPanel("KRLSPoly Model",
                                  verbatimTextOutput(outputId = "krlsp_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "krlsp_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(krlsp_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = krlsp_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "krlsp_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsp_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "krlsp_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsp_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "krlsp_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "krlsp_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "krlsp_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "krlsp_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "krlsp_ModelPlots"),
                                  verbatimTextOutput(outputId = "krlsp_Recipe"),
                                  verbatimTextOutput(outputId = "krlsp_ModelSummary2"),
                                  wellPanel(
                                    # h3("Coefficients"),
                                    # tableOutput(outputId = "krlsp_Coef")
                                  )
                         ),
                         
                         
                         
                         tabPanel("Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models 
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         )
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         
                         
                         
                         
                         
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), selected = "svmPoly", inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
