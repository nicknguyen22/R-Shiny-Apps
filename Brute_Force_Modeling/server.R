shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "Ass3Data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    req(getData())
    # print(dfSummary(getData(),
    #                 method = 'render',
    #                 data.frame()))
    print(dfSummary(getData(), 
                    varnumbers   = FALSE, 
                    valid.col    = FALSE, 
                    graph.magnif = 0.8), 
          method   = 'render',
          headings = FALSE,
          bootstrap.css = FALSE)
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = "svmPoly")  ## change the value parameter to your best method
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rf ---------------------------------------------------------------------------------------------------------------------------
  library(randomForest)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getRfRecipe ----
  getRfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rf_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rf_Go ----
  observeEvent(
    input$rf_Go,
    {
      method <- "rf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rf_Load,
    {
      method  <- "rf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rf_Delete,
    {
      models[["rf"]] <- NULL
      gc()
    }
  )
  
  # output rf_ModelSummary0 (text) ----
  output$rf_ModelSummary0 <- renderText({
    description("rf")   # Use the caret method name here
  })
  
  # output rf_Metrics (table) ----
  output$rf_Metrics <- renderTable({
    req(models$rf)
    models$rf$results[ which.min(models$rf$results[, "RMSE"]), ]
  })
  
  # output rf_ModelPlots (plot) ----
  output$rf_ModelPlots <- renderPlot({
    req(models$rf)
    plot(models$rf)
  })     
  
  # output rf_Recipe (print) ----
  output$rf_Recipe <- renderPrint({
    req(models$rf)
    models$rf$recipe
  })  
  
  # output rf_ModelSummary2 (print) ----
  output$rf_ModelSummary2 <- renderPrint({
    req(models$rf)
    summary(models$rf$finalModel)
  })
  
  # output rf_VIplot (plot) ----
  output$rf_VIplot <- renderPlot({
    req(models$rf)
    varImpPlot(models$rf$finalModel,sort = T,main = 'Variable Importance')
  })
  
  # METHOD * qrf ---------------------------------------------------------------------------------------------------------------------------
  library(quantregForest)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getQrfRecipe ----
  getQrfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$qrf_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rf_Go ----
  observeEvent(
    input$qrf_Go,
    {
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getQrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$qrf_Load,
    {
      method  <- "qrf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$qrf_Delete,
    {
      models[["qrf"]] <- NULL
      gc()
    }
  )
  
  # output qrf_ModelSummary0 (text) ----
  output$qrf_ModelSummary0 <- renderText({
    description("qrf")   # Use the caret method name here
  })
  
  # output qrf_Metrics (table) ----
  output$qrf_Metrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  # output qrf_ModelPlots (plot) ----
  output$qrf_ModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })     
  
  # output qrf_Recipe (print) ----
  output$qrf_Recipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  # output qrf_ModelSummary2 (print) ----
  output$qrf_ModelSummary2 <- renderPrint({
    req(models$qrf)
    summary(models$qrf$finalModel)
  })
  
  # output rf_VIplot (plot) ----
  output$qrf_VIplot <- renderPlot({
    req(models$qrf)
    varImpPlot(models$qrf$finalModel,sort = T,main = 'Variable Importance')
  })
  
  # METHOD * lm ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getLmRecipe ----
  getLmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lm_Go ----
  observeEvent(
    input$lm_Go,
    {
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getLmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  observeEvent(
    input$lm_Load,
    {
      method  <- "lm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lm_Delete,
    {
      models[["lm"]] <- NULL
      gc()
    }
  )
  
  # output lm_ModelSummary0 (text) ----
  output$lm_ModelSummary0 <- renderText({
    description("lm")   # Use the caret method name here
  })
  
  # output lm_Metrics (table) ----
  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })
  
  # output lm_ModelPlots (plot) ----
  output$lm_ModelPlots <- renderPlot({
    req(models$lm)
    plot(models$lm)
  })     
  
  # output lm_Recipe (print) ----
  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  
  
  # output lm_ModelSummary2 (print) ----
  output$lm_ModelSummary2 <- renderPrint({
    req(models$lm)
    summary(models$lm$finalModel)
  })
  
  # output lm_Coef (print) ----
  output$lm_Coef <- renderTable({
    req(models$lm)
    co <- coef(models$lm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * rlm ---------------------------------------------------------------------------------------------------------------------------
  library(MASS)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getRlmRecipe ----
  getRlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rlm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rlm_Go ----
  observeEvent(
    input$rlm_Go,
    {
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  observeEvent(
    input$rlm_Load,
    {
      method  <- "rlm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rlm_Delete,
    {
      models[["rlm"]] <- NULL
      gc()
    }
  )
  
  # output rlm_ModelSummary0 (text) ----
  output$rlm_ModelSummary0 <- renderText({
    description("rlm")   # Use the caret method name here
  })
  
  # output rlm_Metrics (table) ----
  output$rlm_Metrics <- renderTable({
    req(models$rlm)
    models$rlm$results[ which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  # output rlm_ModelPlots (plot) ----
  output$rlm_ModelPlots <- renderPlot({
    req(models$rlm)
    plot(models$rlm)
  })     
  
  # output rlm_Recipe (print) ----
  output$rlm_Recipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  # output rlm_ModelSummary2 (print) ----
  output$rlm_ModelSummary2 <- renderPrint({
    req(models$rlm)
    summary(models$rlm$finalModel)
  })
  
  # output rlm_Coef (print) ----
  output$rlm_Coef <- renderTable({
    req(models$rlm)
    co <- coef(models$rlm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * knn ---------------------------------------------------------------------------------------------------------------------------

  
  # reactive getKnnRecipe ----
  getKnnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$knn_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent knn_Go ----
  observeEvent(
    input$knn_Go,
    {
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getKnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  observeEvent(
    input$knn_Load,
    {
      method  <- "knn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$knn_Delete,
    {
      models[["knn"]] <- NULL
      gc()
    }
  )
  
  # output knn_ModelSummary0 (text) ----
  output$knn_ModelSummary0 <- renderText({
    description("knn")   # Use the caret method name here
  })
  
  # output knn_Metrics (table) ----
  output$knn_Metrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })
  
  # output knn_ModelPlots (plot) ----
  output$knn_ModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })     
  
  # output knn_Recipe (print) ----
  output$knn_Recipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  
  
  # output knn_ModelSummary2 (print) ----
  output$knn_ModelSummary2 <- renderPrint({
    req(models$knn)
    summary(models$knn$finalModel)
  })
  
  # output knn_Coef (print) ----
  output$knn_Coef <- renderTable({
    req(models$knn)
    co <- coef(models$knn$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * bag ---------------------------------------------------------------------------------------------------------------------------
  library(party)
  
  # reactive getBagRecipe ----
  getBagRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bag_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent bag_Go ----
  observeEvent(
    input$bag_Go,
    {
      method <- "bag"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getBagRecipe(), data = getTrainData(), method = method, metric = "RMSE", B = 100,
                              bagControl = bagControl(fit = ctreeBag$fit,predict = ctreeBag$pred,aggregate = ctreeBag$aggregate),trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.omit)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  observeEvent(
    input$bag_Load,
    {
      method  <- "bag"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bag_Delete,
    {
      models[["bag"]] <- NULL
      gc()
    }
  )
  
  # output bag_ModelSummary0 (text) ----
  output$bag_ModelSummary0 <- renderText({
    description("bag")   # Use the caret method name here
  })
  
  # output bag_Metrics (table) ----
  output$bag_Metrics <- renderTable({
    req(models$bag)
    models$bag$results[ which.min(models$bag$results[, "RMSE"]), ]
  })
  
  # output bag_ModelPlots (plot) ----
  output$bag_ModelPlots <- renderPlot({
    req(models$bag)
    plot(models$bag)
  })     
  
  # output bag_Recipe (print) ----
  output$bag_Recipe <- renderPrint({
    req(models$bag)
    models$bag$recipe
  })  
  
  # output bag_ModelSummary2 (print) ----
  output$bag_ModelSummary2 <- renderPrint({
    req(models$bag)
    summary(models$bag$finalModel)
  })
  
  # output bag_Coef (print) ----
  output$bag_Coef <- renderTable({
    req(models$bag)
    co <- coef(models$bag$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * nnet ---------------------------------------------------------------------------------------------------------------------------
  library(nnet)
  
  # reactive getNnetRecipe ----
  getNnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$nnet_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent nnet_Go ----
  observeEvent(
    input$nnet_Go,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getNnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, 
                              maxit = 1000, linout = T, trace = F, tuneLength = 5, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  
  observeEvent(
    input$nnet_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$nnet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()
    }
  )
  
  # output nnet_ModelSummary0 (text) ----
  output$nnet_ModelSummary0 <- renderText({
    description("nnet")   # Use the caret method name here
  })
  
  # output nnet_Metrics (table) ----
  output$nnet_Metrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })
  
  # output nnet_ModelPlots (plot) ----
  output$nnet_ModelPlots <- renderPlot({
    req(models$nnet)
    plot(models$nnet)
  })     
  
  # output nnet_Recipe (print) ----
  output$nnet_Recipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })  
  
  # output nnet_ModelSummary2 (print) ----
  output$nnet_ModelSummary2 <- renderPrint({
    req(models$nnet)
    summary(models$nnet$finalModel)
  })
  
  # output nnet_Coef (print) ----
  output$nnet_Coef <- renderTable({
    req(models$nnet)
    co <- coef(models$nnet$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
   
  
  # METHOD * svmLinear ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getSvmlRecipe ----
  getSvmlRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svml_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svml_Go ----
  observeEvent(
    input$svml_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getSvmlRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneGrid = expand.grid(C = seq(0, 2, length = 25)), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svml_Load,
    {
      method  <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svml_Delete,
    {
      models[["svmLinear"]] <- NULL
      gc()
    }
  )
  
  # output svml_ModelSummary (text) ----
  output$svml_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })
  
  # output svml_Metrics (table) ----
  output$svml_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })
  
  # output svml_ModelPlots (plot) ----
  output$svml_ModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })
  
  # output svml_Recipe (print) ----
  output$svml_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  
  
  # output svml_ModelSummary2 (print) ----
  output$svml_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })
  
  # output svml_Coef (print) ----
  output$svml_Coef <- renderTable({
    req(models$svmLinear)
    co <- as.matrix(coef(models$svmLinear$finalModel)) 
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # METHOD * svmRadial ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getSvmrRecipe ----
  getSvmrRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmr_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmr_Go ----
  observeEvent(
    input$svmr_Go,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getSvmrRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                             tuneLength = 10, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmr_Load,
    {
      method  <- "svmRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmr_Delete,
    {
      models[["svmRadial"]] <- NULL
      gc()
    }
  )
  
  # output svmr_ModelSummary (text) ----
  output$svmr_ModelSummary0 <- renderText({
    description("svmRadial")   # Use the caret method name here
  })
  
  # output svmr_Metrics (table) ----
  output$svmr_Metrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  # output svmr_ModelPlots (plot) ----
  output$svmr_ModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })
  
  # output svmr_Recipe (print) ----
  output$svmr_Recipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  # output svmr_ModelSummary2 (print) ----
  output$svmr_ModelSummary2 <- renderPrint({
    req(models$svmRadial)
    print(models$svmRadial)
  })
  
  # output svmr_Coef (print) ----
  output$svmr_Coef <- renderTable({
    req(models$svmRadial)
    co <- as.matrix(coef(models$svmRadial$finalModel)) 
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * svmRadialCost ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getSvmrcRecipe ----
  getSvmrcRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmrc_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmrc_Go ----
  observeEvent(
    input$svmrc_Go,
    {
      method <- "svmRadialCost"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getSvmrcRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 10, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmrc_Load,
    {
      method  <- "svmRadialCost"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmrc_Delete,
    {
      models[["svmRadialCost"]] <- NULL
      gc()
    }
  )
  
  # output svmrc_ModelSummary (text) ----
  output$svmrc_ModelSummary0 <- renderText({
    description("svmRadialCost")   # Use the caret method name here
  })
  
  # output svmrc_Metrics (table) ----
  output$svmrc_Metrics <- renderTable({
    req(models$svmRadialCost)
    models$svmRadialCost$results[ which.min(models$svmRadialCost$results[, "RMSE"]), ]
  })
  
  # output svmrc_ModelPlots (plot) ----
  output$svmrc_ModelPlots <- renderPlot({
    req(models$svmRadialCost)
    plot(models$svmRadialCost)
  })
  
  # output svmrc_Recipe (print) ----
  output$svmrc_Recipe <- renderPrint({
    req(models$svmRadialCost)
    models$svmRadialCost$recipe
  })  
  
  # output svmrc_ModelSummary2 (print) ----
  output$svmrc_ModelSummary2 <- renderPrint({
    req(models$svmRadialCost)
    print(models$svmRadialCost)
  })
  
  # output svmrc_Coef (print) ----
  output$svmrc_Coef <- renderTable({
    req(models$svmRadialCost)
    co <- as.matrix(coef(models$svmRadialCost$finalModel)) 
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * svmRadialSigma ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getSvmrsRecipe ----
  getSvmrsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmrs_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmrs_Go ----
  observeEvent(
    input$svmrs_Go,
    {
      method <- "svmRadialSigma"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getSvmrsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, 
                              tuneLength = 10, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmrs_Load,
    {
      method  <- "svmRadialSigma"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmrs_Delete,
    {
      models[["svmRadialSigma"]] <- NULL
      gc()
    }
  )
  
  # output svmrs_ModelSummary (text) ----
  output$svmrs_ModelSummary0 <- renderText({
    description("svmRadialSigma")   # Use the caret method name here
  })
  
  # output svmrs_Metrics (table) ----
  output$svmrs_Metrics <- renderTable({
    req(models$svmRadialSigma)
    models$svmRadialSigma$results[ which.min(models$svmRadialSigma$results[, "RMSE"]), ]
  })
  
  # output svmrs_ModelPlots (plot) ----
  output$svmrs_ModelPlots <- renderPlot({
    req(models$svmRadialSigma)
    plot(models$svmRadialSigma)
  })
  
  # output svmrs_Recipe (print) ----
  output$svmrs_Recipe <- renderPrint({
    req(models$svmRadialSigma)
    models$svmRadialSigma$recipe
  })  
  
  # output svmrs_ModelSummary2 (print) ----
  output$svmrs_ModelSummary2 <- renderPrint({
    req(models$svmRadialSigma)
    print(models$svmRadialSigma)
  })
  
  # output svmrs_Coef (print) ----
  output$svmrs_Coef <- renderTable({
    req(models$svmRadialSigma)
    co <- as.matrix(coef(models$svmRadialSigma$finalModel)) 
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * svmPoly ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getSvmpRecipe ----
  getSvmpRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmp_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmp_Go ----
  observeEvent(
    input$svmp_Go,
    {
      method <- "svmPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getSvmpRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont,
                              tuneGrid = expand.grid(degree = seq(1, 4, length = 4), scale =  seq(0.01,0.1, length = 5),C = seq(0.1,5, length = 5)),
                              tuneLength = 1, na.action = na.pass)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmp_Load,
    {
      method  <- "svmPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmp_Delete,
    {
      models[["svmPoly"]] <- NULL
      gc()
    }
  )
  
  # output svmp_ModelSummary (text) ----
  output$svmp_ModelSummary0 <- renderText({
    description("svmPoly")   # Use the caret method name here
  })
  
  # output svmp_Metrics (table) ----
  output$svmp_Metrics <- renderTable({
    req(models$svmPoly)
    models$svmPoly$results[ which.min(models$svmPoly$results[, "RMSE"]), ]
  })
  
  # output svmp_ModelPlots (plot) ----
  output$svmp_ModelPlots <- renderPlot({
    req(models$svmPoly)
    plot(models$svmPoly)
  })
  
  # output svmp_Recipe (print) ----
  output$svmp_Recipe <- renderPrint({
    req(models$svmPoly)
    models$svmPoly$recipe
  })  
  
  # output svmp_ModelSummary2 (print) ----
  output$svmp_ModelSummary2 <- renderPrint({
    req(models$svmPoly)
    print(models$svmPoly)
  })
  
  # output svmp_Coef (print) ----
  output$svmp_Coef <- renderTable({
    req(models$svmPoly)
    co <- as.matrix(coef(models$svmPoly$finalModel)) 
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  # METHOD * krlsRadial ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(KRLS)
  # reactive getKrlsrRecipe ----
  getKrlsrRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$krlsr_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent krlsr_Go ----
  observeEvent(
    input$krlsr_Go,
    {
      method <- "krlsRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getKrlsrRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneGrid = expand.grid(lambda = NA, sigma = seq(0, 200, length = 10)),
                              tuneLength = 3)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$krlsr_Load,
    {
      method  <- "krlsRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$krlsr_Delete,
    {
      models[["krlsRadial"]] <- NULL
      gc()
    }
  )
  
  # output krlsr_ModelSummary (text) ----
  output$krlsr_ModelSummary0 <- renderText({
    description("krlsRadial")   # Use the caret method name here
  })
  
  # output krlsr_Metrics (table) ----
  output$krlsr_Metrics <- renderTable({
    req(models$krlsRadial)
    models$krlsRadial$results[ which.min(models$krlsRadial$results[, "RMSE"]), ]
  })
  
  # output krlsr_ModelPlots (plot) ----
  output$krlsr_ModelPlots <- renderPlot({
    req(models$krlsRadial)
    plot(models$krlsRadial)
  })
  
  # output krlsr_Recipe (print) ----
  output$krlsr_Recipe <- renderPrint({
    req(models$krlsRadial)
    models$krlsRadial$recipe
  })  
  
  # output krlsr_ModelSummary2 (print) ----
  output$krlsr_ModelSummary2 <- renderPrint({
    req(models$krlsRadial)
    print(models$krlsRadial)
  })
  
  
  
  # METHOD * krlsPoly ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(KRLS)
  # reactive getKrlspRecipe ----
  getKrlspRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$krlsp_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent krlsp_Go ----
  observeEvent(
    input$krlsp_Go,
    {
      method <- "krlsPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getKrlspRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$krlsp_Load,
    {
      method  <- "krlsPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$krlsp_Delete,
    {
      models[["krlsPoly"]] <- NULL
      gc()
    }
  )
  
  # output krlsp_ModelSummary (text) ----
  output$krlsp_ModelSummary0 <- renderText({
    description("krlsPoly")   # Use the caret method name here
  })
  
  # output krlsp_Metrics (table) ----
  output$krlsp_Metrics <- renderTable({
    req(models$krlsPoly)
    models$krlsPoly$results[ which.min(models$krlsPoly$results[, "RMSE"]), ]
  })
  
  # output krlsp_ModelPlots (plot) ----
  output$krlsp_ModelPlots <- renderPlot({
    req(models$krlsPoly)
    plot(models$krlsPoly)
  })
  
  # output krlsp_Recipe (print) ----
  output$krlsp_Recipe <- renderPrint({
    req(models$krlsPoly)
    models$krlsPoly$recipe
  })  
  
  # output krlsp_ModelSummary2 (print) ----
  output$krlsp_ModelSummary2 <- renderPrint({
    req(models$krlsPoly)
    print(models$krlsPoly)
  })
  
  # output krlsp_Coef (print) ----
  output$krlsp_Coef <- renderTable({
    req(models$krlsPoly)
    co <- as.matrix(coef(models$krlsPoly$finalModel)) 
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  # METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  

  
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

  
  
})
