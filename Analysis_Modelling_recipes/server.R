shinyServer(function(input, output, session) {

  # ------------ Raw Data section ------------

  rawData = reactive({
    inFile = input$rawfile

    if (!is.null(inFile)){
      read.csv(inFile$datapath, header=input$header, na.strings = input$navalue,
               stringsAsFactors = input$factor
               )
    }
    else {
      read.csv("Ass2Data.csv", header=input$header, na.strings = input$navalue,
               stringsAsFactors = input$factor
               )
    }
  })

  
  observeEvent(rawData(),updateSelectInput(session, "convrt", choices=colnames(rawData()),
                                            selected = "CODE"))
  
  observeEvent(preClean(),updateSelectInput(session, "caVar",
                                                choices=names(which(sapply(preClean(), is.factor))),
                                                selected = c("HEALTHCARE_BASIS")))
  observeEvent(preClean(),updateSelectInput(session, "numVar",
                                                choices = names(which(sapply(preClean(), is.numeric))),
                                                selected = c("HEALTHCARE_COST")))
  
  output$rdata = renderDT({
    rawData = rawData()
    DT::datatable(rawData,rownames = TRUE,selection = "single",extensions = 'FixedColumns',
                  options = list(searching = F,pageLength = 10, paging = FALSE, fixedColumns = list(leftColumns = 2),
                                 ordering = TRUE,scrollCollapse = TRUE, orderClasses = TRUE,
                                 scrollX = TRUE,scrollY = "600px")
                  )
  })
  
  preClean = reactive({
    req(input$convrt)
    data = rawData()
    data[input$convrt] = as.character(data[,input$convrt])
    data
  })

  output$summary = renderPrint({
    rawData = preClean()
    summary(rawData)
})
  
  
  output$str = renderPrint({
    rawData = preClean()
    str(rawData)
})

  output$desc = renderPrint({
    rawData = preClean()
    describe(rawData)
  
})

  output$overview = renderPrint({
    rawData = preClean()
    summary(overview(rawData))
  
})

  output$ovplot = renderPlot({
    rawData = preClean()
    plot(overview(rawData))

})

  # output$mvChart = renderPlot({
  #   rawData = manuImpF()
  #   vis_dat(rawData,  palette = input$mvcolor)
  
  # })

  output$mvdChart = renderPlot({
    rawData = manuImpF()
    vis_miss(rawData, cluster = input$scluster,sort_miss = input$sortMiss) +
      theme(axis.text.x =  element_text(angle = 45))
})
  

  output$usChart = renderPlot({
    rawData = manuImpF()
    gg_miss_upset(rawData, nsets = input$set, nintersects = input$intsec)
  })

  manuImpN = reactive({  
    req(input$numVar)
    data = preClean()
    req(data)
    numVars = input$numVar
    for (numvar in numVars) {
      if (colSums(is.na(data[numvar]))>1) {
        shadowname = paste0(numvar,"_shadow")
        data[shadowname] = as.numeric(is.na(data[,numvar]))
        data[numvar][is.na(data[numvar])] = 0
      }
    }
    data
  })  
  
  manuImpC = reactive({  
    req(input$faVar)
    data =  manuImpN()
    req(data)
    favar = input$faVar
      if (colSums(is.na(data[favar]))>1) {
        shadowname = paste0(favar,"_shadow")
        data[shadowname] = as.numeric(is.na(data[,favar]))
        data[favar] = as.character(data[,favar])
        data[favar][is.na(data[favar])] = input$cusVa
        data[favar] = as.factor(data[,favar])
      }
    data
  })
  
  manuImpF = reactive({  
    req(input$caVar)
    data =  manuImpC()
    req(data)
    caVars = input$caVar
    for (cavar in caVars) {
      if (colSums(is.na(data[cavar]))>1) {
        shadowname = paste0(cavar,"_shadow")
        data[shadowname] = as.numeric(is.na(data[,cavar]))
        data[cavar] = as.character(data[,cavar])
        data[cavar][is.na(data[cavar])] = "NONE"
        data[cavar] = as.factor(data[,cavar])
      }
    }
    data
  })
  
  
  output$impData = renderDT({
    data = manuImpF()
    DT::datatable(data,rownames = TRUE,selection = "single",extensions = 'FixedColumns',
                  options = list(searching = F,pageLength = 10, paging = FALSE, fixedColumns = list(leftColumns = 2),
                                 ordering = TRUE,scrollCollapse = TRUE, orderClasses = TRUE,
                                 scrollX = TRUE,scrollY = "600px")
    )
  })

  output$corrChart = renderPlot({
    data = manuImpF()
    m = is.na(data) + 0
    cm <- colMeans(m)
    m <- m[, cm > 0 & cm < 1, drop = FALSE]
    corrgram::corrgram(cor(m), order = "OLO", abs = T)
  })

  output$rpChart = renderPlot({
    data = manuImpF()
    data$CODE = 1:nrow(data)
    data$missingness = apply(X = is.na(data), MARGIN = 1, FUN = sum)
    tree = caret::train(missingness ~ ., 
                         data = data, 
                         method = "rpart", 
                         na.action = na.rpart)
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               roundint = TRUE, 
               clip.facs = TRUE)
  })
  
  
  # ------------ EDA section ------------
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesM",
                                            choices=names(which(sapply(manuImpF(), is.factor))),
                                            selected = names(which(sapply(manuImpF(), is.factor)))
                                            ))
  output$Mosaic = renderPlot({
    data = manuImpF()
    formula = as.formula(paste("~",paste(input$VariablesM, collapse = " + ")))
    vcd::mosaic(formula, data = data,colorize = TRUE, shade = TRUE, legend = TRUE,
                gp = shading_hcl, gp_args = list(interpolate = c(1, 1.8)))
  })
  
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesBP",
                                            choices=names(which(sapply(manuImpF()[,1:15], is.numeric))),
                                            selected = names(which(sapply(manuImpF()[,1:15], is.numeric)))))
  output$Boxplot = renderPlot({
    numGrp = select_if(manuImpF()[,1:15],is.numeric)
    data = as.matrix(numGrp[input$VariablesBP])
    data = scale(data, center = input$standardise, scale = input$standardise)
    car::Boxplot(y = data, ylab = NA, use.cols = TRUE, notch = input$notch, varwidth = FALSE,  
                 horizontal = FALSE, outline = input$outliers, 
                 col = brewer.pal(n = dim(data)[2], name = "RdBu"),
                 range = input$range, id = ifelse(input$outliers, list(n = Inf, 
                                                                       location = "avoid"), FALSE)) 
  })
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesC",
                                            choices=names(which(sapply(manuImpF()[,1:15], is.numeric))),
                                            selected = names(which(sapply(manuImpF()[,1:15], is.numeric)))))
  
  output$cormat = renderPlot({
    numGrp = select_if(manuImpF()[,1:15],is.numeric)
    corrgram(numGrp[input$VariablesC], 
             order = input$Group, 
             abs = input$abs,
             cor.method = input$CorrMeth,
             upper.panel=panel.cor,
             text.panel = panel.txt
    )
  })
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesRV",
                                            choices=names(which(sapply(manuImpF()[,1:15], is.numeric))),
                                            selected = names(which(sapply(manuImpF()[,1:15], is.numeric)))))
  
  output$rising = renderPlot({
    numGrp = select_if(manuImpF()[,1:15],is.numeric)
    rs = numGrp[input$VariablesRV]
    for (col in 1:ncol(rs)) {
      rs[,col] = rs[order(rs[,col]),col]}
    rs = scale(x = rs, center = input$centerRV, scale = input$scaleRV)
    mypalette = rainbow(ncol(rs))
    matplot(x = seq(1, 100, length.out = nrow(rs)), y = rs, type = "l", 
            xlab = "Percentile (%)", ylab = "Standardised Values", 
            lty = 1, lwd = 1, col = mypalette)
    legend(legend = colnames(rs), x = "topleft", y = "top", lty = 1, lwd = 1, 
           col = mypalette, ncol = round(ncol(rs)^0.3))
    
  })
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesP",
                                            choices=names(manuImpF()[,2:15]),
                                            selected = names(manuImpF()[,2:15])))
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesP1",
                                            choices=names(which(sapply(manuImpF(), is.factor))),
                                            selected = "OBS_TYPE"))
  
  output$Pairs = renderPlot({
    pairsData = drop_na(manuImpF()[,2:15])
    pairsub = pairsData[input$VariablesP]
    GGally::ggpairs(data = pairsub, ggplot2::aes_string(colour = input$VariablesP1, alpha = 0.8))

  })
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesH",
                                            choices=names(which(sapply(manuImpF()[,1:15], is.numeric))),
                                            ))
  
  output$Histogram = renderPlot({
    numGrp = select_if(manuImpF()[,1:15],is.numeric)
    plot_normality(numGrp[input$VariablesH],right = "Box-Cox")
  })
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesS",
                                            choices=names(manuImpF()[,1:15]),
                                            selected = names(manuImpF()[,1:15])))
  
  output$Sequence = renderPlot({
    data = manuImpF()[,1:15]
    sqe = data.frame(row = 1:nrow(data[input$VariablesS]), data[input$VariablesS])  
    tabplot::tableplot(data[input$VariablesS], decreasing = FALSE)
  })
  
  observeEvent(manuImpF(),updateSelectInput(session, "VariablesMP",
                                            choices=names(which(sapply(manuImpF()[,1:15], is.numeric))),
                                            selected = names(which(sapply(manuImpF()[,1:15], is.numeric)))))
  
  output$Matplot = renderPlot({
    numGrp = select_if(manuImpF()[,1:15],is.numeric)
    numData = numGrp[input$VariablesMP]
    numDatatmp = scale(numData[,input$VariablesMP], center = input$centerMP, scale = input$scaleMP) 
    matplot(numDatatmp, type = "l", col = rainbow(ncol(numDatatmp)), xlab = "Observations in sequence", ylab = "Value") 
  })
  

  # ---------- Analysis section ------------
  
  cleanData1 = reactive({
    data = manuImpF()[,1:15]
    vRatio = apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })  
  
  cleanData2 = reactive({
    data = cleanData1()
    data["CODE"] = as.factor(data[,"CODE"])
    oRatio = apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })  
  
  output$msChart = renderPlot({
    visdat::vis_miss(cleanData2(),sort_miss = T ) +
      theme(axis.text.x =  element_text(angle = 80)) + 
      labs(title = paste("Thresholds VarMiss:", input$VarThreshold, "ObsMiss:", input$ObsThreshold))
  }, width = 800)
 
  recipe = reactive({
    req(cleanData2(),input$ImpMethod)
    trainData = cleanData2()[cleanData2()$OBS_TYPE == "Train",]
    if (input$ImpMethod == "KNN") {
      rec = recipes::recipe(DEATH_RATE ~., data = trainData) %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_dummy(all_nominal_predictors()) %>%
        step_impute_knn(all_predictors(), neighbors = 5) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome"))
    } else if (input$ImpMethod == "Partial Del") {
      rec = recipes::recipe(DEATH_RATE ~., data = trainData) %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_naomit(all_predictors(), skip = F) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome"))
    }
    rec
  })  
  
  output$msCount = renderPrint({
    cat("Full Data dimensions based on current thresholds (Observations Variable) : ",
        dim(cleanData2()))
  })
  
  output$msCount1 = renderPrint({  
    cat("Train data dimensions based on current thresholds (Observations Variable) : ",
        dim(cleanData2()[cleanData2()$OBS_TYPE == "Train",]))
  })
  
  output$msCount2 = renderPrint({  
    cat("Test data dimensions based on current thresholds (Observations Variable) : ",
        dim(cleanData2()[cleanData2()$OBS_TYPE == "Test",]))
  })
  
  preRecipe = reactive({
    req(recipe())
    trainData = cleanData2()[cleanData2()$OBS_TYPE == "Train",]
    prep(recipe(),training = trainData)
  })
  
  trainData = reactive({
    req(input$impute)
    isolate({
      req(preRecipe(),cleanData2())
      trainData = cleanData2()[cleanData2()$OBS_TYPE == "Train",]
      bake(preRecipe(),new_data = trainData)
    })
  })
  
  testData = reactive({
    req(input$impute)
    isolate({
      req(preRecipe(),cleanData2())
      testData = cleanData2()[cleanData2()$OBS_TYPE == "Test",]
      bake(preRecipe(),new_data = testData)
    })
  })
  
  allData = reactive({
    req(preRecipe(),cleanData2())
    allData = cleanData2()
    bake(preRecipe(),new_data = allData)
  })
  
  output$sumTrain = renderPrint({
    req(trainData())
    data = trainData()
    summary(data)
  })
  
  output$sumTest = renderPrint({
    req(testData())
    data = testData()
    summary(data)
  })
  
  model = reactive({
    req(input$train)
    isolate({
      req(trainData())
      if (input$trainModel=="glmnet") {
        caret::train(DEATH_RATE ~., 
                     data = trainData(), 
                     method = input$trainModel)}
    })
  })
  

  
  output$modelSum = renderPrint({
    req(model())
    print(model())
  })
  
  predictResult = reactive({
    req(model())
    predict(object = model(), newdata = testData(), type = "raw")
  })
  
  predictResult1 = reactive({
    predict(object = model(), newdata = trainData(), type = "raw")
  })
  
  predictResultA = reactive({
    predict(object = model(), newdata = allData(), type = "raw")
  })
  
  output$preResult = renderPrint({
  req(predictResult())
      testData = testData()
      rmse = sqrt(mean((testData$DEATH_RATE - predictResult())^2, na.rm = TRUE))
      paste("RMSE =", round(rmse,5))
  })
  
  output$trainRMSE = renderPrint({
    req(predictResult1())
    trainData = trainData()
    rmse = sqrt(mean((trainData$DEATH_RATE - predictResult1())^2, na.rm = TRUE))
    paste("RMSE =", round(rmse,5))
  })
  
  output$modelplot = renderPlot({
    req(model())
    plot(model())
  })
  
  output$preplot = renderPlot({
    req(predictResult())
      testData = testData()
      testData$PREDICTION = predictResult()
      rang <- range(c(testData$DEATH_RATE, testData$PREDICTION))
      ggplot(data = testData, mapping = aes(x = PREDICTION, y = DEATH_RATE, label = CODE)) +
        geom_point() +
        geom_abline(slope = 1, col = "blue") +
        labs(y = "Predicted", x = "Actual") + 
        coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE) +
        ggtitle("Death rate prediction of test data") +
        theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$info = renderPrint({
    testData = testData()
    testData$PREDICTION = predictResult()
    nearPoints(testData[,c("CODE","DEATH_RATE","PREDICTION")], input$plot_click, threshold = 10, maxpoints = 1,
               addDist = F)
  })
  
  output$Rboxplot = renderPlot({
    trainData = trainData()
    trainData$PREDICTION = predictResult1()
    coef = input$iqr
    limits = boxplot.stats(x = trainData$PREDICTION, coef = coef)$stats
    trainData$CODE = as.character(trainData$CODE)
    trainData$CODE = ifelse(trainData$PREDICTION < limits[1] | trainData$PREDICTION > limits[5], 
                            trainData$CODE, NA)
    
    ggplot(data = trainData, mapping = aes(x = PREDICTION, y = 1, label = CODE)) +
      geom_boxplot(coef = coef, outlier.colour = "red") + 
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef), x = "Prediction") +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })

  output$Rboxplot1 = renderPlot({
    testData = testData()
    testData$PREDICTION = predictResult()
    coef = input$iqr1
    limits = boxplot.stats(x = testData$PREDICTION, coef = coef)$stats
    testData$CODE = as.character(testData$CODE)
    testData$CODE = ifelse(testData$PREDICTION < limits[1] | testData$PREDICTION > limits[5], 
                           testData$CODE, NA)
    
    ggplot(data = testData, mapping = aes(x = PREDICTION, y = 1, label = CODE)) +
      geom_boxplot(coef = coef, outlier.colour = "red") + 
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef), x = "Prediction") +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$Rboxplot2 = renderPlot({
    allData = allData()
    allData$PREDICTION = predictResultA()
    coef = input$iqr2
    limits = boxplot.stats(x = allData$PREDICTION, coef = coef)$stats
    allData$CODE = as.character(allData$CODE)
    allData$CODE = ifelse(allData$PREDICTION < limits[1] | allData$PREDICTION > limits[5], 
                          allData$CODE, NA)
    
    ggplot(data = allData, mapping = aes(x = PREDICTION, y = 1, label = CODE)) +
      geom_boxplot(coef = coef, outlier.colour = "red") + 
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Uni-variable boxplots at IQR multiplier of", coef), x = "Prediction") +
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  

})
