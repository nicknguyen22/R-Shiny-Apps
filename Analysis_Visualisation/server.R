shinyServer(function(input, output) {

  output$rdata = renderDT({
    DT::datatable(aData,
                  rownames = input$Rownames,
                  selection = input$Selection,
                  filter = list(position = input$Filter),
                  extensions = 'FixedColumns',
                  options = list(searching = TRUE,
                                 lengthMenu = list(c(10,15,20,25, -1), c("10","15","20","25","All")),
                                 pageLength = 10,
                                 dom = paste(input$Dom, collapse = ""),
                                 fixedColumns = list(leftColumns = 3),
                                 ordering = input$Order,
                                 scrollCollapse = TRUE,
                                 orderClasses = TRUE,
                                 scrollX = TRUE,
                                 scrollY = TRUE
                                ),
                  callback = DT::JS(paste0("
                         const dropdown = $('.dataTables_length')
                                             .find('label')
                                             .find('select')
                    
                         let new_bottom = document.createElement('div')
                         new_bottom.classList.add('custom_dropdown')
                         new_bottom.innerHTML = 'Showing '
                         
                         dropdown.appendTo(new_bottom)
                                          
                         new_bottom.append(' of ' +", nrow(aData), " + ' entries')
                                          
                         $('.dataTables_info')
                              .replaceWith(new_bottom)
                                               
                         $('.custom_dropdown')
                             .removeClass('custom_dropdown')
                             .addClass('dataTables_info') 
                                             
                         $('.dataTables_length').remove()"))
                  
    )
    
  })
  
  output$SummaryA <- renderPrint({
    summary(aData)
    
  })
  
  output$SummaryC <- renderPrint({
    summary(factorGrp)
    
  })  
  
  output$SummaryC1 <- renderPrint({
    str(factorGrp)
    
  })  
  
  output$SummaryN <- renderPrint({
    summary(numGrp)
    
  })
  
  output$SummaryN1 <- renderPrint({
    str(numGrp)
    
  })
  
  output$mvChart = renderPlot({
      vis_dat(aData,  palette = input$mvcolor)
              
  })
    
  output$mvdChart = renderPlot({
    vis_miss(aData[input$VariablesMV], cluster = input$scluster,sort_miss = input$sortMiss)
  })

  output$Mosaic = renderPlot({
    formula = as.formula(paste("~",paste(input$VariablesM, collapse = " + ")))
    vcd::mosaic(formula, data = aData,colorize = TRUE, shade = TRUE, legend = TRUE,
                gp = shading_hcl, gp_args = list(interpolate = c(1, 1.8)))
  })
  
  
  output$Boxplot = renderPlot({
    data = as.matrix(numGrp[input$VariablesBP])
    data = scale(data, center = input$standardise, scale = input$standardise)
    car::Boxplot(y = data, ylab = NA, use.cols = TRUE, notch = input$notch, varwidth = FALSE,  
                 horizontal = FALSE, outline = input$outliers, 
                 col = brewer.pal(n = dim(data)[2], name = "RdBu"),
                 range = input$range, id = ifelse(input$outliers, list(n = Inf, 
                 location = "avoid"), FALSE)) 
  })
  
  
  output$cormat = renderPlot({
    corrgram(numGrp[input$VariablesC], 
             order = input$Group, 
             abs = input$abs,
             cor.method = input$CorrMeth,
             upper.panel=panel.cor,
             text.panel = panel.txt
            )
  })
  

  output$rising = renderPlot({
    rs = numGrp[input$VariablesRV]
    for (col in 1:ncol(rs)) {
      rs[,col] <- rs[order(rs[,col]),col]}
    rs = scale(x = rs, center = input$centerRV, scale = input$scaleRV)
    mypalette = rainbow(ncol(rs))
    matplot(x = seq(1, 100, length.out = nrow(rs)), y = rs, type = "l", 
            xlab = "Percentile (%)", ylab = "Standardised Values", 
            lty = 1, lwd = 1, col = mypalette)
    legend(legend = colnames(rs), x = "topleft", y = "top", lty = 1, lwd = 1, 
           col = mypalette, ncol = round(ncol(rs)^0.3))
    
  })
  
  
  output$Pairs <- renderPlot({
    pairsub = pairsData[input$VariablesP]
    GGally::ggpairs(data = pairsub,progress =FALSE, cardinality_threshold = 15)
  })
  
  
  output$Histogram <- renderPlot({
    plot_normality(numGrp[input$VariablesH],right = "Box-Cox")
    
  })
  
  
  output$Sequence <- renderPlot({
    sqe = data.frame(row = 1:nrow(aData[input$VariablesS]), aData[input$VariablesS])  
    tabplot::tableplot(aData[input$VariablesS], decreasing = FALSE)
  })
  
  
  output$Matplot <- renderPlot({
    numData = numGrp[input$VariablesMP]
    numDatatmp = scale(numData[,input$VariablesMP], center = input$centerMP, scale = input$scaleMP) 
    matplot(numDatatmp, type = "l", col = rainbow(ncol(numDatatmp)), xlab = "Observations in sequence", ylab = "Value") 
  })
  
  
  
})
