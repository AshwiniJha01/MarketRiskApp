server <- function(input, output, session){
  
  ## Read Me:
  output$readMeNote <- renderText({return(readMe)})

  ## Data to analyze - for a specific stock, for a specific time period
  stockData <- eventReactive(eventExpr = input$tab1,valueExpr = {
    df <- quantmod::getSymbols(Symbols = paste0(input$selSec,".NS"), src = "yahoo", from = input$dateIp[1], to = input$dateIp[2],auto.assign = F)
    df <- as.data.frame(df)
    df <- tibble::rownames_to_column(df, "Date")
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
    df  <- df %>%
      filter(Open != "null") %>%
      mutate(
        Date = as.Date(Date),
        Open = as.numeric(Open),
        High = as.numeric(High),
        Low = as.numeric(Low),
        Close = as.numeric(Close),
        Volume = as.numeric(Volume),
        Adj.Close = as.numeric(Adj.Close),
        dailyReturnPerc = (Close - Open)/Open
      ) %>%
      arrange(desc(Date))
    return(df)
  })
  # Display company name and exchange for selected symbol:
  output$nameAndExchange <- renderText({
    paste0(exchangeData$Name[exchangeData$Symbol == input$selSec], ", ",exchangeData$Exchange[exchangeData$Symbol == input$selSec])
  })

  
  
  ## Plot of daily return over the selected time period
  output$plotDailyReturn <- renderPlot({

    # # two plots
    p1 <- ggplot(data = stockData(), aes(x = Date, y = Close)) + geom_line(colour = "white") + theme_black()
    p2 <- ggplot(data = stockData(), aes(x = Date, y = dailyReturnPerc)) + geom_line(colour = "green") + 
      theme_black() +
      theme(axis.text.y = element_text(color="green"))%+replace%
      theme(panel.background = element_blank())

    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))

    # overlap the panel of 2nd plot on that of 1st plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t,
                         pp$l, pp$b, pp$l)

    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)


    # draw it
    grid.draw(g)
    # +
    #   scale_color_manual(name = "Data",
    #                      values = c("white","green"),
    #                      labels = c("Daily Closing Value","Daily Return Percentage"))

  })
  
  
  # Reading in input for delta value:
  deltaValue <- eventReactive(eventExpr = input$cnfrmDelta, valueExpr = {return(input$ipDelta)})
  
  ## Half life with delta and percentage of data covered:
  output$halfLife <- renderText({
    paste0(round((log(0.5 + 0.5*deltaValue()^nrow(stockData())))/log(deltaValue()),0)," days")
  })
  
  output$sizeData <- renderText({
    paste0(nrow(stockData())," days")
  })
  
  output$percFullData <- renderText({
    paste0(round((1 - deltaValue()^nrow(stockData()))*100,2), "%")
  })
  
  
  ## Summary statistics:
  summaryStats <- eventReactive(eventExpr = input$cnfrmDelta, valueExpr = {
    output <- statEstimates(dataFrame = stockData(), delta = deltaValue())
    # meanEWMA <- output["meanEWMA"]
    # stdDev <- output["stdDev"]
    # skew <- output["skew"]
    # kurtosis <- output["kurtosis"]
    # return(meanEWMA, stdDev, skew, kurtosis)
    return(output)
  })
  output$meanEWMA <- renderText({paste0(format(summaryStats()$meanEWMA, scientific = F),"%")})
  output$stdDev <- renderText({paste0(summaryStats()$stdDev,"%")})
  output$skew <- renderText({summaryStats()$skew})
  output$kurtosis <- renderText({summaryStats()$kurtosis})
  
  ## Frequency plot of Daily Return Percentage:
  output$dlyRetPrc <- renderPlot({
    ggplot(stockData(), aes(x=dailyReturnPerc)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="green")+
      geom_density(alpha=.5*(deltaValue()/deltaValue()), fill="#006272") + #rgb(102/255,130/255,255/255)
      theme_black() +
      labs(y = "Frequency", x = "Daily Return Percentage")
    
  })

  
  
  ## VaR and Expected Shortfall image:
  output$alphaDepiction <- renderImage({
    # outfile <- tempfile(pattern = "varImage",tmpdir = "/www/", fileext = '.jpg')
    outfile <- normalizePath(file.path('./www',
                            paste('varImage', '.jpg', sep='')))
    list(src = outfile,
         width = 252,
         height = 134,
         alt = paste("VaR Illustration"))
  }, deleteFile = FALSE)
  
  
  ## Reading input of alpha level:
  alphaValue <- eventReactive(eventExpr = input$cnfrmVar, valueExpr = {
    return(input$ipVar)
  })
  
  ## Plot of Log Normal Returns:
  output$deltaNormalPlot <- renderPlot({
    stockData() %>%
      mutate(logReturn = log(Close/Open)) %>%
      ggplot(aes(x=logReturn)) +
      geom_histogram(aes(y=..density..), colour="black", fill="green")+
      geom_density(alpha=.5, fill=rgb(102/255,130/255,255/255)) +
      theme_black() +
      labs(y = "Frequency", x = "Log of Daily Return Percentage") +
      # abline(v = qnorm(p = alphaValue(), mean = mean(stockData()$logReturn), sd = sd(stockData()$logReturn), lower.tail = T, log.p = F)) +
      abline(v = deltaNormalStats()$zValue, col = "red", lwd=3, lty=2)
  })
  
  ## Estimating the VaR using Delta Normal method
  deltaNormalStats <- eventReactive(eventExpr = input$cnfrmVar, valueExpr = {
    return(varDeltaNormalMethod(dataFrame = stockData(), alphaVal = alphaValue(), delta = deltaValue()))
  })
  output$DN_cvar <- renderText({paste0("Rs ",round(deltaNormalStats()$cvar*(-1),2))})
  output$DN_var <- renderText({paste0("Rs ",round(deltaNormalStats()$var*(-1),2))})
  output$DN_mean <- renderText({paste0(round(deltaNormalStats()$mean,4))})
  output$DN_stdDev <- renderText({paste0(round(deltaNormalStats()$stdDev,4))})
  output$DN_zValue <- renderText({paste0(round(deltaNormalStats()$zValue,4))})
  output$DN_latestDollarValue <- renderText({paste0("Rs ",round(deltaNormalStats()$latestDollarValue,2))})
  
  ## Estimating the VaR using Historical Method:
  historicalVarStats <- eventReactive(eventExpr = input$cnfrmVar, valueExpr = {
    return(varHistoricalMethod(dataFrame = stockData(), alphaVal = alphaValue()))
  })
  output$hist_df <- DT::renderDataTable({
    DT::datatable(historicalVarStats()$df, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  output$hist_var <- renderText({paste0("Rs ",round(historicalVarStats()$var*(-1),2))})
  output$hist_cvar <- renderText({paste0("Rs ",round(historicalVarStats()$cVar*(-1),2))})
  output$hist_thresholdDlyRetPc <- renderText({paste0(round(historicalVarStats()$thresholdDlyRetPc,4))})
  output$hist_thresholdMeanDlyRetPc <- renderText({paste0(round(historicalVarStats()$thresholdMeanDlyRetPc,4))})
  
  
  ## Estimating the VaR using Hybrid Historical Method:
  hybridHistVarStats <- eventReactive(eventExpr = input$cnfrmVar, valueExpr = {
    return(varHybridHistMethod(dataFrame = stockData(), alphaVal = alphaValue(), deltaVal = deltaValue()))
  })
  output$hybridHist_df <- DT::renderDataTable({
    DT::datatable(hybridHistVarStats()$df, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  output$hybridHist_var <- renderText({paste0("Rs ",round(hybridHistVarStats()$var*(-1),2))})
  output$hybridHist_cvar <- renderText({paste0("Rs ",round(hybridHistVarStats()$cVar*(-1),2))})
  output$hybridHist_thresholdDlyRetPc <- renderText({paste0(round(hybridHistVarStats()$thresholdDlyRetPc,4))})
  output$hybridHist_thresholdMeanDlyRetPc <- renderText({paste0(round(hybridHistVarStats()$thresholdMeanDlyRetPc,4))})
  
  ## Estimating the VaR using Monte Carlo Simulation:
  dataSize <- eventReactive(eventExpr = input$tab1, valueExpr = {
    s <- nrow(stockData())
    s <- s - s%%10
    return(s)
    })
  observeEvent(dataSize(),{
    
    updateSliderInput(session = session, inputId = "ipDrawSize", min = dataSize()/10, max = dataSize(), value = dataSize()/10+30, step = 1)
  })
  
  MCSVarStats <- eventReactive(eventExpr = (input$runSims+input$cnfrmVar), valueExpr = {
    return(varMonCarlSim(dataFrame = stockData(), alphaVal = alphaValue(), numSims = input$ipNumSims, numCnsctvDays = input$ipNumCnsctvDays, drawSize = input$ipDrawSize))
  })
  output$MCS_var <- renderText({paste0("Rs ",round(MCSVarStats()$var*(-1),2))})
  output$MCS_cvar <- renderText({paste0("Rs ",round(MCSVarStats()$cVar*(-1),2))})
  output$MCS_thresholdDlyRetPc <- renderText({paste0(round(MCSVarStats()$thresholdDlyRetPc,4))})
  output$MCS_thresholdMeanDlyRetPc <- renderText({paste0(round(MCSVarStats()$thresholdMeanDlyRetPc,4))})

}