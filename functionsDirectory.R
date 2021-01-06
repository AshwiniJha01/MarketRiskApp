# Reading in the exchange and stock symbols
exchangeData <- readRDS("stockData/exchangeData.RDS")

# Read me note:
# readMe <- file.path("\\www\\Read Me.txt")
readMe <- HTML("This is a Market Risk Analysis app developed to estimate risks associated with buying equity. I hope you find this application useful in doing your primary research in investing in equity market. <br>

This application has been developed in RShiny using R programming language. To quote the disclaimer associated with using this app: R is free software and comes with ABSOLUTELY NO WARRANTY. <br>

I have performed most of the calculations associated with statistical estimates and VaR/cVaR using base R functions. I have tried to keep all the calculations accurate to the best of my knowledge on the subject and programming nuances associated with R.<br>

Hence, use this application along with guidance from your judgement. I take no ownership of success or failure (hopefully success to you) that you will meet with the aid from this app.<br>

I would like to thank the<br>
1) Authors of the base R functions and following packages:<br>
quantmod, dplyr, DT, GA, shiny, shinythemes, shinydashboard, shinyWidgets, shinyjs, shinyalert, shinyFeedback, ggplot2, shinyBS, shinyjs, shinycssloaders, gtable, grid, gridExtra<br>

2) Author of Quantitative Financial Risk Management, Michael B Miller, the book has been helpful in increasing my knowledge on the subject<br>

3) The community of R users whose contribution through many programming solutions over the internet has always helped me resolve my code bugs<br><br>


Hope you enjoy using this app!<br>
<a href='https://www.linkedin.com/in/ashwini-jha-009646125/'>Ashwini Jha <br>
Data Scientist <br><br><br><br><br><br>Connect with me on LinkedIn</a>")

## Function to make the plot background black
theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}


## Function to estimate statistics using exponentially weighted moving average (EWMA):
## Inputs: Data with daily percentage change, delta which is the decay factor of weights
## Output: mean (EWMA), standard deviation. skewness, kurtosis, half life of data with given delta, percentage data captured with given delta
statEstimates <- function(dataFrame, delta = 0.95){
  
  # Attaching a delta to each data point in data frame:
  dataFrame$weight <- delta^seq(from = 0, to = nrow(dataFrame) - 1, by = 1)
  
  dataFrame$newReturn <- dataFrame$dailyReturnPerc*dataFrame$weight
  dataFrame$newReturnDev <- ((dataFrame$dailyReturnPerc - mean(dataFrame$dailyReturnPerc))^2)*dataFrame$weight
  dataFrame$newReturnSkew <- ((dataFrame$dailyReturnPerc - mean(dataFrame$dailyReturnPerc))^3)*dataFrame$weight
  dataFrame$newReturnKurt <- ((dataFrame$dailyReturnPerc - mean(dataFrame$dailyReturnPerc))^4)*dataFrame$weight
  
  meanEWMA <- (1 - delta)/(1-delta^nrow(dataFrame))*sum(dataFrame$newReturn)
  
  stdDev <- (1-delta)/(1-delta^nrow(dataFrame))*sum(dataFrame$newReturnDev)
  stdDev <- sqrt(stdDev)
  
  skew <- (1-delta)/(1-delta^nrow(dataFrame))*sum(dataFrame$newReturnSkew)/stdDev^3
  
  kurtosis <- (1-delta)/(1-delta^nrow(dataFrame))*sum(dataFrame$newReturnKurt)/stdDev^4
  
  # kurtosisExcess <- kurtosis - 3
  
  return(list(meanEWMA = round(meanEWMA,5), stdDev = round(stdDev,5), skew = round(skew,5), kurtosis = round(kurtosis,5)))
  
}

## Function to calculate VaR using Delta Normal method
varDeltaNormalMethod <- function(dataFrame, alphaVal = 0.05, delta = 0.95){
  
  # Taking the log of daily returns
  dataFrame$logReturn <- log(dataFrame$Close/dataFrame$Open)
  
  # Find the standard deviation using exponentially decaying weights
  dataFrame$weight <- delta^seq(from = 0, to = nrow(dataFrame) - 1, by = 1)
  dataFrame$newReturn <- dataFrame$logReturn*dataFrame$weight
  dataFrame$newReturnDev <- ((dataFrame$logReturn - mean(dataFrame$logReturn))^2)*dataFrame$weight
  meanVal <- (1 - delta)/(1-delta^nrow(dataFrame))*sum(dataFrame$newReturn)
  stdDev <- (1-delta)/(1-delta^nrow(dataFrame))*sum(dataFrame$newReturnDev)
  stdDev <- sqrt(stdDev)
  
  # Estimate the z value for chose alpha, mean of log return is assumed to be zero
  zValueStdNorm <- qnorm(p = alphaVal, mean = 0, sd = 1, lower.tail = T, log.p = F)
  zValue <- zValueStdNorm*stdDev
  
  # Estimating VaR
  latestDollarValue <- dataFrame$Close[dataFrame$Date == max(dataFrame$Date)]
  var <- (exp(zValue)-1)*latestDollarValue
  
  # Estimating CVaR
  base::set.seed(1234)
  simDataPoints <- rnorm(n = 1000, mean = 0, sd = 1)
  simDataPoints <- simDataPoints[simDataPoints <= zValueStdNorm]
  expectedShortFall <- simDataPoints*stdDev
  expectedShortFall <- mean(expectedShortFall)
  expectedShortFall <- (exp(expectedShortFall) - 1)*latestDollarValue
  
  return(list(cvar = expectedShortFall, var = var, zValue = zValue, latestDollarValue = latestDollarValue, meanVal = meanVal, stdDev = stdDev))
  
}


## Function to calculate VaR using Historical VaR method:
varHistoricalMethod <- function(dataFrame, alphaVal = 0.05){
  
  latestDollarValue <- dataFrame$Close[dataFrame$Date == max(dataFrame$Date)]
  colnames(dataFrame)[colnames(dataFrame) == "dailyReturnPerc"] <- "DailyReturnPerc"
  dataFrame <- dataFrame[,c("Date","Close","DailyReturnPerc")]
  dataFrame <- dataFrame %>%
    arrange(DailyReturnPerc) %>%
    mutate(
      Weight = 1/n(),
      CumWeight = cumsum(Weight))
  alphaVal <-  ifelse(min(dataFrame$CumWeight) > alphaVal, min(dataFrame$CumWeight), alphaVal)
  
  thresholdDlyRetPc <- dataFrame %>%
    filter(CumWeight <= alphaVal) %>%
    filter(CumWeight == max(CumWeight)) %>%
    select(DailyReturnPerc)
  thresholdDlyRetPc <- max(thresholdDlyRetPc$DailyReturnPerc)
  var <- round(thresholdDlyRetPc*latestDollarValue,2)
  
  thresholdMeanDlyRetPc <- dataFrame %>%
    filter(CumWeight <= alphaVal) %>%
    select(DailyReturnPerc)
  thresholdMeanDlyRetPc <- mean(thresholdMeanDlyRetPc$DailyReturnPerc)
  
  cVar <- round(thresholdMeanDlyRetPc*latestDollarValue,2)
  dataFrame <- dataFrame %>%
    mutate(
      Close = round(Close,2),
      DailyReturnPerc = round(DailyReturnPerc,4),
      Weight = round(Weight,6),
      CumWeight = round(CumWeight,6)
    )
  
  return(list(df = dataFrame, var = var, cVar = cVar, thresholdMeanDlyRetPc = thresholdMeanDlyRetPc, thresholdDlyRetPc = thresholdDlyRetPc))
}


## Function to calculate VaR using Hybrid VaR Method:
varHybridHistMethod <- function(dataFrame, alphaVal = 0.05, deltaVal = 0.95){
  
  latestDollarValue <- dataFrame$Close[dataFrame$Date == max(dataFrame$Date)]
  colnames(dataFrame)[colnames(dataFrame) == "dailyReturnPerc"] <- "DailyReturnPerc"
  dataFrame <- dataFrame[,c("Date","Close","DailyReturnPerc")]
  
  dataFrame <- dataFrame %>%
    mutate(
      Weight = deltaVal^(row_number()-1),
      WeightPerc = Weight/sum(Weight)
    ) %>%
    arrange(DailyReturnPerc) %>%
    mutate(
      CumWeight = cumsum(WeightPerc))
  
  alphaVal <-  ifelse(min(dataFrame$CumWeight) > alphaVal, min(dataFrame$CumWeight), alphaVal)
  
  thresholdDlyRetPc <- dataFrame %>%
    filter(CumWeight <= alphaVal) %>%
    filter(CumWeight == max(CumWeight)) %>%
    select(DailyReturnPerc)
  thresholdDlyRetPc <- max(thresholdDlyRetPc$DailyReturnPerc)
  var <- round(thresholdDlyRetPc*latestDollarValue,2)
  
  thresholdMeanDlyRetPc <- dataFrame %>%
    filter(CumWeight <= alphaVal) %>%
    select(DailyReturnPerc)
  thresholdMeanDlyRetPc <- mean(thresholdMeanDlyRetPc$DailyReturnPerc)
  
  cVar <- round(thresholdMeanDlyRetPc*latestDollarValue,2)
  dataFrame <- dataFrame %>%
    mutate(
      Close = round(Close,2),
      DailyReturnPerc = round(DailyReturnPerc,4),
      Weight = round(Weight,6),
      WeightPerc = round(WeightPerc,6),
      CumWeight = round(CumWeight,6)
    )
  colnames(dataFrame)[colnames(dataFrame) == "DailyReturnPerc"] <- "DlyRtrnPrc"
  return(list(df = dataFrame, var = var, cVar = cVar, thresholdMeanDlyRetPc = thresholdMeanDlyRetPc, thresholdDlyRetPc = thresholdDlyRetPc))
}


## Function to calculate VaR using Monte Carlo Simulation:
varMonCarlSim <- function(dataFrame, alphaVal = 0.05, numSims = 10, numCnsctvDays = 3, drawSize = 20){
  
  latestDollarValue <- dataFrame$Close[dataFrame$Date == max(dataFrame$Date)]
  varPc <- vector()
  cVarPc <- vector()
  if(drawSize > nrow(dataFrame)){
    drawSize <- nrow(dataFrame)
  }
  for(i in 1:numSims){
    rowNums <- sample(1:(nrow(dataFrame)-numCnsctvDays), size = ceiling(drawSize/numCnsctvDays), replace = F)
    rowNumsExpnd <- vector()
    rowNumsExpnd <- sapply(rowNums, function(x) c(x:(x+numCnsctvDays-1)))
    rowNumsExpnd <- as.vector(rowNumsExpnd)
    if(length(rowNumsExpnd) > drawSize){
      rowNumsExpnd <- rowNumsExpnd[1:drawSize]
    }
    simDataI <- dataFrame[rowNumsExpnd,"dailyReturnPerc"]
    varPcI <- as.vector(quantile(simDataI, alphaVal))
    cVarPcI <- mean(simDataI[simDataI <= varPcI])
    varPc <- c(varPc,varPcI)
    cVarPc <- c(cVarPc, cVarPcI)
  }
  varPc <- mean(varPc)
  cVarPc <- mean(cVarPc)
  var <- varPc*latestDollarValue
  cVar <- cVarPc*latestDollarValue
  return(list(var = var, cVar = cVar, thresholdMeanDlyRetPc = cVarPc, thresholdDlyRetPc = varPc))
}


# dataFrame <- quantmod::getSymbols(Symbols = "GOOG", src = "yahoo", from = Sys.Date() - round(365*0.1), to = Sys.Date(),auto.assign = F)
# dataFrame <- as.data.frame(dataFrame)
# dataFrame <- tibble::rownames_to_column(dataFrame, "Date")
# colnames(dataFrame) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
# dataFrame  <- dataFrame %>%
#   filter(Open != "null") %>%
#   mutate(
#     Date = as.Date(Date),
#     Open = as.numeric(Open),
#     High = as.numeric(High),
#     Low = as.numeric(Low),
#     Close = as.numeric(Close),
#     Volume = as.numeric(Volume),
#     Adj.Close = as.numeric(Adj.Close),
#     dailyReturnPerc = (Close - Open)/Open
#   ) %>%
#   arrange(desc(Date))

# ggplot(dataFrame, aes(x=logReturn)) +
#   geom_histogram(aes(y=..density..), colour="black", fill="green")+
#   geom_density(alpha=.5, fill=rgb(102/255,130/255,255/255)) +
#   theme_black() +
#   labs(y = "Frequency", x = "Daily Return Percentage")
# 
