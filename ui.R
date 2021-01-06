# Installing and reading in the necessary libraries:
libList <- c("dplyr", "DT", "GA", "shiny","shinythemes","shinydashboard","shinyWidgets","shinyjs", "shinyalert","shinyFeedback","ggplot2","shinyBS", "shinyjs","shinycssloaders","gtable","grid","gridExtra","quantmod")

new.packages <- libList[!(libList %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "https://cloud.r-project.org/")
lapply(libList, require, character.only = TRUE)


# Reading in the functions used in analysis
source('functionsDirectory.R')

## Building the ui.R -----------------------------------------------------------

## 1. Header ----------------------------------------------

header <- dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 20px;font-size:20px;font-weight:bold;line-height:20px;"),
          tags$style(".navbar {min-height:1px !important;font-weight:bold;")),title ="Quantitative FRM - Market Risk Analysis",
  tags$li(a(img(src = 'nameLogo.png'),href='https://www.linkedin.com/in/ashwini-jha-009646125/',
            style = "padding-top:0px; padding-bottom:0px;"),class = "dropdown"),titleWidth = 390)


## 2. Sidebar ----------------------------------------------

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id = 'sidebar',
    style = "position: relative; overflow: visible;",
    
    ## 1st tab show the data distribution and basic statistics -----------
    menuItem( "Distribution Description", tabName = 'tabDistributionStats', icon = icon('database'),
              badgeLabel = NULL, badgeColor = "green" ),
    
    ## 2nd tab show VaR -----------
    menuItem( "VaR Estimates", tabName = 'tabVar', icon = icon('exclamation-triangle'),
              badgeLabel = NULL, badgeColor = "green" ),
    
    ## 3rd tab to fit EVT distributions
    # menuItem( "Extreme Value Theory", tabName = 'tabEvt', icon = icon('dice'),
    #           badgeLabel = NULL, badgeColor = "green" ),
    
    ## 4th tab with Read Me note
    menuItem( "Read Me", tabName = 'readMe', icon = icon('align-justify'),
              badgeLabel = NULL, badgeColor = "green" )
  )
)

## 3. Body --------------------------------
bodyD <- dashboardBody(
  
  ## 3.0 Setting skin color, icon sizes, etc. ------------
  
  ## modify the dashboard's skin color
  tags$style(HTML('
                       /* logo */
                       .skin-blue .main-header .logo {
                       background-color: #006272;
                       }
                       /* logo when hovered */
                       .skin-blue .main-header .logo:hover {
                       background-color: #006272;
                       }
                       /* navbar (rest of the header) */
                       .skin-blue .main-header .navbar {
                       background-color: #006272;
                       }
                       /* active selected tab in the sidebarmenu */
                       .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                       background-color: #006272;
                                 }
                       ')
  ),
  
  ## modify icon size in the sub side bar menu
  tags$style(HTML('
                       /* change size of icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview-menu>li>a>.fa {
                      font-size: 15px;
                      }
                      .sidebar .sidebar-menu .treeview-menu>li>a>.glyphicon {
                      font-size: 13px;
                      }
                      /* Hide icons in sub-menu items */
                      .sidebar .sidebar-menu .treeview>a>.fa-angle-left {
                      display: none;
                      }
                      '
  )) ,
  
  ## making background black
  setBackgroundColor(
    color = "black",
    gradient = "radial",
    shinydashboard = T
  ),
  
  
  ## 3.1 Dashboard body --------------
  
  tabItems(
    
    
    ## 3.1 Distribution Description ----------------------------------------------------------
    tabItem(
      tabName = "tabDistributionStats",
      
      ## 1.1 Take inputs about company and time period
      fluidRow(
        box(width = 12,
            background = "black",
            align = "bottom",
            splitLayout(
              cellWidths = c("10%","50%","20%","20%"),
              selectInput(inputId = "selSec", label = "Select Equity", choices = exchangeData$Symbol,
                          selected = "SBIN",multiple = F, selectize = F, width = "200px",
                          size = 1),
              infoBox(title = "Company",value = textOutput("nameAndExchange"), icon = icon("building"), color = "olive", fill = T, width = 10),
              dateRangeInput(inputId = "dateIp", label = "Timeline", start = Sys.Date() - round(365*0.25),
                             end = Sys.Date(), min = "2005-01-01", max = Sys.Date(), autoclose = T),
              # actionBttn(inputId = "tab1", label = "Analyze", icon = icon("arrow-alt-circle-right"), style = "jelly", color = "success", size = "lg", block = T, no_outline = F)
              actionButton(inputId = "tab1", "Analyze", icon = icon("arrow-alt-circle-right"),
                           width = 200,
                           style="color: #fff; background-color: #006272; border-color: #006272;margin-top: 23px")
              )
            )
      ),
      
      
      # 1.2 Display chart for closing value and daily return percentage
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Daily Closing Value and Daily Return Percentage",style = "background-color:#000000; color:#FFFFFF;")
        )
      ),
      fluidRow(
        column(10, align="center", offset = 1,
               plotOutput("plotDailyReturn",height=500,
                          click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                          hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                          brush = brushOpts(id = "plot_brush")
               )
        )
      ),
      br(),
      
      # 1.3 Take input for delta and show half life of data and % weight covered
      chooseSliderSkin(skin = "Flat", color = "#006272"),
      fluidRow(
        box(width = 12,
            background = "black",
            splitLayout(
              cellWidths = c("16%", "28%", "28%", "28%"),
              sliderInput(inputId = "ipDelta", label = "Weight Decay Rate", min = 0.80, max = 0.99, value = 0.95, step = 0.01, round = F, ticks = F, animate = F),
              infoBox(title = "Half Life of Data",value = textOutput("halfLife"), icon = icon("adjust"), color = "olive", fill = T, width = 10),
              infoBox(title = "Selected Data Size",value = textOutput("sizeData"), icon = icon("circle-o"), color = "olive", fill = T, width = 10),
              infoBox(title = "% Data Selected",value = textOutput("percFullData"), icon = icon("circle-o-notch"), color = "olive", fill = T, width = 10)
            ))
      ),
      fluidRow(
        column(2, align="center", offset = 4,
               # actionBttn(inputId = "cnfrmDelta", label = "Confirm Delta", icon = icon("arrow-alt-circle-right"), style = "jelly", color = "success", size = "md", block = T, no_outline = F),
               actionButton(inputId = "cnfrmDelta", "Confirm Delta", icon = icon("arrow-alt-circle-right"),
                            width = 450,
                            style="color: #fff; background-color: #006272; border-color: #006272;"),
               tags$style("display:center-align; height: 70px;")
        )
      ),
      br(),
      # Summary Statistics
      fluidRow(
        box(width = 12,
            background = "black",
            splitLayout(
              cellWidths = c("25%","25%","25%","25%"),
              infoBox(title = "Mean (EWMA)",value = textOutput("meanEWMA"), icon = icon("calculator"), color = "olive", fill = T, width = 10),
              infoBox(title = "Std Dev",value = textOutput("stdDev"), icon = icon("calculator"), color = "olive", fill = T, width = 10),
              infoBox(title = "Skewness",value = textOutput("skew"), icon = icon("calculator"), color = "olive", fill = T, width = 10),
              infoBox(title = "Kurtosis",value = textOutput("kurtosis"), icon = icon("calculator"), color = "olive", fill = T, width = 10)
            )
        )
      ),
      # Plot for Daily Return Percentage
      fluidRow(
        column(8, align="center", offset = 2,
               plotOutput("dlyRetPrc",height=500,
                          click = "plot_click",  # Equiv, to click=clickOpts(id="plot_click")
                          hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                          brush = brushOpts(id = "plot_brush")
               )
        )
      )
      
    ),
    
    
    
    
    
    # 3.2 Var Estimates:
    tabItem(
      tabName = "tabVar",
      
      # Input of delta level of VaR
      chooseSliderSkin(skin = "Flat", color = "#006272"),
      fluidRow(
        box(width = 12,
            background = "black",
            align = "center",
            splitLayout(
              cellWidths = c("25%", "15%","25%", "35%"),
              sliderInput(inputId = "ipVar", label = "Alpha Level of VaR", min = 0.01, max = 0.20, value = 0.05, step = 0.01, round = F, ticks = F, animate = F),
              h5("Illustratitve Image of Alpha",style = "background-color:#000000; color:#FFFFFF;text-align: right;"),
              imageOutput("alphaDepiction", width = 252, height = 134),
              actionButton(inputId = "cnfrmVar", "Confirm Alpha", icon = icon("arrow-alt-circle-right"),
                           width = 400,
                           style="color: #fff; background-color: #006272; border-color: #006272;margin-top: 50px")
            ))
      ),
      
      ## Delta-Normal Method section
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Delta-Normal Approach",style = "background-color:#000000; color:#FFFFFF;")
        )
      ),
      
      fluidRow(
        box(width = 6,
            background = "black",
            plotOutput("deltaNormalPlot")
        ),
        box(width = 6,
            background = "black",
            infoBox(title = "Expected Shortfall",value = textOutput("DN_cvar"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "VaR",value = textOutput("DN_var"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Last Closing Value",value = textOutput("DN_latestDollarValue"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Mean of Log Return",value = textOutput("DN_mean"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Std Dev of Log Return",value = textOutput("DN_stdDev"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Z Score for alpha",value = textOutput("DN_zValue"), icon = icon("calculator"), color = "olive", fill = T, width = 6)
        )
      ),
      
      ## Historical Method section
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Historical Approach",style = "background-color:#000000; color:#FFFFFF;")
        )
      ),
      
      fluidRow(
        box(width = 6,
            background = "olive",
            dataTableOutput("hist_df")
        ),
        box(width = 6,
            background = "black",
            infoBox(title = "Expected Shortfall",value = textOutput("hist_cvar"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "VaR",value = textOutput("hist_var"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Threshold Daily Return",value = textOutput("hist_thresholdDlyRetPc"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Mean Below Threshold",value = textOutput("hist_thresholdMeanDlyRetPc"), icon = icon("calculator"), color = "olive", fill = T, width = 6)
        )
      ),
      
      
      ## Hybrid Historical Method section
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Hybrid Historical Approach",style = "background-color:#000000; color:#FFFFFF;")
        )
      ),
      
      fluidRow(
        box(width = 6,
            background = "olive",
            dataTableOutput("hybridHist_df")
        ),
        box(width = 6,
            background = "black",
            infoBox(title = "Expected Shortfall",value = textOutput("hybridHist_cvar"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "VaR",value = textOutput("hybridHist_var"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Threshold Daily Return",value = textOutput("hybridHist_thresholdDlyRetPc"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Mean Below Threshold",value = textOutput("hybridHist_thresholdMeanDlyRetPc"), icon = icon("calculator"), color = "olive", fill = T, width = 6)
        )
      ),
      
      
      ## Monte Carlo Simulation Method section
      fluidRow(
        column(8, align="center", offset = 2,
               h3("Monte Carlo Simulation",style = "background-color:#000000; color:#FFFFFF;")
        )
      ),
      
      fluidRow(
        chooseSliderSkin(skin = "Flat", color = "#006272"),
        box(width = 6,
            background = "olive",
            align = "center",
            sliderInput(inputId = "ipNumSims", label = "Number of Simulations", min = 10, max = 100, value = 70, step = 10, round = F, ticks = F, animate = F),
            sliderInput(inputId = "ipNumCnsctvDays", label = "Number of Serial Days", min = 1, max = 7, value = 3, step = 1, round = F, ticks = F, animate = F),
            sliderInput(inputId = "ipDrawSize", label = "Sample Size", min = 20, max = 60, value = 30, step = 5, round = F, ticks = F, animate = F),
            actionButton(inputId = "runSims", "Run Simulations", icon = icon("dice"),
                         width = 200,
                         style="color: #fff; background-color: #006272; border-color: #006272;")
        ),
        box(width = 6,
            background = "black",
            infoBox(title = "Expected Shortfall",value = textOutput("MCS_cvar"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "VaR",value = textOutput("MCS_var"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Threshold Daily Return",value = textOutput("MCS_thresholdDlyRetPc"), icon = icon("calculator"), color = "olive", fill = T, width = 6),
            infoBox(title = "Mean Below Threshold",value = textOutput("MCS_thresholdMeanDlyRetPc"), icon = icon("calculator"), color = "olive", fill = T, width = 6)
        )
      )
      
      
      
    ),# Tabitem closes here
    
    
    # 3.n Read Me:
    # tabItem(
    #   tabName = "tabEvt",
    #   
    #   h1("Coming soon......",style = "background-color:#000000; color:#FFFFFF;")
    # ),
    
    
    # 3.n Read Me:
    tabItem(
      tabName = "readMe",
      
      fluidRow(
        column(8, align="left", offset = 2,
               h1("Namaste",style = "background-color:#000000; color:#FFFFFF;font-style: italic;")
        )
      ),
      
      column(8, align="left", offset = 2,
             htmlOutput('readMeNote'),
             tags$head(tags$style("#readMeNote{color: #FFFFFF;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"
             )
             )
      )
    )
    
    
    
  ) # Tabitems closes here
  
  
) # dashboardBody closes here



## put UI together --------------------
ui <-  dashboardPage(header, sidebar, bodyD)
