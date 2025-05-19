# Require packages
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyvalidate)
library(shinyFeedback)
library(shinybusy)
library(echor)
library(dplyr)
library(plotly)
library(lubridate)
library(purrr)
library(readxl)
library(tools)
# library(tinytex) # if needed
library(xml2)
library(httr)
library(stringr)

# runApp(display.mode = "showcase")


# Define UI for application
shinyUI(
  fluidPage(
    theme = shinytheme("yeti"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      # Script for triggering download from server (if that strategy is used)
      # tags$script(HTML("
      #   Shiny.addCustomMessageHandler('triggerAllParamDownload', function(message) {
      #     var link = document.getElementById(message.id);
      #     if (link) {
      #       console.log('Triggering download for:', message.id);
      #       link.click();
      #     } else {
      #       console.error('Download link not found:', message.id);
      #     }
      #   });
      # "))
    ),
    
    # EPA Header HTML ( 그대로 유지 )
    HTML(" <header class='masthead clearfix' role='banner'> ... </header> ... <div class='main-column clearfix'><h1 class='page-title'>R2 Reasonable Potential Tool BETA</h1> ... </div>"), # Truncated for brevity
    
    hr(),
    useShinyFeedback(),
    add_busy_spinner(spin = 'hollow-dots', color = '#0071bc', position = 'top-right', onstart = TRUE, margins = c('50vh', '50vw')),
    
    bsCollapse(id = 'Overview',
               bsCollapsePanel(title = h3(strong('Overview')), value = 'Overview', p("...") ), # Truncated
               bsCollapsePanel(title = h3(strong('Instructions')), value = 'Instructions', p("...") ) # Truncated
    ),
    
    fluidRow(
      hr(), br(), br(),
      column(4, offset = 1, textInput('NPDESID', label = h3('NPDES ID Input'), value = 'PR0024163')),
      column(4, offset = 1, dateRangeInput('dateRange', label = h3('Dates for analysis'), start = today() %m-% years(5), end = today()))
    ),
    
    fluidRow(column(2, offset = 11, uiOutput('nextBtn'))),
    br(),
    fluidRow(column(3, offset = 10, uiOutput('critBtn'))),
    br(), hr(),
    
    fluidRow(
      column(6, offset = 1, h1(textOutput('facility'))),
      column(5, h2(textOutput('street')), h2(textOutput('citystate')))
    ),
    
    fluidRow(column(4, offset = 1, uiOutput('outfallradio'))),
    br(), br(),
    fluidRow(column(2, offset = 11, uiOutput('nextBtn2'))),
    br(), br(),
    
    fluidRow(column(12, uiOutput('tabs'))),
    br(),
    
    fluidRow(
      column(2, uiOutput('pMaxbox')),
      column(2, uiOutput('pSBxbox')),
      column(2, uiOutput('pSDxbox')),
      column(2, uiOutput('pRWCxbox'))
    ),
    
    # Placeholder for the "Download All Parameter Reports" UI from the module
    # This UI will be rendered by output$download_ap_ui_placeholder in server.R
    fluidRow(
      column(4, offset = 8, align="right", # Adjusted column width and offset
             uiOutput('download_ap_ui_placeholder') 
      )
    ),
    
    br(), br(), br(),    
    HTML(" <footer class='main-footer clearfix' role='contentinfo'> ... </footer> ") # Truncated
  )
)

