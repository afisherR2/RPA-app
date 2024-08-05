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
library(tinytex)
library(xml2)
library(httr)
library(stringr)
library(tidyr)


# receiving water concentration function ---------------------------------------
RWC <- function(value, p, dr, dates1, dates2){ # value = , p = parameter, dr = dilution ratio, dates1 = dateslider input 1, dates2 = dateslider input 2
    
    # create empty list
    RWCval <- list()
    
    # filter dmr by parameter and nodi code, select value number
    db <- value %>% 
        filter(parameter_desc == p &
                   nodi_code != 'B') %>% 
        filter(between(monitoring_period_end_date, dates1, dates2)) %>%  # fitler to only be inside the date range slider
        select(dmr_value_nmbr)
    
    df <- db$dmr_value_nmbr %>%  # sort
        sort() %>% 
      as.numeric()

    
    n <- length(df) # number of samples
    max <- max(df) # max of samples
    
    RWCval$min <- min(df) # min of samples
    RWCval$n <- n # number of samples
    RWCval$max <- max # max of samples
    
    m <- mean(df)  # mean of samples
    
    sd <- sd(df) # standard deviation
    
    # assing values to list
    RWCval$m <- m %>% 
        round(2)
    RWCval$sd <- sd %>% 
        round(2)
    
    # coefficient of variation
    if (n > 10) {
        # cv <- round(sd/m,2)
        cv <- (sd/m)
    } else {
        cv <- 0.6
    }
    
    RWCval$cv <- cv %>% 
        round(2)
    
    cv2 <- cv^2 # cv squared
    
    # Percentile Pn
    # for n >= 20, n = 20, else (1 - 0.95)^(1/n)
    if (n >= 20) {
        x <- (1 - 0.95)^(1/20)
    } else {
        x <- (1 - 0.95)^(1/n)}
    
    # z score
    z95 <- 1.645
    zx <- qnorm(x) 
    
    RWCval$z95 <- z95
    RWCval$zx <- round(zx,3)
    
    # FROM "notes on PR DMR and RPA Tools" page 5 - on PR Qlick ShapePoint
    RPM <- (exp(z95*log(1+cv2)^0.5 - (0.5*log(1+cv2)))) / (exp(zx*log(1+cv2)^0.5 - (0.5*log(1+cv2))))
    
    RWCval$RPM <- RPM %>%
        round(2)
    
    # RWC equation
    RWC <- round(max * RPM / as.numeric(dr), 2)
    
    RWCval$RWC <- RWC
    
    return(RWCval)
}



#-------------------------------------------------------------------------------
# Define server logic 
shinyServer(function(input, output) {

    
# Pull facility info from ECHO -------------------------------------------------
    dfinfo1 <- eventReactive(input$nextBtn, {

        req(input$NPDESID) # BREAK must have NPDES ID and WQS input file
        
        feedbackDanger('NPDESID', nchar(input$NPDESID) != 9, # throw error if NPDES isnt 9 char
                        'please enter a valid NPDES ID',
                        icon = NA, color = '#b30000')
        
        # Throw error if the first two characters of the NPDES ID does not match the first two characters of the WQS file
        # feedbackDanger('NPDESID', substr(input$WQSinput$name, 1, 2) != substr(input$NPDESID, 1, 2), 
        #                'please double check the NPDES ID and WQS file',
        #                icon = NA, color = '#b30000')
        
       # feedbackDanger('WQSinput', file_ext(input$WQSinput$datapath) != 'xlsx', # BREAK if NPDES ID isnt valid
       # 'please upload a properly formatted WQS file',
       # icon = NA, color = '#b30000')

        req(nchar(input$NPDESID) == 9) # BREAK if NPDES ID isnt 9 char
       # req(substr(input$WQSinput$name, 1, 2) == substr(input$NPDESID, 1, 2)) # break if NPDES ID doesn't match WQS file
       # req(file_ext(input$WQSinput$datapath) == 'xlsx') # BREAK check for WQS file upload
        
        # showNotification('Let me just pull some files from ICIS-NPDES', type = 'message')
        
        echoWaterGetFacilityInfo(p_pid = input$NPDESID,
                                 output = 'df',
                                 qcolumns = '1,3,4,5,7')

    }, ignoreNULL = FALSE)
    

# NPDES ID validity check ---------------------------------------------------
    dfinfo2 <- reactive({
        req(dfinfo1()) # BREAK dfinfo1
       # req(input$WQSinput) # BREAK for WQS upload
        
        feedbackDanger('NPDESID', nrow(dfinfo1()) != 1, # BREAK if NPDES ID isnt valid
                       'please enter a valid NPDES ID',
                       icon = NA, color = '#b30000')
        
        req(nrow(dfinfo1()) == 1) # BREAK check if NPDES ID is valid
        
        dfinfo1()
        })
    
# Display NPDES ID and Address -------------------------------------------------
    observeEvent(input$nextBtn, {
      
      output$facility <- renderText(dfinfo2()$CWPName)
      output$street <- renderText(dfinfo2()$CWPStreet)
      output$citystate <- renderText({paste(dfinfo2()$CWPCity, 
                                            dfinfo2()$CWPState,
                                            sep = ', ')})
    })

        
### Pull DMR with echor after first next button --------------------------------
    dmr <- eventReactive(input$nextBtn, {
       req(dfinfo2()) # BREAK for WQSinput and dfinfo2
        echoGetEffluent(p_id = input$NPDESID, # pull DMR
                        start_date = format(input$dateRange[1], '%m/%d/%Y'), # 10 years ago
                        end_date = format(input$dateRange[2], '%m/%d/%Y')) %>% # todays date
                        # start_date = format(today() %m-% years(10), '%m/%d/%Y'), # 10 years ago
                        # end_date = format(today(), '%m/%d/%Y')) %>% # todays date
            suppressWarnings() # suppress parsing warning
        
    }, ignoreNULL = FALSE)
    
# read WQS file ----------------------------------------------------------------
    # WQSdf <- eventReactive(input$nextBtn, {
    #     req(input$WQSinput, dfinfo2()) # BREAK for WQSinput and dfinfo2
    #     WQSdf <- read_xlsx(input$WQSinput$datapath, sheet = 'WQS')
    #     return(WQSdf)
    # })
    
# Select outfall to use --------------------------------------------------------
    observeEvent(input$nextBtn,{
       req(dfinfo2())  # BREAK for WQSinput and dfinfo2
        
        otfl <- dmr() %>%  # filter for only internal outfalls
            filter(perm_feature_type_code == 'EXO') %>% 
            select(perm_feature_nmbr) %>% 
            distinct()
        
        output$outfallradio <- renderUI({
                radioButtons('radiob', label = h2('Select Outfall to Use'),
                                   choices = otfl$perm_feature_nmbr)
                })
    })
    
# BUTTONS ----------------------------------------------------------------------
        
# First next button
        output$nextBtn <- renderUI({
            actionButton('nextBtn', 
                         label = '',
                         icon = icon('angle-down'))})
        
# Read in WQS from OST criteria search tool
        wqsraw <- eventReactive(input$nextBtn, {
            
            req(dfinfo2())  # BREAK for WQSinput
            
            # download criteria form EPA OST Criteria Search Tool
            GET('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx', 
                write_disk(tf <- tempfile(fileext = ".xlsx")))

            wqsraw <- read_excel(tf,
                                 skip = 206) # skip the first 206 lines b/c the flat file is formatted weird
        })
        
# link to standards
        observeEvent(input$nextBtn, {

          # match NPDES ID to Entity_Abbr in criteria_sources.csv
          crit_source <- read.csv('www/CRITERIA_SOURCES.csv')
          
          crit_url <- crit_source %>%
            filter(ENTITY_ABBR == substr(input$NPDESID, 1, 2)) %>%
            select(CRIT_SOURCE1) %>%
            pull() # pull url to standards

          # constructing onclick url
          crit_url <- paste0('window.open(', "'", crit_url, "'", ')')


          # create button to standards
          output$critBtn <- renderUI({
            actionButton('critBtn',
                         label = 'Link to Standards',
                         onclick = crit_url)
          })

        })
        
# Second next button 
    observeEvent(input$nextBtn, {
        
        req(dfinfo2())  # BREAK for WQSinput and dfinfo2
        
        output$nextBtn2 <- renderUI(actionButton('nextBtn2', 
                                                 label = '',
                                                 icon = icon('angle-down'))) 
    })


# filter dmr by outfall checkbox, stat base, monitoring location ---------------
    
# download ECHO REF parameter file to link DMR parameter codes to xwalk pollutant codes
    
    echo_ref_p <- eventReactive(input$nextBtn2, { 
      GET('https://echo.epa.gov/system/files/REF_Parameter.csv', 
          write_disk(tf <- tempfile(fileext = ".csv")))
      
      echo_ref_p <- read.csv(tf)
      })
    
    
# change dmr value to numeric and monitoring period  to date
    dmr_of <- eventReactive(input$nextBtn2, {
        
        req(input$radiob)  # BREAK check if outfall is selected
      
      
        
            dmr() %>%
                filter(perm_feature_nmbr == as.character(input$radiob) & # checkbox
                           statistical_base_type_code == 'MAX' & # statistical base type code
                           monitoring_location_code == 1 & # OR 'EG' monitoring location (gross effluent)
                           value_type_code == 'C3' & # concentration based measurements
                           perm_feature_type_code == 'EXO' & # external outfall
                            !(dmr_value_nmbr == '')) %>% # REMOVE dmr_value_nmbr with missing values
                
                select(npdes_id, parameter_desc, perm_feature_nmbr, parameter_code, value_type_code, # select a subset of the database
                       value_type_desc, monitoring_period_end_date,
                       dmr_value_nmbr, dmr_unit_desc, nodi_code) %>%
                
                mutate(dmr_value_nmbr = as.numeric(dmr_value_nmbr), # change to numeric
                       parameter_code = str_remove(parameter_code, '^0+'), # remove leading zeros in parameter code
                       monitoring_period_end_date = as.Date(mdy(monitoring_period_end_date, tz = 'EST')), # change to date
                       dmr_unit_desc = recode(dmr_unit_desc, `#/100mL` = 'MPN/100mL'), # recode unit
                       dmr_unit_desc = na_if(dmr_unit_desc, '')) %>% # if dmr unit is missing, replace with NA
              
              group_by(parameter_code) %>% # group by parameter code
              fill(dmr_unit_desc, .direction = 'downup') %>% # fill NA unit values by parameter
              ungroup() %>%  # ungroup
              
              # recode dmr parameter codes with pollutant codes
              mutate(POLLUTANT_CODE_ECHO = across(parameter_code, ~ with(echo_ref_p(), POLLUTANT_CODE[match(.x, PARAMETER_CODE)]),
                                                  .names = 'POLLUTANT_CODE_ECHO'), .before = parameter_code) %>%
              unpack(POLLUTANT_CODE_ECHO)
            
    }, ignoreNULL = FALSE)
    
    
    # cross reference with ECHO_CST_Xwalk file - cross references CST parameter names and ECHO parameter names
    xwalk <- eventReactive(input$nextBtn2, { 
      xwalk <- read_excel('www/ECHO_CST_Xwalk.xlsx')
      })
    

# filter standards for entity abbreviation and cross walk with CST and WQP
    wqs <- eventReactive(input$nextBtn2, {

      
      wqsfine <- wqsraw() %>% 
        filter(ENTITY_ABBR == substr(input$NPDESID, 1, 2)) %>% 
        
        select(c(ENTITY_NAME, STD_POLL_ID, STD_POLLUTANT_NAME, CRITERION_VALUE, UNIT_NAME,
                 CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
                 USE_CLASS_NAME_LOCATION_ETC_ID, USE_CLASS_NAME_LOCATION_ETC, EFFECTIVE_DATE, LAST_ENTRY_IN_DB)) %>% 
        
        # cross walk ECHO parameter names with CST parameter names
        mutate(POLLUTANT_CODE_ECHO = across(STD_POLL_ID, ~ with(xwalk(), POLLUTANT_CODE[match(.x, STD_POLL_ID_CST)]),
                                            .names = 'POLLUTANT_CODE_ECHO'),  .before = STD_POLL_ID) %>%
        unpack(cols = POLLUTANT_CODE_ECHO)

    })
    

# Insert tabset for parameters -------------------------------------------------
# code structure from: https://thatdatatho.com/how-to-create-dynamic-tabs-with-plotly-plots-in-r-shiny/
    observeEvent(input$nextBtn2, {

        # output$pdr <- renderUI(
        #     numericInput('DR', 
        #                  label = h3('Dilution Ratio:'), width = '50%',
        #                  value = 1))
            
        # icon color code, altered from: https://community.rstudio.com/t/colors-next-to-checkboxes-in-shiny/74908/7
        output$pMaxbox <- renderUI(
            checkboxInput('Maxbox', 
                          label = HTML('<div style="display:flex">
                          <i class="fas fa-minus fa-2x"style="color:#dfc27d;margin-top:-4px;"></i>
                          <i class="fas fa-minus fa-2x"style="color:#dfc27d;margin-top:-4px;margin-left:-3px;"></i>
                          <div style="color:black;padding-left:5px;\"><h3>Max Value</h3></div></div>'),
                          # label = HTML(x_format('#dfc27d', h3('Max Value'))), fas fa-minus
                          value = FALSE))
        
        output$pSBxbox <- renderUI(
            checkboxInput('SBxbox', 
                          label = HTML('<div style="display:flex">
                          <i class="fas fa-ellipsis fa-2x"style="color:#bf812d;margin-top:-4px;">
                          </i><div style="color:black;padding-left:5px;\"><h3>WQS-SB</h3></div></div>'),
                          # label = HTML(x_format('#bf812d', h3('WQS - SB'))),
                          value = FALSE))
        
        output$pSDxbox <- renderUI(
            checkboxInput('SDxbox',
                          label = HTML('<div style="display:flex">
                          <i class="fas fa-minus fa-2x"style="color:#8c510a;margin-top:-4px;"></i>
                          <i class="fas fa-minus fa-2x"style="color:#8c510a;margin-top:-4px;margin-left:7px;"></i>
                          <div style="color:black;padding-left:5px;\"><h3>WQS-SD</h3></div></div>'),
                          # label = HTML(x_format('#8c510a',h3('WQS - SD'))),
                          value = FALSE))
        
        output$pRWCxbox <- renderUI(
            checkboxInput('RWCxbox',
                          label = HTML('<div style="display:flex">
                          <i class="fas fa-minus fa-2x"style="color:#543005;margin-top:-4px;"></i>
                          <i class="fas fa-square fa-2x"style="color:white;margin-top:-4px;margin-left:-6px;"></i>
                          <i class="fas fa-minus fa-2x"style="color:#543005;margin-top:-4px;margin-left:-7px;"></i>
                          <i class="fas fa-square fa-2x"style="color:white;margin-top:-4px;margin-left:-6px;"></i>
                          <div style="color:black;padding-left:5px;margin-left:-20px;\"><h3>RWC</h3></div></div>'),
                          # label = HTML(x_format('#543005',h3('RWC'))),
                          value = FALSE))
        
        
            
        paramtab <- reactiveValues(
          nparam = sort(unique(dmr_of()$parameter_desc))) # list of parameters
        
        # paramtab <- reactiveValues(nparam = sort(unique(dmr_of()$parameter_desc))) # list of parameters
        
        output$tabs <- renderUI({ # render UI
            
            # useShinyjs()
            
            map(paramtab$nparam, # map over the list of parameters - parameters are designated with 'p'
                function(p){
                    
                  # reactive element for summary stats and RWC calcclations
                    pstats <- reactive({
                      RWCvalues <- RWC(dmr_of(), p, pdr(), input$dateSlider[1], input$dateRange[2]) # list of n, mean, max, CV, Z95, Zx, RPM, and RWC
                      
                      data.frame(
                        n = RWCvalues$n,
                        min = RWCvalues$min,
                        m = RWCvalues$m,
                        max = RWCvalues$max,
                        RWC = RWCvalues$RWC,
                        stringsAsFactors = FALSE)
                    })
                    
                    # reactive element for dilution ration
                    pdr <- reactive({
                      as.numeric(input$DR)
                    })

                    
                    # dates of RP evaluation
                    evdates <- dmr_of() %>%
                      filter(parameter_desc == p) %>%
                      select(monitoring_period_end_date) %>%
                      summarise(min = min(monitoring_period_end_date),
                                max = max(monitoring_period_end_date))

                    sdate <- evdates$min # earliest sample date for selected p
                    edate <- evdates$max # todays date

                    
                    # units - from DMR file
                        punits <- dmr_of() %>%
                            filter(parameter_desc == p) %>%
                            select(dmr_unit_desc) %>%
                            unique()
                        
                    # link DMR parameter desc p to dmr POLLUTANT_CODE
                        pc <- dmr_of()  %>%
                          filter(parameter_desc == p) %>%
                          select(POLLUTANT_CODE_ECHO) %>%
                          unique() %>% 
                          pull() %>% 
                          as.numeric()
                        
                    # units - from WQS file
                    if(pc %in% wqs()$POLLUTANT_CODE_ECHO == TRUE){
                      
                        wsbunits <- wqs() %>% 
                          filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5088', '5087')) %>%  #5088 for class sb waters
                          select(UNIT_NAME) %>% 
                          distinct() %>% 
                          pull() %>% 
                          as.character()

                        
                        wsdunits <- wqs() %>% 
                          filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>%  #5089 for class sd waters
                          select(UNIT_NAME) %>% 
                          distinct() %>% 
                          pull() %>% 
                          as.character()
                        
                    }
                    else {
                      wsbunits <- NA
                      wsdunits <- NA
                    }

                    
                    # WQS SB
                    if(pc %in% wqs()$POLLUTANT_CODE_ECHO == TRUE){
                      
                        wqsb <- wqs() %>% 
                          filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5088', '5087')) %>%  # 5088 for class SB waters and 5087 for surface waters
                          select(CRITERION_VALUE) %>% 
                          pull() %>% 
                          str_remove(',') %>% 
                          unique()
                        
                        # check to see if there is a value
                        if (length(wqsb) == 0){
                          
                          wqsb <- NA
                          
                          # check for numeric or "See Equation"
                        } else if (!is.na(as.numeric(wqsb)) == TRUE) {
                          wqsb <- wqsb %>% 
                            as.numeric()
                        }

                    }
                    else {
                        wqsb <- NA

                    }

                    
                    #WQS SD
                    if(pc %in% wqs()$POLLUTANT_CODE_ECHO == TRUE){
                      
                        wqsd <-  wqs() %>% 
                          filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>%  # 5088 for class sd waters, 5087 for surface waters, and 5092 for SD/drinking water
                          select(CRITERION_VALUE) %>% 
                          pull() %>% 
                          unique()
                        
                        # check for numeric or "See Equation"
                        if (!is.na(as.numeric(wqsd)) == TRUE) {
                          wqsd <- wqsd %>% 
                            as.numeric()
                        }
                          
                    }
                    else {
                        wqsd <- NA
                    }

                    # time series plot ui display
                    pl <- dmr_of() %>%
                        filter(parameter_desc == p) %>%
                        ggplot(., aes(x = monitoring_period_end_date, y = dmr_value_nmbr,
                                      text = paste0('Monitoring Period End Date: ', monitoring_period_end_date,
                                                    '<br>Concentration: ', dmr_value_nmbr, ' ', punits),
                                      group = p)) +
                        geom_line(color = '#01665e') +
                        geom_area(position = position_dodge(width = 1), fill = '#c7eae5', alpha = .5) +
                        xlab('Date') +
                        ylab(paste(p, '(', punits, ')')) +
                        theme_light(base_size = 15) +
                        scale_x_date(date_breaks = '1 year',
                                     date_labels = '%Y')
                    
                    # time series plot for report
                    ppl <- reactive({
                        pl + geom_hline(yintercept = pstats$max,
                                              color = '#dfc27d', linetype = 'solid') +
                            
                            geom_hline(yintercept = pstats$RWC,
                                          color = '#543005', linetype = 'dashed') +
                            
                            geom_hline(yintercept = ifelse(is.na(wqsb) == TRUE, 0, wqsb),
                                       alpha = ifelse(is.na(wqsb) == TRUE, 0, 1),
                                       color = '#bf812d', linetype = 'dotted') +
                            
                            geom_hline(yintercept = ifelse(is.na(wqsd) == TRUE, 0, wqsd),
                                       alpha = ifelse(is.na(wqsd) == TRUE, 0, 1),
                                       color = '#8c510a', linetype = 'longdash') +
                            theme(legend.position = 'bottom')
                            })
                    
                    # data table for report with select columns and modified names
                    rdmr <- reactive({
                        rdmr.r <- dmr_of() %>% 
                                    filter(parameter_desc == p) %>% 
                                    as_tibble() %>% 
                            select(npdes_id, perm_feature_nmbr, parameter_desc,
                                   monitoring_period_end_date, dmr_value_nmbr, 
                                   dmr_unit_desc) %>% # nodi_code
                            
                            rename(`NPDES ID` = npdes_id) %>% 
                            rename(Outfall = perm_feature_nmbr) %>% 
                            rename(Parameter = parameter_desc) %>% 
                            rename(`Monitoring Period` = monitoring_period_end_date) %>% 
                            rename(Value = dmr_value_nmbr) %>% 
                            rename(Unit = dmr_unit_desc)
                            # rename(`NODI Code` = nodi_code)
                    })
                    
                
                    tabPanel(title = h3(p), # tab panel for each parameter


                                     fluidRow(
                                         sidebarLayout(
                                             sidebarPanel(
                                               
                                             # Summary statistics
                                             h4(strong(renderText('Summary Stats'))),
                                             
                                             br(),
                                             
                                             h5(renderText(
                                                 paste('# Samples : ',
                                                       pstats()$n))),
                                             
                                             h5(renderText(
                                                 paste('Min : ',
                                                       pstats()$min, ' ',
                                                       punits))),
                                             
                                             h5(renderText(
                                                 paste('Mean : ',
                                                       pstats()$m, ' ',
                                                       punits))),
                                                 
                                             h5(renderText(
                                                 paste('Max : ',
                                                      pstats()$max, ' ',
                                                      punits))),
                                             
                                             br(),
                                             
                                             h5(renderText(
                                                 paste('WQS - SB :',
                                                       wqsb, ' ', 
                                                       wsbunits))), #punits
                                             
                                             h5(renderText(
                                                 paste('WQS - SD :', 
                                                       wqsd, ' ', 
                                                       wsdunits))), #punits
                                             
                                             br(),
                                             
                                             h5(renderText(
                                                 paste('RWC : ', 
                                                       pstats()$RWC, ' ', 
                                                       punits))),
                                             
                                             br(),
                                             
                                             # dilution ratio
                # NEED to make a reactive element
                                             output$pdr <- renderUI(
                                             numericInput('DR',
                                                          label = h5('Dilution Ratio:'), width = '50%',
                                                          value = 1)),
                                             
                                             br(),
                                             
                                             # Date range slider
                                             
                                             output$Dslider <- renderUI(
                                               sliderInput('dateSlider', h5('Date Range:'),
                                                           min = as.Date(sdate, '%Y-%m-%d'), 
                                                           max = as.Date(edate, '%Y-%m-%d'),
                                                           value = c(as.Date(sdate, '%Y-%m-%d'), as.Date(edate, '%Y-%m-%d')),
                                                           ticks = FALSE,
                                                           timeFormat = '%Y')),
                                             

                                             width = 4), # width of the panel
                                             
                                         # modify time series plot by checkbox input
                                         mainPanel(
                                             
                                             output$pplot <- renderPlotly({
                                               
                                               pl <- pl + coord_cartesian(xlim = input$dateSlider)
                                               
                                             
                                             if (input$Maxbox == TRUE) {
                                                 pl <- pl + geom_hline(yintercept = pstats()$max,
                                                                       color = '#dfc27d', linetype = 'solid') +
                                                     theme(text = element_text(family = 'Merriweather'))}

                                             if (input$SBxbox == TRUE && is.na(wqsb) == FALSE) {
                                                 pl <- pl + geom_hline(yintercept = wqsb,
                                                                       color = '#bf812d', linetype = 'dotted') +
                                                     theme(text = element_text(family = 'Merriweather'))}
                                                 
                                             if (input$SDxbox == TRUE && is.na(wqsd) == FALSE) {
                                                 pl <- pl + geom_hline(yintercept = wqsd,
                                                                       color = '#8c510a', linetype = 'longdash') +
                                                     theme(text = element_text(family = 'Merriweather'))}
                                                 
                                             if (input$RWCxbox == TRUE) {
                                                 pl <- pl + geom_hline(yintercept = pstats()$RWC,
                                                                       color = '#543005', linetype = 'dashed')  +
                                                     theme(text = element_text(family = 'Merriweather'))

                                             } else { pl + theme(text = element_text(family = 'Merriweather')) }
                                                 
                                             ggplotly(pl, tooltip = NA)     
                                             # ggplotly(pl, tooltip = c('text'))
                                             })

                                         ) # Main Panel
                                         ),
                                         
# Rmd Report download ----------------------------------------------------------
                                         column(2, offset = 10, # download button placement

                                                output$parport <- downloadHandler(
                                                    filename = paste0(input$NPDESID, 
                                                                      '_', input$radiob, '_', p,' RP Report.pdf'),
                                                    content = function(file){ # copy report to temp directory
                                                        tempReport <- file.path(tempdir(), 'Report.Rmd')
                                                        file.copy('Report.Rmd', tempReport, overwrite = TRUE)
                                                        
                                                        # set up parameters
                                                        params <- list(sdat = sdate,
                                                                       edat = edate,
                                                                       NPDES = input$NPDESID,
                                                                       fac = dfinfo2()$CWPName,
                                                                       street = dfinfo2()$CWPStreet,
                                                                       citystate = paste(dfinfo2()$CWPCity, 
                                                                                         dfinfo2()$CWPState,
                                                                                         sep = ', '),
                                                                       outfall = input$radiob,
                                                                       # WQSfile = input$WQSinput$name,
                                                                       param = p,
                                                                       unts = punits,
                                                                       nsam = pstats$n,
                                                                       pmn = pstats$min,
                                                                       pmean = pstats$m,
                                                                       pmx = pstats$max,
                                                                       RWC = pstats$RWC,
                                                                       pcv = pstats$cv,
                                                                       pz95 = pstats$z95,
                                                                       pzx = pstats$zx,
                                                                       RPM = pstats$RPM,
                                                                       DR = input$DR,
                                                                       WQSB = wqsb,
                                                                       WQSD = wqsd,
                                                                       pplot = ppl(),
                                                                       dmrr = rdmr())
                                                        
                                                        rmarkdown::render(tempReport, output_file = file,
                                                                          params = params,
                                                                          envir = new.env(parent = globalenv()))
                                                    })
                        
                                                ) # end column

                                                
# ------------------------------------------------------------------------------
                                     ) # fluid  Row

                                  ) # end map
                    
                }) -> gap
            do.call(what = tabsetPanel,
                    args = gap %>%
                        append(list(type = 'pills',
                                    id   = 'param_tabs')))
        })
        

    }) # end of tabset

})