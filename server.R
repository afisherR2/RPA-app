# Require packages
require(shiny)
require(shinyBS)
require(shinythemes)
require(shinyvalidate)
require(shinyFeedback)
require(shinybusy)
require(echor)
require(dplyr)
require(plotly)
require(lubridate)
require(purrr)
require(readxl)
require(tools)
require(tinytex)


# receiving water concentration function ---------------------------------------
RWC <- function(value, p, dr){
    
    RWCval <- list()
    
    db <- value %>% 
        filter(parameter_desc == p &
                   nodi_code != 'B') %>% 
        select(dmr_value_nmbr)
    
    df <- db$dmr_value_nmbr %>%  # sort
        sort()
    
    n <- length(df) # number of samples
    max <- max(df) # max of samples
    
    RWCval$min <- min(df) # min of samples
    RWCval$n <- n
    RWCval$max <- max
    
    m <- mean(df) %>% # mean
        round(2)
    
    sd <- sd(df) # standard deviation
    
    RWCval$m <- m
    RWCval$sd <- sd
    
    # coefficient of variation
    if (n > 10) {
        cv <- round(sd/m,2)
    } else {
        cv <- 0.6
    }
    
    RWCval$cv <- cv
    
    cv2 <- cv^2 # cv squared
    
    # Percentile Pn
    # for n > 20, n = 20, else (1 - 0.95)^(1/n)
    if (n > 20) {
        x <- (1 - 0.95)^(1/20)
    } else {
        x <- (1 - 0.95)^(1/n)}
    
    # log transform for percentile and z score
    df_ln <- df %>% 
        log()
    
    m_ln <- mean(df_ln)
    sd_ln <- sd(df_ln)
    
    # z score
    z95 <- 1.645
    zx <- ((quantile(df_ln, x) - m_ln)/sd_ln) %>% 
        unname() %>% 
        round(3)
    
    RWCval$z95 <- z95
    RWCval$zx <- zx
    
    # FROM "notes on PR DMR and RPA Tools" page 5 - on PR Qlick ShapePoint
    RPM <- (exp(z95*log(1+cv2)^0.5 - (0.5*log(1+cv2)))) / (exp(zx*log(1+cv2)^0.5 - (0.5*log(1+cv2))))

    RWCval$RPM <- RPM %>% 
        round(2)
    
    RWC <- round(max * unname(RPM) * as.numeric(dr), 1)
    
    RWCval$RWC <- RWC %>% 
        round(2)
    
    return(RWCval)
}


# checkbox color formatting code -----------------------------------------------
# altered from: https://community.rstudio.com/t/colors-next-to-checkboxes-in-shiny/74908/7

# dashed # fa-solid fa-hyphen
# dotted # fas fa-ellipsis-h
# dashdot # fa-solid fa-period   combine with hyphen?
# solid # fa-solid fa-horizontal-rule

x_format<- function(col,content){
    paste0('<div style="display:flex"><i class="fa fa-square"
                                         style="color:',col,';margin-top:3px;"></i><div style="color:black;padding-left:5px;">',content,'</div></div>')
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
        feedbackDanger('NPDESID', substr(input$WQSinput$name, 1, 2) != substr(input$NPDESID, 1, 2), 
                       'please double check the NPDES ID and WQS file',
                       icon = NA, color = '#b30000')
        
#        feedbackDanger('WQSinput', file_ext(input$WQSinput$datapath) != 'xlsx', # BREAK if NPDES ID isnt valid
#        'please upload a properly formatted WQS file',
#        icon = NA, color = '#b30000')

        req(nchar(input$NPDESID) == 9) # BREAK if NPDES ID isnt 9 char
#        req(substr(input$WQSinput$name, 1, 2) == substr(input$NPDESID, 1, 2)) # break if NPDES ID doesn't match WQS file
#        req(file_ext(input$WQSinput$datapath) == 'xlsx') # BREAK check for WQS file upload
        
        # showNotification('Let me just pull some files from ICIS-NPDES', type = 'message')
        
        echoWaterGetFacilityInfo(p_pid = input$NPDESID,
                                 output = 'df',
                                 qcolumns = '1,3,4,5,6,7')

    }, ignoreNULL = FALSE)
    

# NPDES ID validity check ---------------------------------------------------
    dfinfo2 <- reactive({
        req(dfinfo1()) # BREAK dfinfo1
#        req(input$WQSinput) # BREAK for WQS upload
        
        feedbackDanger('NPDESID', nrow(dfinfo1()) != 1, # BREAK if NPDES ID isnt valid
                       'please enter a valid NPDES ID',
                       icon = NA, color = '#b30000')
        
        req(nrow(dfinfo1()) == 1) # BREAK check if NPDES ID is valid
        
        dfinfo1()
        })
    
# Display NPDES ID and Address -------------------------------------------------
        output$facility <- renderText(dfinfo2()$CWPName)
        output$street <- renderText(dfinfo2()$CWPStreet)
        output$citystate <- renderText({paste(dfinfo2()$CWPCity, 
                                              dfinfo2()$CWPState,
                                              sep = ', ')})
        
### Pull DMR with echor after first next button --------------------------------
    dmr <- eventReactive(input$nextBtn, {
#        req(input$WQSinput, dfinfo2()) # BREAK for WQSinput and dfinfo2
        echoGetEffluent(p_id = input$NPDESID, # pull DMR
                        start_date = format(today() %m-% years(5), '%m/%d/%Y'), # 5 years ago
                        end_date = format(today(), '%m/%d/%Y')) %>% # todays date
            suppressWarnings() # suppress parsing warning
        
    }, ignoreNULL = FALSE)
    
# read WQS file ----------------------------------------------------------------
    # WQSdf <- eventReactive(input$nextBtn, {
    #     req(input$WQSinput, dfinfo2()) # BREAK for WQSinput and dfinfo2
    #     WQSdf <- read_xlsx(input$WQSinput$datapath)
    #     return(WQSdf)
    # })
    
# Select outfall to use --------------------------------------------------------
    observeEvent(input$nextBtn,{
#        req(input$WQSinput, dfinfo2())  # BREAK for WQSinput and dfinfo2
        
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
        actionButton('nextBtn', label = strong('Next'))})

    
# Second next button 
    observeEvent(input$nextBtn, {
#        req(input$WQSinput, dfinfo2())  # BREAK for WQSinput and dfinfo2
        output$nextBtn2 <- renderUI(actionButton('nextBtn2', label = strong('Next'))) 
    })


# filter dmr by outfall checkbox, stat base, monitoring location ---------------
# change dmr value to numeric and monitoring period  to date
    dmr_of <- eventReactive(input$nextBtn2, {
        
        req(input$radiob)  # BREAK check if outfall is selected
        
            dmr() %>%
                filter(perm_feature_nmbr == as.character(input$radiob) & # checkbox
                           statistical_base_type_code == 'MAX' & # statistical base type code
                           monitoring_location_code == 1 & # monitoring location
                           value_type_code == 'C3' & # concentration based measurements
                           perm_feature_type_code == 'EXO' & # external outfall
                            !(dmr_value_nmbr == '')) %>% # REMOVE dmr_value_nmbr with missing values
                
                select(npdes_id, parameter_desc, perm_feature_nmbr, parameter_code, value_type_code, # select a subset of the database
                       value_type_desc, monitoring_period_end_date,
                       dmr_value_nmbr, dmr_unit_desc, nodi_code) %>%
                
                mutate(dmr_value_nmbr = as.numeric(dmr_value_nmbr), # change to numeric
                       monitoring_period_end_date = as.Date(mdy(monitoring_period_end_date, tz = 'EST')), # change to date
                       dmr_unit_desc = recode(dmr_unit_desc,
                                               `#/100mL` = 'MPN/100mL'))
    }, ignoreNULL = FALSE)
    

# Insert tabset for parameters -------------------------------------------------
# code structure from: https://thatdatatho.com/how-to-create-dynamic-tabs-with-plotly-plots-in-r-shiny/
    observeEvent(input$nextBtn2, {
        
        
        output$pdr <- renderUI(
            numericInput('DR', 
                         label = h3('Dilution Ratio:'), width = '50%',
                         value = 1))
            
        output$pMaxbox <- renderUI(
            checkboxInput('Maxbox', 
                          label = HTML(x_format('#dfc27d', h3('Max Value'))),
                          value = FALSE))
        
        output$pSBxbox <- renderUI(
            checkboxInput('SBxbox', 
                          label = HTML(x_format('#bf812d', h3('WQS - SB'))),
                          value = FALSE))
        
        output$pSDxbox <- renderUI(
            checkboxInput('SDxbox', 
                          label = HTML(x_format('#8c510a',h3('WQS - SD'))),
                          value = FALSE))
        
        output$pRWCxbox <- renderUI(
            checkboxInput('RWCxbox', 
                          label = HTML(x_format('#543005',h3('RWC'))),
                          value = FALSE))
            
        
        paramtab <- reactiveValues(nparam = sort(unique(dmr_of()$parameter_desc))) # list of parameters
        
        output$tabs <- renderUI({ # render UI
            
            map(paramtab$nparam, # map over the list of parameters
                function(p){
                    
                    pstats <- RWC(dmr_of(), p, input$DR) # list of n, mean, max, CV, Z95, Zx, RPM, and RWC
                    
                    # dates of RP evaluation
                    sdate = format(today() %m-% years(5), '%m/%d/%Y') # 5 years ago
                    edate = format(today(), '%m/%d/%Y') # todays date
                    
                    # units - from DMR file
                        punits <- dmr_of() %>%
                            filter(parameter_desc == p) %>%
                            select(dmr_unit_desc) %>%
                            unique()
                    
                    # WQS SB
                    if(p %in% WQSdf()$PARAMETER_DESC == TRUE){
                        wqsb <- select(filter(WQSdf(),
                                              PARAMETER_DESC == p), SB)$SB
                    }
                    else {
                        wqsb <- NA
                    }

                    
                    #WQS SD
                    if(p %in% WQSdf()$PARAMETER_DESC == TRUE){
                        wqsd <- select(filter(WQSdf(),
                                              PARAMETER_DESC == p), SD)$SD
                    }
                    else {
                        wqsd <- NA
                    }

                    # time series plot ui display
                    pl <- dmr_of() %>%
                        filter(parameter_desc == p) %>%
                        ggplot(., aes(x = monitoring_period_end_date, y = dmr_value_nmbr)) +
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
                                          color = '#543005', linetype = 'longdash') +
                            
                            geom_hline(yintercept = ifelse(is.na(wqsb) == TRUE, 0, wqsb),
                                       alpha = ifelse(is.na(wqsb) == TRUE, 0, 1),
                                       color = '#bf812d', linetype = 'dotted') +
                            
                            geom_hline(yintercept = ifelse(is.na(wqsd) == TRUE, 0, wqsd),
                                       alpha = ifelse(is.na(wqsd) == TRUE, 0, 1),
                                       color = '#8c510a', linetype = 'dotdash')
                            })
                    
                    # data table for report with select columns and modified names
                    rdmr <- reactive({
                        rdmr.r <- dmr_of() %>% 
                                    filter(parameter_desc == p) %>% 
                                    as_tibble() %>% 
                            select(npdes_id, perm_feature_nmbr, parameter_desc,
                                   monitoring_period_end_date, dmr_value_nmbr, 
                                   dmr_unit_desc, nodi_code) %>% 
                            rename(`NPDES ID` = npdes_id) %>% 
                            rename(Outfall = perm_feature_nmbr) %>% 
                            rename(Parameter = parameter_desc) %>% 
                            rename(`Monitoring Period` = monitoring_period_end_date) %>% 
                            rename(Value = dmr_value_nmbr) %>% 
                            rename(Unit = dmr_unit_desc) %>% 
                            rename(`NODI Code` = nodi_code)
                    })
                
                    tabPanel(title = h3(p), # tab panel for each parameter

                                     fluidRow(
                                         sidebarLayout(
                                             sidebarPanel(

                                             h4(strong(renderText('Summary Stats'))),
                                             
                                             br(),
                                             
                                             h5(renderText(
                                                 paste('# Samples : ',
                                                       pstats$n))),
                                             
                                             h5(renderText(
                                                 paste('Min : ',
                                                       pstats$min, ' ',
                                                       punits))),
                                             
                                             h5(renderText(
                                                 paste('Mean : ',
                                                       pstats$m, ' ',
                                                       punits))),
                                                 
                                             h5(renderText(
                                                 paste('Max : ',
                                                      pstats$max, ' ',
                                                      punits))),
                                             
                                             br(),
                                             
                                             h5(renderText(
                                                 paste('WQS - SB :',
                                                       wqsb, ' ', 
                                                       punits))),
                                             
                                             h5(renderText(
                                                 paste('WQS - SD :', 
                                                       wqsd, ' ', 
                                                       punits))),
                                             
                                             br(),
                                             
                                             h5(renderText(
                                                 paste('RWC : ', 
                                                       pstats$RWC, ' ', 
                                                       punits))),

                                             width = 4), # width of the panel
                                             
                                         # modify time series plot by checkbox input
                                         mainPanel(
                                             
                                             output$pplot <- renderPlotly({
                                             
                                             if (input$Maxbox == TRUE) {
                                                 pl <- pl + geom_hline(yintercept = pstats$max,
                                                                       color = '#dfc27d', linetype = 'solid')}

                                             if (input$SBxbox == TRUE && is.na(wqsb) == FALSE) {
                                                 pl <- pl + geom_hline(yintercept = wqsb,
                                                                       color = '#bf812d', linetype = 'dotted')}
                                                 
                                             if (input$SDxbox == TRUE && is.na(wqsd) == FALSE) {
                                                 pl <- pl + geom_hline(yintercept = wqsd,
                                                                       color = '#8c510a', linetype = 'dotdash')}
                                                 
                                             if (input$RWCxbox == TRUE) {
                                                 pl <- pl + geom_hline(yintercept = pstats$RWC,
                                                                       color = '#543005', linetype = 'longdash')

                                             } else { pl }
                                                 
                                             ggplotly(pl)
                                             })

                                         ) # Main Panel
                                         ),
                                         
# Rmd Report download ----------------------------------------------------------
                                         column(2, offset = 10, # download button placement

                                                output$parport <- downloadHandler(
                                                    filename = paste0(input$NPDESID, 
                                                                      '_', input$radiob, '_',p,' RP Report.pdf'),
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
                                                                       WQSfile = input$WQSinput$name,
                                                                       param = p,
                                                                       unts = punits,
                                                                       nsam = pstats$n,
                                                                       pmn = pstats$min,
                                                                       pmean = pstats$mean,
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
                                     ) # fluidRow

                                  )# end map
                    
                }) -> gap
            do.call(what = tabsetPanel,
                    args = gap %>%
                        append(list(type = 'pills',
                                    id   = 'param_tabs')))
        })
        
        # outputOptions(output, 'tabs', suspendWhenHidden = FALSE)
        
    }) # end of tabset
    
    

    # observeEvent(input$DR, {
    #     updateNumericInput(inputId = 'DR')
    # })
})