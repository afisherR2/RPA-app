# Script to trouble shoot creating a master table of parameter stats
# 4/2/2025
# Adam Fisher


# Require packages
library(shiny)
# library(shinyBS)
# library(shinythemes)
# library(shinyvalidate)
# library(shinyFeedback)
# library(shinybusy)
library(echor)
library(tidyverse)
# library(dplyr)
# library(plotly)
# library(lubridate)
# library(purrr)
library(readxl)
# library(tools)
# library(tinytex)
# library(xml2)
library(httr)
# library(stringr)
# library(tidyr)
library(openxlsx)
# 🔻


# NEED to download CST from URL and work on the server

# receiving water concentration function ---------------------------------------
# RWC <- function(value, p, dr, dates1, dates2){ # value = , p = parameter, dr = dilution ratio, dates1 = dateslider input 1, dates2 = dateslider input 2
#   
#   # create empty list
#   RWCval <- list()
#   
#   # filter dmr by parameter and select value number
#   db <- value %>% 
#     filter(parameter_desc == p) %>% 
#     filter(between(monitoring_period_end_date, dates1, dates2)) %>%  # filter to only be inside the date range slider
#     # filter(monitoring_period_end_date >= dates1 & monitoring_period_end_date <= dates2) %>%
#     select(dmr_value_nmbr)
#   
#   df <- db$dmr_value_nmbr %>%  # sort
#     sort() %>% 
#     as.numeric()
#   
#   
#   n <- length(df) # number of samples
#   max <- max(df) # max of samples
#   
#   RWCval$min <- min(df) # min of samples
#   RWCval$n <- n # number of samples
#   RWCval$max <- max # max of samples
#   
#   m <- mean(df)  # mean of samples
#   
#   sd <- sd(df) # standard deviation
#   
#   # assing values to list
#   RWCval$m <- m %>% 
#     round(2)
#   RWCval$sd <- sd %>% 
#     round(2)
#   
#   # coefficient of variation
#   if (n > 10) {
#     # cv <- round(sd/m,2)
#     cv <- (sd/m)
#   } else {
#     cv <- 0.6
#   }
#   
#   RWCval$cv <- cv %>% 
#     round(2)
#   
#   cv2 <- cv^2 # cv squared
#   
#   # Percentile Pn
#   # for n >= 20, n = 20, else (1 - 0.95)^(1/n)
#   if (n >= 20) {
#     x <- (1 - 0.95)^(1/20) # CHECK on this
#   } else {
#     x <- (1 - 0.95)^(1/n)}
#   
#   # z score
#   z95 <- 1.645
#   zx <- qnorm(x) 
#   
#   RWCval$z95 <- z95
#   RWCval$zx <- round(zx,3)
#   
#   # FROM "notes on PR DMR and RPA Tools" page 5 - on PR Qlick ShapePoint
#   # derived from 1991 Support pg 52
#   RPM <- (exp(z95*log(1+cv2)^0.5 - (0.5*log(1+cv2)))) / (exp(zx*log(1+cv2)^0.5 - (0.5*log(1+cv2))))
#   
#   RWCval$RPM <- RPM %>%
#     round(2)
#   
#   # RWC equation
#   RWC <- round(max * RPM / as.numeric(dr), 2)
#   
#   RWCval$RWC <- RWC
#   
#   return(RWCval)
# }


# download all parameters report module ----------------------------------------
# downloadObjUI <- function(id) {
#   ns <- NS(id)
#   
#   downloadButton(ns('ap_download'), label = 'Download All Parameter Reports')
# }


# create download object
# downloadObj <- function(input, output, session, npdesID, npdesRadio, data, ap_output) {
#   
#   # download handler
#   output$ap_download <- downloadHandler(
#     
#     
#     filename = function() {
#       paste0(npdesID,
#              '_', npdesRadio, '_', 'ALL Parameter RP Report.zip')
#     },
#     
#     content = function(file) {
#       
#       fs <- c()
#       
#       # set reactive value for parameter
#       paramtab <- reactiveValues(
#         nparam = sort(unique(data$parameter_desc))) # list of parameters
#       
#       # loop over the list of effluent parameters and assign report "parameters"
#       for (i in 1:length(paramtab$nparam)) {
#         
#         # set up parameters
#         params <- list(sdat = ap_output$sdat[i],
#                        edat = ap_output$edat[i],
#                        NPDES = ap_output$NPDES[i],
#                        fac = ap_output$fac[i],
#                        street = ap_output$street[i],
#                        citystate = ap_output$citystate[i],
#                        outfall = ap_output$outfall[i],
#                        
#                        param = ap_output$param[i],
#                        # unts = ap_output$unts[i],
#                        nsam = ap_output$nsam[i],
#                        pmn = ap_output$pmn[i],
#                        pmean = ap_output$pmean[i],
#                        pmx = ap_output$pmx[i],
#                        RWC = ap_output$RWC[i],
#                        pcv = ap_output$pcv[i],
#                        pz95 = ap_output$pz95[i],
#                        pzx = ap_output$pzx[i],
#                        RPM = ap_output$RPM[i],
#                        DR = ap_output$DR[i],
#                        WQSB = ap_output$WQSB[i],
#                        WQSD = ap_output$WQSD[i]
#         )
#         
#         path <- paste0(npdesID,
#                        '_', npdesRadio, '_', ap_output$param[i],' RP Report.pdf')
#         
#         rmarkdown::render('AP_Report.Rmd',
#                           params = params,
#                           envir = new.env(parent = globalenv()),
#                           rmarkdown::pdf_document(),
#                           output_file = path)
#         
#         fs <- c(fs, path)
#       } # end map
#       
#       zip(file, fs)
#       
#     }, # end content
#     
#     contentType = 'application/zip'
#     
#   )
# }

#-------------------------------------------------------------------------------
# Define server logic 

# shinyServer(function(input, output) {
  
  
  # Pull facility info from ECHO -------------------------------------------------
  # dfinfo1 <- eventReactive(input$nextBtn, {
  #   
  #   req(input$NPDESID) # BREAK must have NPDES ID and WQS input file
  #   
  #   feedbackDanger('NPDESID', nchar(input$NPDESID) != 9, # throw error if NPDES isnt 9 char
  #                  'please enter a valid NPDES ID',
  #                  icon = NA, color = '#b30000')
  #   
  #   
  #   req(nchar(input$NPDESID) == 9) # BREAK if NPDES ID isnt 9 char
  #   
    # showNotification('Let me just pull some files from ICIS-NPDES', type = 'message')

NPDESID <- 'PR0024163'
NPDESID <- 'PR0001031'
    
dfinfo1 <- echoWaterGetFacilityInfo(p_pid = NPDESID,
                             output = 'df',
                             qcolumns = '1,3,4,5,7') # these are 
    
    
  # }, ignoreNULL = FALSE)
  
  
sdate <- as.Date(today() %m-% years(5), '%m/%d/%Y')
  
edate <- as.Date(today(), '%m/%d/%Y')
  
  ### Pull DMR with echor after first next button --------------------------------
  # dmr <- eventReactive(input$nextBtn, {
  #   req(dfinfo2()) # BREAK for WQSinput and dfinfo2
   dmr <-  echoGetEffluent(p_id = NPDESID, # pull DMR
                           start_date = format(sdate, '%m/%d/%Y'),
                           end_date = format(edate, '%m/%d/%Y'))
    
    
  # }, ignoreNULL = FALSE)
  


  
  # Read in WQS from OST criteria search tool
  # wqsraw <- eventReactive(input$nextBtn, {
  #   
  #   req(dfinfo2())  # BREAK for WQSinput
    
    # # download criteria form EPA OST Criteria Search Tool
    # GET('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx',
    #     write_disk(tf <- tempfile(fileext = '.xlsx')))
    
    # wqsraw <- read_excel('www/criteria-search-tool-data.xlsx',
    #                      skip = 207) # skip the first 200 lines b/c the flat file is formatted weird
    
    
    wqsraw <- read.xlsx('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx',
                         startRow = 208)
    
  # })
  


  # filter dmr by outfall checkbox, stat base, monitoring location ---------------
  
  # download ECHO REF parameter file to link DMR parameter codes to xwalk pollutant codes
  
  echo_ref_p <-  { 
    GET('https://echo.epa.gov/system/files/REF_Parameter.csv', 
        write_disk(tf <- tempfile(fileext = ".csv")))
    
    echo_ref_p <- read.csv(tf)
  }
  

  # change dmr value to numeric and monitoring period to date
  dmr_of <- 
    
    # req(input$radiob)  # BREAK check if outfall is selected
    
    dmr %>%
      filter(
        
        # perm_feature_nmbr == as.character('001') & # checkbox
               statistical_base_type_code == 'MAX' & # statistical base type code
               monitoring_location_code == 1 & # monitoring location (gross effluent)
               value_type_code == 'C3' & # concentration based measurements
               perm_feature_type_code == 'EXO') %>% # external outfalls
      
      select(npdes_id, 
             parameter_desc, 
             perm_feature_nmbr, 
             parameter_code, 
             value_type_code,
             value_type_desc, 
             monitoring_period_end_date,
             dmr_value_nmbr,
             dmr_unit_desc,
             # dmr_value_standard_units, # value converted to standard units
             # standard_unit_desc, # standardized units
             limit_value_nmbr, 
             limit_unit_desc,
             nodi_code) %>%
      
      mutate(dmr_value_nmbr = as.numeric(dmr_value_nmbr), # change to numeric
             limit_value_nmbr = as.numeric(limit_value_nmbr), # change to numeric
             parameter_code = str_remove(parameter_code, '^0+'), # remove leading zeros in parameter code
             monitoring_period_end_date = as.Date(mdy(monitoring_period_end_date, tz = 'EST')), # change to date
             dmr_unit_desc = recode(dmr_unit_desc, `#/100mL` = 'CFU/100mL'), # recode unit  
             
             # replace non-detect/below detection limit (NODI = B) with limit and limit units
             dmr_value_nmbr = if_else(nodi_code == 'B', limit_value_nmbr, dmr_value_nmbr),
             dmr_unit_desc = if_else(nodi_code == 'B', limit_unit_desc, dmr_unit_desc)) %>% 
    
      drop_na(dmr_value_nmbr) |> # drop nas even after subing for limit value
    
      
      # replace blank dmr units with NA and then fill with neighboring units
      mutate(dmr_unit_desc = na_if(dmr_unit_desc, '')) %>% 
      fill(dmr_unit_desc, .direction = 'updown') %>%
      
      # recode dmr parameter codes with pollutant codes
      mutate(POLLUTANT_CODE_ECHO = across(parameter_code, ~ with(echo_ref_p, POLLUTANT_CODE[match(.x, PARAMETER_CODE)]),
                                          .names = 'POLLUTANT_CODE_ECHO'), .before = parameter_code) %>%
      unpack(POLLUTANT_CODE_ECHO) %>% 
      mutate_at(vars(POLLUTANT_CODE_ECHO), as.numeric)
    
  # }, ignoreNULL = TRUE)
  
  
  # cross reference with ECHO_CST_Xwalk file - cross references CST parameter names and ECHO parameter names
  # xwalk <- eventReactive(input$nextBtn2, { 
    xwalk <- read_excel('www/ECHO_CST_Xwalk.xlsx')
  # })
  
  
  # filter standards for entity abbreviation and cross walk with CST and WQP
  # wqs <- eventReactive(input$nextBtn2, {
    
    
    wqsfine <- wqsraw %>% 
      
      # remove rows of cst data until row 1 == Criterion_ID
      # make column headers
      # slice(which.max(wqsraw()[,1] == 'CRITERION_ID') : n()) %>% 
      # set_names(slice(.,1)) %>% 
      # slice(-1) %>% 
      
      filter(ENTITY_ABBR == substr(NPDESID, 1, 2)) %>% # filter for PR or VI
      
      select(c(ENTITY_NAME, STD_POLL_ID, STD_POLLUTANT_NAME, CRITERION_VALUE, UNIT_NAME,
               CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
               USE_CLASS_NAME_LOCATION_ETC_ID, USE_CLASS_NAME_LOCATION_ETC, EFFECTIVE_DATE, LAST_ENTRY_IN_DB)) %>% 
      
      # remove '.0' from STD_POLL_ID and USE_CLASS_NAME_LOCATION_ETC_ID
      mutate(STD_POLL_ID = str_sub(STD_POLL_ID, end = -3)) %>% 
      mutate(USE_CLASS_NAME_LOCATION_ETC_ID = str_sub(USE_CLASS_NAME_LOCATION_ETC_ID, end = -3)) %>% 
      
      # cross walk ECHO parameter names with CST parameter names
      mutate(POLLUTANT_CODE_ECHO = across(STD_POLL_ID, ~ with(xwalk, POLLUTANT_CODE[match(.x, STD_POLL_ID_CST)]),
                                          .names = 'POLLUTANT_CODE_ECHO'),  .before = STD_POLL_ID) %>%
      unpack(cols = POLLUTANT_CODE_ECHO)
    
  # })
  
  
    
#####
    # Compile master pollutant table
    
    
    # unique(dmr_of$perm_feature_nmbr)
    # 
    # dmr_of_sub <- dmr_of |> 
    #   filter(perm_feature_nmbr == '001')
    
    # list of unique parameters in the DMR
    # paramtab <- tibble(Parameter = sort(unique(dmr_of$parameter_desc))) # list of parameters


      
    RWC_group <- function(.data, .dr, .sdate, .edate) {
      
      .data |> 
        group_by(npdes_id, perm_feature_nmbr ,parameter_desc) |>
        filter(between(monitoring_period_end_date, .sdate, .edate)) |>
        summarise(n = n(), # number of samples
                  min = min(dmr_value_nmbr), # min of samples
                  m = mean(dmr_value_nmbr), # mean of samples
                  max = max(dmr_value_nmbr), # max of samples
                  sd = sd(dmr_value_nmbr), # sd of samples
                  cv = if (n > 10) { # coefficient of variation
                      cv = (sd/m)
                    } else {
                      cv = 0.6
                    },
                  
                  cv2 = cv^2, # cv squared
                  
                  # Percentile Pn
                  # for n >= 20, n = 20, else (1 - 0.95)^(1/n)
                  x = if (n >= 20) {
                    (1 - 0.95)^(1/20) # CHECK on this
                  } else {
                    (1 - 0.95)^(1/n)},
                  
                  # z score
                  z95 = 1.645,
                  zx = qnorm(x),

                  RPM = (exp(z95*log(1+cv2)^0.5 - (0.5*log(1+cv2)))) / (exp(zx*log(1+cv2)^0.5 - (0.5*log(1+cv2)))),
                  
                  RWC = round(max * RPM / as.numeric(.dr), 2)
                  )
    }
    

    
    # calculate summary stats and RWC calc for each parameter
    mpstats <- RWC_group(dmr_of, 1, sdate, edate)
    
    paste('# Samples : ',
          mpstats |> 
            filter(perm_feature_nmbr == '001',
                   parameter_desc == 'Ammonia & ammonium- total') |> 
            ungroup() |> 
            select(n))
    
    
  mpstats |> 
    mutate(npdesid = unique(dmr_of$npdes_id))
    
  
  ars <- dmr_of |> 
    filter(parameter_desc == 'Ammonia & ammonium- total') |> 
    filter(between(monitoring_period_end_date, sdate, edate))
    