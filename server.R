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
library(tinytex) #  needed for merging PDFs
library(xml2)
library(httr)
library(stringr)
library(tidyr)
library(openxlsx)
library(readr)
# 🔻


# NEED to download CST from URL and work on the server

# receiving water concentration function ---------------------------------------
RWC <- function(value, p, dr, dates1, dates2){ # value = , p = parameter, dr = dilution ratio, dates1 = dateslider input 1, dates2 = dateslider input 2
  
  # create empty list
  RWCval <- list()
  
  # filter dmr by parameter and select value number
  db <- value %>% 
    filter(parameter_desc == p) %>% 
    filter(between(monitoring_period_end_date, dates1, dates2)) %>%  # filter to only be inside the date range slider
    # filter(monitoring_period_end_date >= dates1 & monitoring_period_end_date <= dates2) %>%
    select(dmr_value_nmbr)
  
  df <- db$dmr_value_nmbr %>%  # sort
    sort() %>% 
    as.numeric()
  
  
  n <- length(df) # number of samples
  max <- max(df) # max of samples
  
  RWCval$min <- if(n>0) min(df) else NA # min of samples
  RWCval$n <- n # number of samples
  RWCval$max <- if(n>0) max(df) else NA # max of samples
  
  m <- if(n>0) mean(df) else NA  # mean of samples
  
  sd_val <- if (n > 1) sd(df) else NA # standard deviation
  
  # assing values to list
  RWCval$m <- round(m, 2)
  RWCval$sd <- round(sd_val, 2)
  
  # coefficient of variation
  if (n > 10 && !is.na(m) && m != 0 && !is.na(sd_val)) { # added checks for NA and division by zero
    # cv <- round(sd/m,2)
    cv <- (sd_val/m)
  } else {
    cv <- 0.6
  }
  
  RWCval$cv <- round(cv, 2)
  
  cv2 <- cv^2 # cv squared
  
  # Percentile Pn
  # for n >= 20, n = 20, else (1 - 0.95)^(1/n)
  x_val <- if (n >= 20) {
    (1 - 0.95)^(1 / 20)
  } else if (n > 0) { # added check n>0
    (1 - 0.95)^(1/n)
  } else {
    NA # set to NA if n is not > 0
  }
  
  # z score
  z95 <- 1.645
  zx <- if (!is.na(x_val)) qnorm(x_val) else NA
  
  RWCval$z95 <- z95
  RWCval$zx <- round(zx, 3)
  
  RPM <- if (!is.na(z95) && !is.na(zx) && !is.na(cv2) && (1 + cv2) > 0) { # added check for log argument, and for NA
    (exp(z95*log(1+cv2)^0.5 - (0.5*log(1+cv2)))) / (exp(zx*log(1+cv2)^0.5 - (0.5*log(1+cv2))))
  } else {
    NA
  }
  RWCval$RPM <- round(RPM, 2)
  
  RWC_calc <- if (!is.na(RWCval$max) && !is.na(RPM) && !is.na(dr) && dr != 0) { # added check for NA and dr != 0
    round(max * RPM / as.numeric(dr), 2)
  } else {
    NA
  }
  RWCval$RWC <- RWC_calc
  
  return(RWCval)
}


# Download module for all parameters report (modified for single PDF) ----------------------------------------
downloadObjUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns('ap_download'), label = 'Download All Parameter Reports (PDF)'),
    textOutput(ns("pdf_status")) # Status
  )
}

downloadObj <- function(input, output, session, npdesID, npdesRadio, data, ap_output, facility_info_val) { # added facility_info_val
  ns <- session$ns
  
  status <- reactiveValues(message = "", pdf_file_path = NULL, in_progress = FALSE)
  
  output$pdf_status <- renderText({ status$message })
  
  observeEvent(input$ap_download, {
    # Prevent multiple clicks while processing
    if (status$in_progress) {
      showNotification("Report generation already in progress.", type = "warning")
      return()
    }
    status$in_progress <- TRUE
    status$message <- "Generating combined report..."
    status$pdf_file_path <- NULL
    
    temp_report_dir <- file.path(tempdir(), "all_params_reports", session$token)
    if (!dir.exists(temp_report_dir)) dir.create(temp_report_dir, recursive = TRUE)
    
    unlink(file.path(temp_report_dir, "*.*"))
    
    # Check if ap_output_val is valid
    if (!is.data.frame(ap_output_val) || nrow(ap_output_val) == 0) {
      status$message <- "No parameters to generate reports for."
      status$in_progress <- FALSE
      return()
    }
    
    # 1. Generate individual reports Rmd paths
    report_files <- lapply(1:nrow(ap_output), function(i) {
      param_row <- ap_output[i, ]
      
      current_params <- list(
        sdat = param_row$sdat, edat = param_row$edat, NPDES = npdesID,  # Use npdesID
        fac = facility_info_val$CWPName, street = facility_info_val$CWPStreet, citystate = paste(facility_info_val$CWPCity, facility_info_val$CWPState, sep = ', '), #facility info
        outfall = npdesRadio,  # Use npdesRadio
        param = param_row$param,
        nsam = param_row$nsam, pmn = param_row$pmn, pmean = param_row$pmean,
        pmx = param_row$pmx, RWC = param_row$RWC, pcv = param_row$pcv,
        pz95 = param_row$pz95, pzx = param_row$pzx, RPM = param_row$RPM,
        DR = param_row$DR, WQSB = param_row$WQSB, WQSD = param_row$WQSD
      )
      
      # Use a filename that includes parameter name
      sanitized_param_name <- gsub("[^A-Za-z0-9_.-]", "_", param_row$param)
      report_filename <- paste0(npdesID, "_", npdesRadio, "_", sanitized_param_name, "_RP_Report.pdf")
      output_path <- file.path(temp_report_dir, report_filename)
      
      # Return a future that renders the report
      future({
        rmarkdown::render(
          'AP_Report.Rmd',
          params = current_params,
          envir = new.env(parent = globalenv()),
          output_file = output_path,  # Create temp PDF
          quiet = TRUE
        )
        return(output_path) # Return path of the generated PDF
      })
    })
    
    # 2.  Filter out NULL paths (failed reports)
    report_files <- report_files[!sapply(report_files, is.null)]
    report_files <- report_files[file.exists(report_files)] # Double check
    
    if (length(report_files) == 0) {
      status$message <- "All reports failed to generate."
      status$in_progress <- FALSE
      return()
    }
    
    # 3. Combine PDFs
    final_pdf_path <- file.path(temp_report_dir, paste0(npdesID, "_", npdesRadio, "_ALL_Parameters_Report.pdf"))
    
    tryCatch({
      pdf_combine(input = report_files, output = final_pdf_path) # From tools
      status$message <- "Combined report generated successfully!"
      status$pdf_file_path <- final_pdf_path
    }, error = function(e) {
      status$message <- paste("Error combining reports: ", e$message)
      status$in_progress <- FALSE
      cat("Error combining PDFs: ", e$message, "\n")
      return(NULL)
    })
    
    # 4. Cleanup individual PDFs
    for (file in report_files) {
      unlink(file)
    }
    
    status$in_progress <- FALSE
  })
  
  output$ap_download <- downloadHandler(
    filename = function() {
      req(status$pdf_file_path)
      basename(status$pdf_file_path)
    },
    content = function(file) {
      req(status$pdf_file_path)
      file.copy(status$pdf_file_path, file, overwrite = TRUE)
    },
    contentType = "application/pdf"
  )
}


#-------------------------------------------------------------------------------
# Define server logic 

shinyServer(function(input, output, session) {
  
  # Use shinyvalidate for input validation
  iv <- InputValidator$new()
  iv$add_rule("NPDESID", sv_required())
  iv$add_rule("NPDESID", sv_regex("^[a-zA-Z0-9]{9}$", "NPDES ID must be 9 alphanumeric characters."))
  iv$enable()
  
  # Reactive value to store facility information
  facility_info <- reactiveVal(NULL)
  
  # Pull facility info from ECHO (Asynchronous)
  dfinfo1_promise <- eventReactive(input$nextBtn, {
    req(input$NPDESID, iv$is_valid()) # Ensure NPDES ID is provided and valid
    
    # Show busy indicator
    show_modal_spinner(text = "Fetching facility information...")
    
    future_promise({
      echor::echoWaterGetFacilityInfo(p_pid = input$NPDESID, output = 'df', qcolumns = '1,3,4,5,7')
    })
  })
  
  # Process facility info once promise resolves
  observeEvent(dfinfo1_promise(), {
    current_info_promise <- dfinfo1_promise() # Get the promise object
    
    current_info_promise %...>% {
      facility_data <- . # This is the resolved data frame
      if (nrow(facility_data) == 1) {
        facility_info(facility_data) # Store valid facility info
        output$facility <- renderText(facility_data$CWPName)
        output$street <- renderText(facility_data$CWPStreet)
        output$citystate <- renderText(paste(facility_data$CWPCity, facility_data$CWPState, sep = ', '))
      } else {
        facility_info(NULL) # Reset if not valid
        showNotification("Invalid NPDES ID or facility not found.", type = "error")
        output$facility <- renderText(NULL)
        output$street <- renderText(NULL)
        output$citystate <- renderText(NULL)
      }
      remove_modal_spinner() # Hide busy indicator
    } %...!% {
      facility_info(NULL)
      showNotification(paste("Error fetching facility info:", .$message), type = "error")
      cat("ECHO API Error (Facility Info): ", .$message, "\n")
      remove_modal_spinner()
    }
  })
  
  # Pull DMR data from ECHO (Asynchronous)
  dmr_promise <- eventReactive(input$nextBtn, {
    req(facility_info()) # Only proceed if facility info is valid
    
    show_modal_spinner(text = "Fetching DMR data...")
    
    future_promise({
      echor::echoGetEffluent(
        p_id = input$NPDESID,
        start_date = format(input$dateRange[1], '%m/%d/%Y'),
        end_date = format(input$dateRange[2], '%m/%d/%Y')
      )
    })
  })
  
  # Reactive for raw DMR data once promise resolves
  dmr_raw_data <- reactiveVal(NULL)
  observeEvent(dmr_promise(), {
    current_dmr_promise <- dmr_promise()
    current_dmr_promise %...>% {
      dmr_raw_data(.) # Store resolved DMR data
      remove_modal_spinner()
    } %...!% {
      dmr_raw_data(NULL)
      showNotification(paste("Error fetching DMR data:", .$message), type = "error")
      cat("ECHO API Error (DMR Data): ", .$message, "\n")
      remove_modal_spinner()
    }
  })
  
  # Select outfall to use
  observeEvent(dmr_raw_data(), { # Triggers when dmr_raw_data is updated
    req(facility_info(), dmr_raw_data())
    
    # Filter for external outfalls from the resolved DMR data
    otfl <- dmr_raw_data() %>%
      filter(perm_feature_type_code == 'EXO') %>%
      select(perm_feature_nmbr) %>%
      distinct()
    
    output$outfallradio <- renderUI({
      radioButtons('radiob', label = h2('Select Outfall to Use'),
                   choices = otfl$perm_feature_nmbr)
    })
  })
  
  # First next button (UI rendered dynamically)
  output$nextBtn <- renderUI({
    actionButton('nextBtn', label = '', icon = icon('angle-down'))
  })
  
  # Reactive for WQS data, filtered by entity (PR or VI)
  # This uses the globally pre-processed WQS_PROCESSED_DATA
  wqsraw <- eventReactive(input$nextBtn, {
    
    req(dfinfo2())  # BREAK for WQSinput
    
    # # download criteria form EPA OST Criteria Search Tool
    # GET('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx',
    #     write_disk(tf <- tempfile(fileext = '.xlsx')))
    
    wqsraw <- read_excel('www/criteria-search-tool-data.xlsx',
                             skip = 207) # skip the first 200 lines b/c the flat file is formatted weird
    
    
    # wqsraw <- read.xlsx('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx',
    #                      startRow = 208)
    
  })
  
  wqs <- eventReactive(input$nextBtn2, {
    
    req(wqsraw())
    
    wqsfine <- wqsraw() %>% 
      
      # remove rows of cst data until row 1 == Criterion_ID
      # make column headers
      slice(which.max(wqsraw()[,1] == 'CRITERION_ID') : n()) %>% 
      set_names(slice(.,1)) %>% 
      slice(-1) %>% 
      
      filter(ENTITY_ABBR == substr(input$NPDESID, 1, 2)) %>% # filter for PR or VI
      
      select(c(ENTITY_NAME, STD_POLL_ID, STD_POLLUTANT_NAME, CRITERION_VALUE, UNIT_NAME,
               CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
               USE_CLASS_NAME_LOCATION_ETC_ID, USE_CLASS_NAME_LOCATION_ETC, EFFECTIVE_DATE, LAST_ENTRY_IN_DB)) %>% 
      
      # remove '.0' from STD_POLL_ID and USE_CLASS_NAME_LOCATION_ETC_ID
      mutate(STD_POLL_ID = str_sub(STD_POLL_ID, end = -3)) %>% 
      mutate(USE_CLASS_NAME_LOCATION_ETC_ID = str_sub(USE_CLASS_NAME_LOCATION_ETC_ID, end = -3)) %>% 
      
      # cross walk ECHO parameter names with CST parameter names
      mutate(POLLUTANT_CODE_ECHO = across(STD_POLL_ID, ~ with(XWALK_DATA, POLLUTANT_CODE[match(.x, STD_POLL_ID_CST)]),
                                          .names = 'POLLUTANT_CODE_ECHO'),  .before = STD_POLL_ID) %>%
      unpack(cols = POLLUTANT_CODE_ECHO)
    
  })
  
  # Link to standards
  observeEvent(input$nextBtn, {
    req(input$NPDESID) # Ensure NPDESID is available
    
    # Use globally loaded CRIT_SOURCE_DATA
    crit_url_df <- CRIT_SOURCE_DATA %>%
      filter(ENTITY_ABBR == substr(input$NPDESID, 1, 2)) %>%
      select(CRIT_SOURCE1)
    
    if (nrow(crit_url_df) > 0) {
      crit_url <- crit_url_df %>% pull()
      crit_url_js <- paste0("window.open('", crit_url, "', '_blank')") # Open in new tab
      
      output$critBtn <- renderUI({
        actionButton('critBtn', label = 'Link to Standards', onclick = crit_url_js)
      })
    } else {
      output$critBtn <- renderUI({ p("No standards link found for this entity.") })
    }
  })
  
  # Second next button (UI rendered dynamically)
  observeEvent(input$nextBtn, {
    req(facility_info()) # Only show if facility info is loaded
    output$nextBtn2 <- renderUI(actionButton('nextBtn2', label = '', icon = icon('angle-down')))
  })
  
  # Processed DMR data (filtered by outfall, etc.)
  # This depends on dmr_raw_data() which is populated asynchronously
  dmr_of <- eventReactive(input$nextBtn2, {
    req(input$radiob)  # BREAK check if outfall is selected
    
    dmr() %>%
      filter(perm_feature_nmbr == as.character(input$radiob) & # checkbox
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
             parameter_code_cleaned = str_remove(parameter_code, '^0+'), # remove leading zeros in parameter code
             monitoring_period_end_date = as.Date(mdy(monitoring_period_end_date, tz = 'EST')), # change to date
             dmr_unit_desc = recode(dmr_unit_desc, `#/100mL` = 'MPN/100mL'), # recode unit
             
             # replace non-detect/below detection limit (NODI = B) with limit and limit units
             dmr_value_nmbr = if_else(nodi_code == 'B', limit_value_nmbr, dmr_value_nmbr),
             dmr_unit_desc = if_else(nodi_code == 'B', limit_unit_desc, dmr_unit_desc)) %>% 
      
      # replace blank dmr units with NA and then fill with neighboring units
      mutate(dmr_unit_desc = na_if(dmr_unit_desc, '')) %>% 
      fill(dmr_unit_desc, .direction = 'updown') %>%
      
      # recode dmr parameter codes with pollutant codes
      left_join(ECHO_REF_P_DATA %>% select(PARAMETER_CODE, POLLUTANT_CODE),
                by = c("parameter_code_cleaned" = "PARAMETER_CODE")) %>%
      rename(POLLUTANT_CODE_ECHO = POLLUTANT_CODE) %>%
      mutate(POLLUTANT_CODE_ECHO = as.numeric(POLLUTANT_CODE_ECHO))
    
  }, ignoreNULL = TRUE)
  
  
  # Insert tabset for parameters
  observeEvent(input$nextBtn2, {
    
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
    
    
    
    # Map over each parameter
    #####
    
    output$tabs <- renderUI({ # render UI
      
      # useShinyjs()
      
      map(paramtab$nparam, # map over the list of parameters - parameters are designated with 'p'
          function(p){
            
            # reactive element for summary stats and RWC calculations
            pstats <- reactive({
              RWCvalues <- RWC(dmr_of(), p, input$DR, input$dateSlider[1], input$dateSlider[2]) # list of n, mean, max, CV, Z95, Zx, RPM, and RWC
              # RWCvalues <- RWC(dmr_of(), p, pdr(), sdate, edate)
              
              tibble(
                n = RWCvalues$n,
                min = RWCvalues$min,
                m = RWCvalues$m,
                max = RWCvalues$max,
                RWC = RWCvalues$RWC,
                cv = RWCvalues$cv,
                z95 = RWCvalues$z95,
                zx = RWCvalues$zx,
                RPM = RWCvalues$RPM,
                stringsAsFactors = FALSE)
            })
            
            # reactive element for dilution ration
            # pdr <- reactive({
            #   as.numeric(input$DR)
            # })
            
            
            # dates of RP evaluation
            evdates <- dmr_of() %>%
              filter(parameter_desc == p) %>%
              select(monitoring_period_end_date) %>%
              summarise(min = min(monitoring_period_end_date),
                        max = max(monitoring_period_end_date))
            
            sdate <- evdates$min # earliest sample date up to 5 years
            edate <- evdates$max # most recent sample date
            
            
            # Units from DMR file for this parameter
            punits_df <- dmr_of() %>%
              filter(parameter_desc == p) %>%
              select(dmr_unit_desc) %>%
              # distinct()
              unique()
            # pull()
            
            # link DMR parameter desc p to dmr POLLUTANT_CODE
            pc <- dmr_of()  %>%
              filter(parameter_desc == p) %>%
              select(POLLUTANT_CODE_ECHO) %>%
              unique() %>% 
              pull() %>% 
              as.numeric()
            
            
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
                  as.character()
              }
              
            }
            else {
              wqsb <- NA
              
            }
            
            
            #WQS SD
            if(pc %in% wqs()$POLLUTANT_CODE_ECHO == TRUE){
              
              wqsd <-  wqs() %>% 
                filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>%  # 5089 for class sd waters, 5087 for surface waters, and 5092 for SD/drinking water
                select(CRITERION_VALUE) %>% 
                pull() %>% 
                unique()
              
              # check to see if there is a value
              if (length(wqsd) == 0){
                
                wqsd <- NA
                
                # check for numeric or "See Equation"
              } else if (!is.na(as.numeric(wqsd)) == TRUE) {
                wqsd <- wqsd %>% 
                  as.character()
              }
              
            }
            else {
              wqsd <- NA
            }
            
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
            
            # Time series plot for UI display
            pl <- dmr_of() %>%
              filter(parameter_desc == p) %>%
              ggplot(., aes(x = monitoring_period_end_date, y = dmr_value_nmbr,
                            group = p)) +
              geom_line(color = '#01665e') +
              # geom_area(position = position_dodge(width = 1), fill = '#c7eae5', alpha = .5) +
              xlab('Date') +
              ylab(paste(p, '(', punits, ')')) +
              theme_light(base_size = 15) +
              scale_x_date(date_breaks = '1 year',
                           date_labels = '%Y') +
              theme(text = element_text(family = 'Merriweather'))
            
            # time series plot for report
            ppl <- reactive({
              pl + geom_hline(yintercept = pstats()$max,
                                color = '#dfc27d', linetype = 'solid') +
                
                geom_hline(yintercept = pstats()$RWC,
                                  color = '#543005', linetype = 'dashed') +
                
                geom_hline(yintercept = if_else(wqsb == 'See Equation', 0, wqsb),
                                  alpha = if_else(wqsb == 'See Equation', 0, 1),
                                  color = '#bf812d', linetype = 'dotted') +
                
                geom_hline(yintercept = if_else(wqsd == 'See Equation', 0, wqsd),
                                  alpha = if_else(wqsd == 'See Equation', 0, 1),
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
                       dmr_unit_desc, nodi_code) %>% # nodi_code
                
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
                           
                           # Summary statistics
                           h4(strong(textOutput(ns_tab("summary_stats_title")))),
                           uiOutput(ns_tab("summary_stats_ui")),
                           numericInput(ns_tab("DR_param"), label = h5("Dilution Ratio:"), value = 1, width = '50%'),
                           sliderInput(ns_tab("dateSlider_param"), h5("Date Range for Analysis:"),
                                       min = as.Date(sdate, '%Y-%m-%d'), 
                                       max = as.Date(edate, '%Y-%m-%d'),
                                       value = c(as.Date(sdate, '%Y-%m-%d'), as.Date(edate, '%Y-%m-%d')),
                                       timeFormat = "%Y-%m-%d", step = 30),
                           width = 4
                     ),
                     mainPanel(
                       plotlyOutput(ns_tab("pplot_param"))
                     )
                   ),
                   fluidRow(
                     column(2, offset = 10, # download button placement
                            downloadButton(ns_tab("param_report_download"), label = "Download Parameter Report"),
                            br(),br(),
                            downloadButton(ns_tab("summary_stats_download"), label = "Download Summary Stats (CSV)")
                     )
                   )
                 ) # end tab panel for each parameter
            
          }) -> gap # end map
      do.call(what = tabsetPanel,
              args = gap %>%
                append(list(type = 'pills',
                            id   = 'param_tabs')))
      
    }) # end of tabset
    
    
    # Rmd Report download ALL ----------------------------------------------------------
    observeEvent(input$nextBtn2, {
      
      output$download_ap <- renderUI(
        downloadObjUI(id = 'downloadap')
      ) 
      
      # All parameters report download ---------------------------------------------------
      
      
      # set reactive value for parameter
      paramtab <- reactiveValues(
        nparam = sort(unique(dmr_of()$parameter_desc))) # list of parameters
      
      
      # create an empty list to hold all parameter params
      all_params_report <- tibble(
        
        sdat = character(),
        edat = character(),
        NPDES = character(),
        fac = character(),
        street = character(),
        citystate = character(),
        outfall = character(),
        param = character(),
        # unts = character(),
        nsam = numeric(),
        pmn = numeric(),
        pmean = numeric(),
        pmx = numeric(),
        RWC = numeric(),
        pcv = numeric(),
        pz95 = numeric(),
        pzx = numeric(),
        RPM = numeric(),
        DR = numeric(),
        WQSB = character(),
        WQSD = character()
      )
      
      
      ap_output <- map_df(paramtab$nparam,
                          function(ap){
                            
                            # reactive element for summary stats and RWC calculations
                            pstats_vals <- RWC(dmr_of(), ap, 1, sdate, edate) # list of n, mean, max, CV, Z95, Zx, RPM, and RWC
                            
                            # dates of RP evaluation
                            evdates <- dmr_of() %>%
                              filter(parameter_desc == ap) %>%
                              select(monitoring_period_end_date) %>%
                              summarise(min = min(monitoring_period_end_date),
                                        max = max(monitoring_period_end_date))
                            
                            sdate_char <- as.character(evdates$min) # earliest sample date for selected p
                            edate_char <- as.character(evdates$max) # todays date
                            
                            
                            # units - from DMR file
                            punits_df <- dmr_of() %>%
                              filter(parameter_desc == ap) %>%
                              select(dmr_unit_desc) %>%
                              unique()
                            
                            
                            # link DMR parameter desc p to dmr POLLUTANT_CODE
                            pc <- dmr_of()  %>%
                              filter(parameter_desc == ap) %>%
                              select(POLLUTANT_CODE_ECHO) %>%
                              unique() %>% 
                              pull() %>% 
                              as.numeric()
                            
                            
                            
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
                                
                                wqsb <- 'NA'
                                
                                # check for numeric or "See Equation"
                              } else if (!is.na(as.numeric(wqsb)) == TRUE) {
                                wqsb <- wqsb %>% 
                                  as.character()
                              }
                              
                            }
                            else {
                              wqsb <- 'NA'
                              
                            }
                            
                            
                            #WQS SD
                            if(pc %in% wqs()$POLLUTANT_CODE_ECHO == TRUE){
                              
                              wqsd <-  wqs() %>% 
                                filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>%  # 5089 for class sd waters, 5087 for surface waters, and 5092 for SD/drinking water
                                select(CRITERION_VALUE) %>% 
                                pull() %>% 
                                unique()
                              
                              # check to see if there is a value
                              if (length(wqsd) == 0){
                                
                                wqsd <- NA
                                
                                # check for numeric or "See Equation"
                              } else if (!is.na(as.numeric(wqsd)) == TRUE) {
                                wqsd <- wqsd %>% 
                                  as.character()
                              }
                              
                            }
                            else {
                              wqsd <- 'NA'
                            }
                            
                            
                            
                            # --------------------------------------------------
                            all_params_report <- all_params_report %>%
                              add_row(
                                
                                sdat = sdate_char,
                                edat = edate_char,
                                NPDES = input$NPDESID,
                                fac = facility_info()$CWPName,
                                street = facility_info()$CWPStreet,
                                citystate = paste(facility_info()$CWPCity, 
                                                  facility_info()$CWPState,
                                                  sep = ', '),
                                outfall = input$radiob,
                                param = ap,
                                # unts = punits,
                                nsam = pstats_vals$n,
                                pmn = pstats_vals$min,
                                pmean = pstats_vals$m,
                                pmx = pstats_vals$max,
                                RWC = pstats_vals$RWC,
                                pcv = pstats_vals$cv,
                                pz95 = pstats_vals$z95,
                                pzx = pstats_vals$zx,
                                RPM = pstats_vals$RPM,
                                DR = 1,
                                WQSB = wqsb,
                                WQSD = wqsd)
                            # pplot = ppl(),
                            # dmrr = rdmr())
                            
                          }) # end map
      
      
      # call all parameter report module - and set function inputs
      callModule(downloadObj, id = 'downloadap', 
                 npdesID = input$NPDESID, 
                 npdesRadio = input$radiob,
                 data = dmr_of(),
                 ap_output = ap_output,
                 facility_info_val = facility_info()) # Pass facility_info_val
      
    }) # end observe event
    
})

})
