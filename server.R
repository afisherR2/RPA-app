# Require packages
library(shiny)
library(shinyBS) # Consider if still needed or if modals/popovers can be base Shiny
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
# library(tinytex) # Only if rendering PDFs locally and not on a server with TeX pre-installed
library(xml2) # From echor dependencies
library(httr) # From echor dependencies
library(stringr)
library(tidyr)
library(openxlsx) # For reading/writing Excel, if still needed beyond initial WQS load
library(readr)
library(future)
library(promises)
library(cachem) # For bindCache

# Configure future plan for asynchronous operations
# multisession will run futures in parallel R sessions
# multicore is an alternative for Linux/macOS if preferred and memory allows
if (Sys.getenv("RSTUDIO_PRODUCT_SERVER") == "1" || .Platform$OS.type == "windows") {
  # RStudio Server Pro / Windows often work better with multisession
  future::plan(multisession)
} else {
  # For local Linux/macOS, multicore can be more efficient
  future::plan(multicore) 
}


# ------------------------------------------------------------------------------
# Global Data Loading & Pre-processing
# Load data that doesn't change during the app's session once at startup.
# ------------------------------------------------------------------------------

# Load ECHO REF Parameter data
# This file helps map DMR parameter codes to pollutant codes.
ECHO_REF_P_DATA <- read_csv('www/REF_Parameter.csv')

# Load Criteria Sources data
# This file links NPDES ID prefixes (like 'PR' for Puerto Rico) to URLs for water quality standards.
CRIT_SOURCE_DATA <- read.csv('www/CRITERIA_SOURCES.csv')

# Load and pre-process Water Quality Standards (WQS) data
# Reading the Excel file and performing initial cleaning/structuring once.
WQS_RAW_DATA <- read_excel('www/criteria-search-tool-data.xlsx', skip = 207)

# Clean up WQS column names and pre-process
# This assumes the first row after skipping contains the correct headers.
wqs_colnames <- WQS_RAW_DATA[1, ]
WQS_PROCESSED_DATA <- WQS_RAW_DATA[-1, ]
colnames(WQS_PROCESSED_DATA) <- make.names(as.character(wqs_colnames), unique = TRUE) # Ensure valid and unique names

# Further pre-process WQS_PROCESSED_DATA if beneficial, e.g., type conversions
# Convert relevant columns to numeric, character, etc.
# Example: WQS_PROCESSED_DATA$CRITERION_VALUE <- as.numeric(WQS_PROCESSED_DATA$CRITERION_VALUE)
# This step is crucial and depends on the exact structure and data types in your Excel file.
# For this example, we'll assume it's mostly character and will be handled later.

# Load ECHO CST Crosswalk data
# This crosswalk links CST parameter names and ECHO parameter names.
XWALK_DATA <- read_excel('www/ECHO_CST_Xwalk.xlsx')


# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

# Receiving Water Concentration (RWC) function (remains largely the same)
# Optimized slightly by ensuring dmr_value_nmbr is numeric upfront.
RWC <- function(value_df, p_desc, dr_val, date_start, date_end) {
  RWCval <- list()
  
  # Filter dmr by parameter and date range, ensure values are numeric
  # Assumes 'value_df' is a dataframe with 'parameter_desc', 'monitoring_period_end_date', 'dmr_value_nmbr'
  # Ensure dmr_value_nmbr is numeric before this function if it comes from reactive sources
  
  # Ensure date_start and date_end are Date objects
  date_start <- as.Date(date_start)
  date_end <- as.Date(date_end)
  
  db <- value_df %>%
    filter(parameter_desc == p_desc & 
             monitoring_period_end_date >= date_start &
             monitoring_period_end_date <= date_end) %>%
    select(dmr_value_nmbr) %>%
    mutate(dmr_value_nmbr = as.numeric(dmr_value_nmbr)) %>% # Ensure numeric
    filter(!is.na(dmr_value_nmbr)) # Remove NAs that can't be used in calculations
  
  if (nrow(db) == 0) {
    # Return default NA structure if no data after filtering
    return(list(n = 0, min = NA, m = NA, max = NA, sd = NA, cv = NA, 
                z95 = 1.645, zx = NA, RPM = NA, RWC = NA))
  }
  
  df_sorted_values <- sort(db$dmr_value_nmbr)
  
  n <- length(df_sorted_values)
  RWCval$n <- n
  RWCval$min <- if (n > 0) min(df_sorted_values) else NA
  RWCval$max <- if (n > 0) max(df_sorted_values) else NA
  
  m <- if (n > 0) mean(df_sorted_values) else NA
  RWCval$m <- round(m, 2)
  
  sd_val <- if (n > 1) sd(df_sorted_values) else NA # SD requires at least 2 points
  RWCval$sd <- round(sd_val, 2)
  
  cv <- if (n > 10 && !is.na(m) && m != 0 && !is.na(sd_val)) { # Check m != 0 to avoid division by zero
    (sd_val / m)
  } else {
    0.6 # Default CV if n <= 10 or mean is 0 or sd is NA
  }
  RWCval$cv <- round(cv, 2)
  
  cv2 <- cv^2
  
  # Percentile Pn
  x_val <- if (n >= 20) {
    (1 - 0.95)^(1 / 20)
  } else if (n > 0) { # Ensure n is positive
    (1 - 0.95)^(1 / n)
  } else {
    NA # Not calculable if n=0
  }
  
  z95 <- 1.645
  zx <- if (!is.na(x_val)) qnorm(x_val) else NA
  
  RWCval$z95 <- z95
  RWCval$zx <- round(zx, 3)
  
  RPM <- if (!is.na(z95) && !is.na(zx) && !is.na(cv2) && (1 + cv2) > 0) { # Add check for log argument
    (exp(z95 * log(1 + cv2)^0.5 - (0.5 * log(1 + cv2)))) / 
      (exp(zx * log(1 + cv2)^0.5 - (0.5 * log(1 + cv2))))
  } else {
    NA
  }
  RWCval$RPM <- round(RPM, 2)
  
  RWC_calc <- if (!is.na(RWCval$max) && !is.na(RPM) && !is.na(dr_val) && dr_val != 0) {
    round(RWCval$max * RPM / as.numeric(dr_val), 2)
  } else {
    NA
  }
  RWCval$RWC <- RWC_calc
  
  return(RWCval)
}


# Download module for all parameters report (modified for async rendering)
downloadObjUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns('ap_download'), label = 'Download All Parameter Reports (Zipped PDFs)'),
    textOutput(ns("zip_status")) # To show status updates
  )
}

downloadObj <- function(input, output, session, npdesID_val, outfall_val, dmr_data_val, ap_output_val, facility_info_val) {
  ns <- session$ns
  
  # Reactive value to track progress and final zip path
  status <- reactiveValues(message = "", zip_file_path = NULL, in_progress = FALSE)
  
  output$zip_status <- renderText({ status$message })
  
  observeEvent(input$ap_download, {
    # Prevent multiple clicks while processing
    if (status$in_progress) {
      showNotification("Report generation already in progress.", type = "warning")
      return()
    }
    status$in_progress <- TRUE
    status$message <- "Generating reports... this may take a few moments."
    status$zip_file_path <- NULL # Reset
    
    # Use a temporary directory for this session's reports
    temp_report_dir <- file.path(tempdir(), "all_params_reports", session$token)
    if (!dir.exists(temp_report_dir)) dir.create(temp_report_dir, recursive = TRUE)
    
    # Clean up old files in this specific session's temp dir if any
    unlink(file.path(temp_report_dir, "*.*"))
    
    fs <- c() # To store paths of generated PDFs
    
    # Create a list of futures for rendering each report
    # ap_output_val is the tibble containing parameters for each report
    
    # Ensure ap_output_val is a dataframe/tibble before proceeding
    if (!is.data.frame(ap_output_val) || nrow(ap_output_val) == 0) {
      status$message <- "No parameters to generate reports for."
      status$in_progress <- FALSE
      return()
    }
    
    # Create a list of promises, each rendering one report
    # Ensure ap_output_val is correctly populated from the main server logic
    # It should be a dataframe where each row corresponds to a parameter and its details
    
    report_promises <- lapply(1:nrow(ap_output_val), function(i) {
      param_row <- ap_output_val[i, ] # Get the i-th row of parameters
      
      # Construct params list for this specific report
      # Ensure all named elements in param_row are correctly accessed
      current_params <- list(
        sdat = param_row$sdat, edat = param_row$edat, NPDES = param_row$NPDES,
        fac = param_row$fac, street = param_row$street, citystate = param_row$citystate,
        outfall = param_row$outfall, param = param_row$param,
        # unts = param_row$unts, # This was commented out in original, check if needed
        nsam = param_row$nsam, pmn = param_row$pmn, pmean = param_row$pmean,
        pmx = param_row$pmx, RWC = param_row$RWC, pcv = param_row$pcv,
        pz95 = param_row$pz95, pzx = param_row$pzx, RPM = param_row$RPM,
        DR = param_row$DR, WQSB = param_row$WQSB, WQSD = param_row$WQSD
        # Note: pplot and dmrr are not included in AP_Report.Rmd, so not passed here.
      )
      
      # Define a unique path for each PDF report
      # Sanitize param_row$param to be a valid filename component
      sanitized_param_name <- gsub("[^A-Za-z0-9_.-]", "_", param_row$param) # Replace invalid chars
      report_filename <- paste0(npdesID_val, "_", outfall_val, "_", sanitized_param_name, "_RP_Report.pdf")
      output_path <- file.path(temp_report_dir, report_filename)
      
      # Return a future that renders the report
      future({
        rmarkdown::render('AP_Report.Rmd', # Use the simpler report for "all parameters"
                          params = current_params,
                          envir = new.env(parent = globalenv()), # Important for isolation
                          output_file = output_path,
                          quiet = TRUE) # Suppress verbose output
        return(output_path) # Return path of the generated PDF
      })
    })
    
    # Execute all promises and handle results
    promise_all(.list = report_promises) %...>%
      {
        generated_files <- unlist(.) # List of paths to generated PDFs
        
        # Filter out any NULLs or errors if a report failed to render
        generated_files <- generated_files[!sapply(generated_files, is.null)]
        generated_files <- generated_files[file.exists(generated_files)]
        
        
        if (length(generated_files) > 0) {
          zipfile_name <- paste0(npdesID_val, "_", outfall_val, "_ALL_Parameter_RP_Reports.zip")
          zip_path <- file.path(temp_report_dir, zipfile_name)
          
          # Create zip file
          # The 'files' argument to zip should be relative to 'zipdir' if 'zipdir' is used.
          # Here, we provide full paths and zip them into 'zip_path'.
          # We need to ensure the paths are relative to a common directory for zipping,
          # or use full paths and manage the archive structure.
          # For simplicity, we'll zip them with their full paths initially, then adjust.
          # A better way is to set working directory for zip, or use 'root' arg in utils::zip
          
          # Create the zip file in the temp_report_dir
          # The files to be zipped are already in temp_report_dir, so use their basenames
          
          # utils::zip(zipfile = zip_path, files = generated_files, flags = "-j") # -j junks paths
          # Using relative paths from the temp_report_dir for cleaner zip structure
          zip(zipfile = zip_path, files = basename(generated_files), root = temp_report_dir)
          
          status$message <- paste("Reports zipped successfully! Click download button again if it doesn't start automatically.")
          status$zip_file_path <- zip_path # Store path for downloadHandler
          
          # Trigger the download (a bit of a hack, direct download is better)
          # This relies on a separate downloadHandler that uses status$zip_file_path
          # For a more robust solution, the downloadHandler itself would be more complex
          # or a downloadLink would be dynamically shown.
          # For now, we will assume the user might need to click the button again,
          # or we create a hidden downloadLink and click it via JavaScript.
          
          # Let's set up the download handler to use this path
          session$sendCustomMessage(type = 'triggerAllParamDownload', message = list(id = ns('final_zip_download')))
          
        } else {
          status$message <- "No reports were successfully generated."
        }
        status$in_progress <- FALSE
      } %...!% # Error handling for the promise_all
      {
        status$message <- "An error occurred during report generation."
        status$in_progress <- FALSE
        cat("Error in batch report generation: ", .$message, "\n") # Log error
      }
    
    # This is important: return NULL because observeEvent should not return a promise
    NULL 
  }) # End observeEvent for ap_download button
  
  # This downloadHandler will be triggered by custom JS message or if the user clicks again
  # when status$zip_file_path is set.
  output$ap_download <- downloadHandler(
    filename = function() {
      req(status$zip_file_path) # Only proceed if zip_file_path is set
      basename(status$zip_file_path)
    },
    content = function(file) {
      req(status$zip_file_path) # Ensure the zip file exists
      file.copy(status$zip_file_path, file, overwrite = TRUE)
      
      # Clean up the temporary directory for this session after download
      # This might be too soon if the user wants to download again without regenerating
      # Consider a more robust cleanup strategy (e.g., on session end)
      # unlink(dirname(status$zip_file_path), recursive = TRUE) 
      # status$zip_file_path <- NULL # Reset
    },
    contentType = 'application/zip'
  )
  
  # JavaScript to trigger the download (optional, for smoother UX)
  # tags$script(HTML(paste0("
  #   Shiny.addCustomMessageHandler('triggerAllParamDownload', function(message) {
  #     setTimeout(function() { $('#", ns('ap_download'), "').click(); }, 500);
  #   });
  # ")))
  # This JS part would typically be in the UI, or managed carefully if added from server.
  # For simplicity, we'll rely on the downloadHandler logic.
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
  wqs_entity_data <- reactive({
    req(input$NPDESID)
    entity_abbr <- substr(input$NPDESID, 1, 2)
    
    # Filter the global WQS data by entity abbreviation
    # This assumes WQS_PROCESSED_DATA has an 'ENTITY_ABBR' column
    # and other necessary columns like 'STD_POLL_ID', 'CRITERION_VALUE', etc.
    
    # Ensure WQS_PROCESSED_DATA has the 'ENTITY_ABBR' column
    if (!"ENTITY_ABBR" %in% colnames(WQS_PROCESSED_DATA)) {
      showNotification("WQS data is missing 'ENTITY_ABBR' column.", type="error")
      return(NULL)
    }
    
    filtered_wqs <- WQS_PROCESSED_DATA %>%
      filter(ENTITY_ABBR == entity_abbr) %>%
      # Crosswalk with XWALK_DATA to get POLLUTANT_CODE_ECHO
      # This step needs careful implementation based on XWALK_DATA structure
      # Assuming XWALK_DATA has 'STD_POLL_ID_CST' and 'POLLUTANT_CODE'
      # And WQS_PROCESSED_DATA has 'STD_POLL_ID'
      mutate(
        STD_POLL_ID_cleaned = str_remove(as.character(STD_POLL_ID), "\\.0$") # Clean STD_POLL_ID
      ) %>%
      left_join(XWALK_DATA %>% select(STD_POLL_ID_CST, POLLUTANT_CODE), 
                by = c("STD_POLL_ID_cleaned" = "STD_POLL_ID_CST")) %>%
      rename(POLLUTANT_CODE_ECHO = POLLUTANT_CODE) %>%
      # Ensure POLLUTANT_CODE_ECHO is numeric if it's not already
      mutate(POLLUTANT_CODE_ECHO = as.numeric(POLLUTANT_CODE_ECHO))
    
    
    return(filtered_wqs)
  }) %>% bindCache(isolate(input$NPDESID), cache = "session") # Cache based on NPDES ID
  
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
    req(facility_info()) # Only show if facility info isloaded
    output$nextBtn2 <- renderUI(actionButton('nextBtn2', label = '', icon = icon('angle-down')))
  })
  
  # Processed DMR data (filtered by outfall, etc.)
  # This depends on dmr_raw_data() which is populated asynchronously
  dmr_of <- eventReactive(input$nextBtn2, {
    req(input$radiob, dmr_raw_data()) # Ensure outfall selected and raw DMR data is available
    
    processed_dmr <- dmr_raw_data() %>%
      filter(perm_feature_nmbr == as.character(input$radiob) &
               statistical_base_type_code == 'MAX' &
               monitoring_location_code == 1 &
               value_type_code == 'C3' &
               perm_feature_type_code == 'EXO') %>%
      select(npdes_id, parameter_desc, perm_feature_nmbr, parameter_code,
             value_type_code, value_type_desc, monitoring_period_end_date,
             dmr_value_nmbr, dmr_unit_desc, limit_value_nmbr, limit_unit_desc, nodi_code) %>%
      mutate(
        dmr_value_nmbr = as.numeric(dmr_value_nmbr),
        limit_value_nmbr = as.numeric(limit_value_nmbr),
        parameter_code_cleaned = str_remove(parameter_code, '^0+'), # Cleaned for matching
        monitoring_period_end_date = mdy(monitoring_period_end_date, tz = 'EST'), # Convert to Date
        dmr_unit_desc = recode(dmr_unit_desc, `#/100mL` = 'MPN/100mL'),
        dmr_value_nmbr = if_else(nodi_code == 'B', limit_value_nmbr, dmr_value_nmbr),
        dmr_unit_desc = if_else(nodi_code == 'B', limit_unit_desc, dmr_unit_desc)
      ) %>%
      mutate(dmr_unit_desc = na_if(dmr_unit_desc, '')) %>%
      group_by(parameter_desc) %>% # Group by parameter to fill units within each parameter group
      fill(dmr_unit_desc, .direction = 'downup') %>% # Fill NA units
      ungroup() %>%
      # Join with ECHO_REF_P_DATA (loaded globally) to get POLLUTANT_CODE
      left_join(ECHO_REF_P_DATA %>% select(PARAMETER_CODE, POLLUTANT_CODE), 
                by = c("parameter_code_cleaned" = "PARAMETER_CODE")) %>%
      rename(POLLUTANT_CODE_ECHO = POLLUTANT_CODE) %>%
      mutate(POLLUTANT_CODE_ECHO = as.numeric(POLLUTANT_CODE_ECHO))
    
    return(processed_dmr)
  }) %>% bindCache(isolate(input$radiob), isolate(dmr_raw_data()), cache="session") # Cache based on outfall and raw DMR
  
  # Insert tabset for parameters
  observeEvent(input$nextBtn2, {
    req(dmr_of()) # Ensure processed DMR data is available
    
    # UI for plot line toggles
    output$pMaxbox <- renderUI(checkboxInput('Maxbox', label = HTML('<div style="display:flex"><i class="fas fa-minus fa-2x"style="color:#dfc27d;margin-top:-4px;"></i><i class="fas fa-minus fa-2x"style="color:#dfc27d;margin-top:-4px;margin-left:-3px;"></i><div style="color:black;padding-left:5px;\"><h3>Max Value</h3></div></div>'), value = FALSE))
    output$pSBxbox <- renderUI(checkboxInput('SBxbox', label = HTML('<div style="display:flex"><i class="fas fa-ellipsis fa-2x"style="color:#bf812d;margin-top:-4px;"></i><div style="color:black;padding-left:5px;\"><h3>WQS-SB</h3></div></div>'), value = FALSE))
    output$pSDxbox <- renderUI(checkboxInput('SDxbox', label = HTML('<div style="display:flex"><i class="fas fa-minus fa-2x"style="color:#8c510a;margin-top:-4px;"></i><i class="fas fa-minus fa-2x"style="color:#8c510a;margin-top:-4px;margin-left:7px;"></i><div style="color:black;padding-left:5px;\"><h3>WQS-SD</h3></div></div>'), value = FALSE))
    output$pRWCxbox <- renderUI(checkboxInput('RWCxbox', label = HTML('<div style="display:flex"><i class="fas fa-minus fa-2x"style="color:#543005;margin-top:-4px;"></i><i class="fas fa-square fa-2x"style="color:white;margin-top:-4px;margin-left:-6px;"></i><i class="fas fa-minus fa-2x"style="color:#543005;margin-top:-4px;margin-left:-7px;"></i><i class="fas fa-square fa-2x"style="color:white;margin-top:-4px;margin-left:-6px;"></i><div style="color:black;padding-left:5px;margin-left:-20px;\"><h3>RWC</h3></div></div>'), value = FALSE))
    
    # Unique parameters from the filtered DMR data
    param_list <- sort(unique(dmr_of()$parameter_desc))
    
    output$tabs <- renderUI({
      # Map over each parameter to create a tab
      tabs_list <- map(param_list, function(p_name) {
        ns_tab <- NS(paste0("tab_", gsub("[^A-Za-z0-9]", "", p_name))) # Create a unique namespace for each tab's content
        
        # Reactive for summary stats and RWC calculations for THIS parameter (p_name)
        # Cached based on inputs relevant to this specific parameter's calculation
        pstats <- reactive({
          req(dmr_of(), input[[ns_tab("DR_param")]], input[[ns_tab("dateSlider_param")]]) # Ensure inputs are available
          RWC_vals <- RWC(
            value_df = dmr_of(), # Already filtered by outfall
            p_desc = p_name,
            dr_val = input[[ns_tab("DR_param")]], # Parameter-specific DR input
            date_start = input[[ns_tab("dateSlider_param")]][1], # Parameter-specific date slider
            date_end = input[[ns_tab("dateSlider_param")]][2]
          )
          tibble( # Convert list to tibble
            n = RWC_vals$n, min = RWC_vals$min, m = RWC_vals$m, max = RWC_vals$max,
            RWC = RWC_vals$RWC, cv = RWC_vals$cv, z95 = RWC_vals$z95,
            zx = RWC_vals$zx, RPM = RWC_vals$RPM,
            # Add other stats if needed from RWC_vals
          )
        }) %>% bindCache(
          isolate(dmr_of()), p_name, isolate(input[[ns_tab("DR_param")]]), isolate(input[[ns_tab("dateSlider_param")]]),
          cache = "session"
        )
        
        # Dates of RP evaluation for this parameter
        evdates <- dmr_of() %>%
          filter(parameter_desc == p_name) %>%
          summarise(min_date = min(monitoring_period_end_date, na.rm = TRUE),
                    max_date = max(monitoring_period_end_date, na.rm = TRUE))
        
        sdate <- if (is.na(evdates$min_date)) today() - years(5) else evdates$min_date
        edate <- if (is.na(evdates$max_date)) today() else evdates$max_date
        
        
        # Units from DMR file for this parameter
        punits_df <- dmr_of() %>%
          filter(parameter_desc == p_name) %>%
          select(dmr_unit_desc) %>%
          distinct()
        punits <- if(nrow(punits_df) > 0) punits_df$dmr_unit_desc[1] else "units" # Handle case with no units
        
        # Pollutant codefor this parameter from DMR data
        pc_df <- dmr_of() %>%
          filter(parameter_desc == p_name) %>%
          select(POLLUTANT_CODE_ECHO) %>%
          distinct()
        pc <- if(nrow(pc_df) > 0 && !is.na(pc_df$POLLUTANT_CODE_ECHO[1])) as.numeric(pc_df$POLLUTANT_CODE_ECHO[1]) else NA
        
        # WQS values (SB and SD) and units for this parameter
        # This uses the pre-filtered wqs_entity_data()
        wq_standards <- reactive({
          req(wqs_entity_data(), !is.na(pc))
          
          # Filter WQS data for the current pollutant code (pc)
          # USE_CLASS_NAME_LOCATION_ETC_ID: '5088', '5087' for SB; '5089', '5087', '5092' for SD
          # These IDs might need adjustment based on your exact WQS data structure
          
          current_wqs <- wqs_entity_data() %>% 
            filter(POLLUTANT_CODE_ECHO == pc)
          
          wqsb_val <- current_wqs %>%
            filter(USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5088', '5087')) %>%
            select(CRITERION_VALUE) %>% pull() %>% unique() %>% na.omit()
          wqsb_val <- if(length(wqsb_val) > 0) ifelse(is.na(as.numeric(wqsb_val[1])), wqsb_val[1], as.numeric(wqsb_val[1])) else NA
          
          wsbunits_val <- current_wqs %>%
            filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5088', '5087')) %>%
            select(UNIT_NAME) %>% distinct() %>% pull() %>% na.omit()
          wsbunits_val <- if(length(wsbunits_val) > 0) wsbunits_val[1] else punits # Fallback to DMR units
          
          wqsd_val <- current_wqs %>%
            filter(USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>%
            select(CRITERION_VALUE) %>% pull() %>% unique() %>% na.omit()
          wqsd_val <- if(length(wqsd_val) > 0) ifelse(is.na(as.numeric(wqsd_val[1])), wqsd_val[1], as.numeric(wqsd_val[1])) else NA
          
          wsdunits_val <- current_wqs %>%
            filter(POLLUTANT_CODE_ECHO == pc & USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>%
            select(UNIT_NAME) %>% distinct() %>% pull() %>% na.omit()
          wsdunits_val <- if(length(wsdunits_val) > 0) wsdunits_val[1] else punits # Fallback to DMR units
          
          list(sb = wqsb_val, sb_units = wsbunits_val, sd = wqsd_val, sd_units = wsdunits_val)
        }) %>% bindCache(isolate(wqs_entity_data()), isolate(pc), cache = "session")
        
        
        # Time series plot for UI display
        # Plot is generated reactively based on parameter-specific date slider
        output[[ns_tab("pplot_param")]] <- renderPlotly({
          req(dmr_of(), pstats(), wq_standards(), input[[ns_tab("dateSlider_param")]])
          
          plot_data <- dmr_of() %>% filter(parameter_desc == p_name)
          
          pl <- ggplot(plot_data, aes(x = monitoring_period_end_date, y = dmr_value_nmbr, group = 1)) + # group=1 for single line
            geom_line(color = '#01665e') +
            xlab('Date') + ylab(paste(p_name, '(', punits, ')')) +
            theme_light(base_size = 15) +
            scale_x_date(date_breaks = '1 year', date_labels = '%Y',
                         limits = as.Date(input[[ns_tab("dateSlider_param")]])) + # Use param-specific slider
            theme(text = element_text(family = 'Merriweather'))
          
          # Add lines based on global checkboxes and parameter-specific values
          if (isTruthy(input$Maxbox) && !is.na(pstats()$max)) {
            pl <- pl + geom_hline(yintercept = pstats()$max, color = '#dfc27d', linetype = 'solid')
          }
          if (isTruthy(input$SBxbox) && !is.na(wq_standards()$sb) && is.numeric(wq_standards()$sb)) {
            pl <- pl + geom_hline(yintercept = as.numeric(wq_standards()$sb), color = '#bf812d', linetype = 'dotted')
          }
          if (isTruthy(input$SDxbox) && !is.na(wq_standards()$sd) && is.numeric(wq_standards()$sd)) {
            pl <- pl + geom_hline(yintercept = as.numeric(wq_standards()$sd), color = '#8c510a', linetype = 'longdash')
          }
          if (isTruthy(input$RWCxbox) && !is.na(pstats()$RWC)) {
            pl <- pl + geom_hline(yintercept = pstats()$RWC, color = '#543005', linetype = 'dashed')
          }
          ggplotly(pl, tooltip = c("x", "y")) # Show x and y in tooltip
        })
        
        # Data table for report
        rdmr_for_report <- reactive({
          dmr_of() %>%
            filter(parameter_desc == p_name) %>%
            select(npdes_id, perm_feature_nmbr, parameter_desc,
                   monitoring_period_end_date, dmr_value_nmbr,
                   dmr_unit_desc, nodi_code) %>%
            rename(`NPDES ID` = npdes_id, Outfall = perm_feature_nmbr, Parameter = parameter_desc,
                   `Monitoring Period` = monitoring_period_end_date, Value = dmr_value_nmbr,
                   Unit = dmr_unit_desc, `NODI Code` = nodi_code)
        })
        
        # Plot for report (static, based on full date range for the parameter)
        ppl_for_report <- reactive({
          req(dmr_of(), pstats(), wq_standards()) # pstats here should use full date range for consistency
          # Recalculate pstats for the report using the full date range of the parameter
          report_pstats <- RWC(dmr_of(), p_name, input[[ns_tab("DR_param")]], sdate, edate)
          
          
          plot_data_report <- dmr_of() %>% filter(parameter_desc == p_name)
          
          pl_report <- ggplot(plot_data_report, aes(x = monitoring_period_end_date, y = dmr_value_nmbr, group = 1)) +
            geom_line(color = '#01665e') +
            xlab('Date') + ylab(paste(p_name, '(', punits, ')')) +
            theme_light(base_size = 12) + # Smaller base size for PDF
            scale_x_date(date_breaks = '1 year', date_labels = '%Y', limits = c(sdate, edate)) +
            theme(text = element_text(family = 'Merriweather'), legend.position = "bottom")
          
          if (!is.na(report_pstats$max)) pl_report <- pl_report + geom_hline(yintercept = report_pstats$max, color = '#dfc27d', linetype = 'solid')
          if (!is.na(wq_standards()$sb) && is.numeric(wq_standards()$sb)) pl_report <- pl_report + geom_hline(yintercept = as.numeric(wq_standards()$sb), color = '#bf812d', linetype = 'dotted')
          if (!is.na(wq_standards()$sd) && is.numeric(wq_standards()$sd)) pl_report <- pl_report + geom_hline(yintercept = as.numeric(wq_standards()$sd), color = '#8c510a', linetype = 'longdash')
          if (!is.na(report_pstats$RWC)) pl_report <- pl_report + geom_hline(yintercept = report_pstats$RWC, color = '#543005', linetype = 'dashed')
          
          pl_report
        })
        
        
        # Tab Panel UI for this parameter
        tabPanel(title = h3(p_name),
                 fluidRow(
                   sidebarLayout(
                     sidebarPanel(
                       h4(strong(textOutput(ns_tab("summary_stats_title")))),
                       uiOutput(ns_tab("summary_stats_ui")),
                       numericInput(ns_tab("DR_param"), label = h5("Dilution Ratio:"), value = 1, width = '50%'),
                       sliderInput(ns_tab("dateSlider_param"), h5("Date Range for Analysis:"),
                                   min = as.Date(sdate), max = as.Date(edate),
                                   value = c(as.Date(sdate), as.Date(edate)),
                                   timeFormat = "%Y-%m-%d", step = 30), # step in days
                       width = 4
                     ),
                     mainPanel(
                       plotlyOutput(ns_tab("pplot_param"))
                     )
                   ),
                   fluidRow(
                     column(4, offset = 8, align = "right", # Adjusted for better layout
                            downloadButton(ns_tab("param_report_download"), label = "Download Parameter Report"),
                            br(),br(),
                            downloadButton(ns_tab("summary_stats_download"), label = "Download Summary Stats (CSV)")
                     )
                   )
                 )
        ) # End TabPanel
        
        # Server-side logic for elements within this tab
        output[[ns_tab("summary_stats_title")]] <- renderText({"Summary Stats"})
        output[[ns_tab("summary_stats_ui")]] <- renderUI({
          req(pstats(), wq_standards())
          tagList(
            h5(paste("# Samples :", pstats()$n)),
            h5(paste("Min :", pstats()$min, punits)),
            h5(paste("Mean :", pstats()$m, punits)),
            h5(paste("Max :", pstats()$max, punits)),
            br(),
            h5(paste("WQS - SB :", wq_standards()$sb, wq_standards()$sb_units)),
            h5(paste("WQS - SD :", wq_standards()$sd, wq_standards()$sd_units)),
            br(),
            h5(paste("RWC :", pstats()$RWC, punits))
          )
        })
        
        # Download handler for individual parameter report
        output[[ns_tab("param_report_download")]] <- downloadHandler(
          filename = function() { paste0(input$NPDESID, "_", input$radiob, "_", gsub("[^A-Za-z0-9_]", "", p_name) ,"_RP_Report.pdf") },
          content = function(file) {
            req(facility_info(), pstats(), wq_standards(), ppl_for_report(), rdmr_for_report())
            # Use full date range for report params
            report_sdate <- sdate 
            report_edate <- edate
            report_pstats_vals <- RWC(dmr_of(), p_name, input[[ns_tab("DR_param")]], report_sdate, report_edate)
            
            
            tempReport <- file.path(tempdir(), "Report.Rmd")
            file.copy("Report.Rmd", tempReport, overwrite = TRUE)
            
            params_list <- list(
              sdat = format(report_sdate, "%Y-%m-%d"), edat = format(report_edate, "%Y-%m-%d"),
              NPDES = input$NPDESID, fac = facility_info()$CWPName, street = facility_info()$CWPStreet,
              citystate = paste(facility_info()$CWPCity, facility_info()$CWPState, sep = ', '),
              outfall = input$radiob, param = p_name, unts = punits,
              nsam = report_pstats_vals$n, pmn = report_pstats_vals$min, pmean = report_pstats_vals$m, pmx = report_pstats_vals$max,
              RWC = report_pstats_vals$RWC, pcv = report_pstats_vals$cv, pz95 = report_pstats_vals$z95,
              pzx = report_pstats_vals$zx, RPM = report_pstats_vals$RPM, DR = input[[ns_tab("DR_param")]],
              WQSB = wq_standards()$sb, WQSD = wq_standards()$sd,
              pplot = ppl_for_report(), dmrr = rdmr_for_report()
            )
            rmarkdown::render(tempReport, output_file = file, params = params_list, envir = new.env(parent = globalenv()))
          }
        )
        
        # Download handler for summary stats CSV
        output[[ns_tab("summary_stats_download")]] <- downloadHandler(
          filename = function() { paste0(input$NPDESID, "_", input$radiob, "_", gsub("[^A-Za-z0-9_]", "", p_name), "_Summary_Stats.csv") },
          content = function(file) {
            req(pstats())
            write.csv(pstats(), file, row.names = FALSE)
          }
        )
        
      }) # End map over param_list
      
      do.call(tabsetPanel, append(tabs_list, list(id = "param_tabs", type = "pills")))
    }) # End renderUI for tabs
  }) # End observeEvent for input$nextBtn2 (tabset generation)
  
  
  # Rmd Report download ALL (setup for the module)
  # This observeEvent prepares the data needed by the downloadObj module
  ap_output_data <- reactiveVal(NULL)
  
  observeEvent(input$nextBtn2, { # Or a dedicated button for "Prepare All Parameter Reports Data"
    req(dmr_of(), wqs_entity_data(), facility_info(), input$radiob, input$NPDESID)
    
    param_list_ap <- sort(unique(dmr_of()$parameter_desc))
    
    # This calculation can be slow if many parameters, consider progress indication
    show_modal_spinner(text = "Preparing data for all parameter reports...")
    
    # Use future_lapply for asynchronous processing of each parameter's data
    all_params_futures <- future_lapply(param_list_ap, function(ap_name) {
      # Similar logic as inside the tab generation for pstats, wq_standards, etc.
      # but without UI dependencies, using fixed DR=1 and full date range for each param.
      
      ap_evdates <- dmr_of() %>%
        filter(parameter_desc == ap_name) %>%
        summarise(min_date = min(monitoring_period_end_date, na.rm = TRUE),
                  max_date = max(monitoring_period_end_date, na.rm = TRUE))
      ap_sdate <- if (is.na(ap_evdates$min_date)) today() - years(5) else ap_evdates$min_date
      ap_edate <- if (is.na(ap_evdates$max_date)) today() else ap_evdates$max_date
      
      ap_pstats_vals <- RWC(dmr_of(), ap_name, 1, ap_sdate, ap_edate) # Assuming DR=1 for batch reports
      
      ap_pc_df <- dmr_of() %>% filter(parameter_desc == ap_name) %>% select(POLLUTANT_CODE_ECHO) %>% distinct()
      ap_pc <- if(nrow(ap_pc_df) > 0 && !is.na(ap_pc_df$POLLUTANT_CODE_ECHO[1])) as.numeric(ap_pc_df$POLLUTANT_CODE_ECHO[1]) else NA
      
      ap_wq_standards_list <- list(sb=NA, sd=NA) # Default
      if(!is.na(ap_pc) && !is.null(wqs_entity_data())) {
        current_wqs_ap <- wqs_entity_data() %>% filter(POLLUTANT_CODE_ECHO == ap_pc)
        wqsb_val_ap <- current_wqs_ap %>% filter(USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5088', '5087')) %>% select(CRITERION_VALUE) %>% pull() %>% unique() %>% na.omit()
        ap_wq_standards_list$sb <- if(length(wqsb_val_ap) > 0) ifelse(is.na(as.numeric(wqsb_val_ap[1])), wqsb_val_ap[1], as.numeric(wqsb_val_ap[1])) else NA
        
        wqsd_val_ap <- current_wqs_ap %>% filter(USE_CLASS_NAME_LOCATION_ETC_ID %in% c('5089', '5087', '5092')) %>% select(CRITERION_VALUE) %>% pull() %>% unique() %>% na.omit()
        ap_wq_standards_list$sd <- if(length(wqsd_val_ap) > 0) ifelse(is.na(as.numeric(wqsd_val_ap[1])), wqsd_val_ap[1], as.numeric(wqsd_val_ap[1])) else NA
      }
      
      # Construct a row for the ap_output_val tibble
      tibble(
        sdat = format(ap_sdate, "%Y-%m-%d"), edat = format(ap_edate, "%Y-%m-%d"),
        NPDES = input$NPDESID, fac = facility_info()$CWPName, street = facility_info()$CWPStreet,
        citystate = paste(facility_info()$CWPCity, facility_info()$CWPState, sep = ', '),
        outfall = input$radiob, param = ap_name,
        nsam = ap_pstats_vals$n, pmn = ap_pstats_vals$min, pmean = ap_pstats_vals$m, pmx = ap_pstats_vals$max,
        RWC = ap_pstats_vals$RWC, pcv = ap_pstats_vals$cv, pz95 = ap_pstats_vals$z95,
        pzx = ap_pstats_vals$zx, RPM = ap_pstats_vals$RPM, DR = 1, # Fixed DR for batch
        WQSB = as.character(ap_wq_standards_list$sb), WQSD = as.character(ap_wq_standards_list$sd)
      )
    }, future.seed = TRUE) # future.seed = TRUE for reproducibility if any randomness involved
    
    # When all futures resolve, combine them into a single tibble
    promise_all(.list = all_params_futures) %...>% {
      ap_output_data(bind_rows(.)) # Store the combined data
      remove_modal_spinner()
      showNotification("Data for all parameter reports is ready.", type = "message")
      # Now that ap_output_data() is populated, the download module can use it.
      # The UI for downloadObjUI should be visible for the user to click.
    } %...!% {
      remove_modal_spinner()
      showNotification("Error preparing data for all parameter reports.", type = "error")
      cat("Error in ap_output_data prep: ", .$message, "\n")
    }
    NULL # observeEvent should return NULL
  })
  
  # Render the UI for the "Download All Parameter Reports" button
  # This UI will call the downloadObj module
  output$download_ap_ui_placeholder <- renderUI({ # Changed ID to avoid conflict if 'download_ap' is used elsewhere
    req(ap_output_data()) # Only render if data is ready
    downloadObjUI(id = 'downloadAllReportsModule') # Use a unique ID for the module instance
  })
  
  # Call the module server function
  # This is called once, and the module handles its own button clicks
  # Pass the prepared ap_output_data() to the module
  # Also pass other necessary values like NPDES ID, outfall, and facility info
  # These should be stable when the download button in the module is clicked
  
  # Need to ensure these values are from the "moment" the data prep was done, or are stable.
  # Using reactive expressions that capture these values when ap_output_data is set.
  
  current_npdes_for_batch <- eventReactive(ap_output_data(), { input$NPDESID })
  current_outfall_for_batch <- eventReactive(ap_output_data(), { input$radiob })
  current_dmr_for_batch <- eventReactive(ap_output_data(), { dmr_of() }) # dmr_of might be large, consider if needed by module
  current_facility_info_for_batch <- eventReactive(ap_output_data(), { facility_info() })
  
  
  # The callModule should ideally happen once.
  # The module's download button (`input$ap_download` inside the module) will trigger its logic.
  # We only call callModule if ap_output_data is available.
  # This might lead to issues if ap_output_data becomes NULL again.
  # A better pattern is to always have the module UI and server, and the module itself
  # disables its button or shows a message if data is not ready.
  
  # For simplicity here, we assume ap_output_data() is populated before the user clicks the module's button.
  # The module UI is rendered when ap_output_data() is ready.
  
  # This callModule should be outside any observeEvent that triggers multiple times for the same session,
  # unless the module ID is dynamically changing, which is not the case here.
  # Let's assume downloadObjUI('downloadAllReportsModule') is always in the main UI,
  # and the module's button is enabled/disabled based on ap_output_data().
  # For this example, we'll stick to the provided structure where UI is conditional.
  
  # This needs to be called when the UI for the module is actually present.
  # A common pattern:
  # 1. UI placeholder in main ui.R: uiOutput("download_ap_ui_placeholder")
  # 2. server: output$download_ap_ui_placeholder <- renderUI({ downloadObjUI("mod_id") })
  # 3. server: callModule(downloadObj, "mod_id", ...)
  # The key is that callModule is called once.
  # If downloadObjUI is dynamically rendered, callModule should still be called unconditionally once.
  # The module itself can then check for req(ap_output_val) internally.
  
  # Let's try to call it when ap_output_data is ready, assuming the UI is also ready then.
  # This might lead to multiple callModule calls if not handled carefully.
  # A flag to ensure it's called only once.
  module_called_downloadAllReportsModule <- reactiveVal(FALSE)
  observe({
    req(ap_output_data(), !module_called_downloadAllReportsModule())
    
    # Data to pass to the module. These should be reactive expressions or static values.
    # The module will take ap_output_data() as its main trigger internally.
    callModule(downloadObj, 'downloadAllReportsModule', 
               npdesID_val = isolate(input$NPDESID), # Use isolate if these shouldn't trigger re-call
               outfall_val = isolate(input$radiob),
               dmr_data_val = isolate(dmr_of()), # This is the processed DMR
               ap_output_val = ap_output_data(), # Pass the prepared data reactively
               facility_info_val = isolate(facility_info())
    )
    module_called_downloadAllReportsModule(TRUE)
  })
  
  
  # Clean up temporary files on session end
  session$onSessionEnded(function() {
    temp_report_dir_session <- file.path(tempdir(), "all_params_reports", session$token)
    if (dir.exists(temp_report_dir_session)) {
      unlink(temp_report_dir_session, recursive = TRUE)
      cat("Cleaned up session temporary report directory:", temp_report_dir_session, "\n")
    }
    # Stop any running futures for this session if possible (more advanced)
    # future::plan(sequential) # Reset plan for this session context if needed
  })
  
}) # End shinyServer
