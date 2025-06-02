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

# runApp(display.mode = "showcase")


# Define UI for application
shinyUI(
    fluidPage(
        # tags$body(class = "html wide-template"),
        # tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
        
        theme = shinytheme("yeti"),
        tags$head(tags$link(rel = "stylesheet",
                            type = "text/css", href = "style.css")),
        
##### Header
#####
        HTML
        (" <header class='masthead clearfix' role='banner'>

       </ul>
       </div>
       </div>
       <div class='main-column clearfix'><!--googleon:all-->
	   <h1  class='page-title'>R2 Reasonable Potential Tool BETA</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>   
	     <p> <b>Created by: Adam Fisher (fisher.adam@epa.gov), US EPA Region 2, Water Division Data Management Coordinator</b>
       <br> <b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Karen O'Brien (obrien.karen@epa.gov), US EPA Region 2, Water Division NPDES Permit Team</b></p>
	   </div>
	   </div>
	   </div>

	"),
#####

# Insert RShiny App here
	hr(),


# shinyFeedback
  useShinyFeedback(),
	
# add progress spinner
	add_busy_spinner(spin = 'hollow-dots',
	                 color = '#0071bc',
	                 position = 'top-right',
	                 onstart = TRUE,
	                 margins = c('50vh', '50vw')), # CSS code to display spinner in middle of screen
	

# Collapsible Overview and Instructions
  bsCollapse(id = 'Overview',
             bsCollapsePanel(title = h3(strong('Overview')), value = 'Overview',
                             p('The R2 Reasonable Potential Analysis Tool is an R Shiny app developed 
          to support NPDES permit writers’ evaluation of concentration based effluent discharged from 
          permitted facilities. The Tool assists in determination of 
          reasonable potential by comparing effluent discharge history with current water 
          quality standards and calculating the Receiving Water Concentration
          (as defined in the EPA', tags$a(href = 'https://www3.epa.gov/npdes/pubs/owm0264.pdf', '1991 Technical Support Document for Water Quality-Based
          Toxics Control', target = 'blank'), ', pages 51-55 & Appendix E). The Receiving Water Concentration
          calculations assume a 95% confidence level and a 95% probability basis. Please review the
          Technical Support Document for further inquiry.'),
      
          
          p('The tool specifically analyzes effluent meeting the following criteria: 
          concentration based effluent,
          reported daily maximum concentration,
          reported as gross effluent, 
          and discharged from external outfalls. 
          Non-detect values (reported as NODI Code = B) are replaced with the respective limit value
          as repoted on the Discharge Monitoring Report. Discharge data for
          the data range specified are extracted from the Enforcement and Compliance History Online
          database', tags$a(href = 'https://echo.epa.gov', '(ECHO)', target = 'blank'), ' 
          and are taken "as is" - no data cleaning or scrubbing has been performed.'),
        
          
          p('No warranty, express or implied, is made by EPA or any other agency of 
          the U.S. Government regarding the accuracy, completeness, or currency 
          of this information.'),
          
          p('For questions, feedback, bugs, or modification suggestions, 
            contact Adam Fisher at', tags$a(href = 'mailto:fisher.adam@epa.gov', 
                                                       'fisher.adam@epa.gov', target = 'blank', '.'))),
          
          bsCollapsePanel(title = h3(strong('Instructions')), value = 'Instructions',
                          p('To start, enter a NPDES ID in the ', strong('NPDES ID Input'), ' field
          and the analysis date range in the ', strong('Dates for Analysis'), '
          field. Select the down arrow button to proceed to the next 
          section.'),
          
          p('Facility information corresponding to the NPDES ID will display with the
          external outfalls discharging from the facility. A link to the corresponding WQS
          will appear is available for reference. Select an outfall to evaluate and 
          select the down arrow button.'),
          
          p('Concentration based effluent discharged from the 
          selected outfall will apear below. 
          Choose between various discharge parameters to view summary statistics, water quality 
          standards, the Receiving Water Concentration (RWC), and 
          a discharge history plot. Change the Dilution Ratio 
          or select various limits to view on the plot. Use the ', strong('Date Range'), 'slider
          to modify the analysis date range.'),
          
          p(' Select the ', strong('Download'),' button to save a formatted PDF containing 
          facility information, summary statistics, the discharge history plot, 
          detailed Receiving Water Concentration calculations,
          and the raw data used in the calculations.'),
          
          p('A formatted water quality standards excel template can be found',
            tags$a(href = 'RPTOOL_WQS_Template.xlsx',
                   'here.', target = 'blank', download = 'RPTOOL_WQS_Template.xlsx')))),
	
# NPDES ID input and WQS csv
	fluidRow(
	  
	  hr(),
	  br(),
	  br(),

	    column(4, offset = 1,
	           textInput('NPDESID', label = h3('NPDES ID Input'),
	                     value = 'PR0024163')),

    column(4, offset = 1,
           dateRangeInput('dateRange', label = h3('Dates for analysis'),
                          start = today() %m-% years(5), end = today())
           )),
	
# First NEXT button  
  fluidRow(
    column(2, offset = 11,
           uiOutput('nextBtn'))),

  br(),

# Criteria Button
  fluidRow(
    column(3, offset = 10,
                  uiOutput('critBtn'))),
  

  	br(),
  	hr(),
	
# LARGE NPDES ID and Address
	fluidRow(
	    column(6, offset = 1,
	           h1(textOutput('facility'))),
	    
	    column(5,
	           h2(textOutput('street')),
	           h2(textOutput('citystate')))),
	
	
# Select outfall to use ONLY if there are more than one

  fluidRow(
      column(4, offset = 1,
            uiOutput('outfallradio'))),
	
	br(),
	br(),
	
# second NEXT button
	fluidRow(
	    column(2, offset = 11,
	           uiOutput('nextBtn2'))),
  
  br(),
  br(),
	

# tabset for each parameter

  fluidRow(
    column(12,
           uiOutput('tabs'))
  ),
  
  br(),
  

  fluidRow(
      
      column(2,
             uiOutput('pMaxbox')),
      
      column(2,
             uiOutput('pSBxbox')),
      
      column(2,
             uiOutput('pSDxbox')),
      
      column(2,
             uiOutput('pRWCxbox'))
  ),

# Downloads
  fluidRow(
    column(2, offset = 9,
           uiOutput('downloadBtn'))), # download parameter report

  br(),

  fluidRow(  
    column(2, offset = 11,
           uiOutput('sscsv'))), # download summary stats csv
  
  fluidRow(
    column(2, offset = 10,
           uiOutput('download_ap'))), # download ALL parameter report

  br(),
  br(),
  br(),	

# Footer
#####
	HTML("
	  </section>
		<footer class='main-footer clearfix' role='contentinfo'>
		
		</footer>
	")
#####
    )
)
