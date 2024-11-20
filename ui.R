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
	   <img alt='' class='site-logo' src='https://www.epa.gov/sites/all/themes/epa/logo.png'>
       <div class='site-name-and-slogan'>
       <h1 class='site-name'><a href='https://www.epa.gov/' rel='home' title='Go to the home page'><span>US EPA</span></a></h1>
       <div class='site-slogan'>
       United States Environmental Protection Agency
       </div>
       </div>
       <div class='region-header'>
       <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'> 
	   </div>
       </div>
	   </header>
       <nav class='nav main-nav clearfix' role='navigation'>
       <div class='nav__inner'>
       <h2 class='element-invisible'>Main menu</h2>
       <ul class='menu' role='menu'>
       <li class='expanded active-trail menu-item' role='presentation'>
       <a class='active-trail menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </nav>
       <div class='mobile-nav' id='mobile-nav'>
       <div class='mobile-bar clearfix'>
       <label class='menu-button' for='mobile-nav-toggle'>Menu</label>
       </div><input checked id='mobile-nav-toggle' type='checkbox'>
       <div class='mobile-links element-hidden' id='mobile-links' style='height:2404px;'>
       <ul class='mobile-menu'>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/environmental-topics' tabindex='-1' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/laws-regulations' tabindex='-1' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/aboutepa' tabindex='-1' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </div>
       <section class='main-content clearfix' id='main-content' lang='en' role='main' tabindex='-1'>
       <div class='region-preface clearfix'>
       <div class='block-views-revision-hublinks-block' id='block-views-revision-hublinks-block'>
       <div class='view view-revision-hublinks view-id-revision_hublinks'>
       <ul class='menu pipeline'>
       </ul>
       </div>
       </div>
       <div class='block block-pane block-pane-epa-web-area-connect' id='block-pane-epa-web-area-connect'>
       <ul class='menu utility-menu'>
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
# 	   
	
# what about adding a map of the facility?

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
          and the analysis date range  in the ', strong('Dates for Analysis'), '
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
	    
	  
	    # column(4, offest = 1,
	    #        fileInput('WQSinput', label = h3('WQS File Upload'),
	    #                  multiple = FALSE,
	    #                  accept = c('.xlsx'),
	    #                  placeholder = 'PR2022Standards-RPTool.xlsx')
	    #        )),

    column(4, offset = 1,
           dateRangeInput('dateRange', label = h3('Dates for analysis'),
                          start = today() %m-% years(10), end = today())
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
	           # h3(textOutput('dfnpdesid')),
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
      
      # column(3, offset = 1,
      #        uiOutput('pdr')),
      
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
  column(2, offset = 9,
         uiOutput('downloadALL'))), # download report for all parameters

  br(),

  fluidRow(  
    column(2, offset = 11,
           uiOutput('sscsv'))), # download summary stats csv





  br(),
  br(),
  br(),	

# Footer
#####
	HTML("
	  </section>
		<footer class='main-footer clearfix' role='contentinfo'>
		<div class='main-footer__inner'>
		<div class='region-footer'>
		<div class='block-pane-epa-global-footer' id='block-pane-epa-global-footer'>
		<div class='row cols-3'>
		<div class='col size-1of3'>
		<div class='col__title'>
		Discover.
		</div>
		<ul class='menu'>
		<li><a href='https://www.epa.gov/accessibility'>Accessibility</a></li>
		<li><a href='https://www.epa.gov/aboutepa/administrator-gina-mccarthy'>EPA Administrator</a></li>
		<li><a href='https://www.epa.gov/planandbudget'>Budget &amp; Performance</a></li>
		<li><a href='https://www.epa.gov/contracts'>Contracting</a></li>
		<li><a href='https://www.epa.gov/home/grants-and-other-funding-opportunities'>Grants</a></li>
		<li><a href='https://19january2017snapshot.epa.gov'>January 19, 2017 Web Snapshot</a></li>
		<li><a href='https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees'>No FEAR Act Data</a></li>
		<li><a href='https://www.epa.gov/privacy'>Privacy</a></li>
		</ul>
		</div>
		<div class='col size-1of3'>
		<div class='col__title'>
		Connect.
		</div>
		<ul class='menu'>
		<li><a href='https://www.data.gov/'>Data.gov</a></li>
		<li><a href='https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general'>Inspector General</a></li>
		<li><a href='https://www.epa.gov/careers'>Jobs</a></li>
		<li><a href='https://www.epa.gov/newsroom'>Newsroom</a></li>
		<li><a href='https://www.epa.gov/open'>Open Government</a></li>
		<li><a href='https://www.regulations.gov/'>Regulations.gov</a></li>
		<li><a href='https://www.epa.gov/newsroom/email-subscriptions'>Subscribe</a></li>
		<li><a href='https://www.usa.gov/'>USA.gov</a></li>
		<li><a href='https://www.whitehouse.gov/'>White House</a></li>
		</ul>
		</div>
		<div class='col size-1of3'>
		<div class='col__title'>
		Ask.
		</div>
		<ul class='menu'>
		<li><a href='https://www.epa.gov/home/forms/contact-us'>Contact Us</a></li>
		<li><a href='https://www.epa.gov/home/epa-hotlines'>Hotlines</a></li>
		<li><a href='https://www.epa.gov/foia'>FOIA Requests</a></li>
		<li><a href='https://www.epa.gov/home/frequent-questions-specific-epa-programstopics'>Frequent Questions</a></li>
		</ul>
		<div class='col__title'>
		Follow.
		</div>
		<ul class='social-menu'>
		<li><a class='menu-link social-facebook' href='https://www.facebook.com/EPA'>Facebook</a></li>
		<li><a class='menu-link social-twitter' href='https://twitter.com/epa'>Twitter</a></li>
		<li><a class='menu-link social-youtube' href='https://www.youtube.com/user/USEPAgov'>YouTube</a></li>
		<li><a class='menu-link social-flickr' href='https://www.flickr.com/photos/usepagov'>Flickr</a></li>
		<li><a class='menu-link social-instagram' href='https://www.instagram.com/epagov'>Instagram</a></li>
		</ul>
		</div>
		</div>
		</div>
		</div>
		</div>
		</footer>
	")
#####
    )
)
