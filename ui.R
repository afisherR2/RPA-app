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
	   <h1  class='page-title'>R2 Reasonable Potential Tool</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>   
	     <p> <b>Created by Adam Fisher (fisher.adam@epa.gov), US EPA Region 2, Data Management Coordinator</b></p>
       <p> <b>Version ALPHA, Last Updated June 13, 2022</b></p>

	   </div>
	   </div>
	   </div>

	"),
#####
# 	   
	
	
	# what about an intro popup window when the app first launches?
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
          to support permit writers’ evaluation of concentration based effluent discharge from NPDES 
          permitted facilities. The app assists permit writers’ determination of 
          reasonable potential by comparing discharge history with current water 
          quality standards and calculating the Receiving Water Concentration
          as defined in EPA’s 1991 Technical Support Document. The Receiving Water Concentration
          calculations assume a 95% confidence level and a 95% probability basis.
          
          The app specifically focuses on reported daily maximum effluent (statistical base type code = MAX),
          discharge from monitoring location code 1, discharge from external outfalls,
          and concentration based discharge. 
          Non-detect values are NOT represented in this analysis. Discharge data for
          the previous 5 years is extracted from ECHO.
          
          Disclaimer: No warranty, express or implied, is 
          made by EPA or any other agency of the U.S. Government regarding the 
          accuracy, completeness, or currency of this information.')),
          
          bsCollapsePanel(title = h3(strong('Instructions')), value = 'Instructions',
                          p('To start, enter a valid NPDES ID in the NPDES ID Input field
          and select a formatted water quality standard excel file to upload in the
          WQS File Upload field. Select the NEXT button. Select an outfall to evaluate. 
          Choose between various discharge parameters, change the Dilution Ratio, 
          or select various limits/water quality standards/or Receiving Water Quality 
          standards ot view on the time series plot. The DOWNLOAD button will download 
          a formatted PDF containing summary statistics, Receiving Water Concentration
          calculations, a time series plot, and a natural log transformed distribution 
          of the effluent discharge.'))),
	
# NPDES ID input and WQS csv
	fluidRow(
	  
	  hr(),
	  br(),
	  br(),

	    column(4, offset = 1,
	           textInput('NPDESID', label = h3('NPDES ID Input'),
	                     value = 'PR0020486')),
	    
	  
	    column(4, offest = 1,
	           fileInput('WQSinput', label = h3('WQS File Upload'),
	                     multiple = FALSE,
	                     accept = c('.xlsx'))
	           )),
	
# First NEXT button
  fluidRow(
    column(2, offset = 10,
           uiOutput('nextBtn'))),
  
  	br(),
  	br(),
  	hr(),
	
# LARGE NPDES ID and Address
# other information to add???
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
	    column(2, offset = 10,
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
      
      column(3, offset = 1,
             uiOutput('pdr')),
      
      column(2,
             uiOutput('pMaxbox')),
      
      column(2,
             uiOutput('pSBxbox')),
      
      column(2,
             uiOutput('pSDxbox')),
      
      column(2,
             uiOutput('pRWCxbox'))
  ),

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
