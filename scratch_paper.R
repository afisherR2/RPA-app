## scrap paper script to test functions and workflows for RP shiny app
# Adam Fisher

library(echor)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(readxl)



# read in wqs
# read in REF param pollutant xwalk from website
# get unique param codes from DMR
# subset dmr params by wqs cas#
# table of dmr param codes and associated wqs standards

GET('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx', 
                          write_disk(tf <- tempfile(fileext = ".xlsx")))

wqsraw <- read_excel(tf,
                     skip = 206) # skip the first 206 lines bc the flat file is formatted weird

wqsfine <- wqsraw %>% 
  filter(ENTITY_ABBR == substr('PR0024163', 1, 2)) %>% 
  
  select(c(ENTITY_NAME, CAS_NO, POLLUTANT_NAME, CRITERION_VALUE, UNIT_NAME,
           CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
           USE_CLASS_NAME_LOCATION_ETC, EFFECTIVE_DATE, LAST_ENTRY_IN_DB)) 




dft <- echoSDWGetSystems(p_reg = '02')

wqsTEST <- readxl::read_xlsx('PuertoRico2019Standards-RPToolEDIT.xlsx')

dft <- echoWaterGetFacilityInfo(p_pid = 'PR0024163',
                                output = 'df',
                                qcolumns = '1,3,4,5,6,7')

unique(df$parameter_code)

names(dft)

df <- echoGetEffluent(p_id = 'PR0024163',
                      start_date = format(today() %m-% years(5), '%m/%d/%Y'),
                      end_date = format(today(), '%m/%d/%Y'))

dftest <- df %>% 
  filter(perm_feature_type_code == 'EXO') %>% 
  select(perm_feature_nmbr) %>% 
  distinct()

dfmod <- df %>% 
  filter(statistical_base_type_code == 'MAX' &
           parameter_desc == 'Lead, total [as Pb]' & # parameter
           value_type_code == 'C3' & # Quantity2 - daily max
           monitoring_location_code == 1 & # monitoring location
           perm_feature_type_code == 'EXO' &
           perm_feature_nmbr == '001' &
           !(dmr_value_nmbr == '')) %>% 
  mutate(monitoring_period_end_date = as.Date(mdy(monitoring_period_end_date, tz = 'EST')),
         dmr_value_nmbr = as.numeric(dmr_value_nmbr)) %>% 
  select(npdes_id, parameter_desc, parameter_code, value_type_code, 
         value_type_desc, monitoring_period_end_date, 
         dmr_value_nmbr, dmr_unit_desc, value_received_date, nodi_code)


unique(dfmod$perm_feature_nmbr)
# time series plot

test.pl <- dfmod %>%
  # filter(parameter_desc == 'Copper, total [as Cu]') %>%
  ggplot(., aes(x = monitoring_period_end_date, y = dmr_value_nmbr)) +
  geom_line(color = '#01665e') +
  geom_area(position = position_dodge(0), fill = '#c7eae5', alpha = .5) +
  xlab('Date') +
  # ylab(paste(p, '(', punits, ')')) +
  theme_light(base_size = 15) +
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y')

ggplotly(test.pl)


DR <- 1 # dilution ratio

p <- 'Lead, total [as Pb]'
c <- 'C3'


RWC <- function(value, p, dr){
  
  # RWCval <- list()
  
  db <- dfmod %>% 
    filter(parameter_desc == p &
             nodi_code != 'B') %>% 
    select(dmr_value_nmbr)
  
  df <- db$dmr_value_nmbr %>%  # sort
    # log()  %>%
    sort()
  
  n <- length(df) # number of samples
  
  # RWCval$n <- n
  
  m <- mean(df) # mean
  sd <- sd(df) # standard deviation
  
  # RWCval$m <- m
  # RWCval$sd <- sd
  
  # coefficient of variation
  if (n > 10) {
    cv <- sd/m
  } else {
    cv <- 0.6
  }
  
  # RWCval$cv <- cv
  
  cv2 <- cv^2 # cv squared
  
  # for n >= 20 use 0.95, else (1 - 0.95)^(1/n)
  if (n >= 20) {
    x <- (1 - 0.95)^(1/20)
  } else {
    x <- (1 - 0.95)^(1/n)}
  
  
  # NEED a log transformed m, sd, and df for z95 and zx???
  
  df_ln <- db$dmr_value_nmbr %>%  # sort
    log()  %>%
    sort()
  
  m_ln <- mean(df_ln) # mean
  sd_ln <- sd(df_ln) # standard deviation
  
  z95 <- 1.645 # 95th percentile
  zx <- qnorm(x)


  # RWCval$z95 <- z95
  # RWCval$zx <- zx
  
  # FROM "notes on PR DMR and RPA Tools" page 5 - on PR Qlick ShapePoint
  RPM <- exp((z95*log(1+cv2)^0.5) - (0.5*log(1+cv2))) / exp((zx*log(1+cv2)^0.5) - (0.5*log(1+cv2)))
  # Max Receiving Water Concentration (RWC) = EEQ or max value * n/cv ratio from table * dilution factor
  RPM
  # RWCval$RPM <- RPM
  
  RWC <- round(max(df) * unname(RPM) * 1, 1)
  RWC
  # RWCval$RWC <- RWC
  
  return(RWC)
}
  
rval <- RWC(dfmod, 'Lead, total [as Pb]', 1)

class(rval$RWC)
str(rval)
dfmod %>% 
  filter(parameter_desc == 'Enterococci' &
           value_type_code == 'C3' &
           nodi_code != 'B') %>% 
  select(dmr_value_nmbr) %>% 
  max()



p <- dfmod %>% 
    # filter(parameter_desc == 'Enterococci' &
    #          value_type_code == 'C3') %>% # Concentration3 - max
    ggplot(., aes(x = monitoring_period_end_date, y = dmr_value_nmbr)) +
    geom_line(color = '#01665e') +
    geom_area(fill = '#c7eae5', alpha = .5) +
    xlab('Date') +
    # ylab('Phenols') +
    theme_light(base_size = 15) +
    geom_hline(yintercept = max(filter(df,
                                       parameter_desc == 'Lead, total [as Pb]' & # parameter
                                         value_type_code == 'C3' & # Quantity2 - daily max
                                         perm_feature_nmbr == '001',
                                         nodi_code != 'B')$dmr_value_nmbr),
               color = '#d8b365') +
  scale_x_date(date_breaks = '1 year',
               date_labels = '%Y') 

p +  geom_hline(yintercept = select(filter(wqsTEST,
                                     PARAMETER_DESC == 'Phenols'), SB)$SB
                )


loadfonts(device = 'win')

# download WQS from criteria search tool ------------------------

# from https://echo.epa.gov/files/echodownloads/npdes_outfalls_layer.zip
temp <- tempfile()
download.file('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx',
              # temp)
              destfile = 'criteria-search-tool-data.xlsx',
              mode = 'wb')
wqs <- read_xlsx('criteria-search-tool-data.xlsx',
                 skip = 206)
unlink(temp)


# filter wqs to match permit
wqs <- wqs %>%
  filter(ENTITY_ABBR == 'PR')

wqs$POLLUTANT_NAME %>% 
  unique()


echoWaterGetParams(code = )
  
