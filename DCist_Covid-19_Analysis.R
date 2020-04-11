### To do:

# Add in a "percent growth" and a "percent growth per capita" option in the plotly dropdowns

# Charts to add in: 
# DC ICU & Ventilator data
# DMV employees by office sick/hospitalized/quarentined
# Hospitalizations by State (& DC)
# Cases by Race, Sex, and Age for DC, MD, and VA state-wide.
library(rvest)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(xml2)
library(curl)
library(httr)
library(splashr)
library(stevedore)
library(rlist)
library(lubridate)
library(RSelenium)
library(wdman)
library(devtools)
library(pdftools)
library(sf)
library(raster)
library(sp)
library(rgdal)
library(leaflet)
library(plotly)
library(mapview)
library(readxl)
library(scales)
library(tidyverse)
setwd("/home/adrian/Documents/DCist_Covid-19_Analysis")
# Import and make conversion tables first
stateConversions <- tibble(Full_Name = state.name, Abbr = state.abb)
stateConversions <- bind_rows(stateConversions, tibble(Full_Name = "District of Columbia", Abbr = "DC")) %>% 
  arrange(Full_Name)

countyStateFIPS <- read_csv("countyFIPSCodes2019.csv")
countyStateFIPS <- bind_rows(countyStateFIPS, tibble(FIPS = 11001, Name = "District of Columbia", State = "DC"))

stateCountyPops <- read_csv("/home/adrian/Documents/US_County_Shapfile_Population/co-est2019-alldata.csv", col_types = cols_only(SUMLEV = "c", STATE = "c", COUNTY = "c", STNAME = "c", CTYNAME = "c", POPESTIMATE2019 = "n"))
stateCountyPops <- stateCountyPops %>% 
  mutate(FIPS = str_c(STATE, COUNTY), TOTAL_POP = POPESTIMATE2019, TOTAL_POP_100K = POPESTIMATE2019 / 100000)


#### This is the data webscraping done for DC, VA, MD, and WV that should be done THE DAY AFTER DATA DESIRED
#### All dashboards/datasites seem to update at around 10am so start just after then.

###### First the webscraping #####
### West Virginia Scraping

Sys.sleep(5)
system('docker pull selenium/standalone-firefox')
Sys.sleep(5)
system('docker run -t -d -p 4445:4444 --memory 1024mb --shm-size 2g selenium/standalone-firefox')
Sys.sleep(5)
# This works...but the download file is still not happening. Let's try again later?
# This link seemed helpful: https://stackoverflow.com/questions/42293193/rselenium-on-docker-where-are-files-downloaded
# fprof <- makeFirefoxProfile(list(browser.download.folderList = 2L,  
#                                  browser.download.manager.showWhenStarting = FALSE,
#                                  browser.download.dir = getwd(),
#                                  browser.helperApps.neverAsk.openFile = "multipart/x-zip,application/zip,application/x-zip-compressed,application/x-compressed,application/msword,application/csv,text/csv,image/png ,image/jpeg, application/pdf, text/html,text/plain,  application/excel, application/vnd.ms-excel, application/x-excel, application/x-msexcel, application/octet-stream",
#                                  browser.helperApps.neverAsk.saveToDisk = "multipart/x-zip,application/zip,application/x-zip-compressed,application/x-compressed,application/msword,application/csv,text/csv,image/png ,image/jpeg, application/pdf, text/html,text/plain,  application/excel, application/vnd.ms-excel, application/x-excel, application/x-msexcel, application/octet-stream",
#                                  browser.helperApps.alwaysAsk.force = FALSE,
#                                  browser.download.manager.showAlertOnComplete = FALSE,
#                                  browser.download.manager.closeWhenDone = TRUE,
#                                  browser.download.manager.showWhenStarting = F 
#                                  #browser.helperApps.neverAsk.saveToDisk = "text/csv/xls/xlsx"
#                                  ))
# fprof <- makeFirefoxProfile("~/.mozilla/firefox/roy2pb8o.default")
# rs <- rsDriver(browser = "firefox", port = 4445L, extraCapabilities = fprof, verbose = T, check = T)
# remDr <- rs[['client']]
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
Sys.sleep(5)
remDr$open()
Sys.sleep(5)

# This was before they made the website dynamic :-/
# WV_URL <- "https://dhhr.wv.gov/COVID-19/Pages/default.aspx"
# 
# # WV_pg <- splash("localhost") %>% render_html(WV_URL, wait = 5)
# # 
# # WV_Counties %>% 
# #   html_node("MSOZoneCell_WebPartWPQ1")
# # WV_Counties %>% 
# #   html_nodes(xpath = '//*[@id="column"]/div/div[8]')
# # 
# WV_Counties <- read_html(WV_URL) %>% 
#   html_node('body') %>% 
#   html_nodes('div') %>% 
#   html_nodes('table')
#   html_node('#WebPartWPQ6') %>% 
#   html_node('p') %>% 
#   html_text()

# Get the WV by county breakout data
remDr$navigate("https://dhhr.wv.gov/COVID-19/Pages/default.aspx")
Sys.sleep(15)
WV_powerbi <- remDr$findElement(using = 'xpath', value = '/html/body/form/div[7]/div/div[1]/div/div[2]/div/iframe')
Sys.sleep(5)
WV_powerbiurl <- WV_powerbi$getElementAttribute("src") # Doing this because the powerbi site is scrapable but the WV health one is not.
WV_powerbiurl <- unlist(WV_powerbiurl)
Sys.sleep(5)
remDr$navigate(WV_powerbiurl)
Sys.sleep(15)




WV_CountiesDiv <- remDr$findElements(using = "class", value = "textbox")
WV_CountiesText <- WV_CountiesDiv[[3]]
WV_Counties <- WV_CountiesText$getElementText()

# # Could also do this as a backup
# WV_Counties <- remDr$findElement(using = "css selector", value = "visual-container-modern.visual-container-component:nth-child(11) > transform:nth-child(1) > div:nth-child(1) > div:nth-child(3) > visual-modern:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > p:nth-child(1) > span:nth-child(2)")
# 
# # or this as a backup
# WV_Counties <- remDr$findElement(using = 'xpath', value = '/html/body/div[1]/ui-view/div/div[1]/div/div/div/div/exploration-container/exploration-container-legacy/div/div/exploration-host/div/div/exploration/div/explore-canvas-modern/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-modern[11]/transform/div/div[3]/visual-modern/div/div/div/p/span[2]')
# WV_Counties <- WV_Counties$getElementText() 

Sys.sleep(5)
# Get the Maryland topline Deaths, Tests, and Hospitalizations data
# The by-county breakout cases data
# and the sex/age breakout cases data
remDr$navigate("https://coronavirus.maryland.gov/")

Sys.sleep(15)
### Maryland Scraping

# MD_Counties <- remDr$findElements(using = 'class', value = 'topBoxH1Text')
# Sys.sleep(5)

# Using CSS class because the website structure changes at times
MD_Deaths_Hospitalizations_Tests_Div <- remDr$findElements(using = 'class', value = 'topBoxH1Text')
confirmedCasesDeathsHospitalizationsTests <- MD_Deaths_Hospitalizations_Tests_Div[[1]]$getElementText()

# MD_Deaths_Hospitalizations_Tests <- remDr$findElement(using = 'xpath', value = '/html/body/div[6]/div[3]/div/div[1]/div/section[5]/div/div/div[2]/div/div/p[1]')
# confirmedCasesDeathsHospitalizationsTests <- MD_Deaths_Hospitalizations_Tests$getElementText()

# CC_D <- MD_Counties[[1]]$getElementText
Sys.sleep(5)
# confirmedCasesAndDeaths <- CC_D()

# CNTY_C <- MD_Counties[[2]]$getElementText

# casesByCounty <- CNTY_C()

# Using CSS class because website changes at times
MD_Data_Div <- remDr$findElements(using = 'class', value = 'topBlackBoxText')
casesByCounty <- MD_Data_Div[[1]]$getElementAttribute("outerHTML")[[1]] %>% 
  read_html(useInternalNodes = T) %>% 
  html_table(fill = T)

# MD_Counties_Path <- remDr$findElement(using = 'xpath', value = '/html/body/div[6]/div[3]/div/div[1]/div/section[5]/div/div/div[2]/div/div/table')
# casesByCounty <- MD_Counties_Path$getElementAttribute("outerHTML")[[1]] %>% read_html(useInternalNodes = T) %>% html_table(fill = T)
# casesByCounty <- MD_Counties_Path$getElementText()
# dcCovid19ByAgeSexToday <- dcCovid19ByAgeSexTodayHTML$getElementAttribute("outerHTML")[[1]] %>% read_html(useInternalNodes = T) %>% html_table(fill = T)
Sys.sleep(5)

# MD_AgeSex_Path <- remDr$findElement(using = 'xpath', value = '/html/body/div[6]/div[3]/div/div[1]/div/section[5]/div/div/div[2]/div/div/strong/table')
casesByAgeAndSex <- MD_Data_Div[[2]]$getElementAttribute("outerHTML")[[1]] %>% 
  read_html(useInternalNodes = T) %>% 
  html_table(fill = T)

# casesByAgeAndSex <- MD_AgeSex_Path$getElementText()
# AGE_SEX <- MD_Counties[[3]]$getElementText
# casesByAgeAndSex <- AGE_SEX()
Sys.sleep(5)

# MD_Race_Path <- remDr$findElement(using = 'xpath', value = '/html/body/div[6]/div[3]/div/div[1]/div/section[5]/div/div/div[2]/div/div/strong/strong/table')

casesByRace <- MD_Data_Div[[3]]$getElementAttribute("outerHTML")[[1]] %>% 
  read_html(useInternalNodes = T) %>% 
  html_table(fill = T)

### Virginia Scraping
# Disregard three lines below, they have download links now
# You will need to download the PDF of the Tableau Dashboard (it updates at 5pm) like this:
# Download button -> Specific sheets from this dashboard -> Cases County (2)
# and save it in your working directory as the file path in the pdf_text call below

# Get each download file link from Virginia site and download county, age, race, and sex breakout CSVs
Sys.sleep(5)
remDr$navigate("http://www.vdh.virginia.gov/coronavirus/")
Sys.sleep(15)
virginiaDownloadLinkCasesByCounty <- remDr$findElement(using = "xpath", value = "/html/body/div[3]/div[2]/div/main/article/div/div/div[2]/div/div/div/div/div/div[1]/div[2]/div/p[2]/a")
Sys.sleep(5)
virginiaDownloadLinkCasesByCountyURL <- virginiaDownloadLinkCasesByCounty$getElementAttribute("href")
Sys.sleep(5)
virginiaDownloadLinkCasesByCountyURL <- unlist(virginiaDownloadLinkCasesByCountyURL)


download.file(virginiaDownloadLinkCasesByCountyURL, destfile = "Virginia_By_County_Today.csv")
Sys.sleep(5)

virginiaDownloadLinkCasesByAge <- remDr$findElement(using = "xpath", value = "/html/body/div[3]/div[2]/div/main/article/div/div/div[2]/div/div/div/div/div/div[1]/div[2]/div/p[3]/a")
Sys.sleep(5)
virginiaDownloadLinkCasesByAgeURL <- virginiaDownloadLinkCasesByAge$getElementAttribute("href")
Sys.sleep(5)
virginiaDownloadLinkCasesByAgeURL <- unlist(virginiaDownloadLinkCasesByAgeURL)

download.file(virginiaDownloadLinkCasesByAgeURL, destfile = "Virginia_By_Age_Today.csv")
Sys.sleep(5)

virginiaDownloadLinkCasesBySex <- remDr$findElement(using = "xpath", value = "/html/body/div[3]/div[2]/div/main/article/div/div/div[2]/div/div/div/div/div/div[1]/div[2]/div/p[4]/a")
Sys.sleep(5)
virginiaDownloadLinkCasesBySexURL <- virginiaDownloadLinkCasesBySex$getElementAttribute("href")
Sys.sleep(5)
virginiaDownloadLinkCasesBySexURL <- unlist(virginiaDownloadLinkCasesBySexURL)

download.file(virginiaDownloadLinkCasesBySexURL, destfile = "Virginia_By_Sex_Today.csv")
Sys.sleep(5)

virginiaDownloadLinkCasesByRace <- remDr$findElement(using = "xpath", value = "/html/body/div[3]/div[2]/div/main/article/div/div/div[2]/div/div/div/div/div/div[1]/div[2]/div/p[5]/a")
Sys.sleep(5)
virginiaDownloadLinkCasesByRaceURL <- virginiaDownloadLinkCasesByRace$getElementAttribute("href")
Sys.sleep(5)

virginiaDownloadLinkCasesByRaceURL <- unlist(virginiaDownloadLinkCasesByRaceURL)

download.file(virginiaDownloadLinkCasesByRaceURL, destfile = "Virginia_By_Race_Today.csv")

Sys.sleep(5)

## DC scraping
# This is the old way before the download link
# install_splash()
# Sys.sleep(5)
# splash_svr <- start_splash()
# Sys.sleep(5)
# 
# splash("localhost") %>% splash_active()
# Sys.sleep(5)
# splash("localhost") %>% 
#   render_html("https://coronavirus.dc.gov/page/coronavirus-data", wait = 5) -> DC_pg
# 
# DC_pg_html <- DC_pg %>% 
#   html_nodes(".even") %>% 
#   html_text()
# stop_splash(splash_svr)
# 
# Get updated total data excel download

remDr$navigate("https://coronavirus.dc.gov/page/coronavirus-data")
Sys.sleep(15)
dcDataA <- remDr$findElement(using = "xpath", value = "/html/body/div[4]/section/div[2]/div/div/div/div[2]/div/div/article/div[1]/div[1]/div/div/ul[1]/li[5]/a")
Sys.sleep(5)
dcDataDownloadLink <- dcDataA$getElementAttribute("href")
Sys.sleep(5)
dcDataDownloadLink <- unlist(dcDataDownloadLink)
Sys.sleep(5)
download.file(dcDataDownloadLink, destfile = "dcCovid-19DataSummaryToday.xlsx")

# This doesn't quite work, I think I have to change my Firefox preferences
# dcDownloadDiv <- remDr$findElement(using = "xpath", value = "/html/body/div[4]/section/div[2]/div/div/div/div[2]/div/div/article/div[1]/div/div/div/div[1]/a[2]")
# dcDownloadLinkFull <- dcDownloadDiv$getElementAttribute("href")
# dcDownloadLinkFull <- unlist(dcDownloadLinkFull)
# remDr$navigate(dcDownloadLinkFull)
# remDr$navigate("https://dcgov.app.box.com/v/DCHealthStatisticsData")
# 
# Sys.sleep(15)
# 
# dcDownloadButton <- remDr$findElement(using = "xpath", value = "/html/body/div[1]/div[5]/span/div/main/div/div/div[1]/header/div/div/button")
# 
# 
# dcDownloadButton$clickElement()
# dcDownloadButton$click()

# Get demographic breakout covid-19 case data
# This is no longer posted on the site directly :-(
# dcCovid19ByAgeSexTodayHTML <- remDr$findElement(using = "xpath", value = "/html/body/div[4]/section/div[2]/div/div/div/div[2]/div/div/article/div[1]/div[1]/div/div/table[1]")
# Sys.sleep(5)
# dcCovid19ByAgeSexToday <- dcCovid19ByAgeSexTodayHTML$getElementAttribute("outerHTML")[[1]] %>% read_html(useInternalNodes = T) %>% html_table(fill = T)
# # Get covid-19 case data by DC ward
# Sys.sleep(5)
# dcCovid19ByWardTodayHTML <- remDr$findElement(using = "xpath", value = "/html/body/div[4]/section/div[2]/div/div/div/div[2]/div/div/article/div[1]/div[1]/div/div/table[2]")
# Sys.sleep(5)
# dcCovid19ByWardToday <- dcCovid19ByWardTodayHTML$getElementAttribute("outerHTML")[[1]] %>% read_html(useInternalNodes = T) %>% html_table(fill = T)
## Stop the web-scraper
Sys.sleep(5)
remDr$close()
Sys.sleep(5)
system("docker stop $(docker ps -a -q)")
system("docker rm $(docker ps -a -q)")
system("docker container prune -f")
system("docker image prune -f")
Sys.sleep(5)

########### DC, VA, MD, and WV data analysis ###########
# The Selenium server is closed and docker container/image is wiped off.

# West Virginia analysis
# Data cleaning and wrangling into dataframe format
WV_Counties <- WV_Counties %>% 
  unlist()

# WV_Counties <- WV_Counties[[1]][1]

WV_CountiesDF <- WV_Counties %>% 
  str_remove_all("CONFIRMED CASES PER COUNTY: ") %>% 
  str_replace_all("\\(", "") %>% 
  str_replace_all("\\)", "") %>% 
  str_split(", ") %>% 
  as_tibble("Combined")

names(WV_CountiesDF) <- "Combined"

WV_CountiesDFCleaned <- WV_CountiesDF %>% 
  separate(Combined, into = c("County", "Cases"), sep = " ") %>% 
  mutate(Cases = as.integer(Cases))
# Adding Date and State Name columns
WV_CountiesDFCleaned$Date <- Sys.Date() - 1
WV_CountiesDFCleaned$State <- "West Virginia"
# Joining in state abbreviation and FIPS code columns
WV_CountiesDFCleaned <- WV_CountiesDFCleaned %>% 
  mutate(Deaths = NA) %>% 
  left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
  left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name")) %>% 
  dplyr::select(County, Cases, Deaths, Date, State, Abbr, FIPS)
# Adding into main file
WV_Counties <- read_csv("WV_Counties.csv")
WV_Counties <- bind_rows(WV_CountiesDFCleaned, WV_Counties)
write_csv(WV_Counties, "WV_Counties.csv")


### Maryland analysis ###
# Data cleaning and adding in state abbreviation and FIPS code column
casesByCounty <- casesByCounty[[1]]
colnames(casesByCounty) <- c("County", "Cases", "Deaths")

casesByCounty <- casesByCounty %>% 
  #separate(col = Cases, into = c("Cases", "Deaths"), sep = ",\\s+") %>% 
  mutate(Deaths = str_remove_all(Deaths, "[\\(\\)]")) %>% 
  mutate(Cases = str_remove_all(Cases, ",")) %>% 
  mutate(Deaths = replace(Deaths, Deaths == "", NA))


MD_By_County_Today <- casesByCounty %>% 
  mutate(Date = Sys.Date() - 1, State = "Maryland", County = str_remove_all(County, "['\\.]"), Cases = as.integer(Cases), Deaths = as.integer(Deaths)) %>% 
  left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
  left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name")) %>% 
  dplyr::select(County, Cases, Deaths, Date, State, Abbr, FIPS)

# MD_By_County_Today <- casesByCounty %>% 
#   str_split("\n") %>% 
#   lapply(str_remove_all, "Cases by County:") %>% 
#   lapply(function(x){ x[!is.na(x) & x != ""]}) %>% 
#   as_tibble(.name_repair = "universal") %>%
#   mutate(Cases = str_extract_all(...1, '\\d+')) %>%
#   mutate(Cases = unlist(Cases)) %>% 
#   mutate(County = str_remove_all(...1, "\\d+")) %>% 
#   mutate(County = str_trim(County)) %>% 
#   mutate(County = str_remove_all(County, "'")) %>% 
#   mutate(Cases = as.integer(Cases)) %>% 
#   mutate(Date = Sys.Date()) %>% 
#   mutate(State = "Maryland") %>% 
#   left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
#   left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name")) %>% 
#   dplyr::select(County, Cases, Date, State, Abbr, FIPS)

# This is Maryland counties in the DMV deaths total solo
All_MD_DMV_Deaths_Today <- MD_By_County_Today %>% 
  filter(County %in% c("Prince Georges", "Montgomery", "Frederick", "Calvert")) %>% 
  summarize(Deaths = sum(Deaths), State = unique(State), Date = unique(Date))

# Adding to main file
MD_By_County <- read_csv("MD_By_County.csv")
MD_By_County <- bind_rows(MD_By_County_Today, MD_By_County)
write_csv(MD_By_County, "MD_By_County.csv")

# Data cleaning and wrangling
MD_Summary_Today <- confirmedCasesDeathsHospitalizationsTests %>% 
  str_split("\n") %>%
  lapply(str_remove_all, "Number of ") %>% 
  lapply(str_remove_all, "ever hospitalized") %>%
  lapply(str_remove_all, ",") %>% 
  as_tibble(.name_repair = "universal") %>% 
  separate(...1, into = c("Measure", "Amount"), sep = ": ") %>% 
  mutate(Amount = as.integer(Amount)) %>% 
  mutate(Date = Sys.Date() - 1) %>% 
  pivot_wider(names_from = Measure, values_from = Amount) %>% 
  mutate(Cases = `Confirmed Cases`, Tests = `negative test results`, State = "Maryland") %>% 
  mutate(Tests = sum(Tests, sum(MD_By_County_Today$Cases))) %>% 
  select(Tests, Deaths, Hospitalizations, Date, State)

# Adding to main file
MD_Summary <- read_csv("MD_Summary.csv")
MD_Summary <- bind_rows(MD_Summary_Today, MD_Summary)
write_csv(MD_Summary, "MD_Summary.csv")


# Data cleaning and wrangling. This needed to create a tibble from scratch and then pivot it.
casesByAgeAndSex <- casesByAgeAndSex[[1]]

MD_By_Sex_Today <- casesByAgeAndSex[10:11,]

MD_By_Sex_Today <- MD_By_Sex_Today %>%
  sapply(str_remove_all, "[,:\\(\\)]") %>% 
  as_tibble()
  # str_remove_all("[,:\\(\\)]") %>% 
  # str_split("\\s+")

colnames(MD_By_Sex_Today) <- c("Sex", "Cases", "Deaths")

MD_By_Sex_Today <- MD_By_Sex_Today %>% 
  mutate(Cases = as.integer(Cases), Deaths = as.integer(Deaths), Date = Sys.Date() - 1, State = "Maryland")

# MD_By_Sex_Today <- tibble(Sex = c(MD_By_Sex_Today[[1]][1], MD_By_Sex_Today[[1]][3]), 
#        Cases = as.integer(c(MD_By_Sex_Today[[1]][2], MD_By_Sex_Today[[1]][4])),
#        Date = as.Date(rep(Sys.Date() - 1, 2)),
#        State = rep("Maryland", 2))

# MD_By_Sex_Today <- MD_By_Sex_Today %>%
#   str_remove_all(",") %>% 
#   str_split("\\s+") %>% 
#   lapply(str_remove_all, ":") %>% 
#   unlist() %>% 
#   as_tibble(.name_repair = "universal")
# 
# colnames(MD_By_Sex_Today) <- unlist(unname(MD_By_Sex_Today[1,]))
# colnames(MD_By_Sex_Today) <- str_trim(unlist(colnames(MD_By_Sex_Today)))
# MD_By_Sex_Today <- MD_By_Sex_Today[2,]

# MD_By_Sex_Today <- MD_By_Sex_Today %>% 
#   mutate(Female = as.integer(Female), Male = as.integer(Male), Date = Sys.Date(), State = "Maryland") %>% 
#   pivot_longer(cols = c("Female", "Male")) %>% 
#   mutate(Sex = name, Cases = value) %>%
#   select(Sex, Cases, Date, State)

# mdAgeSexBreakdown <- casesByAgeAndSex %>% 
#   str_split("\n") %>%
#   unlist() %>% 
#   str_remove(",")
# 
# MD_By_Sex <- mdAgeSexBreakdown[10]
# MD_By_Sex <- MD_By_Sex %>% 
#   str_split(",") %>%
#   unlist() %>% 
#   str_split(": ") %>% 
#   as_tibble(.name_repair = "universal")
# 
# colnames(MD_By_Sex) <- unlist(unname(MD_By_Sex[1,]))
# colnames(MD_By_Sex) <- str_trim(unlist(colnames(MD_By_Sex)))
# MD_By_Sex <- MD_By_Sex[2,]
# 
# MD_By_Sex_Today <- MD_By_Sex %>% 
#   mutate(Female = as.integer(Female), Male = as.integer(Male), Date = Sys.Date(), State = "Maryland") %>% 
#   pivot_longer(cols = c("Female", "Male")) %>% 
#   mutate(Sex = name, Cases = value) %>% 
#   select(Sex, Cases, Date, State)

# Add in to main file
MD_By_Sex <- read_csv("MD_By_Sex.csv")
MD_By_Sex <- bind_rows(MD_By_Sex_Today, MD_By_Sex)
write_csv(MD_By_Sex, "MD_By_Sex.csv")


# Data cleaning and adding State and Date columns
MD_By_Age_Today <- casesByAgeAndSex[1:9,]
colnames(MD_By_Age_Today) <- c("Age_Range", "Cases", "Deaths")

MD_By_Age_Today <- MD_By_Age_Today %>%
  mutate(Cases = str_remove_all(Cases, ","), Deaths = str_remove_all(Deaths, "[\\(\\)]")) %>% 
  mutate(Date = Sys.Date() - 1, State = "Maryland", Cases = as.integer(Cases), Deaths = as.integer(Deaths))


# mdAgeBreakdownToday <- mdAgeSexBreakdown[1:9]
# 
# mdAgeBreakdownToday <- mdAgeBreakdownToday %>% 
#   str_split( " ")
# 
# mdAgeBreakdownToday <- do.call(rbind, mdAgeBreakdownToday)
# 
# colnames(mdAgeBreakdownToday) <- c("Age_Range", "Cases")
# 
# mdAgeBreakdownToday <- mdAgeBreakdownToday %>%
#   as_tibble() %>% 
#   mutate(Cases = as.integer(Cases), Date = Sys.Date(), State = "Maryland")

# Adding into the main file
MD_By_Age <- read_csv("MD_By_Age.csv")
MD_By_Age <- bind_rows(MD_By_Age_Today, MD_By_Age)
write_csv(MD_By_Age, "MD_By_Age.csv")


## Doing Maryland by Race
MD_By_Race_Today <- casesByRace[[1]]

colnames(MD_By_Race_Today) <- c("Race", "Cases", "Deaths")

MD_By_Race_Today <- MD_By_Race_Today %>% 
  mutate(Cases = str_remove_all(Cases, ","), Deaths = str_remove_all(Deaths, "[\\(\\)]")) %>% 
  mutate(Date = Sys.Date() - 1, State = "Maryland", Cases = as.integer(Cases), Deaths = as.integer(Deaths))


MD_By_Race <- read_csv("MD_By_Race.csv")
MD_By_Race <- bind_rows(MD_By_Race_Today, MD_By_Race)
write_csv(MD_By_Race, "MD_By_Race.csv")


Sys.sleep(5)

#### Virginia Analysis 
# Read in and clean by county file. Include state abbreviation and FIPS code column
Virginia_By_County_Today <- read_csv("Virginia_By_County_Today.csv", locale = locale(encoding = "UTF-8"))

Virginia_By_County_Today <- Virginia_By_County_Today %>% 
  rename(County = Locality, Date = `Report Date`, Cases = `Total Cases`) %>% 
  mutate(State = "Virginia", Date = mdy(Date) - 1, Deaths = NA) %>% 
  left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
  dplyr::select(County, Cases, Deaths, Date, State, Abbr, FIPS)

# Add today's data into main file.
Virginia_By_County <- read_csv("Virginia_By_County.csv")
Virginia_By_County <- bind_rows(Virginia_By_County_Today, Virginia_By_County)
write_csv(Virginia_By_County, "Virginia_By_County.csv")

# Read in by age data for today
Virginia_By_Age_Today <- read_csv("Virginia_By_Age_Today.csv")
# Clean up today's Virginia Age data and add in state column
Virginia_By_Age_Today <- Virginia_By_Age_Today %>% 
  mutate(Date = `Report Date`, Age_Range = `Age Group`, Cases = `Number of Cases`, State = "Virginia") %>% 
  mutate(Date = mdy(Date) - 1) %>% 
  select(Age_Range, Cases, Date, State)
# Add today's data into main file.
Virginia_By_Age <- read_csv("Virginia_By_Age.csv")
Virginia_By_Age <- bind_rows(Virginia_By_Age_Today, Virginia_By_Age)
write_csv(Virginia_By_Age, "Virginia_By_Age.csv")

# Read/clean in Virginia's Sex data for today and add in state column
Virginia_By_Sex_Today <- read_csv("Virginia_By_Sex_Today.csv")

Virginia_By_Sex_Today <- Virginia_By_Sex_Today %>% 
  mutate(Date = `Report Date`, Cases = `Number of Cases`, State = "Virginia") %>% 
  mutate(Date = mdy(Date) - 1) %>% 
  select(Sex, Cases, Date, State)
# Add today's data into the main file
Virginia_By_Sex <- read_csv("Virginia_By_Sex.csv")
Virginia_By_Sex <- bind_rows(Virginia_By_Sex_Today, Virginia_By_Sex)
write_csv(Virginia_By_Sex, "Virginia_By_Sex.csv")

# Read/clean Virginia's by race data for today and add state column
Virginia_By_Race_Today <- read_csv("Virginia_By_Race_Today.csv")

Virginia_By_Race_Today <- Virginia_By_Race_Today %>% 
  mutate(Date = `Report Date`, Cases = `Number of Cases`, State = "Virginia") %>% 
  mutate(Date = mdy(Date) - 1) %>% 
  select(Race, Cases, Date, State)
# Add today's data into the main file
Virginia_By_Race <- read_csv("Virginia_By_Race.csv")
Virginia_By_Race <- bind_rows(Virginia_By_Race_Today, Virginia_By_Race)
write_csv(Virginia_By_Race, "Virginia_By_Race.csv")


#  With the new link the Virginia Department of Health put up this is no longer necessary
#
#
# Virginia_By_County <- pdf_text(paste0("VirginiaCovid-19CasesCounty", Sys.Date(), ".pdf")) %>%
#   str_remove_all("Cases County \\(2\\)") %>%
#   str_remove_all("Health District") %>% 
#   str_remove_all("Locality") %>% 
#   str_split('\n') %>%
#   lapply(str_split, "\\s{2,}") 
# 
# 
# Virginia_By_County <- list.filter(Virginia_By_County[[1]], length(.) == 3)
# 
# Virginia_By_County <- Virginia_By_County %>% 
#   as_tibble(.name_repair = "universal")
# 
# Virginia_By_County <- Virginia_By_County[2:3,]
# 
# 
# names(Virginia_By_County) <- Virginia_By_County[1,]
# Virginia_By_County <- Virginia_By_County[2,]
# 
# Virginia_By_County <- pivot_longer(Virginia_By_County, cols = everything(), names_to = "County", values_to = "Cases")  
# 
# Virginia_By_County_Today <- Virginia_By_County %>% 
#   mutate(Date = Sys.Date()) %>% 
#   mutate(State = "Virginia") %>% 
#   mutate(Cases = as.integer(Cases))


# You will need to download the PDF of the Tableau Dashboard (it updates at 5pm) like this:
# Download button -> Specific sheets from this dashboard -> Labs AND Temp Deaths AND Temp Hospitalizations
# and save it in your working directory as the file path in the pdf_text call below

# Create the headers (Tests, Hospitalizations, Deaths) and values 
Virginia_Totals_Numbers <- pdf_text(paste0(paste("Virginia", "COVID-19", "Dashboard", Sys.Date() - 1, sep = " "), ".pdf")) %>% 
  str_remove_all("\n") %>%
  str_remove_all(",") %>% 
  str_remove_all("(\\s{1,})") %>% 
  str_extract_all("\\d+") %>% 
  unlist()

Virginia_Totals_Headers <- pdf_text(paste0(paste("Virginia", "COVID-19", "Dashboard", Sys.Date() - 1, sep = " "), ".pdf")) %>% 
  str_remove_all("\n") %>%
  str_remove_all(",") %>% 
  str_remove_all("(\\s{1,})") %>% 
  str_remove_all("Total") %>%
  str_extract_all("[[:alpha:]]+") %>% 
  unlist()

# This is Virginia's deaths in the Northern Health District solo
Virginia_Regional_Deaths_District_Numbers <- pdf_text(paste0(paste("Deaths", "-", "Region", Sys.Date() - 1, sep = " "), ".pdf")) %>% 
  str_remove_all("\n") %>%
  str_remove_all("(\\s{1,})") %>%
  str_remove_all("DeathsbyHealthPlanningRegion") %>%
  str_extract_all("\\d+") %>% 
  unlist()

Virginia_Regional_Deaths_District <- pdf_text(paste0(paste("Deaths", "-", "Region", Sys.Date() - 1, sep = " "), ".pdf")) %>% 
  str_remove_all("\n") %>%
  str_remove_all("(\\s{1,})") %>%
  str_remove_all("DeathsbyHealthPlanningRegion") %>%
  str_extract_all("[[:alpha:]]+") %>%
  unlist()

All_VA_DMV_Deaths_Today <- tibble("Health_District" = Virginia_Regional_Deaths_District, "Deaths" = Virginia_Regional_Deaths_District_Numbers)  
  
All_VA_DMV_Deaths_Today <- All_VA_DMV_Deaths_Today %>% 
  mutate(Deaths = as.integer(Deaths), State = "Virginia", Date = Sys.Date() - 1) %>% 
  filter(Health_District == "Northern") %>% 
  select(Deaths, State, Date)

# This will be done another later when I have time. I will continue to download the pdfs of it. 
# They posted download links, sweet!!
# I will also email the VDH to hopefully paste it into a download link.
# Virginia_Demo <- pdf_text(paste0(paste("Virginia", "COVID-19", "Dashboard", "Demographics", Sys.Date(), sep = " "), ".pdf")) %>% 
#   
# Virginia_By_Age_Today <- Virginia_Demo[1]  
#   
# Virginia_By_Age_Today <- Virginia_By_Age_Today %>% str_split("\n") %>% unlist() %>% as_tibble()
# 
# Virginia_By_Age_Today <- Virginia_By_Age_Today[3:nrow(Virginia_By_Age_Today),] 
# 
# Virginia_By_Age_Today <- str_split(Virginia_By_Age_Today$value, "\\s") %>% lapply(str_remove_all, "Group|C\\.\\.") %>% lapply(function(x){ x[!is.na(x) & x != ""]})
# 
# Virginia_By_Age_Today <- do.call(rbind.data.frame, Virginia_By_Age_Today)
# 
# colnames(Virginia_By_Age_Today) <- as.character(unlist(unname(Virginia_By_Age_Today[1,])))
# Virginia_By_Age_Today <- Virginia_By_Age_Today[2:nrow(Virginia_By_Age_Today),]
# Virginia_By_Age_Today <- Virginia_By_Age_Today %>% 
#   mutate(Cases = as.integer(Cases)) %>% 
#   mutate(Total = str_remove_all(Total, "%")) %>% 
#   mutate(Total = as.double(Total)) %>% 
#   mutate(Date = Sys.Date()) %>% 
#   mutate(State = "Virginia")
# 
# Virginia_By_Age <- read_csv("Virginia_By_Age.csv")
# Virginia_By_Age <- bind_rows(Virginia_By_Age_Today, Virginia_By_Age)
# 
# Virginia_By_Race_Today <- Virginia_Demo[2]
# 
# Virginia_By_Race_Today <- Virginia_By_Race_Today %>% str_split("\n") %>% unlist() %>% as_tibble()
# 
# Virginia_By_Race_Today <- Virginia_By_Race_Today[3:nrow(Virginia_By_Race_Today),] 
# 
# Virginia_By_Race_Today <- str_split(Virginia_By_Race_Today$value, "\\s") %>% 
#   lapply(str_remove_all, "African|American|^or|Reported") %>% 
#   lapply(str_replace_all, "Not", "Not reported") %>% 
#   lapply(function(x){ x[!is.na(x) & x != ""]})
# 
# Virginia_By_Race_Today <- do.call(rbind.data.frame, Virginia_By_Race_Today)
# 
# colnames(Virginia_By_Race_Today) <- c("Race", "Cases", "Total")
# 
# Virginia_By_Race_Today <- Virginia_By_Race_Today %>% 
#   mutate(Cases = as.integer(Cases)) %>% 
#   mutate(Total = str_remove_all(Total, "%")) %>% 
#   mutate(Total = as.double(Total)) %>% 
#   mutate(Date = Sys.Date()) %>% 
#   mutate(State = "Virginia")
# 
# Virginia_By_Race <- read_csv("Virginia_By_Race.csv")
# Virginia_By_Race <- bind_rows(Virginia_By_Race_Today, Virginia_By_Race)
# 
# Virginia_By_Sex_Today <- Virginia_Demo[3]
# 
# Virginia_By_Sex_Today <- Virginia_By_Sex_Today %>% str_split("\n") %>% unlist() %>% as_tibble()
# 
# Virginia_By_Sex_Today <- Virginia_By_Sex_Today[3:nrow(Virginia_By_Sex_Today),] 
# 
# Virginia_By_Sex_Today <- str_split(Virginia_By_Sex_Today$value, "\\s{2,}") %>% 
#   lapply(function(x){ x[!is.na(x) & x != ""]})
# 
# Virginia_By_Sex_Today <- do.call(rbind.data.frame, Virginia_By_Sex_Today)
# 
# colnames(Virginia_By_Sex_Today) <- c("Sex", "Cases", "Total")
# 
# Virginia_By_Sex_Today <- Virginia_By_Sex_Today %>% 
#   mutate(Cases = as.integer(Cases)) %>% 
#   mutate(Total = str_remove_all(Total, "%")) %>% 
#   mutate(Total = as.double(Total)) %>% 
#   mutate(Date = Sys.Date()) %>% 
#   mutate(State = "Virginia")
# 
# Virginia_By_Race <- read_csv("Virginia_By_Race.csv")
# Virginia_By_Race <- bind_rows(Virginia_By_Race_Today, Virginia_By_Race)
# 


### This is for additional Virginia Demographic Data
## Clean up top-line tests, hospitalizations, and deaths dataframe by adding in date and state column
Virginia_Totals_Headers[1] <- "Tests"

Virginia_Totals <- as.data.frame(rbind(Virginia_Totals_Headers, Virginia_Totals_Numbers), row.names = F, stringsAsFactors = F)
Virginia_Totals <- Virginia_Totals[2,]
colnames(Virginia_Totals) <- Virginia_Totals_Headers

Virginia_Totals$Date <- Sys.Date() - 1
Virginia_Totals$Tests <- as.integer(Virginia_Totals$Tests)
Virginia_Totals$Hospitalizations <- as.integer(Virginia_Totals$Hospitalizations)
Virginia_Totals$Deaths <- as.integer(Virginia_Totals$Deaths)
Virginia_Totals$State <- "Virginia"

# Add topline totals into main file
Virginia_Totals_Today <- Virginia_Totals
Virginia_Totals <- read_csv("VirginiaTotals.csv")
Virginia_Totals <- bind_rows(Virginia_Totals_Today, Virginia_Totals)
write_csv(Virginia_Totals, "VirginiaTotals.csv")

Sys.sleep(5)

#### DC Analysis
# Old stuff # This is before they posted download link!
# DC_data_raw <- DC_pg_html[2]
# 
# 
# 
# DC_data_raw_split <- DC_data_raw %>% 
#   str_split("\n")
# 
# DC_data_raw_split_jscleaned <- DC_data_raw_split[[1]][1:which(sapply(DC_data_raw_split, str_detect, "Number of positive results: 2") == T)]
# 
# DC_data_raw_list <- DC_data_raw_split_jscleaned %>% 
#   str_split("\n") %>% 
#   lapply(str_remove_all, "\t") %>% 
#   lapply(str_remove_all, "Number of ") %>% 
#   lapply(str_remove_all, "\\d+-year-old .*") %>% 
#   lapply(str_remove_all, "DC Public Health.*") %>%
#   lapply(str_remove_all, "DC Health.*") %>%
#   lapply(str_remove_all, "Public Safety.*") %>%
#   lapply(str_remove_all, "Hospital.*") %>%
#   lapply(str_remove_all, "Details.*") %>% 
#   lapply(str_remove_all, "As of ") %>%
#   lapply(str_remove_all, "Download.*") %>%
#   lapply(str_remove_all, "Numbers as of.*") %>%
#   lapply(str_remove_all, "\\*.*") %>% 
#   lapply(str_to_lower) %>% 
#   lapply(function(x){ x[!is.na(x) & x != ""]}) 
# 
# month.name.lower <- str_to_lower(month.name)
# 
# Date <- lapply(DC_data_raw_list, map, str_subset, month.name.lower) %>% 
#   unlist() %>% 
#   str_remove_all("[:|,]")
# 
# Date[str_detect(Date, "2020", negate = T)] <- lapply(Date, function(x) {if (str_detect(x, "2020", negate = T)) { str_c(x, "2020", sep = " ")}}) %>% unlist()
# 
# DC_data_raw_list <- DC_data_raw_list %>% unlist()
# 
# 
# DC_data_raw_list_no_dates <- sapply(DC_data_raw_list, map, str_detect, month.name.lower, negate = T)
# 
# 
# DC_data_raw_list_no_dates <- names(Filter(all, DC_data_raw_list_no_dates))
# 
# #### You will need to see if this will work programatically? Why is the javascript appearing??
# # DC_data_raw_list_no_dates <- DC_data_raw_list_no_dates[1:105]
# ######
# 
# 
# DC_raw_lists <- DC_data_raw_list_no_dates %>%
#   lapply(function(x){ x[!is.na(x) & x != ""]}) %>% ## Will this still be needed as well?
#   lapply(str_remove_all, " ") %>% 
#   lapply(str_remove_all, ",") %>% 
#   enframe() %>% 
#   select(value) %>% 
#   separate(value, into = c("variable", "number"), sep = ":") %>% 
#   mutate(number = str_trim(number)) %>% 
#   pivot_wider(names_from = variable, values_from = number) %>% 
#   sapply(unlist)
# 
# 
# 
# DC_raw_lists$positiveresults <- prepend(DC_raw_lists$positiveresults, rep(NA, (length(Date) - length(DC_raw_lists$positiveresults))), before = 1)
# 
# for (i in 1:length(DC_raw_lists)) {
#   if (unname(lengths(DC_raw_lists[i])) < length(Date)) {
#     DC_raw_lists[[i]] <- append(DC_raw_lists[[i]], rep(NA, (length(Date) - unname(lengths(DC_raw_lists[i])))), after = unname(lengths(DC_raw_lists[i])))
#   }
# }
# 
# 
# dcCovidCleaned <- DC_raw_lists %>% 
#   as_tibble() %>% 
#   mutate_all(na_if, "") %>% 
#   mutate_all(as.integer) %>% 
#   rename_all(list(~make.names(.))) %>% 
#   bind_cols(as.data.frame(Date)) %>% 
#   mutate(Date = mdy(Date)) %>% 
#   mutate(State = "District of Columbia") %>% 
#   mutate(County = "District of Columbia")
# 
# # dcCovidCleaned <- within(dcCovidCleaned, peopletestedoverall[Date == as.Date("2020-03-26")] <- 2166)
# 
# dcCovidCases <- dcCovidCleaned %>%
#   rowwise() %>% 
#   mutate(Cases = sum(phlpositives, commerciallabpositives, positiveresults, na.rm = T)) %>% ## Will this be permanent as well?
#   left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
#   left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name")) %>% 
#   dplyr::select(County, Cases, Date, State, Abbr, FIPS)
# 
# dcSummary <- dcCovidCleaned %>% 
#   filter(Date == max(Date)) %>% 
#   rowwise() %>% 
#   mutate(Cases = sum(phlpositives, commerciallabpositives, positiveresults, na.rm = T)) %>% 
#   mutate(Tests = peopletestedoverall) %>% 
#   mutate(Deaths = deaths) %>% 
#   select(State, Cases, Deaths, Tests, Date)
# 
# DC_By_County <- dcCovidCleaned %>% 
#   filter(Date == max(Date)) %>% 
#   rowwise() %>% 
#   mutate(Cases = sum(phlpositives, commerciallabpositives, positiveresults, na.rm = T)) %>% 
#   left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
#   left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name")) %>% 
#   dplyr::select(County, Cases, Date, State, Abbr, FIPS)
# 
# write_csv(dcCovidCleaned, "dcCovid-19AllVariables.csv")
# 
# write_csv(dcCovidCases, "dcCovidJustCases.csv")

# DC Data analysis
# Clean up crosstable and add in date and state column

# Unlist and column naming is not necessary anymore
# dcCovid19ByAgeSexToday <- dcCovid19ByAgeSexToday[[1]]
# colnames(dcCovid19ByAgeSexToday) <- unname(dcCovid19ByAgeSexToday[2,])
# dcCovid19ByAgeSexToday <- dcCovid19ByAgeSexToday[3:nrow(dcCovid19ByAgeSexToday),]

dcCovid19ByWardToday <- read_excel(paste0("COVID19_DCHealthStatisticsDataV3 (NewFileStructure)", Sys.Date() - 1,  ".xlsx"), sheet = "Total Cases by Ward")
dcCovid19ByAgeSexToday <- read_excel(paste0("COVID19_DCHealthStatisticsDataV3 (NewFileStructure)", Sys.Date() - 1,  ".xlsx"), sheet = "PatientAge-Gender")

dcCovid19ByAgeSexTodayXTab <- dcCovid19ByAgeSexToday %>% 
  mutate(Date = Sys.Date() - 1, State = "District of Columbia", Cases = as.integer(`Total Positives`), Age_Range = `Patient Age (yrs)`, Male = as.integer(Male), Female = as.integer(Female))

if ("Unknown" %in% names(dcCovid19ByAgeSexTodayXTab)) {
  dcCovid19ByAgeSexTodayXTab$Unknown = as.integer(dcCovid19ByAgeSexTodayXTab$Unknown)
} else {
  dcCovid19ByAgeSexTodayXTab$Unknown = rep(NA, nrow(dcCovid19ByAgeSexTodayXTab))
}

dcCovid19ByAgeSexTodayXTab <- dcCovid19ByAgeSexTodayXTab %>% 
  select(Age_Range, Cases, Unknown, Female, Male, Date, State)


# Add today's crosstab into main file
dcCovid19ByAgeSexXTab <- read_csv("dcCovid19ByAgeSexXTab.csv")
dcCovid19ByAgeSexXTab <- bind_rows(dcCovid19ByAgeSexTodayXTab, dcCovid19ByAgeSexXTab)
write_csv(dcCovid19ByAgeSexXTab, "dcCovid19ByAgeSexXTab.csv")

# Get dataframe of just today's breakout by age and add date and state column
dcCovid19ByAgeToday <- dcCovid19ByAgeSexToday[2:nrow(dcCovid19ByAgeSexToday),] %>% #dcCovid19ByAgeSexToday[2:nrow(dcCovid19ByAgeSexToday),] %>% 
  mutate(Age_Range = `Patient Age (yrs)`, Cases = as.integer(`Total Positives`), Date = Sys.Date() - 1, State = "District of Columbia", Male = as.integer(Male), Female = as.integer(Female)) %>% 
  select(Age_Range, Cases, Date, State)
# Add today's breakout to the main file
dcCovid19ByAge <- read_csv("dcCovid19ByAge.csv")
dcCovid19ByAge <- bind_rows(dcCovid19ByAgeToday, dcCovid19ByAge)
write_csv(dcCovid19ByAge, "dcCovid19ByAge.csv")

# Get dataframe of just today's DC Sex breakout data
dcCovid19BySexToday <- dcCovid19ByAgeSexToday[1,] %>%
  mutate(Cases = as.integer(`Total Positives`), Date = Sys.Date() - 1, State = "District of Columbia")

if ("Unknown" %in% names(dcCovid19BySexToday)) {
  dcCovid19BySexToday$Unknown = as.integer(dcCovid19BySexToday$Unknown)
} else {
  dcCovid19BySexToday$Unknown = rep(NA, nrow(dcCovid19BySexToday))
}

dcCovid19BySexToday <- dcCovid19BySexToday %>% 
  select(Unknown, Male, Female, Date, State) %>% 
  gather(-c(Date, State), key = "Sex", value = "Cases") %>% 
  mutate(Cases = as.integer(Cases)) %>% 
  select(Sex, Cases, Date, State)

# Add today's DC sex breakout to main file
dcCovid19BySex <- read_csv("dcCovid19BySex.csv")
dcCovid19BySex <- bind_rows(dcCovid19BySexToday, dcCovid19BySex)
write_csv(dcCovid19BySex, "dcCovid19BySex.csv")


# Cleaning the DC by Ward breakout
# Not needed anymore: dcCovid19ByWardToday <- dcCovid19ByWardToday[[1]]
colnames(dcCovid19ByWardToday) <- c("Ward", as.character(seq.Date(from = as.Date("2020/03/31"), to = (Sys.Date() - 1), by = "day")))
# Not needed anymore dcCovid19ByWardToday <- dcCovid19ByWardToday[2:nrow(dcCovid19ByWardToday),]
# Adding in a date column
dcCovid19ByWardToday <- dcCovid19ByWardToday %>%
  select(Ward, (as.character(Sys.Date() - 1))) %>%
  rename(Cases = (as.character(Sys.Date() - 1))) %>% 
  mutate(Date = Sys.Date() - 1, Cases = as.integer(Cases), Ward = as.integer(Ward)) %>% 
  select(Ward, Cases, Date)
# Adding today's by ward breakout to main file
dcCovid19ByWard <- read_csv("dcCovid19ByWard.csv")
dcCovid19ByWard <- bind_rows(dcCovid19ByWardToday, dcCovid19ByWard)
write_csv(dcCovid19ByWard, "dcCovid19ByWard.csv")


Sys.sleep(5)


# Reading in the full excel file sheet and spliting the Cases/Hospital data from the by Organization data
dcCovid19DataSummaryDCOrgsToday <- read_xlsx("dcCovid-19DataSummaryToday.xlsx", sheet = "Overal Stats", skip = 12)
dcCovid19DataSummaryToday <- read_xlsx("dcCovid-19DataSummaryToday.xlsx", sheet = "Overal Stats", n_max = 11)

# Cleaning the Cases/Hospital data
colnames(dcCovid19DataSummaryToday) <- c("Organization", "Metric", as.character(seq.Date(from = as.Date("2020/03/07"), to = (Sys.Date() - 1), by = "day")))
dcCovid19DataSummaryToday <- dcCovid19DataSummaryToday[2:nrow(dcCovid19DataSummaryToday),]

dcCovid19DataSummaryToday <- dcCovid19DataSummaryToday %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  gather(-c("Organization", "Metric"), key = "Date", value = "Amount") %>% 
  select(Metric, Date, Amount) %>% 
  spread(key = Metric, value = Amount)

colnames(dcCovid19DataSummaryToday) <- make.names(colnames(dcCovid19DataSummaryToday), unique = T)

dcCovid19TestingCases <- dcCovid19DataSummaryToday %>% 
  select(Date, People.Tested.Overall, Total.Positives, Number.of.Deaths, People.Recovered)

# Re-saving the updated Cases/Testing dataframe
write_csv(dcCovid19TestingCases, "dcCovid19TestingCases.csv")

# Saving the latest day's summary 
dcSummary <- dcCovid19TestingCases %>%
  mutate(Date = as.Date(Date)) %>% 
  filter(Date == max(Date)) %>% 
  rename(Tests = People.Tested.Overall, Deaths = Number.of.Deaths, Cases = Total.Positives) %>% 
  mutate(State = "District of Columbia") %>% 
  mutate(Cases = as.integer(Cases), Deaths = as.integer(Deaths), Tests = as.integer(Tests)) %>% 
  select(State, Cases, Deaths, Tests, Date)
# Saving the latest day's summary with State abbreviation and FIPS code column
DC_By_County <- dcCovid19TestingCases %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date == max(Date)) %>% 
  rename(Tests = People.Tested.Overall, Deaths = Number.of.Deaths, Cases = Total.Positives) %>%
  mutate(State = "District of Columbia", County = "District of Columbia") %>%
  mutate(Cases = as.integer(Cases), Deaths = as.integer(Deaths)) %>% 
  left_join(stateConversions, by = c("State" = "Full_Name")) %>%
  left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name")) %>%
  dplyr::select(County, Cases, Deaths, Date, State, Abbr, FIPS)

# Saving DC's daily death totals solo
All_DC_DMV_Deaths_Today <- dcCovid19TestingCases %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date == max(Date)) %>% 
  rename(Deaths = Number.of.Deaths) %>% 
  mutate(State = "District of Columbia") %>% 
  select(Deaths, State, Date)


# Cleaning and saving most updated hosptials dataframe
dcCovid19Hospitals <- dcCovid19DataSummaryToday %>% 
  select(Date, ICU.Beds.Available, Total.Ventilators, Ventilators.in.Use, Ventilators.Available)

write_csv(dcCovid19Hospitals, "dcCovid19Hospitals.csv")
Sys.sleep(5)
colnames(dcCovid19DataSummaryDCOrgsToday) <- c("Organization", "Metric", as.character(seq.Date(from = as.Date("2020/03/07"), to = (Sys.Date() - 1), by = "day")))

# Cleaning and resaving the most updated organization data
dcCovid19DataSummaryDCOrgsToday <- dcCovid19DataSummaryDCOrgsToday %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(!is.na(Organization)) %>% 
  gather(-c("Organization", "Metric"), key = "Date", value = "Amount") %>%
  filter(Metric != "FEMS") %>% 
  spread(key = Metric, value = Amount) %>% 
  mutate(Organization = str_replace_all(Organization, " ", "-")) %>% 
  rename_all(funs(str_replace_all(., " ", ".")))

write_csv(dcCovid19DataSummaryDCOrgsToday, "dcCovid19DataSummaryDCOrgs.csv")

Sys.sleep(5)

### This is for the top-line dataframe as requested by the DCist ####

# Making the Virginia statewide totals for the day
Virginia <- Virginia_Totals_Today[c(1, 2, 4)]

Virginia$Cases <- sum(Virginia_By_County_Today$Cases)
Virginia$State <- "Virginia"

Virginia <- Virginia %>% 
  select(State, Cases, Deaths, Tests, Date)


# Making the Maryland statewide totals for the day
Maryland <- tibble(State = "Maryland", 
                   Cases = sum(MD_By_County_Today$Cases), 
                   Deaths = MD_Summary_Today$Deaths, 
                   Tests = MD_Summary_Today$Tests, # This is fixed now: sum(MD_Summary_Today$Tests, sum(MD_By_County_Today$Cases)), # Fix this earlier by swaping the MD_Cases_By_County before the MD_Summary and replace the old test number with the test number plus the total cases for all counties.
                   Date = Sys.Date() - 1)


# Making the Virginia cases by county
virginiaCounties <- Virginia_By_County_Today %>% 
  filter(County %in% c("Alexandria", "Arlington", "Fairfax", "Loudoun", "Prince William", "Culpeper")) %>% 
  mutate(Deaths = NA) %>% 
  mutate(Tests = NA) %>% 
  select(County, Cases, Deaths, Tests, Date)

# Making the Maryland cases by county
marylandCounties <- MD_By_County_Today %>% 
  filter(County %in% c("Montgomery", "Prince Georges", "Frederick", "Calvert")) %>% 
  mutate(Deaths = NA) %>% 
  mutate(Tests = NA) %>% 
  select(County, Cases, Deaths, Tests, Date)

# Making the daily summary dataframe the DCist requested
dailySummaryToday <- bind_rows(dcSummary, Maryland, marylandCounties, Virginia, virginiaCounties) %>% 
  select(State, County, Cases, Deaths, Tests, Date)


# Adding the daily summary to the main file
dailySummary <- read_csv("covidSummaryDCist.csv")
dailySummary <- bind_rows(dailySummaryToday, dailySummary)
write_csv(dailySummary, "covidSummaryDCist.csv")

# Making daily DMV deaths dataframe
All_DMV_Deaths_Today <- bind_rows(All_DC_DMV_Deaths_Today, All_MD_DMV_Deaths_Today, All_VA_DMV_Deaths_Today)

All_DMV_Deaths_Today <- stateCountyPops %>% 
  filter(FIPS %in% c(11001, 24033, 24031, 24009, 24021, 51107, 51153, 51059, 51013, 51510, 51600, 51610, 51683, 51685)) %>% 
  group_by(STNAME) %>% 
  summarize(TOTAL_POP = sum(TOTAL_POP), TOTAL_POP_100K = sum(TOTAL_POP_100K)) %>% 
  ungroup() %>% 
  left_join(All_DMV_Deaths_Today, by = c("STNAME" = "State"))

All_DMV_Deaths <- read_csv("All_DMV_Deaths.csv")
All_DMV_Deaths <- bind_rows(All_DMV_Deaths_Today, All_DMV_Deaths)
write_csv(All_DMV_Deaths, "All_DMV_Death.csv")


### This is more expanded data for whole Washington Metro Area combined
# Read in the cases by county dataframe for all counties in DC, VA, MD, and WV
Full_States <- read_csv("FullStates.csv")


# Adding in today's by county data fro DC, VA, MD, and WV
Full_States <- bind_rows(Full_States, Virginia_By_County, MD_By_County, WV_CountiesDFCleaned, DC_By_County) %>% 
  arrange(desc(Date))

# Clean up county names so they can be joined on later
Full_States <- Full_States %>%
  mutate(County = unlist(lapply(County, str_remove_all, "'"))) %>%
  mutate(County = unlist(lapply(County, str_remove_all, "/."))) %>% 
   mutate(County = case_when(County %in% c("Alexandria", "Harrisonburg", "Chesapeake", "Newport News", "Williamsburg", "Portsmouth", "Charlottesville", "Virginia Beach", "Suffolk", "Norfolk") ~ paste(County, "City", sep = " "),
                             County %in% c("Franklin County", "Baltimore County") ~ str_remove(County, ".County"),
                             str_detect(County, "Chesterf.*")  ~ "Chesterfield",
                             str_detect(County, 'Sta..ord') ~ "Stafford",
                             str_detect(County, 'Su..olk') ~ "Suffolk City",
                             str_detect(County, ".*Marys") ~ "St Marys",
                             T ~ County)) #%>%
  # left_join(stateConversions, by = c("State" = "Full_Name")) %>% 
  # left_join(countyStateFIPS, by = c("Abbr" = "State", "County" = "Name"))

# Dedup
Full_States %>% 
  distinct() %>% 
  write_csv("FullStates.csv")

# And save
Full_States <- Full_States %>% distinct()

# Make the character vectors of the county names and FIPS codes
DMV_Counties <- c("District of Columbia", 
                  "Calvert", "Charles", "Frederick", "Montgomery", "Prince Georges", 
                  "Alexandria", "Arlington", "Clarke", "Culpeper", "Fairfax", "Farquier", "Fredericksburg", "Loudoun", "Manassas", "Prince William", "Rappahannock", "Spotsylvania", "Stafford", "Warren",
                  "Jefferson")
DMV_FIPS <- c("11001", "24009", "24017", "24021", "24031", "24033", "51510", "51013", "51043", "51047", "51059", "51061", "51630", "51107", "51683", "51153", "51157", "51177", "51179", "51187", "54037", "51600", "51610", "51685")
DMV_Closer_FIPS <- c("24031", "24033", "24021", "24009", "51510", "51013", "51059", "51600", "51107", "51153", "51047", "11001")
# combined1 <- sort(union(levels(Full_States$FIPS), levels(DMV_FIPS)))
Sys.sleep(15)

# Make a subset of Full_States with just counties in Washington Metro Area
DMV_Counties_Covid_Cases <- Full_States %>%
  mutate(FIPS = as.character(FIPS)) %>% 
  filter(FIPS %in% DMV_FIPS)

# Save Washington Metro by county case file
write_csv(DMV_Counties_Covid_Cases, "DMVCountiesCovidCases.csv")

##### Start making plots! #####

# ### Line chart of all cases by county (fill) and state (dot shape)
# ### Bar Chart of most recent totals
# 

covid19Theme <- function() {
  theme_classic() + 
    theme(text = element_text(family = "Georgia", color = "gray25"),
          plot.title = element_text(size = 24, face = "bold"), 
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(color = "gray30", size = 12),
          legend.position = "right",
          legend.text = element_text(color = "gray30", size = 14, face = "bold"),
          legend.title = element_text(color = "gray30", size = 16, face = "bold"),
          axis.text = element_text(color = "gray25", size = 14)
    )
}

ggplotCaption <- "Data from: DC Mayor's Office, MD Dept of Health, VA Dept of Health"
Sys.sleep(5)
dmvCasesByCountyLine <- DMV_Counties_Covid_Cases %>%
  filter(Date >= as.Date("2020-03-23")) %>%
  filter(FIPS %in% DMV_Closer_FIPS) %>%
  ggplot(aes(x = Date, y = Cases)) +
    geom_line(aes(color = County), size = 3) +
    geom_point(aes(shape = State, color = County), size = 6) +
    scale_y_continuous(trans = 'log2', breaks = trans_breaks("log2", function(x) 2^x)) + 
    covid19Theme() +
    scale_color_brewer(palette = "Paired") +
    labs(title = "DMV Covid-19 cases by county", subtitle = "Over time", caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/dmvCasesByCountyLine.png", plot = dmvCasesByCountyLine, width = 300, height = 250, units = 'mm')
Sys.sleep(5)

## Per capita county line chart
dmvCasesByCountyLinePerCap <- DMV_Counties_Covid_Cases %>%
  left_join(stateCountyPops, by = "FIPS") %>% 
  filter(Date >= as.Date("2020-03-23")) %>%
  filter(FIPS %in% DMV_Closer_FIPS) %>%
  ggplot(aes(x = Date, y = (Cases / TOTAL_POP_100K))) +
  geom_line(aes(color = County), size = 3) +
  geom_point(aes(shape = State, color = County), size = 6) +
  scale_y_continuous(trans = 'log2', breaks = trans_breaks("log2", function(x) 2^x)) + 
  covid19Theme() +
  scale_color_brewer(palette = "Paired") +
  labs(title = "DMV Covid-19 case rate per capita by county", subtitle = "Over time, per 100k residents", caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/dmvCasesByCountyLinePerCap.png", plot = dmvCasesByCountyLinePerCap, width = 300, height = 250, units = 'mm')
Sys.sleep(5)
# 
dmvCasesByCountyBar <- DMV_Counties_Covid_Cases %>%
  filter(Date == max(Date)) %>%
  filter(FIPS %in% DMV_Closer_FIPS) %>%
  ggplot(aes(x = State, y = Cases, fill = reorder(County, -Cases))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Cases), vjust = -0.2, size = 5, position = position_dodge2(width = 0.9, reverse = F)) +
  covid19Theme() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "DMV Covid-19 cases by county", subtitle = paste("On", Sys.Date() - 1, sep = " "), caption = ggplotCaption) + xlab("") + ylab("") +
  guides(fill=guide_legend(title="County"))


ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/dmvCasesByCountyBar.png", plot = dmvCasesByCountyBar, width = 300, height = 275, units = 'mm')
Sys.sleep(5)

## Per Capita County Bar Chart
dmvCasesByCountyBarPerCap <- DMV_Counties_Covid_Cases %>%
  left_join(stateCountyPops, by = "FIPS") %>% 
  filter(Date == max(Date)) %>%
  filter(FIPS %in% DMV_Closer_FIPS) %>%
  ggplot(aes(x = State, y = round((Cases / TOTAL_POP_100K), 1), fill = reorder(County, -round((Cases / TOTAL_POP_100K), 1)))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round((Cases / TOTAL_POP_100K), 0)), vjust = -0.2, size = 7, position = position_dodge2(width = 0.9, reverse = F)) +
  covid19Theme() +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "DMV Covid-19 case rate by county per capita", subtitle = paste("On", paste0(Sys.Date() - 1, ","), "per 100k residents", sep = " "), caption = ggplotCaption) + xlab("") + ylab("") +
  guides(fill=guide_legend(title="County"))

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/dmvCasesByCountyBarPerCap.png", plot = dmvCasesByCountyBarPerCap, width = 300, height = 275, units = 'mm')
Sys.sleep(5)

# 
# ### Line chart of all people tested by state
# ### Bar Chart of most recent totals
# 
DMVTestsLine <- dailySummary %>%
  drop_na(Tests) %>%
  ggplot(aes(x = Date, y = Tests)) +
  geom_line(aes(color = State), na.rm = T, size = 3) +
  geom_point(aes(shape = State, color = State), size = 6) +
  scale_y_continuous(breaks = seq(0, (max(dailySummary$Tests, na.rm = T) + 2000), by = 2000)) +
  scale_color_manual(values = c("#E91436", "#ebab00ff", "#00257C")) + 
  covid19Theme() +
  labs(title = "DMV Covid-19 tests", subtitle = "Over time", caption = ggplotCaption) + xlab("") + ylab("")
  
ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/DMVTestsLine.png", plot = DMVTestsLine, width = 300, height = 275, units = 'mm')
Sys.sleep(5)

### Line Chart of testing rate per capita
DMVTestsLinePerCap <- dailySummary %>%
  drop_na(Tests) %>%
  left_join(stateCountyPops, by = c("State" = "STNAME")) %>% 
  filter(SUMLEV == "040") %>% 
  ggplot(aes(x = Date, y = (Tests / TOTAL_POP_100K))) +
  geom_line(aes(color = State), na.rm = T, size = 3) +
  geom_point(aes(shape = State, color = State), size = 6) +
  scale_y_continuous(breaks = seq(0, max(dailySummary$Tests, na.rm = T), by = 100)) +
  scale_color_manual(values = c("#E91436", "#ebab00ff", "#00257C")) + 
  covid19Theme() +
  labs(title = "DMV Covid-19 test rate", subtitle = "Over time, per 100k residents", caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/DMVTestsLinePerCap.png", plot = DMVTestsLinePerCap, width = 300, height = 275, units = 'mm')
Sys.sleep(5)

# 
DMVTestsBar <- dailySummary %>%
  drop_na(Tests) %>%
  filter(Date == max(Date)) %>%
  ggplot(aes(x = State, y = Tests)) +
  geom_col(aes(fill = State), na.rm = T) +
  geom_text(aes(label = Tests), vjust = -0.2, size = 8) +
  scale_fill_manual(values = c("#E91436", "#ebab00ff", "#00257C")) +
  covid19Theme() +
  labs(title = "DMV Covid-19 tests", subtitle = paste("On", Sys.Date() - 1, sep = " "), caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/DMVTestsBar.png", plot = DMVTestsBar, width = 300, height = 275, units = 'mm')
Sys.sleep(5)
# 

DMVTestsBarPerCap <- dailySummary %>%
  drop_na(Tests) %>%
  filter(Date == max(Date)) %>%
  left_join(stateCountyPops, by = c("State" = "STNAME")) %>% 
  filter(SUMLEV == "040") %>%
  ggplot(aes(x = State, y = round((Tests / TOTAL_POP_100K), 1)))  +
  geom_col(aes(fill = State), na.rm = T) +
  geom_text(aes(label = round((Tests / TOTAL_POP_100K), 1)), vjust = -0.2, size = 8) +
  scale_fill_manual(values = c("#E91436", "#ebab00ff", "#00257C")) +
  covid19Theme() +
  labs(title = "DMV Covid-19 test rate", subtitle = paste("On", paste0(Sys.Date() - 1, ","), "per 100k residents", sep = " "), caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/DMVTestsBarPerCap.png", plot = DMVTestsBarPerCap, width = 300, height = 275, units = 'mm')
Sys.sleep(5)


# ### Line Chart of death totals by state over time
dcmdvaDeathsLine <- dailySummary %>% 
  drop_na(State) %>%
  ggplot(aes(x = Date, y = Deaths)) +
  geom_line(aes(color = State), na.rm = T, size = 3) +
  geom_point(aes(color = State, shape = State), na.rm = T, size = 6) +
  scale_color_manual(values = c("#E91436", "#EBAB00", "#00257C")) +
  scale_y_continuous(breaks = seq(min(dailySummary$Deaths, na.rm = T), max(dailySummary$Deaths, na.rm = T), by = 6)) +
  covid19Theme() +
  labs(title = "DMV Covid-19 deaths", subtitle = "Over time", caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/dcmdvaDeathsLine.png", plot = dcmdvaDeathsLine, width = 300, height = 275, units = 'mm')
Sys.sleep(5)
# ### Bar Chart of latest death totals by State
# 
dcmdvaDeathsBar <- dailySummary %>%
  drop_na(State) %>%
  filter(Date == max(Date)) %>%
  ggplot(aes(x = State, y = Deaths)) +
  geom_col(aes(fill = State), na.rm = T) +
  geom_text(aes(label = Deaths), vjust = -0.2, size = 8) +
  scale_fill_manual(values = c("#E91436", "#EBAB00", "#00257C")) +
  covid19Theme() +
  labs(title = "DMV Covid-19 deaths", subtitle = paste("On", Sys.Date() - 1, sep = " "), caption = ggplotCaption) + xlab("") + ylab("")

ggsave(filename = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19/dcmdvaDeathsBar.png", plot = dcmdvaDeathsBar, width = 300, height = 275, units = 'mm')
Sys.sleep(5)
# 
# ### Chloropleth of Washington Metro Area Counties by Cases

# Read in shapfile of US counties
counties <- st_read("/home/adrian/Documents/US_County_Shapfile_Population/tl_2019_us_county.shp")

# Make dataframe of DMV counties and join in their populations for per-capita calculations
DMV <- counties %>%
  mutate(GEOID = as.character(GEOID), NAME = as.character(NAME)) %>% 
  filter(GEOID %in% DMV_FIPS) %>% 
  left_join(stateCountyPops, by = c("GEOID" = "FIPS"))

# Fix Fairfax city name error
DMV[DMV$GEOID == "51600",]$NAME <- "Fairfax City"

# plot(st_geometry(DMV))

# Get only today's DMV Covid-19 cases by county
DMV_Counties_Covid_Cases_Today <- DMV_Counties_Covid_Cases %>%
  filter(Date == max(Date))
# Join
DMV_Cases <- DMV %>%
  left_join(DMV_Counties_Covid_Cases_Today, by = c("GEOID" = "FIPS"))
# Fill NAs with 0
DMV_Cases <- DMV_Cases %>%
  mutate(Cases = if_else(is.na(Cases), 0, Cases))
# Assign new projection
st_crs(DMV_Cases) <- "+proj=longlat +datum=WGS84"

# No longer making this static plot, making a leaflet one!
# DMV_Cases %>%
#   ggplot() +
#   geom_sf(aes(fill = Cases)) +
#   scale_fill_gradient(low = "#fee0d2", high = "#de2d26")
# 
# ### Chloropleth of Washington Metro Area Counties by Cases
#myBins <- round(c(as.integer(unname(quantile(DMV_Cases$Cases, na.rm = T))), Inf), 0)

# Create cases and per cap case rate bins and palettes
myBinsReg <- round(seq(from = 0, to = (max(DMV_Cases$Cases) + 1), by = (max(DMV_Cases$Cases) / 6)), 0)
upperPerCapBoundary <- round(max(DMV_Cases$Cases / DMV_Cases$TOTAL_POP_100K) + 1, 0)

while(upperPerCapBoundary %% 6 != 0) {
  upperPerCapBoundary = upperPerCapBoundary + 1
}
myBinsPerCap <- seq(from = 0, 
                          to = upperPerCapBoundary, 
                          by = upperPerCapBoundary / 6)
dmvPaletteReg <- colorBin(palette = "YlOrRd", domain = DMV_Cases$Cases, na.color = "transparent", bins = myBinsReg)
dmvPalettePerCap <- colorBin(palette = "YlOrRd", domain = DMV_Cases$Cases, na.color = "transparent", bins = myBinsPerCap)
Sys.sleep(15)

# Create tool tip legend text
legendText <- paste0(
  "County: ", DMV_Cases$NAME, "<br/>",
  "State: ", DMV_Cases$State, "<br/>",
  "Cases: ", DMV_Cases$Cases, "<br/>"
) %>% 
  lapply(htmltools::HTML)

# Make the per capita leaflet chloropleth
legendTextPerCap <- paste0(
  "County: ", DMV_Cases$NAME, "<br/>",
  "State: ", DMV_Cases$State, "<br/>",
  "Rate per 100K: ", round((DMV_Cases$Cases / DMV_Cases$TOTAL_POP_100K), 1), "<br/>"
) %>% 
  lapply(htmltools::HTML)
attribution <- htmltools::HTML("Map data &copy; <a href='https://www.openstreetmap.org/'>OpenStreetMap</a> contributors, <a href='https://creativecommons.org/licenses/by-sa/2.0/'>CC-BY-SA</a><br/>Data from: <a href = 'https://dhhr.wv.gov/COVID-19/Pages/default.aspx'>WV DHHR</a>, <a href='https://coronavirus.dc.gov/page/coronavirus-data'>DC Mayor's Office</a>, <a href='https://coronavirus.maryland.gov/'>MD Dept of Health</a>, <a href='http://www.vdh.virginia.gov/coronavirus/'>VA Dept of Health</a>")
Sys.sleep(5)
# Create Leaflet and data attribution
# This was before I figured out the radio buttons and grouping to make one map with both the cases count and case rate

# Create DMV chloropleth of just cases
# dmvChloropleth <- leaflet(DMV_Cases) %>% 
#   addTiles(attribution = attribution) %>% 
#   setView(lat = "38.8858", lng = "-77.1054", zoom = 8) %>% 
#   addPolygons(stroke=T, 
#               opacity = 1,
#               fillOpacity = 0.9, 
#               smoothFactor = 0.5, 
#               color = "black", 
#               fillColor = ~dmvPaletteReg(Cases),
#               weight = 0.5,
#               label = legendText,
#               labelOptions = labelOptions( 
#                 style = list("font-weight" = "normal", padding = "3px 8px"), 
#                 textsize = "13px", 
#                 direction = "auto"
#               )) %>% 
#   addLegend( pal=dmvPaletteReg, values=~Cases, opacity=0.9, title = paste("DMV Covid-19 Cases on", Sys.Date() - 1), position = "topright" )
# # Make sure to add them to directory hooked up to website
# setwd("/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
# Sys.sleep(15)
# mapshot(dmvChloropleth, url = "dmvChloropleth.html")
# Sys.sleep(5)
# 
# 
# dmvChloroplethPerCap <- leaflet(DMV_Cases) %>% 
#   addTiles(attribution = attribution) %>% 
#   setView(lat = "38.8858", lng = "-77.1054", zoom = 8) %>% 
#   addPolygons(stroke=T, 
#               opacity = 1,
#               fillOpacity = 0.9, 
#               smoothFactor = 0.5, 
#               color = "black", 
#               fillColor = ~dmvPalettePerCap((Cases / TOTAL_POP_100K)),
#               weight = 0.5,
#               label = legendTextPerCap,
#               labelOptions = labelOptions( 
#                 style = list("font-weight" = "normal", padding = "3px 8px"), 
#                 textsize = "13px", 
#                 direction = "auto"
#               )) %>% 
#   addLegend( pal=dmvPalettePerCap, values=~(Cases / TOTAL_POP_100K), opacity=0.9, title = htmltools::HTML(paste("DMV Covid-19 Case Rate <br> per 100k on", Sys.Date() - 1)), position = "topright" )
# Sys.sleep(15)
# mapshot(dmvChloroplethPerCap, url = "dmvChloroplethPerCap.html")

dmvChloropleth <- leaflet(DMV_Cases) %>% 
  addTiles(attribution = attribution) %>% 
  setView(lat = "38.87086", lng = "-77.13826", zoom = 8) %>% 
  addPolygons(stroke=T, 
              opacity = 1,
              fillOpacity = 0.9, 
              smoothFactor = 0.5, 
              color = "black", 
              fillColor = ~dmvPaletteReg(Cases),
              weight = 0.5,
              label = legendText,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              ),
              group = paste("Case count", Sys.Date() - 1, sep = " ")) %>% 
  addPolygons(stroke=T, 
              opacity = 1,
              fillOpacity = 0.9, 
              smoothFactor = 0.5, 
              color = "black", 
              fillColor = ~dmvPalettePerCap((Cases / TOTAL_POP_100K)),
              weight = 0.5,
              label = legendTextPerCap,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              ),
              group = paste("Case rate per 100K", Sys.Date() - 1, sep = " ")) %>% 
  addLegend( pal=dmvPaletteReg, 
             values=~Cases, 
             opacity=0.9, 
             title = paste("Case count", Sys.Date() - 1, sep = " "), 
             position = "topright", 
             group = paste("Case count", Sys.Date() - 1, sep = " ")) %>%
  addLegend( pal=dmvPalettePerCap, 
             values=~(Cases / TOTAL_POP_100K), 
             opacity=0.9, 
             title = paste("Case rate per 100K", Sys.Date() - 1, sep = " "), 
             position = "topright", 
             group = paste("Case rate per 100K", Sys.Date() - 1, sep = " ")) %>% 
  addLayersControl(baseGroups = c(paste("Case count", Sys.Date() - 1, sep = " "), paste("Case rate per 100K", Sys.Date() - 1, sep = " ")),
                   position = "topright",
                   options = layersControlOptions(collapsed = F)) %>% 
  htmlwidgets::onRender(
    "function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }"
  )

Sys.sleep(5)
setwd("/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
Sys.sleep(15)
mapshot(dmvChloropleth, url = "dmvChloropleth.html")
Sys.sleep(15)


### Look into plotly plots with Rplotly below
# plotlyExample <- ggplotly(dmvCasesByCountyLine)
# htmlwidgets::saveWidget(plotlyExample, file = "plotlyExample.html", selfcontained = F, libdir = getwd())


dmvCasesByCountyLinePlotly <- DMV_Counties_Covid_Cases %>%
  left_join(stateCountyPops, by = "FIPS") %>% 
  filter(Date >= as.Date("2020-03-23")) %>%
  filter(FIPS %in% DMV_Closer_FIPS) %>% 
  mutate(Case_Rate_100K = (Cases / TOTAL_POP_100K))


## This pretty much works! I need to get it to display only one option on rendering
# https://stackoverflow.com/questions/42081811/plotly-drop-down-menu-not-restyling-y-correctly


### This works!!!

dmvCasesByCountyLinePlotlyGraph <- plot_ly(data = dmvCasesByCountyLinePlotly, x = ~Date) %>% 
  add_trace(y = ~Cases,
            linetype = ~factor(State),
            color = ~factor(County),
            colors = "Set3", 
            mode = "lines+markers", 
            type = "scatter", 
            symbol = ~State,
            hovertemplate = paste(
              "State: ", dmvCasesByCountyLinePlotly$State,
              "<br>County: ", dmvCasesByCountyLinePlotly$County,
              "<br>Cases: ", "%{y}",
              "<br>Date: ", "%{x}",
              "<extra></extra>"
            ), 
            visible = T) %>%
  add_trace(y = ~Case_Rate_100K,
            linetype = ~factor(State),
            color = ~factor(County),
            colors = "Set3", 
            mode = "lines+markers", 
            type = "scatter", 
            symbol = ~State,
            hovertemplate = paste(
              "State: ", dmvCasesByCountyLinePlotly$State,
              "<br>County: ", dmvCasesByCountyLinePlotly$County,
              "<br>Cases: ", "%{y}",
              "<br>Date: ", "%{x}",
              "<extra></extra>"
            ), 
            visible = F) %>%
  layout(yaxis = list(type = "log"),
         title = "DMV Covid-19 Cases & Case Rate by County",
         xaxis = list(title = ""),
         showlegend = F,
         updatemenus = list(
           list(active = 0,
                buttons = list(
                  list(method = "restyle",
                       args = list("visible", append(rep(list(TRUE), 12), rep(list(FALSE), 12))),
                       label = "Count"),
                  list(method = "restyle",
                       args = list("visible", append(rep(list(FALSE), 12), rep(list(TRUE), 12))),
                       label = "Per 100K")))
         ))

Sys.sleep(5)
dmvCasesByCountyBarPerCapPlotly <- DMV_Counties_Covid_Cases %>%
  left_join(stateCountyPops, by = "FIPS") %>% 
  filter(Date == max(Date)) %>%
  filter(FIPS %in% DMV_Closer_FIPS) %>% 
  mutate(Case_Rate_100K = (Cases / TOTAL_POP_100K))
  

dmvCasesByCountyBarPlotlyGraph <- plot_ly(dmvCasesByCountyBarPerCapPlotly, 
        x = ~factor(State), 
        type = "bar", 
        color = ~factor(reorder(County, -Cases)), 
        colors = "Set3") %>% 
  add_trace(y = ~Cases,
            hovertemplate = paste(
              "State: ", dmvCasesByCountyBarPerCapPlotly$State,
              "<br>County: ", dmvCasesByCountyBarPerCapPlotly$County,
              "<br>Cases: ", "%{y}",
              "<br>Date: ", dmvCasesByCountyBarPerCapPlotly$Date,
              "<extra></extra>"
            ), 
            visible = T) %>%
  add_trace(y = ~Case_Rate_100K,
            hovertemplate = paste(
              "State: ", dmvCasesByCountyBarPerCapPlotly$State,
              "<br>County: ", dmvCasesByCountyBarPerCapPlotly$County,
              "<br>Cases: ", "%{y}",
              "<br>Date: ", dmvCasesByCountyBarPerCapPlotly$Date,
              "<extra></extra>"
            ), 
            visible = F) %>% 
  layout(title = paste("DMV Covid-19 Cases & Case Rate by County on", Sys.Date() - 1, sep = " "),
         xaxis = list(title = ""),
         showlegend = F,
         updatemenus = list(
           list(active = 0,
                buttons = list(
                  list(method = "restyle",
                       args = list("visible", append(rep(list(F), 12), rep(list(T), 12))),
                       label = "Count"),
                  list(method = "restyle",
                       args = list("visible", append(rep(list(T), 12), rep(list(F), 12))),
                       label = "Per 100K")))
         ))

Sys.sleep(5)
DMVTestsLinePerCapPlotly <- dailySummary %>%
  drop_na(Tests) %>%
  left_join(stateCountyPops, by = c("State" = "STNAME")) %>% 
  filter(SUMLEV == "040") %>% 
  mutate(Test_Rate_100K = (Tests / TOTAL_POP_100K))



dmvTestsByStateLinePlotlyGraph <- plot_ly(data = DMVTestsLinePerCapPlotly, x = ~Date) %>% 
  add_trace(y = ~Tests,
            linetype = ~factor(State),
            color = ~factor(State),
            colors = c("#E91436", "#ebab00ff", "#00257C"), 
            mode = "lines+markers", 
            type = "scatter", 
            symbol = ~State,
            hovertemplate = paste(
              "State: ", DMVTestsLinePerCapPlotly$State,
              "<br>Tests: ", "%{y}",
              "<br>Date: ", "%{x}",
              "<extra></extra>"
            ), 
            visible = T) %>%
  add_trace(y = ~Test_Rate_100K,
            linetype = ~factor(State),
            color = ~factor(State),
            colors = c("#E91436", "#ebab00ff", "#00257C"), 
            mode = "lines+markers", 
            type = "scatter", 
            symbol = ~State,
            hovertemplate = paste(
              "State: ", DMVTestsLinePerCapPlotly$State,
              "<br>Tests: ", "%{y}",
              "<br>Date: ", "%{x}",
              "<extra></extra>"
            ), 
            visible = F) %>%
  layout(title = "DMV Covid-19 Tests & Test Rate by State",
         xaxis = list(title = ""),
         showlegend = F,
         updatemenus = list(
           list(active = 0,
                buttons = list(
                  list(method = "restyle",
                       args = list("visible", append(rep(list(TRUE), 3), rep(list(FALSE), 3))),
                       label = "Count"),
                  list(method = "restyle",
                       args = list("visible", append(rep(list(FALSE), 3), rep(list(TRUE), 3))),
                       label = "Per 100K")))
         ))

Sys.sleep(5)
DMVTestsBarPerCapPlotly <- dailySummary %>%
  drop_na(Tests) %>%
  filter(Date == max(Date)) %>%
  left_join(stateCountyPops, by = c("State" = "STNAME")) %>% 
  filter(SUMLEV == "040") %>% 
  mutate(Test_Rate_100K = (Tests / TOTAL_POP_100K))



dmvTestsByStateBarPlotlyGraph <- plot_ly(DMVTestsBarPerCapPlotly, 
        x = ~factor(State), 
        type = "bar", 
        color = ~factor(State), 
        colors = c("#E91436", "#ebab00ff", "#00257C")) %>% 
  add_trace(y = ~Tests,
            hovertemplate = paste(
              "State: ", DMVTestsBarPerCapPlotly$State,
              "<br>Tests: ", "%{y}",
              "<br>Date: ", DMVTestsBarPerCapPlotly$Date,
              "<extra></extra>"
            ), 
            visible = T) %>%
  add_trace(y = ~Test_Rate_100K,
            hovertemplate = paste(
              "State: ", DMVTestsBarPerCapPlotly$State,
              "<br>Cases: ", "%{y}",
              "<br>Date: ", DMVTestsBarPerCapPlotly$Date,
              "<extra></extra>"
            ), 
            visible = F) %>% 
  layout(title = paste("DMV Covid-19 Tests & Test Rate by State on", Sys.Date() - 1, sep = " "),
         xaxis = list(title = ""),
         showlegend = F,
         updatemenus = list(
           list(active = 0,
                buttons = list(
                  list(method = "restyle",
                       args = list("visible", append(rep(list(F), 3), rep(list(T), 3))),
                       label = "Count"),
                  list(method = "restyle",
                       args = list("visible", append(rep(list(T), 3), rep(list(F), 3))),
                       label = "Per 100K")))
         ))

Sys.sleep(5)
dcmdvaDeathsLinePlotly <- dailySummary %>% 
  drop_na(State) %>% 
  left_join(stateCountyPops, by = c("State" = "STNAME")) %>% 
  filter(SUMLEV == "040") %>% 
  mutate(Death_Rate_100K = (Deaths / TOTAL_POP_100K))



dmvDeathsByStateLinePlotlyGraph <- plot_ly(data = dcmdvaDeathsLinePlotly, x = ~Date) %>% 
  add_trace(y = ~Deaths,
            linetype = ~factor(State),
            color = ~factor(State),
            colors = c("#E91436", "#ebab00ff", "#00257C"), 
            mode = "lines+markers", 
            type = "scatter", 
            symbol = ~State,
            hovertemplate = paste(
              "State: ", dcmdvaDeathsLinePlotly$State,
              "<br>Deaths: ", "%{y}",
              "<br>Date: ", "%{x}",
              "<extra></extra>"
            ), 
            visible = T) %>%
  add_trace(y = ~Death_Rate_100K,
            linetype = ~factor(State),
            color = ~factor(State),
            colors = c("#E91436", "#ebab00ff", "#00257C"), 
            mode = "lines+markers", 
            type = "scatter", 
            symbol = ~State,
            hovertemplate = paste(
              "State: ", dcmdvaDeathsLinePlotly$State,
              "<br>Deaths: ", "%{y}",
              "<br>Date: ", "%{x}",
              "<extra></extra>"
            ), 
            visible = F) %>%
  layout(title = "DMV Covid-19 Deaths & Death Rate by State",
         showlegend = F,
         xaxis = list(title = ""),
         updatemenus = list(
           list(active = 0,
                buttons = list(
                  list(method = "restyle",
                       args = list("visible", append(rep(list(TRUE), 3), rep(list(FALSE), 3))),
                       label = "Count"),
                  list(method = "restyle",
                       args = list("visible", append(rep(list(FALSE), 3), rep(list(TRUE), 3))),
                       label = "Per 100K")))
         ))
Sys.sleep(5)
dcmdvaDeathsBarPlotly <- dailySummary %>%
  drop_na(State) %>%
  filter(Date == max(Date)) %>% 
  left_join(stateCountyPops, by = c("State" = "STNAME")) %>% 
  filter(SUMLEV == "040") %>% 
  mutate(Death_Rate_100K = as.double((Deaths / TOTAL_POP_100K)), Deaths = as.double(Deaths))


dmvDeathsByStateBarPlotlyGraph <- plot_ly(dcmdvaDeathsBarPlotly, 
        x = ~factor(State), 
        type = "bar", 
        color = ~factor(State), 
        colors = c("#E91436", "#ebab00ff", "#00257C")) %>% 
  add_trace(y = ~Death_Rate_100K,
            hovertemplate = paste(
              "State: ", dcmdvaDeathsBarPlotly$State,
              "<br>Deaths: ", "%{y}",
              "<br>Date: ", dcmdvaDeathsBarPlotly$Date,
              "<extra></extra>"
            ), 
            visible = F) %>%
    add_trace(y = ~Deaths,
            hovertemplate = paste(
              "State: ", dcmdvaDeathsBarPlotly$State,
              "<br>Deaths: ", "%{y}",
              "<br>Date: ", dcmdvaDeathsBarPlotly$Date,
              "<extra></extra>"
            ), 
            visible = T) %>%
  layout(title = paste("DMV Covid-19 Deaths & Death Rate by State on", Sys.Date() - 1, sep = " "),
         xaxis = list(title = ""),
         yaxis = list(title = "Deaths"),
         showlegend = F,
         updatemenus = list(
           list(active = 0,
                buttons = list(
                  list(method = "restyle",
                       args = list("visible", append(rep(list(T), 3), rep(list(F), 3))),
                       label = "Count"),
                  list(method = "restyle",
                       args = list("visible", append(rep(list(F), 3), rep(list(T), 3))),
                       label = "Per 100K")))
         ))
Sys.sleep(5)
plotlyPlots <- list(dmvDeathsByStateBarPlotlyGraph, dmvDeathsByStateLinePlotlyGraph, dmvTestsByStateBarPlotlyGraph, dmvTestsByStateLinePlotlyGraph, dmvCasesByCountyBarPlotlyGraph, dmvCasesByCountyLinePlotlyGraph)

saveWidget(dmvDeathsByStateBarPlotlyGraph, "dmvDeathsByStateBarPlotlyGraph.html", selfcontained = F, libdir = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
Sys.sleep(5)
saveWidget(dmvDeathsByStateLinePlotlyGraph, "dmvDeathsByStateLinePlotlyGraph.html", selfcontained = F, libdir = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
Sys.sleep(5)
saveWidget(dmvTestsByStateBarPlotlyGraph, "dmvTestsByStateBarPlotlyGraph.html", selfcontained = F, libdir = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
Sys.sleep(5)
saveWidget(dmvTestsByStateLinePlotlyGraph, "dmvTestsByStateLinePlotlyGraph.html", selfcontained = F, libdir = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
Sys.sleep(5)
saveWidget(dmvCasesByCountyBarPlotlyGraph, "dmvCasesByCountyBarPlotlyGraph.html", selfcontained = F, libdir = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")
Sys.sleep(5)
saveWidget(dmvCasesByCountyLinePlotlyGraph, "dmvCasesByCountyLinePlotlyGraph.html", selfcontained = F, libdir = "/home/adrian/Documents/Personal_Portfolio_Site/DMV_Covid-19")





