#--------------------------------------------------------------------------------------------------
#Shiny App to visualise the geographical distribution of COVID-19 cases  - Basic example of Code
#--------------------------------------------------------------------------------------------------
#Packages

library(RODBC)
library(dplyr)
#library(stringr)
library(lubridate)
#library(ggplot2)
#library(tidyr)
library(leaflet)
library(leaflet.extras)
install.packages('shinythemes')
library(shinythemes)


#setView(lng = -2.587910, lat = 51.454514, zoom = 10) 

#--------------------------------------------------------------------------------------------------
# Make a connection with SQL to read data into R *edited to remove server information*

con <- RODBC::odbcDriverConnect("driver={SQL Server};\n  server=xxx;\n  trusted_connection=true")

#Data attribute
string_attribute <- "select nhs_number, lsoa, attribute_period
  from
(SELECT *, ROW_NUMBER() OVER (PARTITION BY nhs_number ORDER BY InsertedDate desc) as RN
  FROM [MODELLING_SQL_AREA].[dbo].[primary_care_attributes]
  WHERE isnull(nhs_number,'') <> ''
  and attribute_period >= '2020-02-01') a
where RN = 1" 

attribute  <- RODBC::sqlQuery(con, string_attribute)



#Data supplementary
string_supp <- "select nhs_number, [confirmed_2019_nCoV], [sup_metric_1] as suspectedcovid, attribute_period
  from
(SELECT *, ROW_NUMBER() OVER (PARTITION BY nhs_number ORDER BY InsertedDate desc) as RN
  FROM [MODELLING_SQL_AREA].[dbo].[primary_care_supplemental]
  WHERE isnull(nhs_number,'') <> ''
  and attribute_period >= '2020-02-01') a
where RN = 1" 

supp  <- RODBC::sqlQuery(con, string_supp)



string_lsoapop<- "SELECT Code, Name, [All Ages] as pop
  FROM [Analyst_SQL_Area].[dbo].[tbl_BNSSG_Datasets_LSOA_POP_2018]"

lsoapop  <- RODBC::sqlQuery(con, string_lsoapop)



close(con)


#--------------------------------------------------------------------------------------------------


lsoa_nhs <- left_join(supp, attribute, by = 'nhs_number')

#--------------------------------------------------------------------------------------------------
#Sort out date format

#confirmed_2019_nCoV
str(lsoa_nhs$confirmed_2019_nCoV)
#Dashes or slashes, two different formats

lsoa_nhs$confirmed_2019_nCoV<- parse_date_time(as.character(lsoa_nhs$confirmed_2019_nCoV), c("%Y-%m-%d", "%d/%m/%Y"), tz="Europe/London")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Make a binary variable for date COVID-19 confirmed

lsoa_nhs<- lsoa_nhs %>%
  mutate(confirmedbinary = if_else(!is.na(confirmed_2019_nCoV), 1, 0))
#--------------------------------------------------------------------------------------------------

#Creating a table of LSOAs plus every date being used

#lsoa_nhs$suspecteddate<- as.Date(as.POSIXct(lsoa_nhs$suspectedcovid,tz="Europe/London"),tz="Europe/London")
#Have to do this because grouping by POSIXct with dplyr doesnt work just defaults to lsoa (below in part 2)

lsoa_nhs$confirmeddate<- as.Date(as.POSIXct(lsoa_nhs$confirmed_2019_nCoV),tz="Europe/London")


#We want a table with
#lsoa, number suspected, date month

distinctlsoa<- lsoa_nhs %>% 
  filter(!is.na(lsoa)) %>% 
  distinct(lsoa)


diff_in_days<- difftime(max(lsoa_nhs$confirmeddate, na.rm = T), min(lsoa_nhs$confirmeddate, na.rm = T), units = c("days"))

lsoa_nhs %>% 
  #filter(!is.na(suspecteddate))
  # transform to date format with lubridate
  #mutate(suspecteddate = ymd(suspecteddate)) %>% 
  # find years min and max 
  summarise(min = min(confirmeddate, na.rm = T),
            max = max(confirmeddate, na.rm = T))


days<- NA
days<- c(seq(as.Date("2020-02-27"), by = "day", length.out = diff_in_days + 1)) #Change this as the weeks go up
days<- as.data.frame(days)


lsoabyday<- expand.grid(distinctlsoa$lsoa, days$days)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#2nd Part


confirmedperdaylsoa<- lsoa_nhs %>%
  filter(!is.na(lsoa))%>%
  group_by(lsoa, confirmeddate)%>%
  summarise(totconfirmed =sum(confirmedbinary))

#sum(lsoa_nhs$suspectedbinary)
#11774

#Some people without LSOAs with suspected
sum(confirmedperdaylsoa$totconfirmed)
#11765

#Can check this with the below:

lsoa_nhs %>%
  filter(!is.na(lsoa)) %>% 
  count(confirmedbinary)




#left join THE ABOVE TO lsoabyweek and lsoa to add on totsuspected
#then do cumulative sum


confirmedperdaylsoa<- left_join(lsoabyday, y=confirmedperdaylsoa, by=c("Var2" = "confirmeddate", "Var1" = "lsoa")) 

#The order of the group by and arrange is important when doing cumulative sum
#Here we are grouping by LSOA, ordering by date and the cumulative sum is done then by LSOA

confirmedperdaylsoa<- confirmedperdaylsoa %>% 
  replace(is.na(.), 0) %>%
  rename(lsoa=Var1, confirmeddate=Var2) %>% 
  group_by(lsoa) %>% 
  arrange(confirmeddate) %>% 
  mutate(cumulativeconfirmed =cumsum(totconfirmed)) 

#11,765
sum(confirmedperdaylsoa$totconfirmed)

# join back to JHCovid19
#dplyr::left_join(., y = hospital_data_earliest, 
#                by = c("date"))



#population per LSOA

pop_lsoa<- lsoa_nhs %>%
  filter(!is.na(lsoa))%>%
  group_by(lsoa)%>%
  count()

#lsoa_confirmed_day <- left_join(confirmedperdaylsoa, pop_lsoa, by='lsoa')
#10,542

#alternative method to get population per LSOA


lsoa_confirmed_day <- left_join(confirmedperdaylsoa, lsoapop, by=c('lsoa'='Code'))


#--------------------------------------------------------------------------------------------------
lsoa_confirmed_day<- lsoa_confirmed_day %>% 
  replace(is.na(.), 0) %>%
  rename(lsoa_pop=pop) %>% 
  group_by(lsoa) %>% 
  arrange(confirmeddate) %>% 
  mutate(cumconfirm_per_100000 = cumulativeconfirmed*100000/lsoa_pop,
         cumconfirm_per_100000 = round(cumconfirm_per_100000, digits = 0)) 


#--------------------------------------------------------------------------------------------------

lsoa_confirmed_day<- lsoa_confirmed_day %>% 
  replace(is.na(.), 0) %>%
  rename(lsoa_pop=pop) %>% 
  group_by(lsoa) %>% 
  arrange(confirmeddate) %>% 
  mutate(twoweekroll = rollapplyr(totconfirmed, 14, sum, partial=T),
         twoweekroll_per_100000 = twoweekroll*100000/lsoa_pop,
         twoweekroll_per_100000 = round(twoweekroll_per_100000, digits = 0))                       


#--------------------------------------------------------------------------------------------------

lsoa_confirmed_day$cumsuspect_per_100000<- roundlsoa_confirmed_day$cumsuspect_per_100000, digits = 0)

#Check numbers as expected
sum(lsoa_confirmed_day$totconfirmed)


#10542

#--------------------------------------------------------------------------------------------------
#14-day cumulative number of COVID-19 cases per 100 000
#--------------------------------------------------------------------------------------------------



























#--------------------------------------------------------------------------------------------------

#Read in spatial data (may want to check this is the most up to date source) *edited to remove source of spatial data*

LSOA <- rgdal::readOGR("xxx",
                       "BNSSG_LSOA", stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------------------
#Defining dates to use (latest and earliest week date):
cv_min_date<-as.Date(min(lsoa_confirmed_day$confirmeddate), "%Y-%m-%d")
current_date<- as.Date(max(lsoa_confirmed_day$confirmeddate), "%Y-%m-%d")



library("RColorBrewer")
display.brewer.pal(n = 5, name = 'YlOrRd')
display.brewer.pal(n = 5, name = 'YlOrRd')
#FFFFB2

col<- #FFFFB2
   
#--------------------------------------------------------------------------------------------------
#Basemap


CV_lsoa <- lsoa_confirmed_day %>% filter(lsoa %in% LSOA$lsoa11cd) #cv data only for the lsoas that match

plot_map <- LSOA[LSOA$lsoa11cd %in% CV_lsoa$lsoa, ] #spatial data only for the lsoas that match

cv_min_date<-as.Date(min(CV_lsoa$confirmeddate), "%Y-%m-%d")
current_date<- as.Date(max(CV_lsoa$confirmeddate), "%Y-%m-%d")

summary(CV_lsoa$twoweekroll_per_100000)

bins=c(0,20,40,60,80,100, 120, 140, 160, Inf)
cv_pal <- colorBin("YlOrRd", domain = CV_lsoa$twoweekroll_per_100000, bins = bins, reverse = F)


LSOA_popup <- paste0("<strong>LSOA Name: </strong>",
                     CV_lsoa$lsoa,
                     "<br><strong> confirmed 14-day cumulative number of COVID-19 cases per 100 000: </strong>",
                     CV_lsoa$twoweekroll_per_100000)


#make basemap just outlines of all LSOAs
basemap <- leaflet(plot_map) %>%
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  #Adding Polygons
  addPolygons(data = LSOA,
              stroke = T, 
              weight = 1,
              color = "black",
              fillOpacity = 0, 
              smoothFactor = 0.5) %>%
  addLegend("bottomright", pal = cv_pal, values = ~CV_lsoa$twoweekroll_per_100000,
            title = "GP 14-day cumulative number of COVID-19 cases per 100 000",
            opacity = 1) %>% 
  
  #Add user controls
  addLayersControl(baseGroups = c("OSM", "Carto"))%>% 
  
  #Add additional controls
  addSearchOSM() %>%  #I have added these additionally
  addResetMapButton() %>% 
  setView(lng = -2.587910, lat = 51.454514, zoom = 10)

#--------------------------------------------------------------------------------------------------










#--------------------------------------------------------------------------------------------------
#APP
#--------------------------------------------------------------------------------------------------

ui<- fluidPage(
  theme=shinytheme("superhero"),
  themeSelector(),
  titlePanel("BNSSG 14-day cumulative number of confirmed COVID-19 cases per 100,000 by LSOA"),
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("plot_date", 
                  label = h5("Select mapping date"), 
                  min = as.Date(cv_min_date,"%Y-%m-%d"), 
                  max = as.Date(current_date,"%Y-%m-%d"), 
                  value = as.Date(current_date), 
                  timeFormat = "%d %b",  
                  animate=animationOptions(interval = 3000, loop = FALSE))
    ),
    
    mainPanel(
      
      leafletOutput("mymap", height="800")
    )
  )
)

#--------------------------------------------------------------------------------------------------

server = function(input, output, session) {
  
  reactive_db = reactive({ 
    lsoa_confirmed_day %>% filter(confirmeddate == input$plot_date)   #takes input date by user, subsets data to just that date
  }) 
  
  #reactive_db <- lsoa_confirmed_day %>% filter(confirmeddate == "2020-03-29") #date from input data
  
  reactive_db_2 = reactive({ 
    reactive_db() %>% filter(lsoa %in% LSOA$lsoa11cd)  #Takes the date subsetted data and takes just the LSOAs present in spatial data
  }) 
  #reactive2<- reactive_db %>% filter(lsoa %in% LSOA$lsoa11cd) #only the lsoas that match
  
  reactive_polygons = reactive({ 
    LSOA[LSOA$lsoa11cd %in% reactive_db_2()$lsoa, ] }) #Takes the spatial data for the date subsetted data for 563 lsoas of interest
  
  #reactive_polygons <- LSOA[LSOA$lsoa11cd %in% reactive2$lsoa, ]
  
  output$mymap <- renderLeaflet({  
    basemap 
  }) 
  
  observeEvent(input$plot_date, { 
    leafletProxy("mymap") %>%  
      clearMarkers() %>% 
      clearShapes() %>% 
      addPolygons(data = reactive_polygons(),
                  stroke = T, 
                  color = "black",
                  weight = 1,
                  opacity = 0.5,
                  fillOpacity = 1, 
                  smoothFactor = 0.5, 
                  fillColor = ~cv_pal(reactive_db_2()$twoweekroll_per_100000),
                  popup =  paste0("<strong>LSOA Name: </strong>",
                                  reactive_db_2()$lsoa,
                                  "<br><strong>GP 14-day cumulative number of COVID-19 cases per 100 000: </strong>",
                                  reactive_db_2()$twoweekroll_per_100000),#add reactive popup?
                  highlight = highlightOptions(weight = 5, color = "white",
                                               bringToFront = TRUE)) })
}


#--------------------------------------------------------------------------------------------------

shinyApp(ui=ui, server=server)

#--------------------------------------------------------------------------------------------------



