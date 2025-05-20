install.packages("DBI")
install.packages("RSQLite")
install.packages("sf")  # For spatial queries

library(DBI)
library(RSQLite)

# Connect to the .geodatabase file
con <- dbConnect(SQLite(), "../data/ODC_CRIME_TRAFFICACCIDENTS5YR_P_6962891498747388240.geodatabase")

# List available tables (feature classes)
dbListTables(con)

query <- "select top_traffic_accident_offense, count(*) as nn 
from CRIME_TRAFFICACCIDENTS5YR_P
group by top_traffic_accident_offense 
order by nn desc"
data <- dbGetQuery(con, query)
print(data)

dbGetQuery(con, "select min(reported_date), max(reported_date),
            count(*) from CRIME_TRAFFICACCIDENTS5YR_P")

dbDisconnect()
library(lubridate)

# Convert Julian Date to standard date
jd <- 2460712
standard_date <- as.Date(jd, origin = "1970-01-01") - (2440587 - 1)
print(standard_date)  # Output: "2012-08-10"

##
                  

query <- "PRAGMA table_info(CRIME_TRAFFICACCIDENTS5YR_P);"
column_info <- dbGetQuery(con, query)

###
library(readr)

data1 <- read_csv( "data/ODC_CRIME_TRAFFICACCIDENTS5YR_P_5008541234304418365.csv",
                  show_col_types = FALSE,
                  col_select = 
                      c(name = object_id, 
                        category = top_traffic_accident_offense,
                        date = first_occurrence_date, 
                        lng = geo_lon, 
                        lat = geo_lat, 
                        road = ROAD_DESCRIPTION,
                        vehicle= TU1_VEHICLE_TYPE))
data1$date  <- as.Date(as.POSIXct(data1$date, format = "%m/%d/%Y %I:%M:%S %p") )

    
places <- data1 %>% 
    filter(date >= as.Date("2022-01-01") & 
               date < as.Date("2025-01-01")) %>%
                 filter(complete.cases(.))
    
places <- data.frame(places)
write_csv(places, "data/places.csv")
##
library(dplyr)
library(lubridate)
data2 <- read_csv( "data/ODC_CRIME_TRAFFICACCIDENTS5YR_P_5008541234304418365.csv",
                    show_col_types = FALSE,
                   col_select = 
                       c(object_id, type = top_traffic_accident_offense,
                                                date = first_occurrence_date, geo_lon, 
                         geo_lat, ROAD_DESCRIPTION))

data2$date  <- as.Date(as.POSIXct(data2$date, format = "%m/%d/%Y %I:%M:%S %p") )

weekly_summary <- places %>% filter(date > as.Date("2020-01-01") & 
                                       date < as.Date("2025-01-01")) %>%
    group_by(category,week = floor_date(date, unit = "month")) %>%  # Floors dates to the start of the week
    summarise(
        count = n()
    )
library(ggplot2)
ggplot(weekly_summary, aes(week, count, colour = type)) +
    geom_line()


x_summary <- data2 %>% filter(date > as.Date("2021-01-01")& 
                                  date < as.Date("2025-01-01")) %>%
    group_by(month = month(date), year = year(date)) %>%  # Floors dates to the start of the week
    summarise(
        count = n()
    )

ggplot(x_summary, aes(as.factor(month), count, colour = as.factor(year)) )+
    geom_point()



library(leaflet)
leaflet() %>%
    addTiles() %>%  # Default OpenStreetMap tiles
    setView(lng = -74.006, lat = 40.7128, zoom = 10)  # Centered on New York City



# Create a data frame of locations
locations <- data.frame(
    name = data1$top_traffic_accident_offense,
    lat = data1$geo_lat,
    lng = -data1$geo_lon
)

# Create a Leaflet map with multiple markers
leaflet(locations[1:100,]) %>%
    addTiles() %>%
    addMarkers(~lng, ~lat, popup = ~name)

library(jsonlite)
dat <- fromJSON("https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/arcgis/rest/services/ODC_CRIME_TRAFFICACCIDENTS5YR_P/FeatureServer/325/query?where=1%3D1&outFields=offense_code_extension,top_traffic_accident_offense,reported_date,geo_lon,geo_lat,ROAD_DESCRIPTION,TU1_VEHICLE_TYPE,incident_id&outSR=4326&f=json",
                flatten = TRUE)
xx <- "https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/arcgis/rest/services/ODC_CRIME_TRAFFICACCIDENTS5YR_P/FeatureServer/325/query?where=1%3D1&outFields=offense_code_extension,top_traffic_accident_offense,reported_date,geo_lon,geo_lat,ROAD_DESCRIPTION,TU1_VEHICLE_TYPE,incident_id&returnGeometry=false&returnIdsOnly=true&outSR=4326&f=json"
xx<- "https://services1.arcgis.com/zdB7qR0BtYrg0Xpl/arcgis/rest/services/ODC_CRIME_TRAFFICACCIDENTS5YR_P/FeatureServer/325/query?where=1%3D1&outFields=offense_code_extension,top_traffic_accident_offense,reported_date,geo_lon,geo_lat,ROAD_DESCRIPTION,TU1_VEHICLE_TYPE,incident_id&returnGeometry=false&outSR=4326&f=json"
dat2 <- fromJSON(xx, flatten = TRUE)


df <- dat$features$" attributes"


# View the first few rows of the data frame
head(df)

