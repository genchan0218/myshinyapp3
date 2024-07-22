rm(list = ls())


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)



library(purrr)
library(DT)
library(ggplot2)
library(plotly)
library(scales)
library(kableExtra)
library(htmlwidgets)
library(dplyr)
library(quantmod)
library(tidyverse)
library(lubridate)
library(gtrendsR)
#devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)# to add addSearchFeatures
library(leaflet)
library(sp)
library(raster)
library(rgeos)
library(leaflet)
library(sf)
library(maps)
library(tools)
library(RColorBrewer)
library(networkD3)
library(tidytext)
library(igraph)
library(ggraph)
library(curl)
library(formattable)
library(maptools)

# test <- gtrendsR::gtrends(keyword = c("Amazon","Walmart"), geo = c("US"), time = "2017-12-10 2018-01-20")
# str(test$related_queries)
# str(test$interest_over_time$hits)
# test$interest_by_dma
# 
# table(test$interest_over_time$related_queries, test$related_queries$value)

country_code <- read.csv("data/country_code.csv", stringsAsFactors = F)
states <- read_rds("data/states.rds")
states$ID <- toTitleCase(states$ID)


list_color <- c("Blues","YlOrRd", "BuGn", "BuPu", "GnBu", "Greens", 
                "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", 
                "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr")
single_color <- c("#6BAED6", "#FD8D3C", "#66C2A4", "#8C96C6", "#7BCCC4", "#74C476", 
                  "#969696", "#FD8D3C", "#FC8D59", "#74A9CF", "#67A9CF", "#DF65B0", 
                  "#9E9AC8", "#F768A1", "#FB6A4A", "#78C679", "#41B6C4", "#FE9929")

get_time <- function(time) {
  time <- ifelse(nchar(time) > 10,
                 time %>%
                   str_split(" ") %>%
                   map_chr(2) %>%
                   hms(),"")

}
#https://stackoverflow.com/questions/25199851/r-how-to-get-the-week-number-of-the-month
nth_week<- function(dates = NULL,
                    count_weeks_in = c("month","year"),
                    begin_week_on = "Sunday"){
  count_weeks_in <- tolower(count_weeks_in[1])

  # day_names and day_index are for beginning the week on a day other than Sunday
  # (this vector ordering matters, so careful about changing it)
  day_names<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

  # index integer of first match
  day_index<- pmatch(tolower(begin_week_on),
                     tolower(day_names))[1]


  ### Calculate week index of each day

  if (!is.na(pmatch(count_weeks_in, "year"))) {

    # For year:
    # sum the day of year, index for day of week at start of year, and constant 5
    #  then integer divide quantity by 7
    # (explicit on package so lubridate and data.table don't fight)
    n_week<- (5 +
                lubridate::yday(dates) +
                lubridate::wday(floor_date(dates, 'year'),
                                week_start = day_index)
    ) %/% 7

  } else {

    # For month:
    # same algorithm as above, but for month rather than year
    n_week<- (5 +
                lubridate::day(dates) +
                lubridate::wday(floor_date(dates, 'month'),
                                week_start = day_index)
    ) %/% 7

  }

  # naming very helpful for review
  names(n_week)<- paste0(lubridate::wday(dates,T), '-', dates)

  n_week

}

