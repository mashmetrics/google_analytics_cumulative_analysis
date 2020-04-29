
#### load neccessary libraries for analysis ----
library(googleAnalyticsR)
library(googleAuthR)
library(tidyverse)
library(googlesheets4)
library(xlsx)
library(bigrquery)

#### account authentication ----
ga_auth()

#### Setting the view Id for the analysis ----
view_id <- add your view id

#### Pull the data from google analytics ----
gadata <- google_analytics_4(view_id,
                             date_range = c("2019-01-01", "2020-03-31"),
                             metrics = c("uniquePageviews"),
                             dimensions = c("date", "landingPagePath", "deviceCategory"),
                             anti_sample = TRUE  )

gadata$landingPagePath <- sub("\\?.+", "", gadata$landingPagePath)

gadata <- gadata %>%
  
  group_by(date, landingPagePath) %>%
  summarise(pageviews = sum(uniquePageviews))


#### remove not set landing pages ----
gadata <- gadata %>% 
  filter(landingPagePath != "(not set)") %>%
  filter(!str_detect(landingPagePath, pattern = "apis|archive|realestate")) %>%
  filter(str_detect(landingPagePath, "blog"))
  

#### add column with cumulative performance, grouped by landing page & date ----
gadata$cumsum <- ave(gadata$pageviews, gadata$landingPagePath, FUN=cumsum)


#### add rank column per landing page & date ----
gadata <- gadata %>% group_by(landingPagePath) %>% mutate(rank = rank(date)) %>% arrange(landingPagePath, rank)

gadata_30days <- gadata %>%
  group_by(landingPagePath) %>%
  filter(rank < 30) %>%
  mutate(first30days = mean(cumsum))

gadatax <- merge(gadata,gadata_30days, by="landingPagePath")
  
      
    

#### push data from R to BigQuery ----
insert_upload_job("rtodatastudio", "rdata", "gadata", gadata)


#### send data to DoMO
install.packages("devtools")

library("devtools")
install_github(repo="domoinc-r/DomoR")
install.packages(domo)

domo_token <- "add token"

DomoR::init('mashmetrics', domo_token)

DomoR::create(data = gadata, name = "Cumulative Analysis")
DomoR::replace_ds(data = gadata, data_source_id = "add your token")
