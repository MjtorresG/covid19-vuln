# adapted from https://github.com/humberto-ortiz/pr-ypll and https://github.com/rafalab/pr-covid
# convert json_file to dataframe

library("tidyverse")
library("lubridate")
library("scales")
library("epiR")

json_file <- "https://bioportal.salud.pr.gov/api/administration/reports/deaths/summary"

first_day <- make_date(2020, 3, 12)
deaths <- jsonlite::fromJSON(json_file) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")),
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(date = as_date(ymd_hms(deathDate, tz = "America/Puerto_Rico"))) %>%
  mutate(date = if_else(date < first_day | date > today(),
                        as_date(ymd_hms(reportDate, tz = "America/Puerto_Rico")),
                        date))

# YPLL graphs by region and date
ypll <- deaths %>%
  mutate(ypll = 75 - (1 + age_end + age_start)/2) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(region, date) %>%
  summarize(ypll = sum(ypll), .groups = "drop")

arecibo_ypll<- filter(ypll, region == "Arecibo")
ggplot(arecibo_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Arecibo Region")

bayamon_ypll<- filter(ypll, region == "Bayamon")
ggplot(bayamon_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Bayamon Region")

metro_ypll<- filter(ypll, region == "Metro")
ggplot(metro_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Metro Region")

caguas_ypll<- filter(ypll, region == "Caguas")
ggplot(caguas_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Caguas Region")

fajardo_ypll<- filter(ypll, region == "Fajardo")
ggplot(fajardo_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Fajardo Region")

mayaguez_ypll<- filter(ypll, region == "Mayaguez")
ggplot(mayaguez_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Mayagüez Region")

ponce_ypll<- filter(ypll, region == "Ponce")
ggplot(ponce_ypll, aes(x=date, y=cumsum(ypll))) + geom_line()
+ ggtitle("Cumulative YPLL - Ponce Region")

# U.S. 2000 standard population
ages <-c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
      "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
      "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", 
      "90 to 94", "95 to 99")

st_pop <- function(x) {
  if (x == "0 to 4") {
    result <- 18987
  } else if (x == "5 to 9") {
    result <- 19920
  } else if (x == "10 to 14") {
    result <- 20057
  } else if (x == "15 to 19") {
    result <- 19820
  } else if (x == "20 to 24") {
    result <- 18257
  } else if (x == "25 to 29") {
    result <- 17722
  } else if (x == "30 to 34") {
    result <- 19511
  } else if (x == "35 to 39") {
    result <- 22180 
  } else if (x == "40 to 44") {
    result <- 22479
  } else if (x == "45 to 49") {
    result <- 19806
  } else if (x == "50 to 54") {
    result <- 17224
  } else if (x == "55 to 59") {
    result <- 13307
  } else if (x == "60 to 64") {
    result <- 10654
  } else if (x == "65 to 69") {
    result <- 9410
  } else if (x == "70 to 74") {
    result <- 8726
  } else if (x == "75 to 79") {
    result <- 7415
  } else if (x == "80 to 84") {
    result <- 4900
  } else {
  result <- 4259
  }
  return(result)
}

# PR population estimates
# read_csv("prm-est2019-agesex.csv")

# Age-adjusted YPLL for PR

ypll <- deaths %>%
  mutate(ypll = 75 - (1 + age_end + age_start)/2) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(region, ageRange) %>%
  summarize(ypll = sum(ypll), .groups = "drop") %>% 
  mutate(st_pop = lapply(ageRange, st_pop))

# adjusted_ypll <- epi.directadj(ypll, pop_est, st_pop)
