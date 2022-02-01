# adapted from https://github.com/humberto-ortiz/pr-ypll and https://github.com/rafalab/pr-covid

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
# U.S. 2000 standard population
ages <-c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
      "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
      "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", 
      "90 to 94", "95 to 99","100 to 104","105 to 109")

st_pop_function <- function(x) {
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
pop_estimates <- read_csv("prm-est2019-agesex.csv")

pop_est_df <- pop_estimates %>% select(YEAR, MUNICIPIO, NAME, AGE04_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, 
           AGE2529_TOT, AGE3034_TOT, AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT,
           AGE5559_TOT, AGE6064_TOT, AGE6569_TOT, AGE7074_TOT, AGE7579_TOT, AGE8084_TOT,
           AGE85PLUS_TOT) %>% filter(YEAR == 12) %>% mutate(Region = case_when(MUNICIPIO %in% c("013","017","027","039",
          "054","065","081","091","101","115","141","145")~"Arecibo",
          MUNICIPIO %in% c("019","021","033","047","051","105","107","135","137","143","045")~"Bayamon",
          MUNICIPIO %in% c("029","031","061","087","127","139")~"Metro",
          MUNICIPIO %in% c("007","009","025","035","041","063","069","077","085","095","103","129","151")~"Caguas",
          MUNICIPIO %in% c("037","049","053","089","119","147")~"Fajardo",
          MUNICIPIO %in% c("003","005","011","023","067","071","079","083","093","097","099","117","121","125","131")~"Mayaguez",
          MUNICIPIO %in% c("001","015","055","057","059","073","075","043","109","111","113","123","133","149","153")~"Ponce"))

# merging pop_est_by_region and ypll

ypll <- deaths %>%
  mutate(ypll = 75 - (1 + age_end + age_start)/2) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(region, ageRange) %>%
  summarize(ypll = sum(ypll), .groups = "drop") %>%
  mutate(st_pop = lapply(ageRange, st_pop_function))

agegrp_function <- function(x) {
  if (x == "AGE04_TOT") {
    result <- "0 to 4"
  } else if (x == "AGE59_TOT") {
    result <- "5 to 9"
  } else if (x == "AGE1014_TOT") {
    result <- "10 to 14"
  } else if (x == "AGE1519_TOT") {
    result <- "15 to 19"
  } else if (x == "AGE2024_TOT") {
    result <- "20 to 24"
  } else if (x == "AGE2529_TOT") {
    result <- "25 to 29"
  } else if (x == "AGE3034_TOT") {
    result <- "30 to 34"
  } else if (x == "AGE3539_TOT") {
    result <- "35 to 39"
  } else if (x == "AGE4044_TOT") {
    result <- "40 to 44"
  } else if (x == "AGE4549_TOT") {
    result <- "45 to 49"
  } else if (x == "AGE5054_TOT") {
    result <- "50 to 54"
  } else if (x == "AGE5559_TOT") {
    result <- "55 to 59"
  } else if (x == "AGE6064_TOT") {
    result <- "60 to 64"
  } else if (x == "AGE6569_TOT") {
    result <- "65 to 69"
  } else if (x == "AGE7074_TOT") {
    result <- "70 to 74"
  } else if (x == "AGE7579_TOT") {
    result <- "75 to 79"
  } else if (x == "AGE8084_TOT") {
    result <- "80 to 84"
  } else {
    result <- "85+"
  }
  return(result)
}

pop_est_by_region <- pop_est_df %>% select(-c(NAME, YEAR, MUNICIPIO)) %>% 
  pivot_longer(-Region, names_to = "age_group", values_to = "pop_est") %>% 
  mutate(ageRange = lapply(age_group, agegrp_function)) %>% 
  group_by(Region, ageRange,.add = T) %>%
  rename(region = Region) %>% summarize(pop_est = sum(pop_est), .groups = "keep")

merged <- merge(ypll, pop_est_by_region, by=c("region","ageRange"))

# Age-adjusted YPLL for PR

obs<- matrix(merged$ypll, dimnames = list(c(merged$ageRange),c("Arecibo","Bayamon",
                          "Caguas","Fajardo","Mayaguez","Metro","Ponce")))

tar <- matrix(merged$pop_est, dimnames = list(c(merged$ageRange),c("Arecibo","Bayamon",
                              "Caguas","Fajardo","Mayaguez","Metro","Ponce")))
                              
std <- matrix(merged$st_pop, dimnames = list(c(merged$ageRange),c("Arecibo","Bayamon",
                            "Caguas","Fajardo","Mayaguez","Metro","Ponce")))

adjusted_ypll_pr <- epi.directadj(obs, tar, std)

# YPLL graphs by region and date
ypll <- deaths %>%
  mutate(ypll = 75 - (1 + age_end + age_start)/2) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(region, date) %>%
  summarize(ypll = sum(ypll), .groups = "drop")

arecibo_ypll<- filter(ypll, region == "Arecibo")
ggplot(arecibo_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() + 
  ggtitle("Cumulative YPLL \n Arecibo Region")

bayamon_ypll<- filter(ypll, region == "Bayamon")
ggplot(bayamon_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Bayamon Region")

metro_ypll<- filter(ypll, region == "Metro")
ggplot(metro_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Metro Region")

caguas_ypll<- filter(ypll, region == "Caguas")
ggplot(caguas_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Caguas Region")

fajardo_ypll<- filter(ypll, region == "Fajardo")
ggplot(fajardo_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Fajardo Region")

mayaguez_ypll<- filter(ypll, region == "Mayaguez")
ggplot(mayaguez_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Mayagüez Region")

ponce_ypll<- filter(ypll, region == "Ponce")
ggplot(ponce_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Ponce Region")
  