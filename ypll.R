# adapted from https://github.com/humberto-ortiz/pr-ypll and https://github.com/rafalab/pr-covid

library("tidyverse")
library("lubridate")
library("scales")
library("epiR")

json_file <- "https://bioportal.salud.pr.gov/api/administration/reports/deaths/summary"

#cases by region
json_file1 <- "https://bioportal.salud.pr.gov/api/administration/reports/cases/dashboard-region"
url1 <- jsonlite::fromJSON(json_file1)

#cases by age-group and sex
json_file2 <- "https://bioportal.salud.pr.gov/api/administration/reports/cases/dashboard-age-group"
url2 <- jsonlite::fromJSON(json_file2)

#code that does not work:
#json_file2 <- "https://bioportal.salud.pr.gov/api/administration/reports/minimal-info-unique-tests"
#url3 <- jsonlite::fromJSON(json_file3)

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
  mutate(st_pop = lapply(ageRange, st_pop_function)) %>%
  subset(ageRange != "105 to 109")

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
    result <- "85 to 89"
  }
  return(result)
}

pop_est_by_region <- pop_est_df %>% select(-c(NAME, YEAR, MUNICIPIO)) %>% 
  pivot_longer(-Region, names_to = "age_group", values_to = "pop_est") %>% 
  mutate(ageRange = lapply(age_group, agegrp_function)) %>% 
  group_by(Region, ageRange,.add = T) %>%
  rename(region = Region) %>% summarize(pop_est = sum(pop_est), .groups = "keep") 

#merging
merged <- merge(ypll, pop_est_by_region, by=c("region","ageRange"))

#check which age ranges do not exist, by region
ypll %>% count(region,ageRange=="0 to 4") 
ypll %>% count(region,ageRange=="5 to 9")
ypll%>% count(region,ageRange=="10 to 14")
ypll %>% count(region,ageRange=="15 to 19")
ypll %>% count(region,ageRange=="20 to 24")
ypll %>% count(region,ageRange=="25 to 29")
ypll %>% count(region,ageRange=="30 to 34")
ypll %>% count(region,ageRange=="35 to 39")
ypll %>% count(region,ageRange=="40 to 44")
ypll %>% count(region,ageRange=="45 to 49")
ypll %>% count(region,ageRange=="50 to 54")
ypll %>% count(region,ageRange=="55 to 59")
ypll %>% count(region,ageRange=="60 to 64")
ypll %>% count(region,ageRange=="65 to 69")
ypll %>% count(region,ageRange=="70 to 74")
ypll %>% count(region,ageRange=="75 to 79")
ypll %>% count(region,ageRange=="80 to 84")
ypll %>% count(region,ageRange=="85 to 89")
ypll %>% count(region,ageRange=="90 to 94")
ypll %>% count(region,ageRange=="95 to 99")
ypll %>% count(region,ageRange=="100 to 104")
ypll %>% count(region,ageRange=="105 to 109")

#add rows where age ranges do not exist
add<-data.frame("Arecibo","0 to 4",0.0,18987,14707)
names(add) <- names(merged)

add1<-data.frame("Fajardo","0 to 4",0.0,18987,4172)
names(add1) <- names(merged)

add2<-data.frame("Caguas","0 to 4",0.0,18987,18970)
names(add2) <- names(merged)

add3<-data.frame("Mayaguez","0 to 4",0.0,18987,15795)
names(add3) <- names(merged)

add4<-data.frame("Metro","0 to 4",0.0,18987,24646)
names(add4) <- names(merged)

add5<-data.frame("Ponce","0 to 4",0.0,18987,18761)
names(add5) <- names(merged)

add6<-data.frame("Arecibo","5 to 9",0.0,19920,19691)
names(add6) <- names(merged)

add7<-data.frame("Bayamon","5 to 9",0.0,19920,27551)
names(add7) <- names(merged)

add8<-data.frame("Caguas","5 to 9",0.0,19920,26167)
names(add8) <- names(merged)

add9<-data.frame("Mayaguez","5 to 9",0.0,19920,21573)
names(add9) <- names(merged)

add10<-data.frame("Metro","5 to 9",0.0,19920,31585)
names(add10) <- names(merged)

add11<-data.frame("Arecibo","10 to 14",0.0,20057,22406)
names(add11) <- names(merged)

add12<-data.frame("Fajardo","10 to 14",0.0,20057,6556)
names(add12) <- names(merged)

add13<-data.frame("Mayaguez","10 to 14",0.0,20057,25988)
names(add13) <- names(merged)

add14<-data.frame("Metro","10 to 14",0.0,20057,36179)
names(add14) <- names(merged)

add15<-data.frame("Ponce","10 to 14",0.0,20057,29066)
names(add15) <- names(merged)

add16<-data.frame("Arecibo","15 to 19",0.0,19820,24590)
names(add16) <- names(merged)

add17<-data.frame("Bayamon","15 to 19",0.0,19820,34412)
names(add17) <- names(merged)

add18<-data.frame("Caguas","15 to 19",0.0,19820,33926)
names(add18) <- names(merged)

add19<-data.frame("Mayaguez","15 to 19",0.0,19820,29367)
names(add19) <- names(merged)

add20<-data.frame("Caguas","20 to 24",0.0,18257,34814)
names(add20) <- names(merged)

add22<-data.frame("Ponce","20 to 24",0.0,18257,31836)
names(add22) <- names(merged)

add23<-data.frame("Fajardo","30 to 34",0.0,19511,6529)
names(add23) <- names(merged)

merged <- merged %>% rbind(add,add1,add2,add3,add4,add5,
                           add6,add7,add8,add9,add10,add11,add12,add13,add14,add15,
                           add16,add17,add18,add19,add20,add22,add23)
# put age ranges in order
merged2<- merged %>% arrange(factor(ageRange, levels = ages),region)

#convert to matrix
ages2<-c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
         "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
         "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89")

obs <- matrix(merged2$ypll, nrow=7, dimnames = list(c("Arecibo","Bayamon",
                                                      "Caguas","Fajardo","Mayaguez","Metro","Ponce"),ages2))

tar <- matrix(merged2$pop_est, nrow=7, dimnames = list(c("Arecibo","Bayamon",
                                                         "Caguas","Fajardo","Mayaguez","Metro","Ponce"),ages2))

#eliminate regions for standard pop (picking one region since st_pop is the same for all)
merged_std <- merged2 %>% filter(region=="Arecibo")
std <- matrix(merged_std$st_pop, nrow=1,dimnames = list(c("region"),ages2))

std2<-as.matrix(unlist(std))
rownames(std2) <- ages2
colnames(std2) <- "region"

#Age-adjusted YPLL by region of PR
adjusted_ypll_pr <- epi.directadj(obs, tar, std2)

# YPLL graphs by region and date 
ypll <- deaths %>%
  mutate(ypll = 75 - (1 + age_end + age_start)/2) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(region, date) %>%
  summarize(ypll = sum(ypll), .groups = "drop")

arecibo_ypll<- filter(merged, region == "Arecibo")
ggplot(arecibo_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() + 
  ggtitle("Cumulative YPLL \n Arecibo Region")

bayamon_ypll<- filter(merged, region == "Bayamon")
ggplot(bayamon_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Bayamon Region")

metro_ypll<- filter(merged, region == "Metro")
ggplot(metro_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Metro Region")

caguas_ypll<- filter(merged, region == "Caguas")
ggplot(caguas_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Caguas Region")

fajardo_ypll<- filter(merged, region == "Fajardo")
ggplot(fajardo_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Fajardo Region")

mayaguez_ypll<- filter(merged, region == "Mayaguez")
ggplot(mayaguez_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Mayagüez Region")

ponce_ypll<- filter(merged, region == "Ponce")
ggplot(ponce_ypll, aes(x=date, y=cumsum(ypll))) + geom_line() +
  ggtitle("Cumulative YPLL \n Ponce Region")

#-------------------------------                    ------------------------------
#YPLL for Hispanics

json_fileUS <-"https://data.cdc.gov/resource/ks3g-spdg.json"

agegrp_function2 <- function(x) {
  if (x == "Under 1 year") {
    result <- "0"
  } else if (x == "1-4 years") {
    result <- "1 to 4"
  } else if (x == "5-14 years") {
    result <- "5 to 14"
  } else if (x == "15-24 years") {
    result <- "15 to 24"
  } else if (x == "25-34 years") {
    result <- "25 to 34"
  } else if (x == "35-44 years") {
    result <- "35 to 44"
  } else if (x == "45-54 years") {
    result <- "45 to 54"
  } else if (x == "55-64 years") {
    result <- "55 to 64"
  } else if (x == "65-74 years") {
    result <- "65 to 74"
  } else if (x == "75-84 years") {
    result <- "75 to 84"
  } else {
    result <- "85"
  }
  return(result)
}

deathsUS <- jsonlite::fromJSON(json_fileUS) %>% 
  filter(state == "United States",race_and_hispanic_origin == "Hispanic") %>% 
  filter(age_group_new !="All Ages",age_group_new !="0-17 years",
         age_group_new !="18-29 years",age_group_new !="30-49 years",age_group_new !="50-64 years") %>%
  mutate(ageRange = lapply(age_group_new, agegrp_function2)) %>% 
  select(ageRange, covid_19_deaths) %>% 
  mutate(covid_19_deaths=sapply(covid_19_deaths, function(x) as.numeric(as.character(x))))

#adding 0 to 4 age range
to_4 <- deathsUS %>% filter(ageRange=="0" | ageRange=="1 to 4")

zero_to_4 <- data.frame("0 to 4", with(to_4,sum(covid_19_deaths)))
names(zero_to_4) <- names(deathsUS)

ages3<-c("0 to 4","5 to 14","15 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 to 84","85")

ypll_us <- deathsUS %>% rbind(zero_to_4) 

#adding standard pop
st_pop_function2 <- function(x) {
  if (x == "0 to 4") {
    result <- 18987
  } else if (x == "5 to 14") {
    result <- 39977
  } else if (x == "15 to 24") {
    result <- 38077
  } else if (x == "25 to 34") {
    result <- 37233
  } else if (x == "35 to 44") {
    result <- 44659
  } else if (x == "45 to 54") {
    result <- 37030
  } else if (x == "55 to 64") {
    result <- 23961
  } else if (x == "65 to 74") {
    result <- 18136
  } else if (x == "75 to 84") {
    result <- 12315
  } else {
    result <- 4259
  }
  return(result)
}

#pop_estimates for Hispanics in the US
pop_est_hispanics <- function(x) {
  if (x == "0 to 4") {
    result <- 5094211
  } else if (x == "5 to 14") {
    result <- 10567586
  } else if (x == "15 to 24") {
    result <- 9884590
  } else if (x == "25 to 34") {
    result <- 9522263
  } else if (x == "35 to 44") {
    result <- 8656485
  } else if (x == "45 to 54") {
    result <- 7128208
  } else if (x == "55 to 64") {
    result <- 5076928
  } else if (x == "65 to 74") {
    result <- 2822119
  } else if (x == "75 to 84") {
    result <- 1307751
  } else {
    result <- 509096
  }
  return(result)
}

ypll_us<-ypll_us %>% filter(ageRange!="0",ageRange!="1 to 4") %>%
  arrange(factor(ageRange, levels = ages3)) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")), 
         age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>%
  mutate(ypll = covid_19_deaths*(75 - (1 + age_end + age_start)/2)) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(ageRange) %>%
  summarize(ypll = sum(ypll), .groups = "drop") %>%
  mutate(st_pop = lapply(ageRange, st_pop_function2)) %>%
  mutate(pop_est = lapply(ageRange, pop_est_hispanics))


#Adjusting PR YPLL to fit ageRanges
pop_est_total_pr <- pop_est_by_region %>% ungroup() %>% group_by(ageRange,.add = T) %>% summarize(pop_est = sum(pop_est), .groups = "drop") 

ypll_total_pr <- merged2 %>% ungroup() %>% group_by(ageRange,.add = T) %>% summarize(ypll = sum(ypll), .groups = "drop") 

ypll_pr <- merge(ypll_total_pr, pop_est_total_pr, by="ageRange") 

#(add after changing ageRanges)
#%>% mutate(st_pop = lapply(ageRange, st_pop_function2))

