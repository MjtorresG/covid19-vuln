# adapted from https://github.com/humberto-ortiz/pr-ypll and https://github.com/rafalab/pr-covid

library("tidyverse")
library("lubridate")
library("scales")
library("epiR")
library("gtsummary")
library("openxlsx")
library("readxl")


#ypll2020PR <- jsonlite::fromJSON(json_file) %>%
#mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")),
#age_end = as.numeric(str_extract(ageRange, "\\d+$"))) %>% mutate(yr=year(deathDate)) %>% filter(yr==2020) %>% select(-c(reportDate,yr))

#write.xlsx(ypll2020PR, file = "PRypll2020deaths.xlsx")
PRdeaths <- read.xlsx("data/PRypll2020deaths.xlsx")

ages <-c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", 
         "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", 
         "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", 
         "90 to 94", "95 to 99","100 to 104","105 to 109")

#adding standard pop

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

st_pop_function2 <- function(x) {
  if (x == "0 to 29") {
    result <- 114763
  } else if (x == "30 to 39") {
    result <- 41691
  } else if (x == "40 to 49") {
    result <- 42285
  } else if (x == "50 to 64") {
    result <- 41185
  } else if (x == "65 to 74") {
    result <- 18136
  } else {
    result <- 0
  }
  return(result)
}

#popy<-read_csv("data/NC-EST2020-ALLDATA-R-File22.csv") %>% filter(MONTH==7) %>%
   #select(c(AGE, H_MALE, H_FEMALE)) %>% mutate(hispanics=H_MALE+H_FEMALE) %>% select(-c(H_MALE,H_FEMALE)) %>% filter(AGE!=999,AGE<=74)
#write.xlsx(popy,file=("data/pop_estUS.xlsx"))

popUS <-read.xlsx("data/pop_estUS.xlsx")

pop_est_hispanics <- function(x) {
  if (x == "0 to 29") {
    result <- 30516828
  } else if (x == "30 to 39") {
    result <- 9178580
  } else if (x == "40 to 49") {
    result <- 8091692
  } else if (x == "50 to 64") {
    result <- 8643959
  } else if (x == "65 to 74") {
    result <- 2978586
  } else {
    result <- 0
  }
  return(result)
}
# PR population estimates
pop_estimates <- read_csv("data/PRM-EST2020-AGESEX.csv")

pop_est_df <- pop_estimates %>% select(YEAR, MUNICIPIO, NAME, AGE04_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, 
                                       AGE2529_TOT, AGE3034_TOT, AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT,
                                       AGE5559_TOT, AGE6064_TOT, AGE6569_TOT, AGE7074_TOT) %>% filter(YEAR == 14) %>% mutate(Region = case_when(MUNICIPIO %in% c("013","017","027","039",
                                       "054","065","081","091","101","115","141","145")~"Arecibo",
                                       MUNICIPIO %in% c("019","021","033","047","051","105","107","135","137","143","045")~"Bayamon",
                                       MUNICIPIO %in% c("029","031","061","087","127","139")~"Metro",
                                       MUNICIPIO %in% c("007","009","025","035","041","063","069","077","085","095","103","129","151")~"Caguas",
                                       MUNICIPIO %in% c("037","049","053","089","119","147")~"Fajardo",
                                       MUNICIPIO %in% c("003","005","011","023","067","071","079","083","093","097","099","117","121","125","131")~"Mayaguez",
                                       MUNICIPIO %in% c("001","015","055","057","059","073","075","043","109","111","113","123","133","149","153")~"Ponce"))

# merging pop_est_by_region and ypll

ypll <- PRdeaths %>%
  mutate(ypll = 75 - (1 + age_end + age_start)/2) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  filter(age_end<=74) %>%
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
    result <- "85 to 89"
  }
  return(result)
}

pop_est_by_region <- pop_est_df %>% select(-c(NAME, YEAR, MUNICIPIO)) %>% 
  pivot_longer(-Region, names_to = "age_group", values_to = "pop_est") %>% 
  mutate(ageRange = lapply(age_group, agegrp_function)) %>% 
  group_by(Region, ageRange,.add = T) %>%
  rename(region = Region) %>% summarize(pop_est = sum(pop_est), .groups = "keep")


#adding US ypll
USdeaths<-read.xlsx("data/hisp_deaths2020.xlsx") %>% mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")),
                                                  age_end = as.numeric(str_extract(ageRange, "\\d+$")))

ages3<-c("0 to 29","30 to 39","40 to 49","50 to 64","65 to 74")

ypll_us <- USdeaths %>% arrange(factor(ageRange, levels = ages3)) %>%
  mutate(ypll = deaths*(75 - (1 + age_end + age_start)/2)) %>%
  mutate(ypll = if_else(ypll < 0, 0, ypll)) %>%
  group_by(ageRange) %>%
  summarize(ypll = sum(ypll), .groups = "drop") %>%
  mutate(st_pop = lapply(ageRange, st_pop_function2),
         pop_est = lapply(ageRange, pop_est_hispanics),
         population = rep("US",times=5)) %>%
  select(-ageRange)

#adding total PR YPLL and adjusting ageRanges
pop_est_by_age <- pop_est_by_region %>% ungroup() %>% group_by(ageRange) %>% summarize(pop_est=sum(pop_est))

pop_est_pr <- function(x) {
  if (x == "0 to 29") {
    result <- 1056917
  } else if (x == "30 to 39") {
    result <- 372182
  } else if (x == "40 to 49") {
    result <- 397547
  } else if (x == "50 to 64") {
    result <- 635880
  } else if (x == "65 to 74") {
    result <- 369181
  } else {
    result <- 0
  }
  return(result)
}

#merging
merged <- merge(ypll, pop_est_by_region, by=c("region","ageRange"))
merged2<- merged %>% arrange(factor(ageRange, levels = ages))

ypll_total_pr <- merged2 %>% ungroup() %>% group_by(ageRange,.add = T) %>% summarize(ypll = sum(ypll), .groups = "drop") 

#adjusting age groups on excel
#write.xlsx(ypll_total_pr, file="ypllPRage_groups.xlsx")

ypll_pr <- read.xlsx("data/ypllPRage_groups.xlsx") %>%
  group_by(ageRange) %>%
  summarize(ypll = sum(ypll)) %>%
  mutate(st_pop = lapply(ageRange, st_pop_function2),
         pop_est = lapply(ageRange, pop_est_pr),
         population = rep("PR",times=5)) %>%
  arrange(factor(ageRange, levels = ages3))

ypll_pr2 <- ypll_pr %>% select(-ageRange)

#merging PR and US Hispanics
mergedPR_US<- bind_rows(ypll_pr2,ypll_us)

#adding ageRange again
ageRange <- as.matrix(rep(ages3,times=2))

mergedPR_US1 <- mergedPR_US %>% mutate(ageRange)

# make age ranges repeat by population
mergedPR_US2<- mergedPR_US1 %>% arrange(factor(ageRange, levels = ages3),population)

#convert to matrix
obs <- matrix(mergedPR_US2$ypll, nrow=2,dimnames = list(c("PR","US"),ages3))

tar <- matrix(unlist(mergedPR_US2$pop_est), nrow=2, dimnames = list(c("PR","US"),ages3))

#picking one population since st_pop is the same for all
mergedPR_US_std <- mergedPR_US2 %>% filter(population=="PR")
std <- matrix(mergedPR_US_std$st_pop, nrow=1,dimnames = list(c("population"),ages3))

std2<-as.matrix(unlist(std))
rownames(std2) <- ages3
colnames(std2) <- "population"

# Age-adjusted YPLL of PR vs Hispanics in the US
adjusted_ypll <- epi.directadj(obs, tar, std2,units=100000)

#Comparing
adjusted_ypll1 <- as.data.frame(adjusted_ypll) %>% select(crude.strata,crude.cov,crude.est,adj.strata.strata, adj.strata.est, adj.strata.lower, adj.strata.upper)

adjusted_ypll2 <- adjusted_ypll1 %>% select(adj.strata.strata, adj.strata.est, adj.strata.lower, adj.strata.upper) %>%
  slice(1:2)
