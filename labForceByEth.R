# Labor force participation rate by ethnicity
library(here); library(dplyr); library(tidyr)
library(magrittr); library(ggplot2); library(car)
l <- list.files(here("data", "acs"))
# Qualifying counties
qct <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
# Employment files
lEmp <- subset(l, substr(l, 12, 16) == "S2301")
# ALAS, 4 data structures from 2005-2017
# This section uploads data sets in chunks
ethEmp05 <- read.csv(here("data", "acs", lEmp[1])) %>%
  select(GEOID = GEO.id2,
         totHisp = HC01_EST_VC18,
         totWht = HC01_EST_VC19,
         labHisp = HC02_EST_VC18,
         labWht = HC02_EST_VC19) %>%
  mutate_all(funs(replace(., .== "N", NA))) %>%
  mutate(year = 5)
ethEmp06_09 <- NULL
for (i in 2:5){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totHisp = HC01_EST_VC19,
           totWht = HC01_EST_VC20,
           labHisp = HC02_EST_VC19,
           labWht = HC02_EST_VC20) %>%
    mutate_all(funs(replace(., .== "N", NA))) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6)))
  ethEmp06_09 <- rbind(ethEmp06_09, temp)
}
ethEmp10_14 <- NULL
for (i in 6:10){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totHisp = HC01_EST_VC21,
           totWht = HC01_EST_VC22,
           labHisp = HC02_EST_VC21,
           labWht = HC02_EST_VC22) %>%
    mutate_all(funs(replace(., .== "N", NA))) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6)))
  ethEmp10_14 <- rbind(ethEmp10_14, temp)
}
ethEmp15_17 <- NULL
for (i in 11:13){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totHisp = HC01_EST_VC23,
           totWht = HC01_EST_VC24,
           labHisp = HC02_EST_VC23,
           labWht = HC02_EST_VC24) %>%
    mutate_all(funs(replace(., .== "N", NA))) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6)))
  ethEmp15_17 <- rbind(ethEmp15_17, temp)
}
ethEmp <- bind_rows(ethEmp05, ethEmp06_09) %>%
  bind_rows(., ethEmp10_14) %>%
  bind_rows(., ethEmp15_17) %>%
  filter(GEOID %in% qct) %>%
  mutate_at(c("totHisp", "labHisp"), as.numeric)
# append suburban county averages and regional averages
burbs <-  rbind(filter(ethEmp, GEOID %in% c(42017, 42029, 42045, 42091)) %>%
                  group_by(year) %>%
                  summarize(GEOID = "PA Suburban Counties",
                            sumHisp = sum(totHisp),
                            sumWht = sum(totWht),
                            labHisp = weighted.mean(labHisp, totHisp),
                            labWht = weighted.mean(labWht, totWht)),
                filter(ethEmp, GEOID <= 34021) %>%
                  group_by(year) %>%
                  summarize(GEOID = "NJ Suburban Counties",
                            sumHisp = sum(totHisp),
                            sumWht = sum(totWht),
                            labHisp = weighted.mean(labHisp, totHisp),
                            labWht = weighted.mean(labWht, totWht)))
region <- ethEmp %>%
  group_by(year) %>%
  summarize(GEOID = "Region",
            sumHisp = sum(totHisp, na.rm = TRUE),
            sumWht = sum(totWht, na.rm = TRUE),
            labHisp = weighted.mean(labHisp, totHisp, na.rm = TRUE),
            labWht = weighted.mean(labWht, totWht, na.rm = TRUE)) %>%
  bind_rows(., burbs) %>%
  rename(totHisp = sumHisp,
         totWht = sumWht)
ethEmp %<>% mutate_at(c("GEOID"), as.character) %>%
  bind_rows(., region) %>%
  mutate(difference = labWht - labHisp) %>%
  mutate(yearTxt = recode(year,"0=2000;5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;
                          12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")) %>%
  select(-year) %>%
  rename(year = yearTxt) %>%
  mutate_at(c("GEOID"), as.factor) %>%
  mutate(geography = recode(GEOID,"34005='Burlington';34007='Camden';34015='Gloucester';34021='Mercer';
                            42017='Bucks';42029='Chester';42045='Delaware';42091='Montgomery';42101='Philadelphia'")) %>%
  select(geography, year, labHisp, labWht, difference) %>%
  mutate_if(is.numeric, ~round(., 1))
write.csv(ethEmp, here("outputs", "labForceByEth.csv"), row.names = FALSE)
# transform data for web applications
wide1 <- spread(ethEmp, geography, labHisp) %>%
  select(-labWht, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_labHisp", .)) %>%
  mutate(year = c(2005:2017))
is.na(wide1) <- sapply(wide1, is.infinite)
wide2 <- spread(ethEmp, geography, labWht) %>%
  select(-labHisp, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_labWht", .)) %>%
  mutate(year = c(2005:2017))
is.na(wide2) <- sapply(wide2, is.infinite)
wide3 <- spread(ethEmp, geography, difference) %>%
  select(-labWht, -labHisp) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_difference", .)) %>%
  mutate(year = c(2005:2017))
is.na(wide3) <- sapply(wide3, is.infinite)

res <- left_join(wide1, wide2, by = "year") %>%
  left_join(., wide3, by = "year")
write.csv(res, here("outputs", "labForceByEth_wide.csv"), row.names = FALSE)
