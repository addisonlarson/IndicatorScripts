# Labor force participation rate by sex
library(here); library(dplyr); library(tidyr)
library(magrittr); library(ggplot2); library(car)
l <- list.files(here("data", "acs"))
# Qualifying counties
qct <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
# Employment files
lEmp <- subset(l, substr(l, 12, 16) == "S2301")
# ALAS, 4 data structures from 2005-2017
# This section uploads data sets in chunks
racEmp05 <- read.csv(here("data", "acs", lEmp[1])) %>%
  select(GEOID = GEO.id2,
         totWht = HC01_EST_VC11,
         totBlk = HC01_EST_VC12,
         totAmind = HC01_EST_VC13, # American Indian / Alaska Native
         totAsn = HC01_EST_VC14,
         totHPI = HC01_EST_VC15, # Native Hawaiian / Other Pacific Islander
         totOth = HC01_EST_VC16,
         totTwo = HC01_EST_VC17,
         labWht = HC02_EST_VC11,
         labBlk = HC02_EST_VC12,
         labAmind = HC02_EST_VC13,
         labAsn = HC02_EST_VC14,
         labHPI = HC02_EST_VC15, # Native Hawaiian / Other Pacific Islander
         labOth = HC02_EST_VC16,
         labTwo = HC02_EST_VC17) %>%
  mutate(year = 5) %>%
  replace(.=="N", NA)
racEmp06_09 <- NULL
for (i in 2:5){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totWht = HC01_EST_VC12,
           totBlk = HC01_EST_VC13,
           totAmind = HC01_EST_VC14,
           totAsn = HC01_EST_VC15,
           totHPI = HC01_EST_VC16,
           totOth = HC01_EST_VC17,
           totTwo = HC01_EST_VC18,
           labWht = HC02_EST_VC12,
           labBlk = HC02_EST_VC13,
           labAmind = HC02_EST_VC14,
           labAsn = HC02_EST_VC15,
           labHPI = HC02_EST_VC16,
           labOth = HC02_EST_VC17,
           labTwo = HC02_EST_VC18) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6))) %>%
    replace(.=="N", NA)
  racEmp06_09 <- rbind(racEmp06_09, temp)
}
racEmp10_14 <- NULL
for (i in 6:10){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totWht = HC01_EST_VC13,
           totBlk = HC01_EST_VC14,
           totAmind = HC01_EST_VC15,
           totAsn = HC01_EST_VC16,
           totHPI = HC01_EST_VC17,
           totOth = HC01_EST_VC18,
           totTwo = HC01_EST_VC19,
           labWht = HC02_EST_VC13,
           labBlk = HC02_EST_VC14,
           labAmind = HC02_EST_VC15,
           labAsn = HC02_EST_VC16,
           labHPI = HC02_EST_VC17,
           labOth = HC02_EST_VC18,
           labTwo = HC02_EST_VC19) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6))) %>%
    replace(.=="N", NA)
  racEmp10_14 <- rbind(racEmp10_14, temp)
}
racEmp15_17 <- NULL
for (i in 11:13){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totWht = HC01_EST_VC15,
           totBlk = HC01_EST_VC16,
           totAmind = HC01_EST_VC17,
           totAsn = HC01_EST_VC18,
           totHPI = HC01_EST_VC19,
           totOth = HC01_EST_VC20,
           totTwo = HC01_EST_VC21,
           labWht = HC02_EST_VC15,
           labBlk = HC02_EST_VC16,
           labAmind = HC02_EST_VC17,
           labAsn = HC02_EST_VC18,
           labHPI = HC02_EST_VC19,
           labOth = HC02_EST_VC20,
           labTwo = HC02_EST_VC21) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6))) %>%
    replace(.=="N", NA)
  racEmp15_17 <- rbind(racEmp15_17, temp)
}
racEmp <- bind_rows(racEmp05, racEmp06_09) %>%
  bind_rows(., racEmp10_14) %>%
  bind_rows(., racEmp15_17) %>%
  filter(GEOID %in% qct) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, as.numeric)
# compute nonwhite labor force participation + append back to df
nonWht <- racEmp %>%
  group_by(GEOID, year) %>%
  replace(is.na(.), 0) %>%
  summarize(totNonWht = sum(totBlk, totAmind, totAsn, totHPI, totOth, totTwo, na.rm = TRUE),
            labNonWht = (totBlk * labBlk + totAmind * labAmind + totAsn * labAsn +
                           totHPI * labHPI + totOth * labOth + totTwo * labTwo) / totNonWht)
is.na(nonWht) <- sapply(nonWht, is.nan)
racEmp %<>% left_join(nonWht, by = c("GEOID", "year")) %>%
  select(GEOID, year, totWht, totNonWht, labWht, labNonWht)
# append suburban county averages and regional averages
burbs <- rbind(filter(racEmp, GEOID %in% c(42017, 42029, 42045, 42091)) %>%
                  group_by(year) %>%
                  summarize(GEOID = "PA Suburban Counties",
                            sumWht = sum(totWht),
                            sumNonWht = sum(totNonWht),
                            labWht = weighted.mean(labWht, totWht),
                            labNonWht = weighted.mean(labNonWht, totNonWht)) %>%
                 rename(totWht = sumWht, totNonWht = sumNonWht),
                filter(racEmp, GEOID <= 34021) %>%
                  group_by(year) %>%
                  summarize(GEOID = "NJ Suburban Counties",
                            sumWht = sum(totWht),
                            sumNonWht = sum(totNonWht),
                            labWht = weighted.mean(labWht, totWht),
                            labNonWht = weighted.mean(labNonWht, totNonWht)) %>%
                 rename(totWht = sumWht, totNonWht = sumNonWht))
region <- racEmp %>%
  group_by(year) %>%
  summarize(GEOID = "Region",
            sumWht = sum(totWht),
            sumNonWht = sum(totNonWht),
            labWht = weighted.mean(labWht, totWht),
            labNonWht = weighted.mean(labNonWht, totNonWht)) %>%
  rename(totWht = sumWht, totNonWht = sumNonWht) %>%
  bind_rows(., burbs)
racEmp %<>% mutate_at(c("GEOID"), as.character) %>%
  bind_rows(., region) %>%
  mutate(difference = labWht - labNonWht) %>%
  mutate(yearTxt = recode(year,"0=2000;5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;
                          12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")) %>%
  select(-year) %>%
  rename(year = yearTxt) %>%
  mutate_at(c("GEOID"), as.factor) %>%
  mutate(geography = recode(GEOID,"34005='Burlington';34007='Camden';34015='Gloucester';34021='Mercer';
                            42017='Bucks';42029='Chester';42045='Delaware';42091='Montgomery';42101='Philadelphia'")) %>%
  select(geography, year, labWht, labNonWht, difference) %>%
  mutate_if(is.numeric, ~round(., 1))
write.csv(racEmp, here("outputs", "labForceByRace.csv"), row.names = FALSE)
# transform data for web applications
wide1 <- spread(racEmp, geography, labWht) %>%
  select(-labNonWht, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_labWht", .)) %>%
  mutate(year = c(2005:2017))
is.na(wide1) <- sapply(wide1, is.infinite)
wide2 <- spread(racEmp, geography, labNonWht) %>%
  select(-labWht, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_labNonWht", .)) %>%
  mutate(year = c(2005:2017))
is.na(wide2) <- sapply(wide2, is.infinite)
wide3 <- spread(racEmp, geography, difference) %>%
  select(-labNonWht, -labWht) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_difference", .)) %>%
  mutate(year = c(2005:2017))
is.na(wide3) <- sapply(wide3, is.infinite)
res <- left_join(wide1, wide2, by = "year") %>%
  left_join(., wide3, by = "year")
write.csv(res, here("outputs", "labForceByRace_wide.csv"), row.names = FALSE)
