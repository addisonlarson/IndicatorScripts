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
sexEmp05 <- read.csv(here("data", "acs", lEmp[1])) %>%
  select(GEOID = GEO.id2,
         totMl = HC01_EST_VC22,
         totFm = HC01_EST_VC23,
         labMl = HC02_EST_VC22,
         labFm = HC02_EST_VC23) %>%
  mutate(year = 5)
sexEmp06_09 <- NULL
for (i in 2:5){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totMl = HC01_EST_VC23,
           totFm = HC01_EST_VC24,
           labMl = HC02_EST_VC23,
           labFm = HC02_EST_VC24) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6)))
  sexEmp06_09 <- rbind(sexEmp06_09, temp)
}
sexEmp10_14 <- NULL
for (i in 6:10){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totMl = HC01_EST_VC26,
           totFm = HC01_EST_VC27,
           labMl = HC02_EST_VC26,
           labFm = HC02_EST_VC27) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6)))
  sexEmp10_14 <- rbind(sexEmp10_14, temp)
}
sexEmp15_17 <- NULL
for (i in 11:13){
  temp <- read.csv(here("data", "acs", lEmp[i])) %>%
    select(GEOID = GEO.id2,
           totMl = HC01_EST_VC28,
           totFm = HC01_EST_VC29,
           labMl = HC02_EST_VC28,
           labFm = HC02_EST_VC29) %>%
    mutate(year = as.numeric(substr(lEmp[i], 5, 6)))
  sexEmp15_17 <- rbind(sexEmp15_17, temp)
}
sexEmp <- bind_rows(sexEmp05, sexEmp06_09) %>%
  bind_rows(., sexEmp10_14) %>%
  bind_rows(., sexEmp15_17) %>%
  filter(GEOID %in% qct)
# append suburban county averages and regional averages
burbs <-  rbind(filter(sexEmp, GEOID %in% c(42017, 42029, 42045, 42091)) %>%
                  group_by(year) %>%
                  summarize(GEOID = "PA Suburban Counties",
                            sumMl = sum(totMl),
                            sumFm = sum(totFm),
                            labMl = weighted.mean(labMl, totMl),
                            labFm = weighted.mean(labFm, totFm)),
                filter(sexEmp, GEOID <= 34021) %>%
                  group_by(year) %>%
                  summarize(GEOID = "NJ Suburban Counties",
                            sumMl = sum(totMl),
                            sumFm = sum(totFm),
                            labMl = weighted.mean(labMl, totMl),
                            labFm = weighted.mean(labFm, totFm)))
region <- sexEmp %>%
  group_by(year) %>%
  summarize(GEOID = "Region",
            sumMl = sum(totMl),
            sumFm = sum(totFm),
            labMl = weighted.mean(labMl, totMl),
            labFm = weighted.mean(labFm, totFm)) %>%
  bind_rows(., burbs) %>%
  rename(totMl = sumMl,
         totFm = sumFm)
sexEmp %<>% mutate_at(c("GEOID"), as.character) %>%
  bind_rows(., region) %>%
  mutate(difference = labMl - labFm) %>%
  mutate(yearTxt = recode(year,"0=2000;5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;
                          12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")) %>%
  select(-year) %>%
  rename(year = yearTxt) %>%
  mutate_at(c("GEOID"), as.factor) %>%
  mutate(geography = recode(GEOID,"34005='Burlington';34007='Camden';34015='Gloucester';34021='Mercer';
                            42017='Bucks';42029='Chester';42045='Delaware';42091='Montgomery';42101='Philadelphia'")) %>%
  select(geography, year, labMl, labFm, difference) %>%
  mutate_if(is.numeric, ~round(., 1))
write.csv(sexEmp, here("outputs", "labForceBySex.csv"), row.names = FALSE)
# transform data for web applications
wide1 <- spread(sexEmp, geography, labMl) %>%
  select(-labFm, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_labMl", .)) %>%
  mutate(year = c(2005:2017))
wide2 <- spread(sexEmp, geography, labFm) %>%
  select(-labMl, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_labFm", .)) %>%
  mutate(year = c(2005:2017))
wide3 <- spread(sexEmp, geography, difference) %>%
  select(-labFm, -labMl) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_difference", .)) %>%
  mutate(year = c(2005:2017))
res <- left_join(wide1, wide2, by = "year") %>%
  left_join(., wide3, by = "year")
write.csv(res, here("outputs", "labForceBySex_wide.csv"), row.names = FALSE)
