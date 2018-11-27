# Census data on race and ethnicity have changed over time.
# A = White alone, B = Black alone, C = American Indian alone,
# D = Asian alone, E = Native Hawaiian alone, F = Other,
# G = Two or more, H = White alone, not Hispanic or Latino, I = Hispanic or Latino

# FOR Decennial 2000, I is recoded to H

# ACS data by year and categories available
#       cat
# year  A B D F G H I
# 5     9 9 8 5 0 9 0
# 6     9 9 8 6 4 9 0
# 7     9 9 9 6 5 9 0
# 8     9 9 8 4 6 9 0
# 9     9 9 8 4 5 9 0
# 10    9 9 8 3 7 9 0
# 11    9 9 9 3 8 9 0
# 12    9 9 9 4 7 9 0
# 13    9 9 9 5 8 9 0
# 14    9 9 9 5 8 9 0
# 15    9 9 9 5 7 9 0
# 16    9 9 9 6 7 9 0
# 17    9 9 9 5 8 9 9

library(here); library(dplyr); library(tidyr)
library(magrittr); library(ggplot2); library(car)
l <- list.files(here("data", "acs"))
# Qualifying counties
qct <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
# Median income files
lMed <- subset(l, substr(l, 12, 17) == "B19013")
medInc <- data.frame()
# Population by race / eth. files
lRac <- subset(l, substr(l, 12, 17) == "B01001")
racEth <- data.frame()
# CPI
inf <- read.csv(here("data", "bls", "inflation.csv"))
colnames(inf) <- c("year", "cpi")
inf$year <- as.numeric(substr(inf$year, 3, 4))

for (i in 1:length(lMed)){
  temp <- read.csv(here("data", "acs", lMed[i]))
  temp <- temp[-1,]
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lMed[i], 5, 6))
  temp$cat <- substr(lMed[i], 18, 18)
  medInc <- rbind(medInc, temp)
}
colnames(medInc)[4:5] <- c("medInc_E", "medInc_M")

for (i in 1:length(lRac)){
  temp <- read.csv(here("data", "acs", lRac[i]))
  temp <- temp[-1,]
  temp <- temp[c(2, 4)]
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lRac[i], 5, 6))
  temp$cat <- substr(lRac[i], 18, 18)
  racEth <- rbind(racEth, temp)
}
colnames(racEth)[2] <- "obs"
racEth$obs <- as.numeric(as.character(racEth$obs))

# add in Decennial
l <- list.files(here("data", "decennial"))
lMed <- subset(l, substr(l, 12, 15) == "P152")
add <- data.frame()
for (i in 1:length(lMed)){
  temp <- read.csv(here("data", "decennial", lMed[i]))
  temp <- temp[-1,]
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lMed[i], 5, 6))
  temp$cat <- substr(lMed[i], 16, 16)
  temp$medInc_M <- as.factor(0)
  add <- rbind(add, temp)
}
colnames(add)[4] <- "medInc_E"
add$cat <- ifelse(add$cat == "I", "H", add$cat)
medInc <- rbind(medInc, add)

racEth00 <- read.csv(here("data", "decennial", "DEC_00_SF1_H009_with_ann.csv")); racEth00 <- racEth00[-1,]
racEth00 %<>%
  filter(GEO.id2 %in% qct) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_at(c(4:18), as.numeric) %>%
  mutate_at(c(2), as.factor) %>%
  mutate(H = VD03,
         A = VD01 + VD10,
         B = VD04 + VD11,
         C = VD05 + VD12,
         D = VD06 + VD13,
         E = VD07 + VD14,
         F = VD08 + VD15,
         year = 0) %>%
  gather(cat, obs, A,B,C,D,E,F,H) %>%
  select(GEO.id2, obs, year, cat)
racEth <- rbind(racEth, racEth00)

racEth$GEO.id2 <- as.character(racEth$GEO.id2)
medInc$GEO.id2 <- as.character(medInc$GEO.id2)

medInc %<>% left_join(racEth, by = c("GEO.id2", "year", "cat")) %>%
  left_join(inf, by = c("year")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_at(c(2, 4, 5), as.numeric) %>%
  na.omit()

# Adjust for inflation
medInc$medInc_A <- medInc$medInc_E * inf[18,2] / medInc$cpi

# Separate white non-Hispanic from others
t1 <- subset(medInc, cat != "A" & cat != "I")
t1$flag <- ifelse(t1$cat == "H", "White Non-Hispanic", "Minority")
t1L <- split(t1, t1$flag)

t2 <- t1L[[1]] %>%
  group_by(year, GEO.id2) %>%
  summarize(minorityInc = weighted.mean(medInc_A, obs),
            minorityPop = sum(obs))
t3 <- t1L[[2]] %>%
  group_by(year, GEO.id2) %>%
  summarize(whtNonHispInc = weighted.mean(medInc_A, obs),
            whtNonHispPop = sum(obs))
# t2 contains white non-Hispanic vs. minority incomes by county
t2 %<>% left_join(t3, by = c("year", "GEO.id2"))
# append suburban county averages and regional averages
t3 <- rbind(filter(t2, GEO.id2 %in% c(42017, 42029, 42045, 42091)) %>%
  group_by(year) %>%
  summarize(GEO.id2 = "PA Suburban Counties",
            whtNonHispInc = weighted.mean(whtNonHispInc, whtNonHispPop),
            minorityInc = weighted.mean(minorityInc, minorityPop)),
  filter(t2, GEO.id2 <= 34021) %>%
    group_by(year) %>%
    summarize(GEO.id2 = "NJ Suburban Counties",
              whtNonHispInc = weighted.mean(whtNonHispInc, whtNonHispPop),
              minorityInc = weighted.mean(minorityInc, minorityPop)))
t4 <- t2 %>%
  group_by(year) %>%
  summarize(GEO.id2 = "Region",
            whtNonHispInc = weighted.mean(whtNonHispInc, whtNonHispPop),
            minorityInc = weighted.mean(minorityInc, minorityPop))
t2 %<>% mutate_at(c(2), as.character)
final <- bind_rows(t2, t3) %>%
  bind_rows(., t4) %>%
  mutate(difference = whtNonHispInc - minorityInc) %>%
  mutate_if(is.numeric, ~round(., 0))
final$year <- recode(final$year,"0=2000;5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")
colnames(final)[2] <- "geography"; final$geography <- as.factor(final$geography)
final$geography <- recode(final$geography,
                          "34005='Burlington';34007='Camden';34015='Gloucester';34021='Mercer';
                          42017='Bucks';42029='Chester';42045='Delaware';42091='Montgomery';42101='Philadelphia'")
write.csv(final[c(1:3,5,7)], here("outputs", "medIncByRace.csv"), row.names = FALSE)
# sample graph to show Ben
t1 <- filter(final, geography == "Region") %>%
  select(-minorityPop, -whtNonHispPop) %>%
  gather(income, val, minorityInc:whtNonHispInc)
ggplot(data = t1, aes(x = year, y = val)) +
  geom_line(aes(color = income)) +
  labs(title = "Sample graph of income over time",
       y = "Annual median household income, 2017 USD",
       x = "Year")
ggplot(data = t1, aes(x = year, y = difference)) +
  geom_line() +
  labs(title = "Sample graph of income over time",
       y = "Annual median household income, 2017 USD",
       x = "Year")
# transform data for web applications
wide1 <- spread(final, geography, minorityInc) %>%
  select(-minorityPop, -whtNonHispInc, -whtNonHispPop, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_minorityInc", .)) %>%
  mutate(year = c(2000, 2005:2017))
wide2 <- spread(final, geography, whtNonHispInc) %>%
  select(-minorityPop, -minorityInc, -whtNonHispPop, -difference) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_whtNonHispInc", .)) %>%
  mutate(year = c(2000, 2005:2017))
wide3 <- spread(final, geography, difference) %>%
  select(-minorityPop, -minorityInc, -whtNonHispPop, -whtNonHispInc) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_difference", .)) %>%
  mutate(year = c(2000, 2005:2017))
res <- left_join(wide1, wide2, by = "year") %>%
  left_join(., wide3, by = "year")
write.csv(res, here("outputs", "medIncByRace_wide.csv"), row.names = FALSE)
