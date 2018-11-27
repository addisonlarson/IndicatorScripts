# check out population sizes and MOEs
library(here); library(dplyr); library(tidyr)
library(magrittr); library(ggplot2); library(car)
l <- list.files(here("data", "acs"))
# Qualifying counties (here, just Gloucester and Chester)
# qct <- c("34015", "42029")
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
  temp <- temp[c(2,4)]
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lRac[i], 5, 6))
  temp$cat <- substr(lRac[i], 18, 18)
  racEth <- rbind(racEth, temp)
}
colnames(racEth)[2] <- "obs"
racEth$obs <- as.numeric(as.character(racEth$obs))

racEth$GEO.id2 <- as.character(racEth$GEO.id2)
medInc$GEO.id2 <- as.character(medInc$GEO.id2)

# at this point, subset for Gloucester and Mercer. Adding Philly for comparison
medInc <- subset(medInc, GEO.id2 %in% c("34015", "42029", "42101"))

medInc %<>% left_join(racEth, by = c("GEO.id2", "year", "cat")) %>%
  left_join(inf, by = c("year")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_at(c(2, 4, 5), as.numeric) %>%
  na.omit() %>%
  mutate(medInc_A = medInc_E * inf[18,2] / cpi)

# Separate white non-Hispanic from others
t1 <- subset(medInc, cat != "A" & cat != "I")
t1$flag <- ifelse(t1$cat == "H", "White Non-Hispanic", "Minority")
t1L <- split(t1, t1$flag)

t2 <- t1L[[1]] %>%
  group_by(year, GEO.id2) %>%
  summarize(minorityInc = weighted.mean(medInc_A, obs),
            minorityPop = sum(obs),
            minorityMOE = weighted.mean(medInc_M, obs))
t3 <- t1L[[2]] %>%
  group_by(year, GEO.id2) %>%
  summarize(whtNonHispInc = weighted.mean(medInc_A, obs),
            whtNonHispPop = sum(obs),
            whtNonHispMOE = weighted.mean(medInc_M, obs))
# t2 contains white non-Hispanic vs. minority incomes by county
t2 %<>% left_join(t3, by = c("year", "GEO.id2")) %>%
  mutate_if(is.numeric, ~round(., 0))
final <- t2 %>%
  mutate(minorityMOEratio = minorityMOE / minorityInc,
         whtNonHispMOEratio = whtNonHispMOE / whtNonHispInc)
final$year <- recode(final$year,"5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")
colnames(final)[2] <- "geography"; final$geography <- as.factor(final$geography)
final$geography <- recode(final$geography, "34015='Gloucester';42029='Chester';42101='Philadelphia'")

# Sample graphs
for(i in c("Gloucester", "Chester", "Philadelphia")){
  t1 <- filter(final, geography == i) %>%
    select(c(1,9,10)) %>%
    gather(group, ratio, minorityMOEratio:whtNonHispMOEratio) %>%
    mutate(group = if_else(group == "minorityMOEratio", "Minority", "White Non-Hispanic"))
  p <- ggplot(data = t1, aes(x = year, y = ratio)) +
    geom_line(aes(color = group)) +
    scale_y_continuous(limits = c(0, 0.6)) +
    labs(title = paste0(i, ": Ratio of MOEs to estimates by group"),
         y = "Ratio",
         x = "Year")
  tiff(paste0(here("figures"), "/moe", i, ".tiff"), units = "in", width = 6, height = 6, res = 600, compression = "lzw")
  plot(p)
  dev.off()
}
