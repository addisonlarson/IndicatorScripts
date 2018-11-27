library(here); library(dplyr); library(tidyr);
library(magrittr); library(ggplot2); library(car)
# Income by quintiles
l <- list.files(here("data", "acs"))
# Quintile income files
lQuin <- subset(l, substr(l, 12, 17) == "B19081")
# CPI
inf <- read.csv(here("data", "bls", "inflation.csv"))
colnames(inf) <- c("year", "cpi")
inf$year <- as.numeric(substr(inf$year, 3, 4))
# Qualifying counties
qct <- c("34005", "34007", "34015", "34021", "42017", "42029", "42045", "42091", "42101")
quinInc <- data.frame()
for (i in 1:length(lQuin)){
  temp <- read.csv(here("data", "acs", lQuin[i]))
  temp <- temp[-1,]
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lQuin[i], 5, 6))
  quinInc <- rbind(quinInc, temp)
}
quinInc <- quinInc[c(1:4,seq(6,16, by = 2))]

# Still need population for region means
lPop <- subset(l, substr(l, 12, 17) == "B01003")
pop <- data.frame()
for (i in 1:length(lPop)){
  temp <- read.csv(here("data", "acs", lPop[i]))
  temp <- temp[-1,]
  temp <- subset(temp, GEO.id2 %in% qct)
  temp$year <- as.numeric(substr(lPop[i], 5, 6))
  pop <- rbind(pop, temp)
}
pop <- pop[c("GEO.id2", "year", "HD01_VD01")]; colnames(pop)[3] <- "totPop"

quinInc %<>% left_join(pop, by = c("year", "GEO.id2")) %>%
  left_join(inf, by = c("year")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate_at(c(4:12), as.numeric) %>%
  mutate_at(c(2), as.factor) %>%
  gather(quintile, income, HD01_VD02, HD01_VD03, HD01_VD04, HD01_VD05, HD01_VD06, HD01_VD07) %>%
  mutate(incomeAdj = income * inf[18,2] / cpi) %>%
  select(GEO.id2, year, totPop, quintile, incomeAdj)

# append suburban county averages and regional averages
t2 <- filter(quinInc, GEO.id2 %in% c(42017, 42029, 42045, 42091)) %>%
  group_by(year, quintile) %>%
  summarize(GEO.id2 = "PA Suburban Counties",
            incomeAdj = weighted.mean(incomeAdj, totPop),
            totPop = sum(totPop))
t3 <- filter(quinInc, GEO.id2 %in% c(34005, 34007, 34015, 34021)) %>%
  group_by(year, quintile) %>%
  summarize(GEO.id2 = "NJ Suburban Counties",
            incomeAdj = weighted.mean(incomeAdj, totPop),
            totPop = sum(totPop))
t4 <- quinInc %>%
  group_by(year, quintile) %>%
  summarize(GEO.id2 = "Region",
            incomeAdj = weighted.mean(incomeAdj, totPop),
            totPop = sum(totPop))

final <- bind_rows(quinInc, t2) %>%
  bind_rows(., t3) %>%
  bind_rows(., t4) %>%
  mutate_if(is.numeric, ~round(., 0))
final$year <- recode(final$year,"0=2000;5=2005;6=2006;7=2007;8=2008;9=2009;10=2010;11=2011;12=2012;13=2013;14=2014;15=2015;16=2016;17=2017")
colnames(final)[1] <- "geography"; final$geography <- as.factor(final$geography)
final$geography <- recode(final$geography,
                          "34005='Burlington';34007='Camden';34015='Gloucester';34021='Mercer';
                          42017='Bucks';42029='Chester';42045='Delaware';42091='Montgomery';42101='Philadelphia'")
final$quintile <- recode(final$quintile, "'HD01_VD02'='Lowest';'HD01_VD03'='Second';'HD01_VD04'='Third';
                         'HD01_VD05'='Fourth';'HD01_VD06'='Highest';'HD01_VD07'='Top 5%'")
# sample graph to show Ben
t1 <- filter(final, geography == "Region")
ggplot(data = t1, aes(x = year, y = incomeAdj)) +
  geom_line(aes(color = quintile)) +
  labs(title = "Sample graph of income quintiles over time",
       y = "Annual mean income, 2017 USD",
       x = "Year")
final %<>% spread(quintile, incomeAdj)
write.csv(final[c(-3)], here("outputs", "quintileInc.csv"), row.names = FALSE)

# transform data for web applications
wide1 <- spread(final, geography, Lowest) %>%
  select(-c(2:7)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_Lowest", .)) %>%
  mutate(year = c(2006:2017))
wide2 <- spread(final, geography, Second) %>%
  select(-c(2:7)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_Second", .)) %>%
  mutate(year = c(2006:2017))
wide3 <- spread(final, geography, Third) %>%
  select(-c(2:7)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_Third", .)) %>%
  mutate(year = c(2006:2017))
wide4 <- spread(final, geography, Fourth) %>%
  select(-c(2:7)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_Fourth", .)) %>%
  mutate(year = c(2006:2017))
wide5 <- spread(final, geography, Highest) %>%
  select(-c(2:7)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_Highest", .)) %>%
  mutate(year = c(2006:2017))
wide6 <- spread(final, geography, `Top 5%`) %>%
  select(-c(2:7)) %>%
  group_by(year) %>%
  summarise_all(max, na.rm = TRUE) %>%
  select_at(c(2:13), ~sub("$", "_Top 5%", .)) %>%
  mutate(year = c(2006:2017))
res <- left_join(wide1, wide2, by = "year") %>%
  left_join(., wide3, by = "year") %>%
  left_join(., wide4, by = "year") %>%
  left_join(., wide5, by = "year") %>%
  left_join(., wide6, by = "year")
write.csv(res, here("outputs", "quintileInc_wide.csv"), row.names = FALSE)
