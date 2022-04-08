# Datenquelle: https://www.westlotto.de/westlotto-medien/zahlen-fakten/gewinnzahlendownload/lotto6aus49.zip

library(readxl)
library(dplyr)
library(tidyr)

lotto <- read_excel("Lotto/Data/lotto6aus49/LOTTO6aus49.xlsx", sheet = "1955", skip = 7, 
                    col_names = FALSE) %>%
  rename(datum = "...2") %>%
  rename(z1 = "...3", z2 = "...4", z3 = "...5",
         z4 = "...6", z5 = "...7", z6 = "...8") %>%
  select(z1, z2, z3, z4, z5, z6) %>%
  na.omit()

# Schleife über die Jahre 1955 bis 1999
for (i in 1956:1999) {
  dummy <- read_excel("Lotto/Data/lotto6aus49/LOTTO6aus49.xlsx", sheet = paste0(i), skip = 7, 
                      col_names = FALSE) %>%
    rename(datum = "...2") %>%
    rename(z1 = "...3", z2 = "...4", z3 = "...5",
           z4 = "...6", z5 = "...7", z6 = "...8") %>%
    select(z1, z2, z3, z4, z5, z6) %>%
    na.omit()
  
  lotto <- bind_rows(lotto, dummy)
}
  
# Ab 2000 anderes Format. Schleife über die Jahre danach.

for (i in 2000:2021) {
  dummy <- read_excel("Lotto/Data/lotto6aus49/LOTTO6aus49_C.xlsx", sheet = paste0(i), skip = 7, guess_max = 5,
                    col_names = FALSE) %>%
  rename(datum = "...2") %>%
  rename(z1 = "...4", z2 = "...5", z3 = "...6",
         z4 = "...7", z5 = "...8", z6 = "...9") %>%
  select(z1, z2, z3, z4, z5, z6) %>%
  na.omit()
  
  lotto <- bind_rows(lotto, dummy)
}

lotto <- lotto  %>%
  mutate(ziehung = row_number())

write.csv2(lotto, "Lotto/Data/lotto.csv", row.names = FALSE)

lotto_long <- lotto %>%
  pivot_longer(!ziehung, names_to = NULL,
               values_to = "Ziffer")

lotto %>%
  rowwise(ziehung) %>%
  summarise(richtige = sum(chris %in% c(z1,z2,z3,z4,z5,z6))) %>%
  ungroup() %>%
  count(richtige)

lotto %>%
  rowwise(datum) %>%
  summarise(richtige = sum(chris %in% c(z1,z2,z3,z4,z5,z6))) %>%
  ungroup() %>%
  summarise(mean = mean(richtige))

chris <- c(49,47,38,17,9,13)
