## Prepare World Bank Data
maternal <- read.csv("./data/raw/maternal_mortality.csv", header = TRUE)
View(maternal)

# 1a
library(tidyverse)
test <- pivot_longer(maternal, cols=starts_with('X'), names_to='year', names_prefix='X', values_to='maternal_mortality') |>
  mutate(year=as.numeric(year)) |>
  select(iso, year, maternal_mortality)
test

# 1b
infant <- read.csv("./data/raw/infant_mortality.csv", header = TRUE)
neonatal <- read.csv("./data/raw/neonatal_mortality.csv", header = TRUE)
under5 <- read.csv("./data/raw/under5_mortality.csv", header = TRUE)

transform <- function(d) {
  pivot_longer(d, cols=starts_with('X'), names_to='year', names_prefix='X', values_to='mortality') |>
    mutate(year=as.numeric(year)) |>
    select(iso, year, mortality)
}

maternal <- transform(maternal)
maternal <- maternal %>% rename(maternal_mortality=mortality)

infant <- transform(infant)
infant <- infant %>% rename(infant_mortality=mortality)

neonatal <- transform(neonatal)
neonatal <- neonatal %>% rename(neonatal_mortality=mortality)

under5 <- transform(under5)
under5 <- under5 %>% rename(under5_mortality=mortality)

## Prepare Disaster Data
disaster <- read.csv('./data/raw/disaster.csv', header=TRUE)
View(disaster)

# 1a
install.packages('janitor')
library(janitor)

disaster <- clean_names(disaster, case='snake')
View(disaster)

# 1b, c
library(dplyr)
disaster <- disaster %>% filter(disaster_type %in% c('Earthquake', 'Drought'), between(year, 2000, 2019)) |>
  select(year, iso, disaster_type)
View(disaster)

# 1d, e
disaster$drought <- as.numeric(disaster$disaster_type == "Drought")
disaster$earthquake <- as.numeric(disaster$disaster_type == "Earthquake")
View(disaster)

## Prepare Conflict Data
conflict <- read.csv('./data/raw/conflict.csv', header=TRUE)
View(conflict)

# 1
conflict$binary <- as.numeric(conflict$best >= 25)
conflict$year <- conflict$year + 1 
conflict <- conflict %>% filter(between(year, 2000, 2019))

## Merge all data
final <- merge(maternal, infant, by=c('iso', 'year'), all=TRUE) |>
  merge(neonatal, by=c('iso', 'year'), all=TRUE) |>
  merge(under5, by=c('iso', 'year'), all=TRUE) |>
  merge(disaster, by=c('iso', 'year'), all=TRUE) |>
  merge(conflict, by=c('iso', 'year'), all=TRUE)

write.csv(final, "data/processed/final.csv", row.names = FALSE)