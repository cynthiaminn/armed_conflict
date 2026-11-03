## Table 1
library(table1)
library(tidyverse)
library(gtsummary)
library(flextable)

final_data <- read.csv('./data/processed/final_data.csv')
View(final_data)

table1_data <- final_data %>%
  mutate(
    armed_conflict=factor(
        armed_conflict, 
        levels=c(0,1), 
        labels=c('No', 'Yes')
    ),
    drought=factor(
        drought, levels=c(0, 1),labels=c('No', 'Yes')),
    earthquake=factor(earthquake, levels=c(0, 1),labels=c('No', 'Yes')),
    oecd=factor(oecd, levels=c(0, 1),labels=c('No', 'Yes')))

missing <- final_data %>%
  summarise(across(
    everything(),
    ~sum(is.na(.x))
  )) %>%
  tidyr::pivot_longer(
    cols=everything(),
    names_to='Variable',
    values_to='Missing'
  )

print(missing)

dir.create('reports', showWarnings=FALSE)

write_csv(
    missing, 'reports/table1_missing.csv'
)

table1 <- table1_data %>%
    select(-iso) %>%
    tbl_summary(
        by=armed_conflict,
        type=list(
            c(drought, earthquake, oecd) ~ 'dichotomous'
        ),
        statistic=list(
            all_continuous() ~ '{median} ({p25}, {p75})',
            all_categorical() ~ '{n} ({p}%)'
        ),
        digits=list(
            all_continuous() ~ 2,
            all_categorical() ~ c(0, 1)
        ),
        missing='always',
        missing_text='Missing',
        label=list(
            year ~ "Year",
      maternal_mortality ~ "Maternal mortality",
      infant_mortality ~ "Infant mortality",
      neonatal_mortality ~ "Neonatal mortality",
      under5_mortality ~ "Under-five mortality",
      drought ~ "Drought",
      earthquake ~ "Earthquake",
      gdp_1000 ~ "GDP (thousands)",
      oecd ~ "OECD membership",
      pop_dens ~ "Population density",
      urban ~ "Urban population",
      age_dep ~ "Age dependency",
      male_edu ~ "Male education",
      temp ~ "Temperature",
      rainfall_1000 ~ "Rainfall (thousands)"
        )
    ) %>%
    add_overall(last=FALSE) %>%
    modify_header(
        label ~ '**Characteristics**',
        stat_0 ~ '**Overall**',
        stat_1 ~ '**No armed conflict**',
        stat_2 ~ '**Armed conflict**'
    ) %>%
    modify_caption(
        "**Table 1.** Characteristics of countries with and without armed conflict"
    ) %>%
    bold_labels()

table1

table1_word <- as_flex_table(table1)

save_as_docx(
    'Table 1 - Armed Conflict Characteristics' = table1_word,
    path='reports/table1_armed_conflict.docx'
)

