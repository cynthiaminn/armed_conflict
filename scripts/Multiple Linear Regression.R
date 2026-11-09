final_data <- read.csv('./data/processed/final_data.csv')

## Simplify the code for model formula
preds <- as.formula(" ~ armed_conflict + log(gdp_1000) + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

## Fit four mortality models
maternal_mortalitymod <- plm(update.formula(preds, maternal_mortality ~ .), 
  index=c("iso", "year"), effect = "twoways", model="within", data = final_data)
un5mormod <- plm(update.formula(preds, under5_mortality ~ .), 
  index=c("iso", "year"), effect = "twoways", model="within", data = final_data)
infmormod <- plm(update.formula(preds, infant_mortality ~ .), 
  index=c("iso", "year"), effect = "twoways", model="within", data = final_data)
neomormod <- plm(update.formula(preds, neonatal_mortality ~ .), 
  index=c("iso", "year"), effect = "twoways", model="within", data = final_data)

## Table 2
library(stargazer)

stargazer(
  maternal_mortalitymod,
  un5mormod,
  infmormod,
  neomormod,
  type = "html",
  out = "reports/Table2.html",
  title = "Table 2. The association between armed conflict and maternal and child mortality (adjusted beta coefficients, 95% confidence intervals).",
  column.labels = c("Maternal mortality ratio <br> per 100,000 live births", 
                    "Under-5 mortality ratio <br> per 1,000 live births", 
                    "Infant mortality ratio <br> per 1,000 live births", 
                    "Neonatal mortality ratio <br> per 1,000 live births"),
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  model.numbers=FALSE,
  covariate.labels = c(
    "Armed conflict",
    "Log(GDP per capita)",
    "OECD member",
    "Population density",
    "Urban residence",
    "Age dependency ratio",
    "Male education",
    "Temperature",
    "Rainfall",
    "Earthquakes",
    "Droughts"
  ),
  ci = TRUE,
  digits = 2,
  keep.stat = "n"
)

# Make table wider 
html <- readLines("reports/Table2.html")

html <- gsub(
  "<table",
  '<table style="width: 1000px; border-spacing: 25px 0;"',
  html,
  fixed = TRUE
)

writeLines(html, "reports/Table2.html")