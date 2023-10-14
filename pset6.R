library(haven)
library(tidyverse)
library(panelView)
library(fixest)
library(glue)
library(broom)

setwd("C:/Users/wangy/OneDrive/GitHub/cncustoms_clone")

scheve1_s <- read_rds("scheve1_s.rds")
# year_mean_topitax is the mean of topitax for each year. country_mean_topitax is the mean of topitax for each country. 
# year_mean_war2p is the mean of war2p for each year. country_mean_war2p is the mean of war2p for each country. 
scheve1_s$topitax_mean <- scheve1_s$topitax - scheve1_s$year_mean_topitax - scheve1_s$country_mean_topitax
scheve1_s$war2p_mean <- scheve1_s$war2p - scheve1_s$year_mean_war2p - scheve1_s$country_mean_war2p

# three ways of getting fixed effects
# classic 
feols(topitax ~ war2p | country + year, scheve1_s)
feols(topitax ~ war2p + country + as.factor(year), scheve1_s)

# method2 - subtract country and year means
feols(topitax_mean ~ war2p_mean, scheve1_s)

# method 3 - residualize both treatment and control 
# note that you need to specify year as factor if you put it in as a control. 
# Otherwise, R would take it as a continuous variable.
mod1 <- feols(topitax ~ country + as.factor(year), scheve1_s)  
mod2 <- feols(war2p ~ country + as.factor(year), scheve1_s)
scheve1_s$topitax_resid <- mod1$residuals
scheve1_s$war2p_resid <- mod2$residuals
feols(topitax_resid ~ war2p_resid, data = scheve1_s)

# example for panel view
scheve1 <- read_dta("ScheveStasavage_annual.dta")
scheve1 |> panelview(
  topitax ~ war2p,
  index = c("country", "year"),
  axis.lab.gap = c(10, 0),
  xlab = "",
  ylab = "",
  main = "War Mobilization Treatment"
)


# parallel trend assumption
# The counterfactual trend among treated units absent treatment is the same as those of the control units. (why this is not testable)

scheve5 <- read_dta("ScheveStasavage_5year.dta") 
# create lags and leads -- think of it as shuffling vectors
scheve5_UK <- scheve5 |> filter(country %in% c("UK"))

scheve5_UK <- scheve5_UK |>
  mutate(
    war_lag = lag(war2p, 1),
    war_lead = lead(war2p, 1),
  )

scheve5_UK <- scheve5_UK |> 
  select(country, year, topitax, war2p, war_lag, war_lead)


# regular expression 
trend_colnames <- str_subset(colnames(scheve5), "trend_") 

# this is equivalent to 
colnames(scheve5)[grep("trend_", colnames(scheve5))]

# glue 
# glue allows you to evaluate expression in brackets as code 
# a simple example
TF <- c("Changwook", "Sean", "Robin")
glue('The TF whose name contains Ch is {str_subset(TF, "Ch")}')

# when writing long regression formulae in R, we can use glue to save some typing

coef_names <- "war2p + rgdppc"
spec3_form <- glue("topitax ~ {coef_names} + {str_c(trend_colnames, collapse = ' + ')} | 
                   country + year")

feols(as.formula(spec3_form), scheve5_fmt)


# geom error bar 
fit <- feols(topitax ~ war2p + leftexec|country + year, scheve5)

fit |>
  tidy() |>
  mutate(term = recode_factor(
    term,
    war2p = "War",
    leftexec = "Left",
  )) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2*std.error, 
                    ymax = estimate + 2*std.error), width = 0) +
  labs(y = "Effects of Mobilization and Left Executive on\nInheritance Tax Rate",
       x = "Variables",
       caption = "Note: Units measured in percentage points (10 = 10pp).")





