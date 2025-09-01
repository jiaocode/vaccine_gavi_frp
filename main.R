library(tidyverse)
library(plyr)

source("sim_income.R")     # function to simulate income distribution
source("sim_case_util.R")  # function to simulate cases and utilization

gavi57 <- read.csv("gavi57.csv")           # list of Gavi-eligible countries included in analysis
income <- read.csv("income.csv")           # country-level consumption data (GNI, consumption) by year
oop_cost <- read.csv("oop_cost.csv")       # out-of-pocket treatment cost per disease/country/year
population <- read.csv("population.csv")   # target population size by country, year, disease, vaccine

case <- read.csv("case.csv")               # disease case probabilities in absence of vaccine by wealth quintile
util <- read.csv("util.csv")               # healthcare utilization rates by wealth quintile and disease

# merge the data
data <- inner_join(oop_cost, population, by = c("disease", "country"))
data <- inner_join(data, income, by = c("country"))
data$pop <- 5000

# merge the data
data_q <- inner_join(case, util, by = c("country", "quintile", "disease"))

############################################
################ Simulate FRP ##############
############################################

final_result <- tibble(
  country = character(),
  vaccine = character(),
  year = double(),
  quintile = double(),
  CHE_averted = double()
)

country_list <- intersect(unique(data$country), unique(data_q$country))
country_list <- country_list[country_list %in% gavi57$country]

for (coun in 1:length(country_list)) {
  data_temp <- data %>%
    dplyr::filter(country == country_list[coun])

  year_list <- unique(data_temp$year)

  for (t in 1:length(year_list)) {
    data_temp2 <- data_temp %>%
      dplyr::filter(year == year_list[t])

    vaccine_list <- unique(data_temp2$vaccine_delivery)

    for (vc in 1:length(vaccine_list)) {
      data_temp3 <- data_temp2 %>%
        dplyr::filter(vaccine_delivery == vaccine_list[vc])

      data_sim <- tibble(
        country = country_list[coun],
        year = year_list[t],
        vaccine = vaccine_list[vc],
        income = sim_income(
          gini = data_temp3$gini,
          pop = data_temp3$pop,
          ave_income = data_temp3$gni
        ),
        quintile = ntile(income, 5),
        case = 0,
        util = 0
      )

      data_q_temp <- data_q %>%
        dplyr::filter(country == country_list[coun] &
          year == year_list[t] &
          vaccine_delivery == vaccine_list[vc])

      case_prob <-
        with(
          data_q_temp,
          c(
            case_prob[quintile == 1],
            case_prob[quintile == 2],
            case_prob[quintile == 3],
            case_prob[quintile == 4],
            case_prob[quintile == 5]
          )
        )

      util_prob <-
        with(
          data_q_temp,
          c(
            util[quintile == 1],
            util[quintile == 2],
            util[quintile == 3],
            util[quintile == 4],
            util[quintile == 5]
          )
        )

      data_sim <-
        sim_case_util(
          data = data_sim,
          case_prob = case_prob,
          util_prob = util_prob
        ) %>%
        mutate(cost = case * util * data_temp3$total_cost)

      data_result <-
        data_sim %>%
        mutate(CHE_10 = (cost / income) >= 0.1) %>%
        mutate(CHE_25 = (cost / income) >= 0.25) %>%
        mutate(CHE_40 = (cost / income) >= 0.4) %>%
        group_by(quintile) %>%
        dplyr::summarise(
          CHE_10_averted = sum(CHE_10),
          CHE_25_averted = sum(CHE_25),
          CHE_40_averted = sum(CHE_40)
        ) %>%
        mutate(
          country = country_list[coun],
          vaccine = vaccine_list[vc],
          year = year_list[t]
        )

      final_result <- rbind(final_result, data_result)
    }
  }
}

final_result <-
  final_result %>%
  filter(!is.na(final_result$CHE_40_averted))

final_result$quintile <- as.factor(final_result$quintile)

final_result$quintile <- revalue(
  final_result$quintile,
  c(
    "1" = "Poorest",
    "2" = "Poorer",
    "3" = "Middle",
    "4" = "Richer",
    "5" = "Richest"
  )
)

final_result$quintile <- factor(final_result$quintile,
  levels = c(
    "Poorest",
    "Poorer",
    "Middle",
    "Richer",
    "Richest"
  )
)

final_result$country <- factor(final_result$country,
  levels = sort(unique(final_result$country),
    decreasing = TRUE
  )
)

write.csv(final_result, "final_result.csv")

vaccine_list <-
  c(
    "[Rout] HepB",
    "[Rout] PCV3",
    "[Rout] Hib3",
    "[Rout] Rota",
    "[SIA] Measles",
    "[Rout] MCV1"
  )


vaccine_list <-
  c(
    "[Rout] HepB",
    "[Rout] PCV3",
    "[Rout] Hib3",
    "[Rout] Rota",
    "[SIA] Measles",
    "[Rout] MCV1",
    "[Rout] MCV2",
    "[Rout] HepB_BD"
  )


target_pop <- population %>%
  mutate(
    vaccine = vaccine_delivery,
    pop = pop / 5
  ) %>%
  dplyr::select(country, year, vaccine, pop)

# Output: country- and quintile-specific CH averted per 1000 and total CHE averted
country_che <- left_join(final_result, target_pop, by = c("year", "country", "vaccine")) %>%
  filter(year >= 2000 & year <= 2030) %>%
  mutate(
    CHE_10_averted = CHE_10_averted / 1000 * pop,
    CHE_25_averted = CHE_25_averted / 1000 * pop,
    CHE_40_averted = CHE_40_averted / 1000 * pop
  ) %>%
  filter(vaccine %in% vaccine_list)

write.csv(country_che, "country_che.csv")
