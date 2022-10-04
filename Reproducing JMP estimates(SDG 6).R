## ------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(modelr)
library(broom)


## ------------------------------------------------------------------------------------------------------------------------------------
jmpraw <- read_rds("data/2020-09-30_jmp_sanitation_raw_data.rds")


## ------------------------------------------------------------------------------------------------------------------------------------

#We will begin by counting the estimates of each country

jmp_summary_raw <- jmpraw %>%
    filter(var_long == "Improved") %>% 
    group_by(iso3, year) %>% 
    summarise(mean_value = mean(value))

jmp_summary_raw %>% 
    count(iso3)




## ------------------------------------------------------------------------------------------------------------------------------------
jmp_urban_rural_improved <- jmpraw |> 
    filter(residence != "national",
           var_long %in% c("Improved"))

jmp_urban_rural_improved



## ------------------------------------------------------------------------------------------------------------------------------------
# summarise data by year

jmp_all_mean_value <- jmp_urban_rural_improved |> 
    group_by(iso3, residence, var_long, year) |> 
    summarise(mean_value = mean(value)) |> 
    ungroup()

jmp_all_mean_value


## ------------------------------------------------------------------------------------------------------------------------------------
# Generating a tibble 

min_max_year_iso3 <- jmp_all_mean_value |> 
    group_by(iso3) |> 
    summarise(
        min = min(year),
        max = max(year)
    ) 


diff_smaller_five_year_vec <- min_max_year_iso3 |> 
    mutate(diff = max - min) |> 
    filter(diff < 5) |>
    pull(iso3)

diff_greater_four_year_vec <- min_max_year_iso3 |> 
    mutate(diff = max - min) |> 
    filter(diff >= 5) |>
    pull(iso3)




## ------------------------------------------------------------------------------------------------------------------------------------
year_seq <- list()

for (name in min_max_year_iso3$iso3) {
    
    year_seq[[name]] <- tibble(
        iso3 = name,
        year = seq(filter(min_max_year_iso3, iso3 == name)$min - 2,
                   filter(min_max_year_iso3, iso3 == name)$max + 2,
                   by = 1)
    )
}

year_seq_df <- year_seq |> 
    bind_rows() |> 
    filter(year >= 2000 & year <= 2020)


## ------------------------------------------------------------------------------------------------------------------------------------
jmp_all_mean_value %>%
    filter(iso3 == "AFG" | 
               iso3 == "UGA") 

jmp_all_mean_value %>%
    filter(iso3 %in% c("AFG", "UGA"))

iso3_vector <- c("AFG", "UGA")

jmp_all_mean_value %>%
    filter(iso3 %in% iso3_vector)


## ------------------------------------------------------------------------------------------------------------------------------------

jmp_mean_estimates <- jmp_all_mean_value %>%
    filter(iso3 %in% diff_smaller_five_year_vec) |> 
    group_by(iso3, residence, var_long) %>% 
    summarise(average_estimate = mean(mean_value)) |> 
    left_join(year_seq_df)




## ------------------------------------------------------------------------------------------------------------------------------------

jmp_models <- jmp_all_mean_value %>% 
    filter(iso3 %in% diff_greater_four_year_vec) |> 
    nest_by(iso3, residence, var_long) %>% 
    mutate(model = list(lm(mean_value ~ year, data = data))) %>% 
    summarise(augment(model, newdata = filter(year_seq_df, iso3 %in% diff_greater_four_year_vec))) 

jmp_models



## ------------------------------------------------------------------------------------------------------------------------------------
# nest_by documentation: https://dplyr.tidyverse.org/reference/nest_by.html

# Create a tidy dataframe with model intercept and coefficient
jmp_models <- jmp_all_mean_value |> 
    nest_by(iso3, residence, var_long) |> 
    mutate(model = list(lm(mean_value ~ year, data = data))) 


jmp_models |> 
    summarise(rsq = summary(model)$r.squared)

jmp_models |> 
    summarise(broom::glance(model))

jmp_models |> 
    summarise(broom::tidy(model))


## ------------------------------------------------------------------------------------------------------------------------------------

# Adding predicted/fitted values to the model

dat_year <- tibble(year = seq(2000, 2020, 1))

jmp_fitted <- jmp_models |> 
    summarise(augment(model, newdata = dat_year))
jmp_fitted



## ------------------------------------------------------------------------------------------------------------------------------------
jmp_urban_rural_sewer <- jmpraw |> 
    filter(residence != "national",
           var_long %in% c("Sewer"))

jmp_urban_rural_sewer


## ------------------------------------------------------------------------------------------------------------------------------------
# summarise data by year

jmp_all_mean_value <- jmp_urban_rural_sewer |> 
    group_by(iso3, residence, var_long, year) |> 
    summarise(mean_value = mean(value)) |> 
    ungroup()


## ------------------------------------------------------------------------------------------------------------------------------------
# Create a tidy dataframe with model intercept and coefficient
jmp_models <- jmp_all_mean_value |> 
    nest_by(iso3, residence, var_long) |> 
    mutate(model = list(lm(mean_value ~ year, data = data))) 


jmp_models |> 
    summarise(rsq = summary(model)$r.squared)

jmp_models |> 
    summarise(broom::glance(model))

jmp_models |> 
    summarise(broom::tidy(model))


## ------------------------------------------------------------------------------------------------------------------------------------
# Adding predicted/fitted values to the model

dat_year <- tibble(year = seq(2000, 2020, 1))

jmp_fitted <- jmp_models |> 
    summarise(augment(model, newdata = dat_year))
jmp_fitted



## ------------------------------------------------------------------------------------------------------------------------------------
jmp_urban_rural_septic <- jmpraw |> 
    filter(residence != "national",
           var_long %in% c("Septic"))

jmp_urban_rural_septic


## ------------------------------------------------------------------------------------------------------------------------------------
jmp_all_mean_value <- jmp_urban_rural_septic |> 
    group_by(iso3, residence, var_long, year) |> 
    summarise(mean_value = mean(value)) |> 
    ungroup()


## ------------------------------------------------------------------------------------------------------------------------------------
# Create a tidy dataframe with model intercept and coefficient
jmp_models <- jmp_all_mean_value |> 
    nest_by(iso3, residence, var_long) |> 
    mutate(model = list(lm(mean_value ~ year, data = data))) 


jmp_models |> 
    summarise(rsq = summary(model)$r.squared)

jmp_models |> 
    summarise(broom::glance(model))

jmp_models |> 
    summarise(broom::tidy(model))


## ------------------------------------------------------------------------------------------------------------------------------------
# Adding predicted/fitted values to the model

dat_year <- tibble(year = seq(2000, 2020, 1))

jmp_fitted <- jmp_models |> 
    summarise(augment(model, newdata = dat_year))
jmp_fitted


## ------------------------------------------------------------------------------------------------------------------------------------
jmp_urban_rural_opendefecation <- jmpraw |> 
    filter(residence != "national",
           var_long %in% c("Open defecation"))

jmp_urban_rural_opendefecation


## ------------------------------------------------------------------------------------------------------------------------------------
jmp_all_mean_value <- jmp_urban_rural_opendefecation |> 
    group_by(iso3, residence, var_long, year) |> 
    summarise(mean_value = mean(value)) |> 
    ungroup()



## ------------------------------------------------------------------------------------------------------------------------------------
# Create a tidy dataframe with model intercept and coefficient
jmp_models <- jmp_all_mean_value |> 
    nest_by(iso3, residence, var_long) |> 
    mutate(model = list(lm(mean_value ~ year, data = data))) 


jmp_models |> 
    summarise(rsq = summary(model)$r.squared)

jmp_models |> 
    summarise(broom::glance(model))

jmp_models |> 
    summarise(broom::tidy(model))


## ------------------------------------------------------------------------------------------------------------------------------------
dat_year <- tibble(year = seq(2000, 2020, 1))

jmp_fitted <- jmp_models |> 
    summarise(augment(model, newdata = dat_year))
jmp_fitted

