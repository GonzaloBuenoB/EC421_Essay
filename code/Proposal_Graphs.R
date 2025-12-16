library(tidyverse)
library(ggplot2)
library(readxl)

rm(list = ls())

fx_data <- read_excel("../data/ExchangeRateData.xlsx", sheet = "FX")

plot_fx <- fx_data %>%
      pivot_longer(
        cols = -c(Month),
        names_to = "Currency",
        values_to = "Exchange_Rate"
      ) %>%
      ggplot(aes(x = Month, y = Exchange_Rate, color = Currency)) +
      geom_line(aes(linewidth = Currency == "Peruvian Sol")) +  
      scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
      theme_minimal() +
      labs(
        title = "",
        x = "Month",
        y = "FX rate (2002 Jan. = 100)",
        color = "Currency per USD"
      ) +
      theme(plot.title = element_text(hjust = 0.5))

print(plot_fx)
ggsave("../paper/figures/fx_plot.png", plot = plot_fx, width = 6.25, height = 3, dpi = 300)

fx_data_sd <- fx_data %>%
  mutate(across(
    .cols = -c(Month),
    .fns  = ~log(.) - log(lag(.))
    )) %>%
    na.omit() %>%
  summarise(across(
    .cols = -c(Month),
    .fns  = ~round(sd(.)*100,2)
  ))

developed <- c("Euro", "Yen", "British Pound", "Swiss Franc")
emerging <- c("Brasilian Real", "Chilean Peso", "Colombian Peso", "Mexican Peso")
china <- c("Yuan")

vol_ratios <- fx_data_sd %>%
  summarise(
    sd_sol = `Peruvian Sol`,
    Developed = mean(c_across(all_of(developed))) / sd_sol,
    Emerging = mean(c_across(all_of(emerging)))   / sd_sol,
    China = mean(c_across(all_of(china)))         / sd_sol
  )

## Coefficient of Dollarization

dol_coef <- read_excel("../data/ExchangeRateData.xlsx", sheet = 'CoefDoll')

plot_dol_coef <- dol_coef %>%
                ggplot(aes(x = Month, y = Coeficiente_Dolarización)) +
                geom_line(color = "blue", size = 1) +
                labs(
                  title = "",
                  x = "Month",
                  y = "Coefficient of Dollarization"
                ) +
                theme_minimal()
                
print(plot_dol_coef)
ggsave("../paper/figures/doll_plot.png", plot = plot_dol_coef, width = 5.5, height = 2.5, dpi = 300)