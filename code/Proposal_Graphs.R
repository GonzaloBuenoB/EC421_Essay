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

## FX volatility

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

## Real Exchange Rate
q_data <- read_csv("../data/dataset_2026-04-07T12_42_00.811848836Z_DEFAULT_INTEGRATION_IMF.STA_CPI_5.0.0.csv") %>%
  select(COUNTRY, matches("^\\d{4}-M\\d{2}$")) %>%
  pivot_longer(cols = -COUNTRY, names_to = "Month", values_to = "value") %>%
  pivot_wider(names_from = COUNTRY, values_from = value) %>%
  slice(-(1:12)) %>%
  mutate(Month = ym(Month)) %>%
  rename(China = `China, People's Republic of`)

CPI_peru <- read_excel("../data/ExchangeRateData.xlsx", sheet = "CPI") %>%
  mutate(Month = as.Date(Month))

fx_aligned <- fx_data %>%
  mutate(Month = as.Date(Month)) %>%  
  rename(
    Japan                          = `Yen`,
    Brazil                         = `Brasilian Real`,
    `United Kingdom`               = `British Pound`,
    Chile                          = `Chilean Peso`,
    China                          = `Yuan`,
    Colombia                       = `Colombian Peso`,
    Mexico                         = `Mexican Peso`,
    Switzerland                    = `Swiss Franc`,
    Peru                           = `Peruvian Sol`
  )

cpi_data <- q_data %>%
  left_join(CPI_peru, by = "Month") %>%
  rename(Peru = CPI_PER)

countries <- intersect(names(fx_aligned), names(cpi_data)) %>% setdiff("Month")

rer_data <- cpi_data %>%
  mutate(across(
    all_of(countries),
    ~fx_aligned[[cur_column()]][match(Month, fx_aligned$Month)] * `United States` / .x
  )) %>%
  select(Month, all_of(countries))

## Normalize RER (first row = 100)
rer_data <- rer_data %>%
  mutate(across(-Month, ~. / first(.) * 100))

## Plot RER
plot_rer <- rer_data %>%
  pivot_longer(cols = -Month, names_to = "Country", values_to = "RER") %>%
  ggplot(aes(x = Month, y = RER, color = Country)) +
  geom_line(aes(linewidth = Country == "Peru")) +
  scale_linewidth_manual(values = c(0.5, 1), guide = "none") +
  theme_minimal() +
  labs(
    x = "Month",
    y = "RER (2002 Jan. = 100)",
    color = "Country"
  )

print(plot_rer)
ggsave("../paper/figures/rer_plot.png", plot = plot_rer, width = 6.25, height = 3, dpi = 300)

## RER volatility
rer_data_sd <- rer_data %>%
  mutate(across(-Month, ~log(.) - log(lag(.)))) %>%
  na.omit() %>%
  summarise(across(-Month, ~round(sd(.) * 100, 2)))

developed_rer <- c("Japan", "United Kingdom", "Switzerland")
emerging_rer  <- c("Brazil", "Chile", "Colombia", "Mexico")

vol_ratios_rer <- tibble(
  sd_peru   = rer_data_sd$Peru,
  Developed = mean(as.numeric(rer_data_sd[, developed_rer])) / sd_peru,
  Emerging  = mean(as.numeric(rer_data_sd[, emerging_rer]))  / sd_peru,
  China     = rer_data_sd$China                              / sd_peru
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

## FX Intervention

df_daily <- read_excel("../data/DataParametrization.xlsx", sheet = "Daily") 

df_daily <- df_daily %>%
  mutate(
    fxi = fx_spot - fx_cdr - fx_swap,
    Quarter = floor_date(date, "quarter")
  )

fxi_quarterly <- df_daily %>%
  group_by(Quarter) %>%
  summarise(fxi = sum(fxi, na.rm = TRUE)) %>%
  ungroup()

data_quarterly <- read_excel("../data/DataParametrization.xlsx", sheet = "Quarterly")
data_quarterly <- data_quarterly %>%
  mutate(across(c(X, M, Y), as.numeric))

n <- nrow(fxi_quarterly) - 2
fxi_quarterly <- fxi_quarterly %>%
  slice(1:n) %>%
  mutate(
    fxi = fxi / (data_quarterly$X[1:n] + data_quarterly$M[1:n]) *
      (data_quarterly$Y[1:n] / data_quarterly$Y_e[1:n])
  )

plot_fxi <- fxi_quarterly %>%
  ggplot(aes(x = Quarter, y = fxi)) +
  geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
                ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.5, inherit.aes = FALSE) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  theme_minimal() +
  labs(x = "Quarter", y = "FXI (fraction of GDP)")

print(plot_fxi)
ggsave("../paper/figures/fxi_plot.png", plot = plot_fxi, width = 6.25, height = 3, dpi = 300)

# Build component quarterly series
components_quarterly <- df_daily %>%
  group_by(Quarter) %>%
  summarise(
    fx_spot = sum(fx_spot, na.rm = TRUE),
    fx_cdr  = sum(-fx_cdr, na.rm = TRUE),
    fx_swap = sum(-fx_swap, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  slice(1:n) %>%
  mutate(across(c(fx_spot, fx_cdr, fx_swap),
                ~ . / (data_quarterly$X[1:n] + data_quarterly$M[1:n]) *
                  (data_quarterly$Y[1:n] / data_quarterly$Y_e[1:n])
  ))

# Shared layers
shade <- geom_rect(aes(xmin = as.Date("2020-01-01"), xmax = as.Date("2021-12-31"),
                       ymin = -Inf, ymax = Inf),
                   fill = "grey90", alpha = 0.5, inherit.aes = FALSE)
zero  <- geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")
base_theme <- list(theme_minimal(), labs(x = ""))

p1 <- fxi_quarterly %>%
  ggplot(aes(x = Quarter, y = fxi)) +
  shade + geom_line(color = "blue", size = 0.8) + zero + base_theme +
  labs(y = "FXI (total)")

p2 <- components_quarterly %>%
  ggplot(aes(x = Quarter, y = fx_spot)) +
  shade + geom_line(color = "darkgreen", size = 0.8) + zero + base_theme +
  labs(y = "Spot")

p3 <- components_quarterly %>%
  ggplot(aes(x = Quarter, y = fx_cdr)) +
  shade + geom_line(color = "red", size = 0.8) + zero + base_theme +
  labs(y = "CDR")

p4 <- components_quarterly %>%
  ggplot(aes(x = Quarter, y = fx_swap)) +
  shade + geom_line(color = "purple", size = 0.8) + zero + base_theme +
  labs(y = "Swap")

library(patchwork)
plot_fxi_panel <- (p1 + p2) / (p3 + p4)

print(plot_fxi_panel)
ggsave("../paper/figures/fxi_panel.png", plot = plot_fxi_panel, width = 6.25, height = 5, dpi = 300)