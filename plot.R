setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

file <- "water_tests.csv"

# conversion multipliers to Nitrogen mg/l

conNH4 <- 0.776490
conNO2 <- 0.304457
conNO3 <- 0.225897

d <- file |>
  read_csv() |>
  dplyr::select(Date, NH4, NO2, NO3) |>
  mutate(NH4=NH4*conNH4, NO2=NO2*conNO2, NO3=NO3*conNO3, Date=as.Date(Date, "%d/%m/%y")) |>
  gather(key = "variable", value = "value", -Date)

p <- d |>
  ggplot(aes(x=Date, y=value, color=variable)) +
  geom_point(size=2.5) +
  geom_line(size=1) +
  scale_color_manual(values=c("#4fa54a", "#874aa5", "#c6532f")) +
  theme_light() +
  labs(y="Nitrogen (mg/l)") +
  theme(legend.title = element_blank())

p
