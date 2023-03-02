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
  scale_y_log10() +
  geom_point(size=2.5) +
  geom_line(size=1) +
  scale_color_manual(values=c("#4fa54a", "#874aa5", "#c6532f")) +
  theme_light() +
  labs(y="Nitrogen (mg/l)") +
  theme(legend.title = element_blank())

p


#function from chatgpt
convert_to_total_ammonia <- function(NH4_concentration, pH, temperature) {
  # Calculate the ionization constant for ammonia
  Kb <- 0.000096 + (0.0000004 * temperature)
  
  # Calculate the dissociation constant for ammonium ion
  Ka <- Kb * 10^(14 - pH)
  
  # Calculate the ratio of ammonium ion to ammonia
  ratio <- Ka / (Ka + 1)
  
  # Calculate the total concentration of ammonia
  total_ammonia_concentration <- (NH4_concentration / ratio) + NH4_concentration
  
  return(total_ammonia_concentration)
}
