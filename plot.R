setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(tidyverse)

file <- "water_tests.csv"

# conversion multipliers to Nitrogen mg/l
# 
# conNH4 <- 0.776490
# conNO2 <- 0.304457
# conNO3 <- 0.225897

d <- file |>
  read_csv() |>
  dplyr::select(Date, NH4, NO2, NO3, WaterChange, Dentritol) |>
  mutate(Date=as.Date(Date, "%d/%m/%y")) |>
  gather(key = "variable", value = "value", -c(Date, WaterChange, Dentritol))

p <- d |>
  ggplot(aes(x=Date, y=value, color=variable)) +
  geom_rect(xmin = as.Date("2023-02-05"), xmax = as.Date("2023-02-17"), ymin = -Inf, ymax = Inf, color=NA, fill = "#e5bec420", alpha = 0.01) +
  geom_point(size=2.5) +
  geom_line(size=1) +
  geom_vline(xintercept = subset(d, !is.na(WaterChange))$Date, linetype=2, size=1, color="darkblue") +
  geom_vline(xintercept = subset(d, !is.na(Dentritol))$Date, color="#3e7687", linetype="dotted", size=1) +
  scale_color_manual(values=c("#4fa54a", "#874aa5", "#c6532f"), guide="none") +
  theme_light() +
  labs(y="concentration (mg/l)") +
  facet_wrap(~variable, ncol=1, scales="free_y") +
  theme(legend.title = element_blank()) +
  theme(strip.background =element_rect(fill="#ccdde5")) +
  theme(strip.text = element_text(colour = "black", face="bold", size=14))


p <- p + geom_text(label="bacteria doses", x=as.Date("2023-02-07"), y=Inf, angle=90, hjust=1.2, vjust=-0.5, color="#3e7687", size=2.5)
p <- p + geom_text(label="water changes 25%", x=as.Date("2023-02-23"), y=Inf, angle=90, hjust=1.2, vjust=-0.5, color="darkblue", size=2.5)
p <- p + geom_text(label="ammonia source (prawn)", x=as.Date("2023-02-11"), y=-Inf, vjust=-1, size=2.5, color="#af9296")
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
