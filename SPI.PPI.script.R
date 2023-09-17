getwd()
library(dplyr)
library(tidyr)
library(writexl)

# Create a data frame with the provided data
data <- data.frame(
  Factor = c("ppi_psh_75", "ppi_wheat_75", "ppi_poly_80", "ppi_ wheat_80", "ppi_oats_80", "ppi_oats_82.5", "ppi_wheat_82.5", "ppi_poly_82.5", "ppi_oats_85", "ppi_wheat_85", "ppi_poly_85", "ppi_psh_85"),
  FL1 = c(0.037, 0.059, 0.194, 0.151, 0.241, 0.376, 0.287, 0.21, 0.449, 0.407, 0.432, 0.029),
  FL2 = c(0.129, 0.119, NA, 0.03, 0.244, 0.338, 0.105, 0.26, 0.414, 0.253, 0.43, 0.135),
  FL3 = c(0.015, NA, 0.163, 0.157, 0.245, 0.259, 0.16, NA, 0.387, 0.379, 0.46, 0.068),
  CV1 = c(1906.3, 1021.1, 45.846, 73.6, 51.41, 8.86, 32.163, 13.08, 1.95, 2.27, 1.814, 283.2),
  CV2 = c(2430.8, 904.66, 43.705, 114.3, 47.96, 10.05, 9.7258, 25.28, 2.29, 2.89, 1.858, 374.23),
  CV3 = c(1601.9, NA, 45.801, 65.954, 46.53, 14.13, 23.238, NA, 2.43, 2.5917, 1.6632, 402.42)
)

# Reshape the data from wide to long format
data_long <- data %>%
  pivot_longer(cols = -Factor, names_to = "Type", values_to = "Observation")

write_xlsx(data_long, "SPI.PPI_data.xlsx")

# Filter the data for FL columns (FL1, FL2, FL3)
SPI.PPI_FL <- data_long %>%
  filter(Type %in% c("FL1", "FL2", "FL3"))

SPI.PPI_FL$Factor <- as.factor(SPI.PPI_FL$Factor)
SPI.PPI_FL$Observation <- as.numeric(SPI.PPI_FL$Observation)

# Filter the data for CV columns (CV1, CV2, CV3)
SPI.PPI_CV <- data_long %>%
  filter(Type %in% c("CV1", "CV2", "CV3"))

SPI.PPI_CV$Factor <- as.factor(SPI.PPI_CV$Factor)
SPI.PPI_CV$Observation <- as.numeric(SPI.PPI_CV$Observation)


###### One way ANOVA ######
kruskal.test(Factor~ Observation, data = SPI.PPI_FL)
kruskal.test(Factor~ Observation, data = SPI.PPI_CV)



