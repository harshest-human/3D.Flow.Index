######## packages #####
getwd()
library(dplyr)
library(tidyr)
library(writexl)

##### data.ppi Import #####
# Create a data.ppi frame with the provided data.ppi
data.ppi <- data.frame(
  Factor = c("ppi_psh_75", "ppi_wheat_75", "ppi_poly_80", "ppi_ wheat_80", "ppi_oats_80", "ppi_oats_82.5", "ppi_wheat_82.5", "ppi_poly_82.5", "ppi_oats_85", "ppi_wheat_85", "ppi_poly_85", "ppi_psh_85"),
  FL.ppi1 = c(0.037, 0.059, 0.194, 0.151, 0.241, 0.376, 0.287, 0.21, 0.449, 0.407, 0.432, 0.029),
  FL.ppi2 = c(0.129, 0.119, NA, 0.03, 0.244, 0.338, 0.105, 0.26, 0.414, 0.253, 0.43, 0.135),
  FL.ppi3 = c(0.015, NA, 0.163, 0.157, 0.245, 0.259, 0.16, NA, 0.387, 0.379, 0.46, 0.068),
  CV.ppi1 = c(1906.3, 1021.1, 45.846, 73.6, 51.41, 8.86, 32.163, 13.08, 1.95, 2.27, 1.814, 283.2),
  CV.ppi2 = c(2430.8, 904.66, 43.705, 114.3, 47.96, 10.05, 9.7258, 25.28, 2.29, 2.89, 1.858, 374.23),
  CV.ppi3 = c(1601.9, NA, 45.801, 65.954, 46.53, 14.13, 23.238, NA, 2.43, 2.5917, 1.6632, 402.42)
)


###### data.ppi Processing #######
# Reshape the data.ppi from wide to long format
data.ppi_long <- data.ppi %>%
  pivot_longer(cols = -Factor, names_to = "Type", values_to = "Observation")

write_xlsx(data.ppi_long, "PPI_data.ppi.xlsx")

# Filter the data.ppi for FL.ppi columns (FL.ppi1, FL.ppi2, FL.ppi3)
PPI_FL.ppi <- data.ppi_long %>%
  filter(Type %in% c("FL.ppi1", "FL.ppi2", "FL.ppi3"))

PPI_FL.ppi$Factor <- as.factor(PPI_FL.ppi$Factor)
PPI_FL.ppi$Observation <- as.numeric(PPI_FL.ppi$Observation)

# Filter the data.ppi for CV.ppi columns (CV.ppi1, CV.ppi2, CV.ppi3)
PPI_CV.ppi <- data.ppi_long %>%
  filter(Type %in% c("CV.ppi1", "CV.ppi2", "CV.ppi3"))

PPI_CV.ppi$Factor <- as.factor(PPI_CV.ppi$Factor)
PPI_CV.ppi$Observation <- as.numeric(PPI_CV.ppi$Observation)


###### One way ANOVA ######
FL.ppi <- kruskal.test(Factor ~ Observation, data = PPI_FL.ppi)
CV.ppi <- kruskal.test(Factor ~ Observation, data = PPI_CV.ppi)

# Interpretation
Result <- function(test_result) {
  if (test_result$p.value > 0.05) {
    cat("Based on Kruskal-Wallis test, there is no significant difference (p =", test_result$p.value,").\n")
  } else {
    cat("Based on Kruskal-Wallis test, there is a significant difference (p =", test_result$p.value,").\n")
    # Add alternative interpretation if needed
  }
}

cat("For PPI_FL.ppi:\n")
Result(FL.ppi)

cat("\nFor PPI_CV.ppi:\n")
Result(CV.ppi)


##### data.spi Import #####
# Create a data.spi frame with the provided data.spi
data.spi <- data.frame(
  Factor = c("spi_wheat_77.5", "spi_oats_77.5", "spi_psh_77.5", "spi_pol_ 77.5", "spi_oat_80", "spi_psh_80", "spi_wheat_82.5", "spi_poly_82.5", "spi_wheat_85", "spi_oats_85", "spi_poly_85"),
  FL.spi1 = c(0.409, 0.152, 0.101, 0.231, 0.075, 0.16, 0.365, 0.81, 0.366, 0.444, 0.41),
  FL.spi2 = c(0.468, 0.352, 0.197, 0.13, 0.036, 0.234, 0.371, 0.78, 0.429, 0.441, 0.405),
  FL.spi3 = c(0.379, 0.101, 0.212, 0.174, NA, 0.139, 0.331, NA, 0.4, NA, 0.426),
  CV.spi1 = c(5559.3, 3976.9, 788.6, 753.73, 341.16, 578.95, 47.5, 8.217, 42.19, 34.04, 30.37),
  CV.spi2 = c(7813.9, 940.53, 1298.4, 580.48, 286.47, 760.13, 42.87, 6.911, 34.68, 38.01, 27.7),
  CV.spi3 = c(9922.9, 711.71, 826.4, 807.77, NA, 875.67, 39.21, NA, 38.27, NA, 25.44)
)


###### data.spi Processing #######
# Reshape the data.spi from wide to long format
data.spi_long <- data.spi %>%
  pivot_longer(cols = -Factor, names_to = "Type", values_to = "Observation")

write_xlsx(data.spi_long, "SPI_data.spi.xlsx")

# Filter the data.spi for FL.spi columns (FL.spi1, FL.spi2, FL.spi3)
SPI_FL.spi <- data.spi_long %>%
  filter(Type %in% c("FL.spi1", "FL.spi2", "FL.spi3"))

SPI_FL.spi$Factor <- as.factor(SPI_FL.spi$Factor)
SPI_FL.spi$Observation <- as.numeric(SPI_FL.spi$Observation)

# Filter the data.spi for CV.spi columns (CV.spi1, CV.spi2, CV.spi3)
SPI_CV.spi <- data.spi_long %>%
  filter(Type %in% c("CV.spi1", "CV.spi2", "CV.spi3"))

SPI_CV.spi$Factor <- as.factor(SPI_CV.spi$Factor)
SPI_CV.spi$Observation <- as.numeric(SPI_CV.spi$Observation)


###### One way ANOVA ######
FL.spi <- kruskal.test(Factor ~ Observation, data = SPI_FL.spi)
CV.spi <- kruskal.test(Factor ~ Observation, data = SPI_CV.spi)

# Interpretation
Result <- function(test_result) {
  if (test_result$p.value > 0.05) {
    cat("Based on Kruskal-Wallis test, there is no significant difference (p =", test_result$p.value,").\n")
  } else {
    cat("Based on Kruskal-Wallis test, there is a significant difference (p =", test_result$p.value,").\n")
    # Add alternative interpretation if needed
  }
}

cat("For SPI_FL.spi:\n")
Result(FL.spi)

cat("\nFor SPI_CV.spi:\n")
Result(CV.spi)