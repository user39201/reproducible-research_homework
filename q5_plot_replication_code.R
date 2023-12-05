install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

virus_data <- read.csv("Cui_etal2014.csv")

subset_columns <- function(data, column_names) {
  subset_data <- data %>%
    select(all_of(column_names))
  return(subset_data)
}

volume_genome_data <- subset_columns(virus_data, c("Virion.volume..nm.nm.nm.", "Genome.length..kb."))

volume_genome_data$Virion.volume..nm.nm.nm. <- log(volume_genome_data$Virion.volume..nm.nm.nm.)

volume_genome_data$Genome.length..kb. <- log(volume_genome_data$Genome.length..kb.)

linear_model <- lm(Virion.volume..nm.nm.nm. ~ Genome.length..kb., volume_genome_data)
summary(linear_model)

plot <- ggplot(volume_genome_data, aes(x = Genome.length..kb., y = Virion.volume..nm.nm.nm.)) + # Generates plot
  geom_smooth(method = "lm", se = TRUE, color = "blue", formula = y ~ x) +
  geom_point(size = 1) +
  labs(x = "log[Genome length(kb)]", y = "log[Virion volume (nm3)]") +
  theme_bw()

print(plot)
