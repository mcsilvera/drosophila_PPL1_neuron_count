# Install necessary packages one by one
install.packages("readxl")
install.packages("openxlsx")
install.packages ("carData")
install.packages("dunn.test")
install.packages("PMCMRplus")
install.packages("ggsignif")
install.packages("stringr")
install.packages("ggpubr")


# Load necessary packages
library(readxl)
library(car)
library(openxlsx)
library(dunn.test)
library(PMCMRplus)
library(ggsignif)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggpubr)

# Set working directory and import data from Excel file
Análisis_N_DA <- read_excel("Análisis_N_DA.xlsx", sheet ="PPL1")

# Generate vectors with data 
w1118D5 <- Análisis_N_DA$w1118D5
w1118D25 <- Análisis_N_DA$w1118D25
prtpΔ1D5 <- Análisis_N_DA$prtpΔ1D5
prtpΔ1D25 <- Análisis_N_DA$prtpΔ1D25
parkinaD5 <- Análisis_N_DA$parkinaD5
parkinaD25 <- Análisis_N_DA$parkinaD25                                                                        
prtpΔ1parkinaD5 <- Análisis_N_DA$prtpΔ1parkinaD5
prtpΔ1parkinaD25 <- Análisis_N_DA$prtpΔ1parkinaD25

#Generate data frames
df <- data.frame(
  group = c(rep("w1118D5", length(w1118D5)),
            rep("w1118D25", length(w1118D25)),
            rep("prtpΔ1D5", length(prtpΔ1D5)),
            rep("prtpΔ1D25", length(prtpΔ1D25)),
            rep("parkinaD5", length(parkinaD5)),
            rep("parkinaD25", length(parkinaD25)),
            rep("prtpΔ1parkinaD5", length(prtpΔ1parkinaD5)),
            rep("prtpΔ1parkinaD25", length(prtpΔ1parkinaD25))),
  value = c(w1118D5, w1118D25, prtpΔ1D5, prtpΔ1D25, parkinaD5, parkinaD25, prtpΔ1parkinaD5, prtpΔ1parkinaD25)
)

D5T <- data.frame(w1118D5, prtpΔ1D5, parkinaD5, prtpΔ1parkinaD5)
D25T <- data.frame(w1118D25, prtpΔ1D25, parkinaD25, prtpΔ1parkinaD25)

data_longD5T <- gather(D5T, key = "group", value = "value")
data_longD25T <- gather(D25T, key = "group", value = "value")


#STATISTICS
#Histogram
hist(w1118D5)
hist(w1118D25)
hist(prtpΔ1D5)
hist(prtpΔ1D25)
hist(parkinaD5)
hist(parkinaD25)
hist(prtpΔ1parkinaD5)
hist(prtpΔ1parkinaD25)

#Normality 
shapiro.test(w1118D5)
shapiro.test(w1118D25)
shapiro.test(prtpΔ1D5)
shapiro.test(prtpΔ1D25)
shapiro.test(parkinaD5)
shapiro.test(parkinaD25)
shapiro.test(prtpΔ1parkinaD5)
shapiro.test(prtpΔ1parkinaD25)

# Perform the Levene tests
leveneTest(value ~ group, data = d5T)
leveneTest(value ~ group, data = d25T)


#Mann and Whitney (Wilcoxon test) D5
wilcox.test(w1118D5, prtpΔ1D5)
wilcox.test(w1118D5, parkinaD5)
wilcox.test(w1118D5, prtpΔ1parkinaD5)

wilcox.test(prtpΔ1D5, parkinaD5)
wilcox.test(prtpΔ1D5, prtpΔ1parkinaD5)

wilcox.test(parkinaD5, prtpΔ1parkinaD5)

#Mann and Whitney (Wilcoxon test) D25
wilcox.test(w1118D25, prtpΔ1D25)
wilcox.test(w1118D25, parkinaD25)
wilcox.test(w1118D25, prtpΔ1parkinaD25)

wilcox.test(prtpΔ1D25, parkinaD25)
wilcox.test(prtpΔ1D25, prtpΔ1parkinaD25)

wilcox.test(parkinaD25, prtpΔ1parkinaD25)

# Run Dunn's test (Kruskal-Wallis included)
dunn_test <- dunn.test(df$value, df$group, method = "bonferroni")

# Print results
print(dunn_test)

# New names for plot references
nuevos_nombres5 <- c("w", "prtp", "park","prtp-park")

# Define new order of groups
new_orderD5 <- c("w1118D5", "prtpΔ1D5", "parkinaD5", "prtpΔ1parkinaD5")
new_orderD25 <- c("w1118D25", "prtpΔ1D25", "parkinaD25", "prtpΔ1parkinaD25")

# Change order of levels of "group" factor
data_longD5T$group <- factor(data_longD5T$group, levels = new_orderD5)
data_longD25T$group <- factor(data_longD25T$group, levels = new_orderD25)

#Inter-genotype comparisons vs park

pD5T <- ggplot(data_longD5T, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1, fill = "black") +
  labs(x = "D5", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#5C5C5C", "#D5B73C", "#D85353", "#97B15A"), 
                    labels = nuevos_nombres5) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(1.05, 0.95),
        legend.text = element_text (size = 12, color = "black",face = "italic"),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 17), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("w1118D5", "parkinaD5")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(14, 14.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1D5", "parkinaD5")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1parkinaD5", "parkinaD5")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1D5", "w1118D5")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(15, 15.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD5", "w1118D5")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(16, 16.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD5", "prtpΔ1D5")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(15, 15.5)) 
pD5T

pD25T <- ggplot(data_longD25T, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1, fill = "black") +
  labs(x = "D25", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#5C5C5C", "#D5B73C", "#D85353", "#97B15A"), 
                    labels = nuevos_nombres5) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(1.05, 0.95),
        legend.text = element_text (size = 12, color = "black",face = "italic"),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 17), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("w1118D25", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(14, 14.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1D25", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1parkinaD25", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1D25", "w1118D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(15, 15.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD25", "w1118D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(16, 16.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD25", "prtpΔ1D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(15, 15.5)) 
pD25T

plot <- ggarrange(pD5T, pD25T)
plot





pD25T2 <- ggplot(data_longD25T, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.3, fill = alpha("black", 0.3)) +
  labs(x = "D25", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#5C5C5C", "#D5B73C", "#D85353", "#97B15A"), 
                    labels = nuevos_nombres5) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 25, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(1.05, 0.95),
        legend.text = element_text (size = 12, color = "black",face = "italic"),
        legend.title = element_blank(),
        plot.margin = unit(c(5, 5, 5, 5), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 17), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("w1118D25", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(14, 14.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1D25", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1parkinaD25", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5)) + 
  geom_signif(comparisons = list(c("prtpΔ1D25", "w1118D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(15, 15.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD25", "w1118D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(16, 16.5)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD25", "prtpΔ1D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(15, 15.5)) 
pD25T2
