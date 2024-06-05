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

#Create data frames
df1 <- data.frame(
  group = c(rep("w1118D5", length(w1118D5)),
            rep("w1118D25", length(w1118D25))),
  value = c(w1118D5, w1118D25)
)

df2 <- data.frame(
  group = c(rep("prtpΔ1D5", length(prtpΔ1D5)),
            rep("prtpΔ1D25", length(prtpΔ1D25))),
  value = c(prtpΔ1D5, prtpΔ1D25)
)

df3 <- data.frame(
  group = c(rep("parkinaD5", length(parkinaD5)),
            rep("parkinaD25", length(parkinaD25))),
  value = c(parkinaD5, parkinaD25)
)

df4 <- data.frame(
  group = c(rep("prtpΔ1parkinaD5", length(prtpΔ1parkinaD5)),
            rep("prtpΔ1parkinaD25", length(prtpΔ1parkinaD25))),
  value = c(prtpΔ1parkinaD5, prtpΔ1parkinaD25)
)

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
leveneTest(value ~ group, data = df1)
leveneTest(value ~ group, data = df2)
leveneTest(value ~ group, data = df3)
leveneTest(value ~ group, data = df4)

#Mann and Whitney (Wilcoxon test)

wilcox.test(w1118D5, w1118D25)
wilcox.test(prtpΔ1D5, prtpΔ1D25)
wilcox.test(parkinaD5, parkinaD25 )
wilcox.test(prtpΔ1parkinaD5, prtpΔ1parkinaD25)

# Combine vectors into single data frame
data1 <- data.frame(w1118D5, w1118D25)
data2 <- data.frame(prtpΔ1D5, prtpΔ1D25)
data3 <- data.frame(parkinaD5, parkinaD25)
data4 <- data.frame(prtpΔ1parkinaD5, prtpΔ1parkinaD25)

# Convert data from wide to long format
data_long1 <- gather(data1, key = "group", value = "value")
data_long2 <- gather(data2, key = "group", value = "value")
data_long3 <- gather(data3, key = "group", value = "value")
data_long4 <- gather(data4, key = "group", value = "value")

# Define new order of groups
new_order1 <- c("w1118D5", "w1118D25" )
new_order2 <- c("prtpΔ1D5", "prtpΔ1D25" )
new_order3 <- c("parkinaD5", "parkinaD25" )
new_order4 <- c("prtpΔ1parkinaD5", "prtpΔ1parkinaD25" )

# Change order of levels of "group" factor
data_long1$group <- factor(data_long1$group, levels = new_order1)
data_long2$group <- factor(data_long2$group, levels = new_order2)
data_long3$group <- factor(data_long3$group, levels = new_order3)
data_long4$group <- factor(data_long4$group, levels = new_order4)

# New names for plot references
nuevos_nombres <- c("D5", "D25")

# Boxplot with statistical layer 1
p1 <- ggplot(data_long1, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.2, fill = "black") +
  labs(x = "w", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#DFDCDC", "#5C5C5C"), 
                    labels = nuevos_nombres) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(0.95, 0.95),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 14), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("w1118D5", "w1118D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5))
p1

# Boxplot with statistical layer 2
p2 <- ggplot(data_long2, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.2, fill = "black") +
  labs(x = "prtp", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#D5B73C", "#876F0E"), 
                    labels = nuevos_nombres) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(0.95, 0.95),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 14), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("prtpΔ1D5", "prtpΔ1D25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5))
p2

# Boxplot with statistical layer 3
p3 <- ggplot(data_long3, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.2, fill = "black") +
  labs(x = "park", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#F46F6B", "#BB0404"), 
                    labels = nuevos_nombres) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(0.95, 0.95),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 14), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("parkinaD5", "parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5))
p3

# Boxplot with statistical layer 4
p4 <- ggplot(data_long4, aes(x = group, y = value, fill = group)) + 
  geom_boxplot(size = 0.5, width = 0.3, outlier.shape = 1, outlier.color = "black") +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.2, fill = "black") +
  labs(x = "prtp-park", y = "Número de neuronas PPL1") +  
  scale_fill_manual(values = c("#97B15A", "#577C00"), 
                    labels = nuevos_nombres) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 14, face = "bold.italic"),
        axis.text.y = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(vjust = 4, size = 14, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        legend.position = c(0.95, 0.95),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_blank(),
        plot.margin = unit(c(1, 5, 1, 1), "lines"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  scale_y_continuous(limits = c(5.5, 14), breaks = seq(6, 14, by = 1), expand = c(0,0)) +
  geom_signif(comparisons = list(c("prtpΔ1parkinaD5", "prtpΔ1parkinaD25")),
              test = "wilcox.test",
              map_signif_level = TRUE, 
              color = "black", 
              size = 0.5,
              textsize = 5,
              y_position = c(13, 13.5))
p4

#Create composite plot
plot <- ggarrange(p1, p2, p3, p4)
plot


