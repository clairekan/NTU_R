library(vcd)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# Bike
sbi <- read.csv('D:/GitHub/NTU-CS-X/Final Project/Youbike/data/Youbike_sbi3.csv')
sbi_g <- gather(sbi, time, quan, 6:23)

# Table
sbi_tab <- with(sbi_g, tapply(quan, list(sarea, time), FUN=sum))

# Chi Test
# Expected
chisq.test(sbi_tab)$expected

# Residuals
round(chisq.test(sbi_tab)$residuals, 2)

# mosaic
mosaic(sbi_tab, shade = T, color = T, labeling = labeling_border(rot_labels = c(90, 90, 0, 0)))

