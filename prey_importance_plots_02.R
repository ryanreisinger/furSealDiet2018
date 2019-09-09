library(plyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(MASS)
library(gridExtra)

#-----------------------------------------------------

# Seal colours
# trop
# light orange #f1a340
# dark orange #e66101
# gaz
# ligt purple #998ec3
# dark purple #5e3c99

#-----------------------------------------------------

setwd("C:/Users/Ryan Reisinger/Documents/Academic/PEI_Toppredators/Fur Seal Diet/Data/Working")

#Read in
summer.gaz <- readRDS("gaz_sum_summer.rds")
summer.trop <- readRDS("trop_sum_summer.rds")

winter.gaz <- readRDS("gaz_sum_winter.rds")
winter.trop <- readRDS("trop_sum_winter.rds")

#Specify season
summer.gaz$season <- rep("summer", nrow(summer.gaz))
winter.gaz$season <- rep("winter", nrow(winter.gaz))
summer.trop$season <- rep("summer", nrow(summer.trop))
winter.trop$season <- rep("winter", nrow(winter.trop))

#Specify species
summer.gaz$seal <- rep("gaz", nrow(summer.gaz))
winter.gaz$seal <- rep("gaz", nrow(winter.gaz))
summer.trop$seal <- rep("trop", nrow(summer.trop))
winter.trop$seal <- rep("trop", nrow(winter.trop))

#Bind
data <- rbind(summer.gaz, winter.gaz, summer.trop, winter.trop)
dat.gaz <- rbind(summer.gaz, winter.gaz)
dat.trop <- rbind(summer.trop, winter.trop)

#Ordered plot
#-----------------------
#GAz
#Get the rankings from overall
gaz.over <- readRDS("summary_alltime_gaz.rds")

#Merge
gaz <- merge(dat.gaz, gaz.over[ , c("prey", "rank.fo.inv", "rank.na.inv")], by = "prey", all.x = T)
gaz <- gaz[order(gaz$rank.na.inv, decreasing = T), ]
#gaz$prey <- as.factor(gaz$prey)

#Select only rank < 11
gaz <- gaz[gaz$rank.na.inv < 10, ]

#Plot
p1 <- ggplot(gaz, aes(x = reorder(prey, -rank.na.inv), y = na.percent)) +
  geom_col(fill = "#998ec3", width = 0.8) +
  geom_errorbar(aes(x = prey, ymin = na.percent.LCL, ymax = na.percent.UCL), width = 0.3, size = 0.5) +
  facet_grid(season ~ .) +
  scale_y_continuous(limits=c(0, 40), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())
p1 <- p1 + coord_flip()
p1

#-----------------------
#Trop
#Get the rankings from overall
trop.over <- readRDS("summary_alltime_trop.rds")

#Merge
trop <- merge(dat.trop, trop.over[ , c("prey", "rank.fo.inv", "rank.na.inv")], by = "prey", all.x = T)
trop <- trop[order(trop$rank.na.inv, decreasing = T), ]
#trop$prey<- factor(trop$prey, levels = names(sort(table(trop$prey), decreasing = TRUE)))

#Select only rank < 11
trop <- trop[trop$rank.na.inv < 10, ]

#Plot
p2 <- ggplot(trop, aes(x = reorder(prey, -rank.na.inv), y = na.percent)) +
  geom_col(fill = "#f1a340", width = 0.8) +
  geom_errorbar(aes(x = prey, ymin = na.percent.LCL, ymax = na.percent.UCL), width = 0.3, size = 0.5) +
  facet_grid(season ~ .) +
  scale_y_continuous(limits=c(0, 40), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())
p2 <- p2 + coord_flip()
p2


#Together
grid.arrange(p1, p2)


## Try and plot in a single plot

trop$seal <- "Subantarctic"
gaz$seal <- "Antarctic"

to <- rbind(gaz, trop)

p3 <- ggplot(to, aes(x = reorder(prey, -rank.na.inv), y = na.percent, fill = seal)) +
  geom_col(width = 0.8) +
  geom_errorbar(aes(x = prey, ymin = na.percent.LCL, ymax = na.percent.UCL), width = 0.3, size = 0.5) +
  scale_fill_manual(values = c("#998ec3", "#f1a340")) + 
  facet_grid(season ~ seal) +
  scale_y_continuous(limits=c(0, 40), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())
p3 <- p3 + coord_flip()
p3