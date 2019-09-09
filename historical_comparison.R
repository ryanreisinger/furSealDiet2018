#Fur seals historical comp.

library(ggplot2)
library(directlabels)
library(gridExtra)

setwd("D:/PEI_Toppredators/Fur Seal Diet/Data/Working")
dat <- read.csv("historical.csv", stringsAsFactors = FALSE)

#check prey
#unique(dat$Prey)

#-----------------------------------------
#gaz
dat.gaz <- dat[dat$seal == "gaz", ]
dat.gaz <- dat.gaz[dat.gaz$Prey == "Electrona carlsbergi" |
                       dat.gaz$Prey == "Gymnoscopelus fraseri" |
                       dat.gaz$Prey == "Gymnoscopelus nicholsi" |
                       dat.gaz$Prey == "Gymnoscopelus piabilis" |
                       dat.gaz$Prey == "Metelectrona ventralis" |
                       dat.gaz$Prey == "Protomyctophum tenisoni"|
                     dat.gaz$Prey == "Cephalopod" |
                     dat.gaz$Prey == "Crustacean", ]

#%F
ggplot(dat.gaz, aes(x=year, y=X.F, colour = Prey)) + geom_point() + geom_line(aes(group = Prey)) +
  geom_dl(aes(label = Prey), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  theme_bw()

#%N
p1 <- ggplot(dat.gaz, aes(x=year, y=X.F)) +
  geom_point(colour = "#998ec3") +
  geom_line(aes(group = Prey), colour = "#998ec3") +
  geom_dl(aes(label = Prey), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  labs(x="Study period", y="Numerical abundance (%)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())

#-----------------------------------------
#trop
dat.trop <- dat[dat$seal == "trop", ]
dat.trop <- dat.trop[dat.trop$Prey == "Electrona carlsbergi" |
                       dat.trop$Prey == "Gymnoscopelus fraseri" |
                       dat.trop$Prey == "Gymnoscopelus nicholsi" |
                       dat.trop$Prey == "Gymnoscopelus piabilis" |
                       dat.trop$Prey == "Metelectrona ventralis" |
                       dat.trop$Prey == "Protomyctophum tenisoni" |
                       dat.trop$Prey == "Cephalopod" |
                       dat.trop$Prey == "Crustacean", ]

#%F
ggplot(dat.trop, aes(x=year, y=X.F, colour = Prey)) + geom_point() + geom_line(aes(group = Prey)) +
  geom_dl(aes(label = Prey), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  theme_bw()

#%N
p2 <- ggplot(dat.trop, aes(x=year, y=X.F)) +
  geom_point(colour = "#f1a340") +
  geom_line(aes(group = Prey), colour = "#f1a340") +
  geom_dl(aes(label = Prey), method = list(dl.combine("first.points", "last.points"), cex = 0.8)) +
  labs(x="Study period", y="Numerical abundance (%)") +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())

grid.arrange(p1, p2)


#correlations
library(reshape2)

dat.cop <- dat
dat.cop <- melt(dat.cop, id.vars = c("Prey", "seal", "year"), measure.vars = c("X.N"))
dat.cop$variable <- NULL
dat.cop <- dcast(dat.cop, Prey + seal ~ year, fun.aggregate = sum)

cor(as.numeric(dat.cop[dat.cop$seal == "trop" & dat.cop$Prey == "Electrona carlsbergi", c("1989-1995",
                                                                              "1996-2000",
                                                                              "2006-2010")]),
    as.numeric(dat.cop[dat.cop$seal == "gaz" & dat.cop$Prey == "Electrona carlsbergi", c("1989-1995",
                                                                             "1996-2000",
                                                                             "2006-2010")]))

cor(as.numeric(dat.cop[dat.cop$seal == "trop" & dat.cop$Prey == "Gymnoscopelus fraseri", c("1989-1995",
                                                                                          "1996-2000",
                                                                                          "2006-2010")]),
    as.numeric(dat.cop[dat.cop$seal == "gaz" & dat.cop$Prey == "Gymnoscopelus fraseri", c("1989-1995",
                                                                                         "1996-2000",
                                                                                         "2006-2010")]))

cor(as.numeric(dat.cop[dat.cop$seal == "trop" & dat.cop$Prey == "Gymnoscopelus piabilis", c("1989-1995",
                                                                                          "1996-2000",
                                                                                          "2006-2010")]),
    as.numeric(dat.cop[dat.cop$seal == "gaz" & dat.cop$Prey == "Gymnoscopelus piabilis", c("1989-1995",
                                                                                         "1996-2000",
                                                                                         "2006-2010")]))

cor(as.numeric(dat.cop[dat.cop$seal == "trop" & dat.cop$Prey == "Protomyctophum tenisoni", c("1989-1995",
                                                                                          "1996-2000",
                                                                                          "2006-2010")]),
    as.numeric(dat.cop[dat.cop$seal == "gaz" & dat.cop$Prey == "Protomyctophum tenisoni", c("1989-1995",
                                                                                         "1996-2000",
                                                                                         "2006-2010")]))

######
#Compare years with wilcoxon test after apply clr trans

## GAZ
gaz.wide <- dat.cop[dat.cop$seal == "gaz", ]
gaz.mat <- gaz.wide
gaz.mat$seal <- NULL
gaz.mat <- gaz.mat[complete.cases(gaz.mat), ]
gaz.mat <- melt(gaz.mat, id.vars = "Prey")
gaz.mat <- dcast(gaz.mat, variable ~ Prey)
gaz.matr <- as.matrix(gaz.mat[ , c(2:length(gaz.mat))])

#clr transformation
library(Hotelling)

#remove zeros, need to work on the matrix, otherwise character conversion
gaz.clr <- gaz.matr[, apply(X = gaz.matr, 2, FUN = min) > 0.00]
#apply clr transformation
gaz.clr <- clr(gaz.clr)
#wilcox
wilcox.test(gaz.clr[1, ], gaz.clr[2, ], paired = T)
wilcox.test(gaz.clr[1, ], gaz.clr[3, ], paired = T)
wilcox.test(gaz.clr[2, ], gaz.clr[3, ], paired = T)

## TROP
trop.wide <- dat.cop[dat.cop$seal == "trop", ]
trop.mat <- trop.wide
trop.mat$seal <- NULL
trop.mat <- trop.mat[complete.cases(trop.mat), ]
trop.mat <- melt(trop.mat, id.vars = "Prey")
trop.mat <- dcast(trop.mat, variable ~ Prey)
trop.matr <- as.matrix(trop.mat[ , c(2:length(trop.mat))])

#clr transformation
#library(Hotelling)

#remove zeros, need to work on the matrix, otherwise character conversion
trop.clr <- trop.matr[, apply(X = trop.matr, 2, FUN = min) > 0.00]
#apply clr transformation
trop.clr <- clr(trop.clr)
#wilcox
wilcox.test(trop.clr[1, ], trop.clr[2, ], paired = T)
wilcox.test(trop.clr[1, ], trop.clr[3, ], paired = T)
wilcox.test(trop.clr[2, ], trop.clr[3, ], paired = T)

#Plots
#gaz
dat.gaz.full <- dat[dat$seal == "gaz", ]
dat.gaz.full <- dat.gaz.full[complete.cases(dat.gaz.full), ]

dat.gaz.temp <- dat.gaz.full[dat.gaz.full$year == "2006-2010", ]
dat.gaz.temp <- dat.gaz.temp[order(dat.gaz.temp$X.F, decreasing = F), ]
dat.gaz.full$Prey<- factor(dat.gaz.full$Prey, levels = paste(dat.gaz.temp$Prey))
dat.gaz.full <- dat.gaz.full[!is.na(dat.gaz.full$Prey), ]

#Remove undidentified species
dat.gaz.full <- dat.gaz.full[dat.gaz.full$Prey != "Bathylagus sp.", ]
dat.gaz.full <- dat.gaz.full[dat.gaz.full$Prey != "Electrona sp.", ]
dat.gaz.full <- dat.gaz.full[dat.gaz.full$Prey != "Gymnoscopelus sp.", ]
dat.gaz.full <- dat.gaz.full[dat.gaz.full$Prey != "Sternoptychidae", ]

p1 <- ggplot(dat.gaz.full, aes(x = Prey, y = X.F)) +
  geom_col(fill = "#998ec3", width = 0.8) +
  #geom_errorbar(aes(x = prey, ymin = na.percent.LCL, ymax = na.percent.UCL), width = 0.3, size = 0.5) +
  facet_grid(. ~ year) +
  #scale_y_continuous(limits=c(0, 40), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())
p1 <- p1 + coord_flip()
p1

#trop
dat.trop.full <- dat[dat$seal == "trop", ]
dat.trop.full <- dat.trop.full[complete.cases(dat.trop.full), ]

dat.trop.temp <- dat.trop.full[dat.trop.full$year == "2006-2010", ]
dat.trop.temp <- dat.trop.temp[order(dat.trop.temp$X.F, decreasing = F), ]
dat.trop.full$Prey<- factor(dat.trop.full$Prey, levels = paste(dat.trop.temp$Prey))
dat.trop.full <- dat.trop.full[!is.na(dat.trop.full$Prey), ]

#Remove undidentified species
dat.trop.full <- dat.trop.full[dat.trop.full$Prey != "Bathylagus sp.", ]
dat.trop.full <- dat.trop.full[dat.trop.full$Prey != "Electrona sp.", ]
dat.trop.full <- dat.trop.full[dat.trop.full$Prey != "Gymnoscopelus sp.", ]
dat.trop.full <- dat.trop.full[dat.trop.full$Prey != "Paralepis sp.", ]
dat.trop.full <- dat.trop.full[dat.trop.full$Prey != "Sternoptychidae", ]

p2 <- ggplot(dat.trop.full, aes(x = Prey, y = X.F)) +
  geom_col(fill = "#f1a340", width = 0.8) +
  #geom_errorbar(aes(x = prey, ymin = na.percent.LCL, ymax = na.percent.UCL), width = 0.3, size = 0.5) +
  facet_grid(. ~ year) +
  #scale_y_continuous(limits=c(0, 40), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank())
p2 <- p2 + coord_flip()
p2

library(gridExtra)
grid.arrange(p1, p2)
