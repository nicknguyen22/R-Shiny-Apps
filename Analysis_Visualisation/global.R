
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(dplyr)
library(visdat)
library(ggplot2)
library(shinycssloaders)
library(vcd)
library(car)
library(corrgram)
library(RColorBrewer)
library(GGally)
library(Hmisc)
library(tidyr)
library(tabplot)
library(dlookr)

aData = read.csv("Ass1Data.csv")

aData$Date = as.Date(aData$Date, format= "%Y-%m-%d")

cols = c("Location","Agreed","State","Class","Surface")
aData[cols] = lapply(aData[cols],factor)

aData$Priority = factor(aData$Priority,levels=c("Low","Medium","High") ,ordered = TRUE)
aData$Price = factor(aData$Price,levels=c("Cheap","Fair","Expensive") ,ordered = TRUE)
aData$Speed = factor(aData$Speed,levels=c("Slow","Medium","Fast") ,ordered = TRUE)
aData$Duration = factor(aData$Duration,levels=c("Short","Long","Very Long") ,ordered = TRUE)
aData$Temp = factor(aData$Temp,levels=c("Cold","Warm","Hot") ,ordered = TRUE)

chrGrp = aData[c(1:350),c(2:3)]
dateGrp = aData[c(1:350),c(4)]
factorGrp = aData[c(1:350),c(3,5:14)]
numGrp = aData[c(1:350),c(1,15:44)]

choiChr = colnames(chrGrp)
choiFac = colnames(factorGrp)
choiNum = colnames(numGrp)
choiAll = colnames(aData[c(1),c(1,3:44)])
domChoices <- c("l","f","r","t","i","p")

pairsData = drop_na(aData)