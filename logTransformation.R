####################################################################################
#Data analysis for IT Study - Applying log transformation and ttest for mean and CI
####################################################################################

#############################################################################
#Mean and CI of Completion time bar chart
#############################################################################

setwd("C:\\Elio\\Thesis\\Project 1 - ScreenPad\\Studies\\Interaction techniques study\\Analysis")

#Libraries
source("bootstrap_macros.R")
source("plotting.R")
library(PropCIs)
library(xlsx) #To read excel files

#Read data 
#Dataset <- read.xlsx("Combined.xlsx", 1, header = TRUE)
##@Manu Tu peux faire le loading du fichier comme tu faisais deja avec le pressep papier

#Apply log transformation to time values
Dataset$TimeLog = log(Dataset$valeur)


#Put values in different vars for each technique
TimeLogMagnet = subset(Dataset, TI=="MagnetCursor")$TimeLog
TimeLogScTouch = subset(Dataset, TI=="ScTouch")$TimeLog
TimeLogMouse = subset(Dataset, TI=="Mouse")$TimeLog

#Run Ttest on logged time values
TMag = t.test(TimeLogMagnet, conf.level = 0.95)
TSc = t.test(TimeLogScTouch, conf.level = 0.95)
TMouse = t.test(TimeLogMouse, conf.level = 0.95)

#Calculate mean of time values
#Exp of mean because the values are logged
meanMag = exp(mean(TimeLogMagnet))
meanSc = exp(mean(TimeLogScTouch))
meanMouse = exp(mean(TimeLogMouse))

#Get min and max of CI
ciMinMag = exp(TMag$conf.int[1])
ciMinSc = exp(TSc$conf.int[1])
ciMinMouse = exp(TMouse$conf.int[1])

ciMaxMag = exp(TMag$conf.int[2])
ciMaxSc = exp(TSc$conf.int[2])
ciMaxMouse = exp(TMouse$conf.int[2])

#Put evereything in a table
analysisData=c()
analysisData$ratio = c("ScTouch","MagnetCursor","Mouse")
analysisData$pointEstimate = c(meanSc,meanMag,meanMouse)
analysisData$ci.max = c(ciMaxSc,ciMaxMag,ciMaxMouse)
analysisData$ci.min = c(ciMinSc,ciMinMag,ciMinMouse)

#Move evthing to a data frame format for plotting
datatoprint <- data.frame(factor(analysisData$ratio),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI ")
#Display it in the console for copy pasting elsewhere
datatoprint

#Generate Barchart
#Don't forget updating the parameters
barChart(datatoprint,analysisData$ratio ,nbTechs = 3, ymin = 0, ymax = 2005, "", "Completion Time per Technique in ms. Error Bars, 95% CIs")

#############################################################################
#Effect size computation
#We calculate the ratio between the techniques
#Since the value is logged, we're going to do a substraction
#log(x) - log(y) = log(x/y)
#############################################################################

#Caluclate the difference of time after applying the log function
DiffMouseSc = TimeLogMouse - TimeLogScTouch
DiffMouseMag = TimeLogMouse - TimeLogMagnet
DiffScMag = TimeLogScTouch - TimeLogMagnet

#Run Ttest on the difference
TMouseSc = t.test(DiffMouseSc, conf.level = 0.95)
TMouseMag = t.test(DiffMouseMag, conf.level = 0.95)
TScMag = t.test(DiffScMag, conf.level = 0.95)

#Calculate mean of time values
#Exp of mean because the values are logged
meanMouseSc = exp(mean(DiffMouseSc))
meanMouseMag = exp(mean(DiffMouseMag))
meanScMag = exp(mean(DiffScMag))

#Get min and max of CI
ciminMouseSc = exp(TMouseSc$conf.int[1])
ciminMouseMag= exp(TMouseMag$conf.int[1])
ciminScMag= exp(TScMag$conf.int[1])

cimaxMouseSc= exp(TMouseSc$conf.int[2])
cimaxMouseMag= exp(TMouseMag$conf.int[2])
cimaxScMag= exp(TScMag$conf.int[2])

#Put evereything in a table
analysisData=c()
analysisData$ratio = c("Mouse/ScTouch","Mouse/MagnetCursor","ScTouch/MagnetCursor")
analysisData$pointEstimate = c(meanMouseSc,meanMouseMag,meanScMag)
analysisData$ci.max = c(cimaxMouseSc,cimaxMouseMag,cimaxScMag)
analysisData$ci.min = c(ciminMouseSc,ciminMouseMag,ciminScMag)

datatoprint <- data.frame(factor(analysisData$ratio),analysisData$pointEstimate, analysisData$ci.min, analysisData$ci.max)
colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI ")
datatoprint

#Plot it
barChart(datatoprint,analysisData$ratio ,nbTechs = 3, ymin = 0, ymax = 2, "", "Pair-wise differences. Error Bars, 95% CIs")