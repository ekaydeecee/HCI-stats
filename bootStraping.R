####################################################################################
#Data analysis for IT Study - Means and CI by bootstraping
####################################################################################

setwd("C:\\Elio\\Thesis\\Project 1 - ScreenPad\\Studies\\Interaction techniques study\\Analysis")

#Libraries
source("bootstrap_macros.R")
source("plotting.R")
library(PropCIs)
library(xlsx) #To read excel files

#Read data 
#Dataset <- read.xlsx("Combined.xlsx", 1, header = TRUE)
##@Manu Tu peux faire le loading du fichier comme tu faisais deja avec le pressep papier

#Separate each technique in a variable
MouseData = subset(Dataset$valeur, Dataset$TI=="Mouse")
MagData = subset(Dataset$valeur, Dataset$TI=="MagnetCursor")
ScData = subset(Dataset$valeur, Dataset$TI=="ScTouch")

#Calculate mean of data
#Apply bootstraping and caluclate 95% CI
#Return for each variable the mean, Lower and uppperbound of the calculated CI
resMouse = bootstrapMeanCI(MouseData)
resSC = bootstrapMeanCI(ScData)
resMag = bootstrapMeanCI(MagData)

#Put in table
analysisData=c()
analysisData$ratio = c("ScTouch","MagnetCursor","Mouse")
analysisData$pointEstimate = c(resSC[1], resMag[1], resMouse[1])
analysisData$ci.min = c(resSC[2], resMag[2], resMouse[2])
analysisData$ci.max = c(resSC[3], resMag[3], resMouse[3])

#Move evthing to a data frame format for plotting
datatoprint <- data.frame((analysisData$ratio),analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min)
colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI ")
#Display it in the console for copy pasting elsewhere
datatoprint

#Generate Barchart
#Don't forget updating the parameters
barChart(datatoprint,analysisData$ratio ,nbTechs = 3, ymin = 0, ymax = 2000, "", "Completion time per technique. Error Bars, Bootstrap 95% CIs")

#############################################################################
#Effect size computation
#We calculate the difference between the means of the techniques
#############################################################################

#Calculate the diff between initial value
DiffMouseSc2 = MouseData - ScData
DiffMouseMag2 = MouseData - MagData
DiffScMag2 =  ScData - MagData

#Calculate mean of this difference
#Apply bootstraping and caluclate 95% CI
#Return for each variable the mean, Lower and uppperbound of the calculated CI
resMouseSc2 = bootstrapMeanCI(DiffMouseSc2)
resMouseMag2 = bootstrapMeanCI(DiffMouseMag2)
resScMag2 = bootstrapMeanCI(DiffScMag2)

#Put in table
analysisData=c()
analysisData$ratio = c("Mouse - ScTouch","Mouse - MagnetCursor","Sc - Magnet cursor")
analysisData$pointEstimate = c(resMouseSc2[1], resMouseMag2[1], resScMag2[1])
analysisData$ci.min = c(resMouseSc2[2], resMouseMag2[2], resScMag2[2])
analysisData$ci.max = c(resMouseSc2[3], resMouseMag2[3], resScMag2[3])
#Move evthing to a data frame format for plotting
datatoprint <- data.frame((analysisData$ratio),analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min)
colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI ")
#Display it in the console for copy pasting elsewhere
datatoprint

#Generate Barchart
#Don't forget updating the parameters
barChart(datatoprint,analysisData$ratio ,nbTechs = 3, ymin = -1000, ymax = 1000, "", "Pair wise differences. Error Bars, Bootstrap 95% CIs")

#############################################################################
#THIS CALCULATION IS NOT DONE BY LONNI
#I CREATED IT FOLLOWING THE PREVIOUS LOGIN, FOR TESTING PURPOSES
#LONNI DOES NOT CALCULATIO THE RATIO USING BOOTSTRAPING IN HIS LATEST CODE UPDATE

#Effect size computation
#We calculate the ratio between the techniques
#############################################################################
DiffMouseSc3 = MouseData / ScData
DiffMouseMag3 = MouseData / MagData
DiffScMag3 =  ScData / MagData

resMouseSc3 = bootstrapMeanCI(DiffMouseSc3)
resMouseMag3 = bootstrapMeanCI(DiffMouseMag3)
resScMag3 = bootstrapMeanCI(DiffScMag3)

#Put in table
analysisData=c()
analysisData$ratio = c("Mouse / ScTouch","Mouse / MagnetCursor","Sc / Magnet cursor")
analysisData$pointEstimate = c(resMouseSc3[1], resMouseMag3[1], resScMag3[1])
analysisData$ci.min = c(resMouseSc3[2], resMouseMag3[2], resScMag3[2])
analysisData$ci.max = c(resMouseSc3[3], resMouseMag3[3], resScMag3[3])

datatoprint <- data.frame((analysisData$ratio),analysisData$pointEstimate, analysisData$ci.max, analysisData$ci.min)
colnames(datatoprint) <- c("technique", "mean_time", "lowerBound_CI", "upperBound_CI ")
datatoprint

barChart(datatoprint,analysisData$ratio ,nbTechs = 3, ymin = 0, ymax = 2, "", "Pair wise differences. Error Bars, Bootstrap 95% CIs")