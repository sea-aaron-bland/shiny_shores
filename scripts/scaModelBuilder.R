# Building dataset of shoreline change analysis confidence levels
# Using Monte Carlo simulations of Shoreline Change Analyses ----

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(janitor)

projDir <-  'G:/.shortcut-targets-by-id/1avidn3l8UPCyW6_A-M00EH-A2Q3J-f3P/BakerLabDrive/LabProjects/2757RB_RESTORE_LivingShorelines/Data/Shoreline_Data/Shoreline Methods Analyses/SCA_Sim'

setwd(projDir)

#load input error sets
load('Inputs/HudError.rda')
load('Inputs/RTKError.rda')
load('Inputs/TransectError.rda')
load('Inputs/GBseasonalError.rda')

# load('Outputs/ModelDF_5.rda')



#Error Functions ----

#for a position in meters (x), simulate the measurement of that position with HUD from imagery
getHUDError <- function(x, resolut, georect, seasonal = FALSE) {
  HUD <- sample(Hud.Error, 1)
  SEAS <- ifelse(seasonal, sample(GB.seasonal.error, 1), 0)
  R <- runif(1, min = resolut/-2, max = resolut/2)
  G <- rnorm(1, mean = 0, sd = georect)
  
  return(x + HUD + SEAS + R + G)
  
}

getHUDError.UAS.good <- function(x, seasonal = FALSE) {
  #0.025 m resolution, 0.25 m RMSE/Std Dev positioning error
  return(getHUDError(x, 0.025, 0.25, seasonal))
}

getHUDError.UAS.poor <- function(x, seasonal = FALSE) {
  #0.050 m resolution, 1.5 m RMSE/Std Dev positioning error
  return(getHUDError(x, 0.05, 1.5, seasonal))
}


#NAIP
#https://www.usgs.gov/centers/eros/science/usgs-eros-archive-aerial-photography-national-agriculture-imagery-program-naip

getHUDError.aerial.good <- function(x, seasonal = FALSE) {
  #0.5 m resolution
  #+/- 4 m, assuming 90% confidence
  z <- qnorm(0.1/2, lower.tail = FALSE) #get two-sided z-score for 90% confidence
  
  return(getHUDError(x, 0.5, 4/z, seasonal))
  
}

getHUDError.aerial.poor <- function(x, seasonal = FALSE) {
  #1 m resolution
  #+/- 5 m, assuming 90% confidence
  z <- qnorm(0.1/2, lower.tail = FALSE) #get two-sided z-score for 90% confidence
  
  return(getHUDError(x, 1, 5/z, seasonal))
  
}

#for a position in meters (x), simulate the measurement of that position with an RTK
getRTKError <- function(x, precision = 0.015, seasonal = FALSE) {
  RTK <- sample(RTK.Error, 1)
  SEAS <- ifelse(seasonal, sample(GB.seasonal.error, 1), 0)
  P <- rnorm(1, mean = 0, sd = precision)
  
  return(x + RTK + SEAS + P)
}

#Modelling Functions ----

#for a given rate of shoreline change, number of years monitored, monitoring interval,
#create a number of estimates (reps) for the shoreline change rate
#using the provided function

#if transects is greater than 1, modify each slope by along-transect variability
#and average across all transects
scaModel <- function(rate, years = 10, inter = 2, reps = 1000, func = getRTKError, transects = 1, s = FALSE){
  
  date = seq(0, by = inter, to = years)
  pos = seq(from = 0, by = rate*inter, along.with = date)
  slopes <- numeric(reps)
  for(i in 1:reps){
    simSlopes <- numeric(transects)
    
    for(j in 1:transects){
      
      meas <- sapply(pos, func, seasonal = s)
      model <- lm(meas ~ date)
      
      #if using transect errors (transects > 1), sample a transect error from loaded distribution
      tranErr <- ifelse(transects == 1, 0, sample(transect.Error, 1))
      
      simSlopes[j] <- model$coefficients[2] + tranErr
    
    }
    slopes[i] <- mean(simSlopes) #average of slopes from each transect
  }
  
  return(slopes)
  
}


addModelResults <- function(rate, years, interval, reps = 1000, func = getRTKError, transects = 1, method, seasonal = FALSE){
  
  ModelSlopes <- scaModel(rate, years, interval, reps, func, transects, seasonal)
  
  out <- data.frame(Rate = rate, Years = years, Interval = interval, Method = method, Transects = transects,
                        Est = mean(ModelSlopes), 
                        Lower = quantile(ModelSlopes, 0.025), Upper = quantile(ModelSlopes, 0.975),
                        Range = quantile(ModelSlopes, 0.975) - quantile(ModelSlopes, 0.025),
                    Replicates = reps,
                    Seasonal = seasonal)
  
  return(out)
}

#Testing ----

#set parameters to test

#these parameters were adjusted manually as needed
#not every combination was exhaustively tested

rateVec <- c(-10, -5, -2, -1.5, -1, -.75, -.1)
yearVec <- c(3, 6, 8, 12, 16, 24)
interVec <- c(1, 2, 3)
transectVec <- c(1, 30, 100)
seasonalVec <- c(TRUE)

funcList <- list(getRTKError,
                 getHUDError.UAS.good,
                 getHUDError.UAS.poor,
                 getHUDError.aerial.good,
                 getHUDError.aerial.poor)

methodVec <- c("RTK",
               "UAS Good",
               "UAS Poor",
               "Aerial Good",
               "Aerial Poor")

totalLength <- length(rateVec) * length(yearVec) * length(interVec)

#first run only
# ModelDF <- data.frame(Rate = numeric(totalLength),
#                       Years = numeric(totalLength),
#                       Interval = numeric(totalLength),
#                       Method = character(totalLength),
#                       Transects = numeric(totalLength),
#                       Est = numeric(totalLength),
#                       Lower = numeric(totalLength),
#                       Upper = numeric(totalLength),
#                       Range = numeric(totalLength))

for(b in 1:length(seasonalVec)){
  for(a in 1:length(transectVec)){
    for(m in 1:length(funcList)){
      addDF <- data.frame(Rate = numeric(totalLength),
                          Years = numeric(totalLength),
                          Interval = numeric(totalLength),
                          Method = character(totalLength),
                          Transects = numeric(totalLength),
                          Est = numeric(totalLength),
                          Lower = numeric(totalLength),
                          Upper = numeric(totalLength),
                          Range = numeric(totalLength),
                          Replicates = numeric(totalLength),
                          Seasonal = logical(totalLength))
      
      rowNum = 1
      for(i in 1:length(rateVec)){
        for(j in 1:length(yearVec)){
          for(k in 1:length(interVec)){
            print(paste0("Seasonal:   ", seasonalVec[b]))
            print(paste0("Transects:   ", transectVec[a]))
            print(paste0("Method:   ", methodVec[m]))
            print(paste0("Rate:     ", rateVec[i]))
            print(paste0("Years:    ", yearVec[j]))
            print(paste0("Interval: ", interVec[k]))
            print("~~~~~~~~~~~~~~~~~~~~")
            
            addDF[rowNum, ] = addModelResults(rateVec[i], yearVec[j], interVec[k], 
                                              reps = 1000, #I've adjusted this to 100 for simulations of many transects 
                                              func = funcList[[m]], method = methodVec[m],
                                              transects = transectVec[a],
                                              seasonal = seasonalVec[b])
            
            rowNum = rowNum + 1
          }
        }
      }
      
      ModelDF <- bind_rows(ModelDF, addDF) 
    }
  }
}

# ModelDF <- distinct(ModelDF, Method, Rate, Years, Interval, Transects, Replicates, .keep_all = TRUE)

# ModelDF2 <- ModelDF
# 
# for(i in 1:nrow(ModelDF)){
#   prog = i/nrow(ModelDF)*100
#   print(paste0(format(prog, digits = 3), "%"))
#   
#   m <- ModelDF$Method[i]
#   
#   ModelDF2[i,] <- addModelResults(rate = ModelDF$Rate[i],
#                                   years = ModelDF$Years[i],
#                                   interval = ModelDF$Interval[i],
#                                   reps = ModelDF$Replicates[i],
#                                   func = ifelse(m == "RTK",
#                                                 getRTKError,
#                                                 ifelse(m == "Aerial Good",
#                                                        getHUDError.aerial.good,
#                                                        ifelse(m == "Aerial Poor",
#                                                               getHUDError.aerial.poor,
#                                                               ifelse(m == "UAS Good",
#                                                                      getHUDError.UAS.good,
#                                                                      getHUDError.UAS.poor)))),
#                                   transects = ModelDF$Transects[i],
#                                   method = m,
#                                   seasonal = ModelDF$Seasonal[i])
#   
# }
# 
# save(ModelDF2, file = 'Outputs/ModelDF_6.rda')
