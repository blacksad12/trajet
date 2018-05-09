library(data.table)

source("functions.R")

urlFileName <- "url.txt"
if (!file.exists(urlFileName)) {
  stop(paste("URL file missing:", urlFileName))
}

travelTime      <- gmap.getTravelTime(urlFileName)
historyFileName <- "history.csv"
historyDt       <- addToHistory(historyFileName, travelTime)

