library(data.table)

source("functions.R")

## Define the name of the text file containing the gmap travel
urlFileName <- "url.txt"
if (!file.exists(urlFileName)) {
  stop(paste("URL file is missing:", urlFileName))
}

travelTime      <- gmap.getTravelTime(urlFileName)
historyFileName <- paste0("data/", Sys.Date() , "_history.csv")
historyDt       <- addToHistory(historyFileName, travelTime)

