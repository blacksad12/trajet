# -############################################################################-
#' Get travel time from google maps web page, using Selenium
#'
#' @return
#' @export
#'
#' @examples gmap.getTravelTime("url.txt")
gmap.getTravelTime <- function(urlFileName) {
  library(RSelenium)
  library(stringr)
  
  ## Firefox
  # rD <- RSelenium::rsDriver(port = 5555L, 'firefox')
  
  ## Chrome
  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800')
  ))
  rD <- RSelenium::rsDriver(port = 5555L, 'chrome', extraCapabilities = eCaps)
  
  ## Create client
  remDr <- rD[["client"]]
  
  ## Read Gmap url from file
  appURL <- readLines(urlFileName)
  
  ## Go to Gmap page
  remDr$navigate(appURL)
  
  ## Wait few seconds for the right-hand panel to display
  Sys.sleep(4)
  
  ## Browse to the travel duration element
  element <- try(tableElem <- remDr$findElement("id", "section-directions-trip-0"))
  durationElement <- element$findChildElement("class", "section-directions-trip-duration")
  durationHtml <- durationElement$getElementAttribute("innerHTML")[[1]]
  
  remDr$close()
  
  ## Extract travel duration from string
  durationString <- str_match(durationHtml, ">(.*?)<")[2]
  durationHour <- as.numeric(sub(' h.*', '', durationString))
  durationMinute <- as.numeric(str_match(durationString, " h (.*?)&nbsp;")[2])
  
  durationTime <- durationHour * 60 + durationMinute
  
  return(durationTime)
}

# -############################################################################-
#' Add the travelTime to history file
#'
#' @param historyFileName String.
#' @param travelTime Numeric.
#'
#' @return \code{data.table}
#' @export
#'
#' @examples \dontrun{"NO EXAMPLE"}
addToHistory <- function(historyFileName, travelTime) {
  if (file.exists(historyFileName)) {
    historyDt <- fread(historyFileName)
    historyDt <- historyDt[, datetime := as.POSIXct(datetime)]
  } else {
    historyDt <- data.table()
  }
  
  currentTimeDt <- data.table(datetime = Sys.time(), travelTime = travelTime)
  
  historyDt <- rbind(historyDt, currentTimeDt)
  
  write.csv2(historyDt, historyFileName, quote = FALSE, row.names = FALSE)
  
  return(historyDt)
}