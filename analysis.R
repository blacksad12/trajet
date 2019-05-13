library(data.table)
library(ggplot2)
library(lubridate)

analysis.getData <- function() {
  files <- list.files("data", full.names = TRUE)
  trajetDt <- data.table()
  for(file in files) {
    dayDt <- fread(file)
    trajetDt <- rbind(trajetDt, dayDt)
  }
  trajetDt <- trajetDt[, datetime := as.POSIXct(datetime)]
  trajetDt <- trajetDt[is.na(travelTime), travelTime := 120]
  return(trajetDt)
}

analysis.getTimePlot <- function() {
  trajetDt <- analysis.getData()
  trajetDt <- trajetDt[, wday := ifelse(wday(datetime)==1,7,wday(datetime)-1)]
  trajetDt <- trajetDt[, datetime := floor_date(datetime, "min")]
  trajetDt <- trajetDt[, hour := hour(datetime) + minute(datetime) / 60]
  trajetDt <- trajetDt[, day := strftime(datetime, format = "%Y-%m-%d")]
  
  ## All days
  plotDt <- trajetDt[, .(mean = mean(travelTime), sd = sd(travelTime)),
                     by = .(hour, wday)]
  ggplot(plotDt, aes(x = hour, y = mean)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = as.character(wday)),
                alpha = 0.5) +
    geom_line(aes(color = as.character(wday))) +
    scale_x_continuous(breaks = seq(0,23), expand = c(0,0)) +
    theme_bw()
  
  ## Sundays
  plotDt <- trajetDt[wday == 7,
                     .(mean = mean(travelTime)),
                     by = .(hour, day)]
  ggplot(plotDt, aes(x = hour, y = mean)) +
    geom_line(aes(color = as.character(day))) +
    scale_x_continuous(breaks = seq(0,23), expand = c(0,0)) +
    scale_color_brewer(type = "seq", palette = "Set1") +
    theme_bw()
  
}

