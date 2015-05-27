add.nas <- function(xts.obj) {
  xts.dates <- index(xts.obj)
  dates <- seq(xts.dates[1], xts.dates[length(xts.dates)], by=1)
  na.xts <- as.xts(rep(NA, length(dates)), order.by=as.Date(dates))
  xts.w.nas <- merge(na.xts, xts.obj)
  
  # drop place-holder column
  xts.w.nas <- xts.w.nas[,!names(xts.w.nas) == "na.xts"]
  
  xts.w.nas
}