#' Aggregate the raw data into an equally-spaced data frame
#'
#' @param data Data in a data.frame format
#' @param variable Name of variable in the data set you would like to re-structure
#' @param ID Name of ID variable in the data set
#' @param timestamp Name of time variable in the data set
#' @param format Date/time format of timestamp. Default is "%Y-%m-%dT%H:%M:%S" (i.e. yyyy-mm-dd hh:mm:ss).
#' @param interval The interval of time in minute. Default is 60 (i.e. The measurement is evaluated every hour.)
#' @param aggregatefun Function used for aggregation. Default is sum.
#' @param is.na.zero If TRUE, NA is considered as 0. If FALSE, the NA is considered as missing. Default is TRUE.
#' @param remove.na If TRUE, the NAs will be removed in the aggregated data. Default is TRUE.
#'
#' @importFrom hms hms
#' @importFrom hms as_hms
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
#' @export

interval.aggregation = function(data, variable, ID, timestamp, format = "%Y-%m-%dT%H:%M:%S", interval = 60,
                                aggregatefun = sum, is.na.zero = TRUE, remove.na = TRUE)
{
  if(is.na.zero && sum(is.na(data[,colnames(data) == variable])) > 0){data[,colnames(data) == variable][is.na(data[,colnames(data) == variable])] = 0}
  data[,colnames(data) == timestamp] = as.character(data[,colnames(data) == timestamp])
  tstamp = strptime(data[,colnames(data) == timestamp], format = format)
  data$date = as.Date(tstamp)
  data$time = as_hms(tstamp)

  data = data[order(data$date, data$time),]

  # Create a sequence of time by a minute
  # Hour, minute
  tseq0 = as_hms(seq(hms(0, 0, 0), hms(00, 00, 24), length = as.numeric(difftime(hms(0, 0, 24), hms(0, 0, 0), units = "min")) + 1))
  tseq0 = tseq0[-length(tseq0)] # 24:00:00
  tlength0 = length(tseq0)

  # Every (interval) minutes
  if(interval == 1) {tseq = tseq0; tlength = tlength0} else{

    timesep = interval * (1:round((tlength0 + 1)/interval)-1) + 1
    timesep2 = c((timesep-1)[-1], tlength0)
    timeint = data.frame(start = timesep, end = timesep2) # Interval: Start and end time
    timeint2 = t(apply(timeint, 1, function(x){x[1]:x[2]})) # tlength x interval matrix

    tseq = tseq0[timesep] # Aggregated time stamp
    tlength = length(tseq)
  }

  # Time interval indicator
  data$time2 = c()
  for(t in 1:nrow(data))
  {
    data$time2[t] = tseq[apply(timeint2, 1, function(x){sum(x %in% which(grepl(data$time[t], tseq0)))}) == 1]
  }

  data$time2 = as_hms(data$time2)

  out = data.frame(data %>% group_by(!!as.name(ID), date, time2) %>% summarize(outvar = aggregatefun(!!as.name(variable))))

  colnames(out) = c(ID, "date", "time", variable)
  return(out)
}
