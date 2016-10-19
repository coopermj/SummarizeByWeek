#' This function summarizes all columns by week and returns a dataframe with summarized data and week dates
#' @param df Dataframe on which to operate. Must have column DateStamp
#' @param summarizationMethod String that indicates mean or sum
#' @param flipNA TRUE or FALSE to convert NAs to 0
#' @keywords summarize week weekly
#' @export
#' @examples
#' SummarizeByWeek(df, "mean", TRUE)

SummarizeByWeek <- function(df, summarizationMethod, flipNA) {
  library(plyr)
  library(dplyr)
  # Find the beginning/end of the range
  df <- createsInOrOut
  mindate <- min(df$DateStamp)
  maxdate <- max(df$DateStamp)
  # Create a data frame with all the dates
  ts <- seq.POSIXt(as.POSIXct(mindate), as.POSIXct(maxdate), by="day")
  ts <- format.POSIXct(ts,'%Y-%m-%d')
  dfr <- data.frame(DateStamp=ts)
  # Merge the two dataframes to fill in missing dates
  data_with_missing_times <- full_join(dfr,df)
  # shortening to make typing (and copying examples) easiers
  df <- data_with_missing_times
  # create timestamp to make it easier to convert to week
  df$timestamp <- as.numeric(as.POSIXct(df$DateStamp))
  # convert to week
  df$week <- as.Date("1970-01-01")+7*trunc((df$timestamp)/(3600*24*7))

  # Because we're looping, we need a tempoary dataframe to hold our results
  # We start with just a dataframe of the weeks (made unique)
  tempdf <- data.frame(week=unique(df$week))


  for (columnName in names(df)) {
    # columnName <- i # switch i to columnName to make it
    if(columnName == "DateStamp") next # skip the DateStamp column
    if(columnName == "timestamp") next # skip the DateStamp column
    if(columnName == "week") next # skip the DateStamp column

    # Make a dataframe that is a simplified with just what we need
    data <- data.frame()

    data <- df[columnName]
    data$week <- df$week
    data$DateStamp <- df$DateStamp
    # from http://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame-in-r
    names(data)[names(data) == columnName] <- 'values'
    # colnames(data)[3] <- "values"
    # names(df)[names(df) == 'old.var.name'] <- 'new.var.name'

    # Take any NA and make it 0
    if(flipNA)
      data$values[is.na(data$values)] <- 0

    # Take the column name and create two column names
    meanColumnName <- paste0(columnName,"Mn")
    sdColumnName <- paste0(columnName,"Sd")

    # Create dfs for mean and stddev
    # meanDF <- ddply(data, .(week), summarise, meanColumnName=mean(values))
    meanDF <- ddply(data, .(week), summarise, ValuesMn=mean(values))
    colnames(meanDF)[2] <- meanColumnName
    sdDF <- ddply(data, .(week), summarise, ValuesSD=sd(values))
    colnames(sdDF)[2] <- sdColumnName

    jnDF <- full_join(tempdf, meanDF)
    tempdf <- full_join(jnDF, sdDF)

  }

  return(tempdf)
}
