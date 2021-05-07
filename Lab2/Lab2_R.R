# 'Lab2: Time series'
author: "Katarzyna Goch"
date: "21st Oct 2018"
output:
  html_document: default
  pdf_document: default

knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(ggplot2)
library(reshape2)
(WD <- getwd())
if (!is.null(WD)) setwd(WD)

## Time series1: Air quality in Italy
# Read in the first time-series data legend
dfl1 <- read.csv2("TimeSeries1_AirQualityUCI_legend.csv",
                header=TRUE,
                quote="\"",
                stringsAsFactors = FALSE,
                strip.white = TRUE)

# Print out the legend
kable (dfl1)

# Read in the time-series data for Italy
df1 <- read.csv2("TimeSeries1_AirQualityUCI_2004_03_week.csv",
                header=TRUE,
                quote="\"",
                stringsAsFactors = FALSE,
                strip.white = TRUE)


# Omit empty values
df1 <- na.omit(df1, cols="CO_GT_")

# Create a new column and store date-time data
new_col <-as.POSIXct(paste(df1$Date, df1$Time), format="%d.%m.%Y %H:%M:%S")
df1$datetime = new_col

# Remove columns "Date" and "Time"
df11 <- df1[colnames(df1)[4:16]]

# Prepare data for plotting
meltdf11 <- melt(df11,id='datetime')

# Plot
ggplot(data = meltdf11, aes(x = datetime, y = value, color = variable)) + geom_line() + labs(x="Date", y="Value", colour = "Measurement", title="Hourly averaged responses from metal oxide chemical sensors")
 
# In the assignment the **true hourly averaged concentration CO** is analyzed.
ggplot(data = df11, aes(x = datetime, y = CO_GT_, color=CO_GT_)) + geom_line() +
  labs(x="Date", y="True hourly averaged concentration CO in mg/m^3 (reference analyzer)", colour = "Measurement", title="Hourly CO concentration")

# Extract the column with years
df1[,'Date'] <- as.Date(df1[,'Date'],"%d.%m.%Y")
dates1 <- df1[,'Date']
startDate <- min(dates1)
endDate <- max(dates1)
period = as.numeric(format(as.Date(endDate,"%Y-%m-%d"),"%d")) - as.numeric(format(as.Date(startDate,"%Y-%m-%d"),"%d")) +1

## Time series 2: GDP for Poland
# Read in the second time-series data legend
df2_l <- read.csv2("TimeSeries2_Penn_World_Tables_Poland_legend.csv",
                header=TRUE,
                quote="\"",
                stringsAsFactors = FALSE,
                strip.white = TRUE)

# Print out the legend
kable (df2_l)

# Read in the time-series data for Poland
df2 <- read.csv2("TimeSeries2_Penn_World_Tables_Poland.csv",
                header=TRUE,
                quote="\"",
                stringsAsFactors = FALSE,
                strip.white = TRUE)

# Plot GDP for Poland
plotGDP <- qplot(year, rgdpe, data=df2,xlab="Year", ylab="Expenditure-side real GDP at chained PPPs (in mil. 2011US$)",main="Expenditure-side real GDP at chained PPPs in Poland, 1970-2014", geom="line")
plotGDP

# Extract the column with years
years <- df2[,'year']
period = max(years)-min(years) + 1
years <- as.Date(paste(years, 12, 31, sep = "-")) # JAK TEGO NIE DRUKOWAÆ
startDate <- min(years)
endDate <- max(years)

