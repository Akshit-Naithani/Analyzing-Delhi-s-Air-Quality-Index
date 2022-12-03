  #Libraries Used
  library(ggplot2)
  library(moments) 
  
  #Mode Function
  my_mode <- function(x)
  {
    unique_x <- unique(x)
    tabulate_x <- tabulate(match(x, unique_x))
    unique_x[tabulate_x == max(tabulate_x)]
  }
  #Reading the Data-Set
  df = read.csv("Delhi AQI V1.1.csv")
  #Cleaning The Attributes
  df$PM2.5 = ifelse(is.na(df$PM2.5),mean(df$PM2.5,na.rm = TRUE),df$PM2.5)
  df$PM10 = ifelse(is.na(df$PM10),mean(df$PM10,na.rm = TRUE),df$PM10)
  df$NO = ifelse(is.na(df$NO),mean(df$NO,na.rm = TRUE),df$NO)
  df$NO2 = ifelse(is.na(df$NO2),mean(df$NO2,na.rm = TRUE),df$NO2)
  df$NOx = ifelse(is.na(df$NOx),mean(df$NOx,na.rm = TRUE),df$NOx)
  df$CO = ifelse(is.na(df$CO),mean(df$CO,na.rm = TRUE),df$CO)
  
  #Analysis Of Data-Set
  
  #PM2.5
  
  #Max
  max(df$PM2.5)
  
  #Mean
  mean(df$PM2.5)
  
  #Median
  median(df$PM2.5)
  
  #Mode
  my_mode(df$PM2.5)
  
  #Standard Deviation
  sd(df$PM2.5)
  
  #Plotting PM2.5 Indexes
  ggplot(df, aes(x= factor(From.Date) , y= PM2.5),main = 'PM2.5 index by Time',ylab = "PM2.5 Index", xlab = "Dates") + geom_jitter(width = 0.1, alpha = 0.5)
  
  #Skewness Of PM2.5 Indexes
  skewness(df$PM2.5,na.rm = TRUE)
  
  #Kurtosis Of PM2.5 Indexes
  kurtosis(df$PM2.5,na.rm = TRUE)
  
  
  #PM10
  #Max
  max(df$PM10)
  
  #Mean
  mean(df$PM10)
  
  #Median
  median(df$PM10)
  
  #Mode
  my_mode(df$PM10)
  
  #Standard Deviation
  sd(df$PM10)
  
  #Plotting PM10 Indexes
  ggplot(df, aes(x= factor(From.Date) , y= PM10),main = 'PM10 index by Time',ylab = "PM10 Index", xlab = "Dates") + geom_jitter(width = 0.1, alpha = 0.5)
  
  #Skewness Of PM10 Indexes
  skewness(df$PM10,na.rm = TRUE)
  
  #Kurtosis Of PM10 Indexes
  kurtosis(df$PM10,na.rm = TRUE)
  
  
  
  #NO
  #Max
  max(df$NO)
  
  #Mean
  mean(df$NO)
  
  #Median
  median(df$NO)
  
  #Mode
  my_mode(df$NO)
  
  #Standard Deviation
  sd(df$NO)
  
  #Plotting NO Indexes
  ggplot(df, aes(x= factor(From.Date) , y= NO),main = 'NO index by Time',ylab = "NO Index", xlab = "Dates") + geom_jitter(width = 0.1, alpha = 0.5)
  
  #Skewness Of NO Indexes
  skewness(df$NO,na.rm = TRUE)
  
  #Kurtosis Of NO Indexes
  kurtosis(df$NO,na.rm = TRUE)
  
  
  
  #NO2
  #Max
  max(df$NO2)
  
  #Mean
  mean(df$NO2)
  
  #Median
  median(df$NO2)
  
  #Mode
  my_mode(df$NO2)
  
  #Standard Deviation
  sd(df$NO2)
  
  #Plotting NO2 Indexes
  ggplot(df, aes(x= factor(From.Date) , y= NO2),main = 'NO2 index by Time',ylab = "NO2 Index", xlab = "Dates") + geom_jitter(width = 0.1, alpha = 0.5)
  
  #Skewness Of NO2 Indexes
  skewness(df$NO2,na.rm = TRUE)
  
  #Kurtosis Of NO2 Indexes
  kurtosis(df$NO2,na.rm = TRUE)
  
  
  
  #NOx
  #Max
  max(df$NOx)
  
  #Mean
  mean(df$NOx)
  
  #Median
  median(df$NOx)
  
  #Mode
  my_mode(df$NOx)
  
  #Standard Deviation
  sd(df$NOx)
  
  #Plotting NOx Indexes
  ggplot(df, aes(x= factor(From.Date) , y= NOx),main = 'NOx index by Time',ylab = "NOx Index", xlab = "Dates") + geom_jitter(width = 0.1, alpha = 0.5)
  
  #Skewness Of NOx Indexes
  skewness(df$NOx,na.rm = TRUE)
  
  #Kurtosis Of NOx Indexes
  kurtosis(df$NOx,na.rm = TRUE)
  
  
  
  #CO
  #Max
  max(df$CO)
  
  #Mean
  mean(df$CO)
  
  #Median
  median(df$CO)
  
  #Mode
  my_mode(df$CO)
  
  #Standard Deviation
  sd(df$CO)
  
  #Plotting CO Indexes
  ggplot(df, aes(x= factor(From.Date) , y= CO),main = 'CO index by Time',ylab = "CO Index", xlab = "Dates") + geom_jitter(width = 0.1, alpha = 0.5)
  
  #Skewness Of CO Indexes
  skewness(df$CO,na.rm = TRUE)
  
  #Kurtosis Of CO Indexes
  kurtosis(df$CO,na.rm = TRUE)