Sys.setenv(TZ = "UTC")
setwd("D:/MOD_COVID19/MOD")

library(zoo)
library(xts)
library(COVID19)

file.names <- dir("data/cli_daily", pattern =".csv")
file.names <- sapply(file.names, substr, 1,3)

for (iso_code in file.names) {
  #iso_code <- "CIV"
  print(paste("processing",iso_code))
  
  cc <- read.csv(paste0("data/covid_daily/",iso_code,"_COVID.csv"),
                 header = TRUE, sep = ",", dec = ".")[,2]
  
  n_days <- length(cc)
  
  df <- read.csv(paste0("data/cli_daily/",iso_code,"_CLI.csv"),
                 header = TRUE, sep = ",", dec = ".")  
  df <- tail(df, n_days)
  
  dates <- df$Date
  dates <- df$Date <- as.Date(dates)
  
  #cc <- append(cc[1], diff(cc))
  cc <- as.xts(cc, order.by = dates)
  cc <- apply.weekly(cc, sum)
  for (i in 2:length(cc)) 
    if (as.numeric(cc[i]) < as.numeric(cc[i-1])) cc[i] <- cc[i-1]
  #cc[,1] <- cumsum(cc[,1])

  pr <- as.xts(df$PRECTOTCORR, order.by = dates)
  pr <- apply.weekly(pr, sum)

  wspd <- as.xts(df$WS2M, order.by = dates)
  wspd <- apply.weekly(wspd, mean)
  
  rh <- as.xts(df$RH2M, order.by = dates)
  rh <- apply.weekly(rh, mean)
  
  tdew <- as.xts(df$T2MDEW, order.by = dates)
  tdew <- apply.weekly(tdew, mean)
  
  tmax <- as.xts(df$T2M_MAX, order.by = dates)
  tmax <- apply.weekly(tmax, mean)
  
  tmin <- as.xts(df$T2M_MIN, order.by = dates)
  tmin <- apply.weekly(tmin, mean)
  
  insol <- as.xts(df$ALLSKY_SFC_SW_DWN, order.by = dates)
  insol <- apply.weekly(insol, mean)
  
  tmoy <- as.xts(df$T2M, order.by = dates)
  tmoy <- apply.weekly(tmoy, mean)

  ps <- as.xts(df$PS, order.by = dates)
  ps <- apply.weekly(ps, mean)
  
  wspd10 <- as.xts(df$WS10M, order.by = dates)
  wspd10 <- apply.weekly(wspd10, mean)  
  
  ah <- as.xts(df$AH, order.by = dates)
  ah <- apply.weekly(ah, mean)    
  
  final_df <- data.frame(time(cc),cc, pr, tdew, tmin, tmoy, tmax, wspd, wspd10, rh, ah, insol, ps)
  colnames(final_df)[1] <- "Dates"
  
  final_df <- head(final_df, nrow(df)-1) #throw out the last week

  write.csv(final_df, file = paste0("data/weekly/",iso_code,"_DAT.csv"), 
            row.names = FALSE)
}

print("finished")