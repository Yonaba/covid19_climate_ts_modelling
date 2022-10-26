setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(nasapower)
library(zoo)

cli_params <- c("PRECTOTCORR","WS2M", "RH2M", "T2MDEW","T2M_MAX",
                 "T2M_MIN","ALLSKY_SFC_SW_DWN","T2M","PS","WS10M")

start_date <- "2020-01-01"
end_date <- "2022-09-30"
SLEEP_TIME_BETWEEN_REQUESTS <- 10

countries <- read.csv("data/coord_africa.csv",header = T, sep = ",", dec = ".")

df.out <- data.frame(matrix(nrow = 0, ncol = length(cli_params)))
df.names <- c("pr","ws2m", "rh2m", "tdew", "tmax", "tmin", "insol", "tmoy","ps","ws10m")

for (i in 1:length(countries$Code_ISO)) {
  #i <- "5"
  country <- countries[i,"Code_ISO"]
  print(paste("Processing:", country))
  cli_df <- get_power(
    community = "ag",
    lonlat = c(countries[i,"Longitude"], countries[i,"Latitude"]),
    pars = cli_params,
    dates = c(start_date, end_date),
    temporal_api = "daily")
  df <- data.frame(cli_df[,7:17])
  colnames(df)[1] <- "Date"
  
  df[df == -999] <- NA
  
  for (col in colnames(df)) {
    if (col != "Date") {
      if (is.na(df[1, col])) df[1, col] <- mean(df[,col], na.rm=T)
      if (is.na(df[nrow(df), col])) df[nrow(df), col] <- mean(df[,col], na.rm=T)
      df[,col] <- na.approx(df[,col], na.rm = T)
    }
  }
  # Calculating AH based on Clausius-clapeyron diagram approximation
  df$AH <- (6.112 * exp((17.67*df$T2M)/(df$T2M + 243.5)) * df$RH2M * 2.1674) / (df$T2M + 273.15)
  write.csv(df, file = paste0("data/cli_daily/",country,"_CLI.csv"), row.names = F)
  Sys.sleep(SLEEP_TIME_BETWEEN_REQUESTS)
}

print("finished")