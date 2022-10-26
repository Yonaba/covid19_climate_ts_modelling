setwd("D:/MOD_COVID19/MOD/")
Sys.setenv(TZ = "UTC")

library(zoo)
library(COVID19)
s_date <- "2020-01-01"
e_date <- "2022-09-30"

countries <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")$Code_ISO

for (iso_code in countries) {
  #iso_code <- "ETH"
  print(paste("Processing",iso_code))
  data <- covid19(country = iso_code, level = 1, start = s_date, end = e_date, verbose = F)
  
  start_index <- which.min(is.na(data$confirmed))
  start_date <- data$date[start_index]
  covid_cases <- data$confirmed[start_index:length(data$confirmed)]
  covid_cases <- floor(na.approx(covid_cases))
  
  dates <- seq(as.Date(start_date),as.Date(e_date),by="day")
  padn <- as.numeric((length(dates) - length(covid_cases)))
  if (padn > 0) {
    print(paste0("Padded ", iso_code, " : ",padn," values"))
    padn <- rep(covid_cases[length(covid_cases)], padn)
    covid_cases <- append(covid_cases, padn)
  }
  df <- data.frame(dates, covid_cases)
  colnames(df) <- c("dates", "cases")
  write.csv(df, file=paste0("data/covid_daily/",iso_code,"_COVID.csv"),row.names = F)
}

print("finished")
