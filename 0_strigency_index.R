setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(zoo)
library(xts)

countries <- read.csv(paste0("data/coord_africa.csv"), header = T, sep = ",", dec = ".")$Code_ISO
df <- read.csv(paste0("data/covid-containment-and-health-index.csv"), header = T, sep = ",", dec = ".")

s_date <- "2020-01-01"
e_date <- "2022-08-31"

for (iso_code in countries) {
  
  #iso_code <- "BFA"
  print(paste0("Processing ",iso_code))
  
  ddf <- df[df$Code==iso_code,]
  if (nrow(ddf) == 0) {
    dates <- seq(as.Date(s_date), as.Date(e_date), by="day")
    ent <- rep("xxx", length(dates))
    isos <- rep(iso_code,length(dates))
    idx <- rep(0,length(dates))
    ddf <- data.frame(ent,isos,dates,idx)
    colnames(ddf) <- c("Entity", "Code", "Day", "containment_index")
    print(paste0("  >> No strigency ", iso_code))
  }
  ddf$Day <- as.Date(ddf$Day)
  ddf <- ddf[,c(3,4)]
  ss_date <- ddf$Day[1]
  dates <- seq(ss_date, as.Date(e_date), by="day")
  miss_n <- length(dates) - nrow(ddf)
  if (miss_n < 0) ddf <- head(ddf,miss_n)
  padn <- c()
  if (miss_n > 0) padn <- rep(ddf[nrow(ddf),2], miss_n)
  stri <- append(ddf[,2], padn)
  ddf <- data.frame(dates, stri)
  
  dates <- data.frame(seq(as.Date(s_date), as.Date(e_date), by="day"))
  colnames(dates) <- "dates"
  df.out <- merge(dates, ddf,by = "dates", all.x = T)
  df.out[is.na(df.out)] <- 0
  
  striw <- as.xts(df.out[,2], order.by = df.out$dates)
  striw <- apply.weekly(striw, mean)
  
  striw <- data.frame(time(striw),striw)
  colnames(striw) <- c("dates","strig")
  
  write.csv(striw, file = paste0("data/strigency_weekly/",iso_code,".csv"), row.names = F)
}

print("finished")


