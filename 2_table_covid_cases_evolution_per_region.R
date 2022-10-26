setwd("D:/MOD_COVID19/MOD/")
Sys.setenv(TZ = "UTC")

library(dplyr)

countries <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")
rownames(countries) <- countries$Code_ISO
cli.vars <- c("pr","wspd","rh","tdew","tmax","tmin","insol","tmoy","ah")

dd <- data.frame(matrix(nrow=0, ncol=11))
colnames(dd) <- c("cases", "cases.pm",cli.vars)

for (iso_code in countries$Code_ISO) {
  #iso_code <- "ETH"
  print(paste("Processing",iso_code))
  data <- read.csv(paste0("data/covid_daily/",iso_code,"_COVID.csv"), header = T, sep = ",", dec = ".")
  cc <- data$cases[nrow(data)]
  
  cli.data <- read.csv(paste0("data/cli_daily/",iso_code,"_CLI.csv"), header = T, sep = ",", dec = ".")
  cli.data$Date <- NULL

  pr <- cli.data$PRECTOTCORR
  cli.data$PRECTOTCORR <- cli.data$PS <- cli.data$WS10M <- NULL
  cli.data <- c(sum(pr), as.numeric(colMeans(cli.data)))

  cc <- c(cc, ((cc/countries[iso_code,"Pop"])*1e6),cli.data)
  dd[nrow(dd)+1,] <- cc
}
#dd$Code_ISO <- countries$Code_ISO
dd$Regions <- countries$Regions

write.csv(dd, file=paste0("tables/stats_regions.csv"),row.names = F)

print("finished")