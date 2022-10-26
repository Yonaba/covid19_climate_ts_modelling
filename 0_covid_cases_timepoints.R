setwd("D:/MOD_COVID19/MOD/")
Sys.setenv(TZ = "UTC")

library(COVID19)
dates <- c("2020-06-30", "2020-12-31",
           "2021-06-30", "2021-12-31",
           "2022-06-30", "2022-09-30")

countries <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")$Code_ISO
pop <-  read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")$Pop

df <- data.frame(matrix(nrow = 0, ncol = length(dates)))
colnames(df) <- dates

for (iso_code in countries) {
  #iso_code <- "ZAF"
  print(iso_code)
  cc <- c()
  for (d in dates) {
    #d <- dates[1]
    #print(d)
    data <- covid19(country = iso_code, level = 1, 
                  start = "2020-01-01" , 
                  end = d, verbose = F)
    cases <- as.numeric(data[[nrow(data),"confirmed"]])
    cc <- append(cc, cases)
  }
  df[nrow(df)+1,] <- cc
}

df2 <- (df/pop * 1e6)
rownames(df) <- rownames(df2) <- countries

View(df)
write.csv(df2, file="data/covid_cases_data_t.csv",row.names = T)
