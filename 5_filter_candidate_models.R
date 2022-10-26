rm(list=ls())
setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(stringr)
library(ggplot2)
library(patchwork)

cbind.na <- function(x, y){
  if(length(x = x) > length(x = y)){
    len_diff <- length(x) - length(y)
    y <- c(y, rep(NA, len_diff))
  }else if(length(x = x) < length(x = y)){
    len_diff <- length(y) - length(x)
    x <- c(x, rep(NA, len_diff))
  }
  return (cbind(x, y))
}

add_param <- function(df) {
  cli.par <- str_replace(df$Model,"ARIMAX/","")
  spl <- str_split(cli.par,"_",simplify = T)
  df$cli.par <- spl[,1]
  df$cli.lag <- spl[,2]
  df$cli.lag <- as.numeric(df$cli.lag)
  return (df)
}

df <- read.csv("models/models_pm_stri_daily.csv", header = T, sep = ",", dec = ".")
df <- df[df$Model != "ARIMA",]
df <- add_param(df)
df$rho.abs <- abs(df$rho.spearman)
df <- df[order(df$Code_ISO, df$cli.par, -df$rho.abs),]
df$rho.abs <- NULL
xx <- df[!duplicated(df[c("Code_ISO","cli.par")]),]

write.csv(xx, file = paste0("models/models_pm_stri_daily_f.csv"), row.names = F)
