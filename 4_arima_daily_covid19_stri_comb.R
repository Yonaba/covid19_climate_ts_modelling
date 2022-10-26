rm(list=ls())
setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(xts)
library(data.table)
library(forecast)
library(COVID19)
library(lmtest)

source(file = "utils.R")
source(file = "tsCV_ex.R")

cli.params <- c("pr","tdew","tmin","tmoy","tmax","wspd","rh","ah","insol")

#VAL.P <- 0.15
MAX.P.CLI <- 1
LAG.MAX <- 28
criterion <- "bic"

out.param <- c("Model", "Param", "RMSE.calib", "MAE.calib", "Qtest.calib",rep("Coef", MAX.P.CLI),"pv_model_p","pv_strig","rho.spearman","rho.pvalue")

df.out <- data.frame(matrix(nrow = 0, ncol = length(out.param)+1))
df.stri <- read.csv(paste0("data/covid-containment-and-health-index.csv"), header = T, sep = ",", dec = ".")
colnames(df.out) <- c("Code_ISO",out.param)

countries <- read.csv(paste0("data/coord_africa.csv"), header = T, sep = ",", dec = ".")$Code_ISO
countries <- countries[!countries %in% c("TZA")]

cur.am <- 0

s_date <- "2020-01-01"
e_date <- "2022-09-30"

#countries <- c("UGA")

for (iso_code in countries) {
  #iso_code <- "STP"
  print(paste0("Processing ",iso_code))
  # Exportation des predictions en CrossValidation
  cv_fam <- data.frame(matrix(nrow = 0, ncol = 3))
  
  #r?cup?rer les donn?es concernant les cas cumul?s
  df <- read.csv(paste0("data/covid_daily/",iso_code,"_COVID.csv"), header = T, sep = ",", dec = ".")
  stri <- df.stri[df.stri$Code==iso_code,]
  if (nrow(stri) == 0) {
    stri.dates <- seq(as.Date(s_date), as.Date(e_date), by="day")
    ent <- rep("xxx", length(stri.dates))
    isos <- rep(iso_code,length(stri.dates))
    idx <- rep(0,length(stri.dates))
    stri <- data.frame(ent,isos,stri.dates,idx)
    colnames(stri) <- c("Entity", "Code", "Day", "containment_index")
    print(paste0("  >> No strigency ", iso_code))
  }  
  stri$Day <- as.Date(stri$Day)
  stri <- stri[,c(3,4)]
  ss_date <- stri$Day[1]
  dates <- seq(ss_date, as.Date(e_date), by="day")
  miss_n <- length(dates) - nrow(stri)
  if (miss_n < 0) stri <- head(stri,miss_n)
  padn <- c()
  if (miss_n > 0) padn <- rep(stri[nrow(stri),2], miss_n)
  stri <- append(stri[,2], padn)
  stri <- data.frame(dates, stri)
  
  dates <- data.frame(seq(as.Date(s_date), as.Date(e_date), by="day"))
  colnames(dates) <- "dates"
  stri <- merge(dates, stri,by = "dates", all.x = T)
  stri[is.na(stri)] <- 0

  # stri <- read.csv(paste0("data/strigency_weekly/",iso_code,".csv"), header = T, sep = ",", dec = ".")
  # stri <- tail(stri, nrow(df))
  # dates <- stri[,1]
  # stri <- stri[,2]
  # rs <- rescale.vector(stri,0,1)
  # if (all(stri==0)) rs <- stri
  # stri <- data.frame(dates,rs)
  # colnames(stri) <- c("dates", "stri")
  # shift.n <- 5
  # covid_cases <- shift(df$cases,n=shift.n)
  # covid_cases[1:shift.n] <- 0
  covid_cases <- df$cases
  # cc <- shift(df$cases, n=1,fill = 0)
  # covid_cases <- ((covid_cases - cc)/cc)*100
  # covid_cases[covid_cases<0] <- 0
  # covid_cases[is.infinite(covid_cases)] <- 0
  # covid_cases <- cumsum(covid_cases)
  
  #covid_cases <- rollmean(covid_cases,k=7,na.pad=0)
  covid_cases <- rescale.vector(covid_cases,0,1)
  #plot(covid_cases,type="l")
  
  #covid_cases <- rescale.vector(covid_cases,0,1)
  #covid_cases <- log(covid_cases + 1)
  #covid_cases <- append(covid_cases[1], diff(covid_cases))
  #covid_cases[covid_cases < 0] <- 0
  
  # pop.af <- read.csv(paste0("data/pop_tot.csv"), header = T, sep = ",", dec = ".")
  # rownames(pop.af) <- pop.af$Code_ISO
  # iso_pop <- pop.af[iso_code,"Pop_Tot"]
   
  #covid_cases <- (covid_cases / iso_pop) * 1e6
  #cc <- c(0)
  #for (i in 2:length(covid_cases)) cc[i] <- (covid_cases[i] - covid_cases[i-1])/covid_cases[i-1]
  #covid_cases <- cc*100
  st_date <- df$dates[1]
  e_date <- df$dates[length(df$dates)]
  inds <- seq(as.Date(st_date),as.Date(e_date), by = "day")
  ts.data <- ts(data = covid_cases,
                start = c(year(st_date), as.numeric(format(inds[1], "%j"))),
                frequency = 365.25)
  
  stri <- tail(stri$stri, length(ts.data))
  stri <- rescale.vector(stri, 0, 1)
  #plot(ts.data, ylab = "Weekly new cases pm", main = iso_code)
  
  ndiff <- ndiffs(ts.data, test = "adf")
  ndiff <- max(ndiff,1)
  
  cli.data <- read.csv(paste0("data/cli_daily/",iso_code,"_CLI.csv"), header = T, sep = ",", dec = ".")
  cli.data$Date <- NULL
  cli.data <- tail(cli.data, length(ts.data))
  colnames(cli.data) <- c("pr","wspd","rh","tdew","tmax","tmin","insol","tmoy","ps","wspd10","ah")
  cli.data$ps <- cli.data$wspd10 <- NULL
  cli.params <- colnames(cli.data)
  
  reg <- c()
  col_reg <- c()
  for (var in cli.params) {
    print(var)
    #var <- "wspd"
    cli.d <- ts(data = cli.data[[var]],
                start = c(year(st_date), as.numeric(format(inds[1], "%j"))),
                frequency = 365.25)
    sig.lags <- get.sig.lags(cli.d, ts.data, ndiff, criterion, LAG.MAX)
    
    if (length(sig.lags) > 0) {
      for (lag in sig.lags) {
        lag.data <- shift(as.numeric(cli.d), n = -lag)
        col_reg <- append(col_reg, paste0(var,"_",-lag))
        reg <- cbind(reg, lag.data)      
      }
    }
  }
  colnames(reg) <- col_reg
  if (!is.null(reg)) {
    reg <- as.matrix(rescale.df(reg,0,1))
    #reg <- rollmean(reg,k=7,na.pad=0)
  }
  
  n_len <- as.numeric(length(ts.data))
  #n_calib <- ceiling((1-VAL.P) * n_len)
  #n_valid <- n_len - n_calib
  
  cases.calib <- head(ts.data, n = n_len, keepnums = F)
  #cases.valid <- tail(ts.data, n = n_valid, keepnums = F)
  
  if (!is.null(reg)) {
    reg.calib <- head(reg, n = n_len, keepnums = F)
    #reg.valid <- tail(reg, n = n_valid, keepnums = F)
  }

  am <- auto.arima(ts.data, d = ndiff, ic = criterion, trace = T, method = "ML", seasonal = F)
  # ct <- try(coeftest(am)[1,4],silent = T)
  # ct <- ifelse(class(ct)=="try-error",0,ct)
  diag.model <-c(diagnose.model(am, ts.data, simple = T, MAX.COEF = MAX.P.CLI, lag = LAG.MAX),NA,NA,NA,NA)
  df.out[nrow(df.out)+1,] <- c(iso_code,diag.model)
  #cur.am <- nrow(df.out)
  plot(ts.data, ylab = "Weekly new cases pm", main = iso_code)
  lines(am$fitted, col="red")
  
  if (!is.null(reg)) {
    params <- list()
    #selection des meilleurs mod?les arimax
    NPARAM <- NCOL(reg)
    if (NPARAM > 0) {
      nnp <- min(NPARAM, MAX.P.CLI)
      for (nparam in 1:nnp) {
        #nparam <- 1
        #print(paste("nparam",nparam))
        comb <- transpose(data.frame(combn(colnames(reg),nparam, simplify = T)))
        for (row in 1:NROW(comb)) {
          #row <- 1
          param_set <- as.character(comb[row,])
          print(paste("Evaluation set:",paste(param_set, collapse =" / "),paste("(", row, "/", NROW(comb), ")")))
          reg_calib <- reg.calib[,param_set,drop=F]
          if (!all(stri==0)) reg_calib <- as.matrix(data.frame(stri, reg_calib))          
          amc <- auto.arima(ts.data, d = ndiff,ic = criterion, trace = F, method = "ML", seasonal=F, xreg = reg_calib)
          amc_coef <- coeftest(amc)
          #print(amc_coef)
          sig_comb <- c()
          for (p in param_set){
            if (amc_coef[p,4] < 0.05) sig_comb <- append(sig_comb,p)
          }
          if (length(sig_comb) == nparam) params[[length(params)+1]] <- sig_comb
        }
      }
    }
    
    #evaluation statistique des meilleurs ARIMA
    nparams <- length(params)
    np <- 0
    if (nparams > 0) {
      #rnames <- c("ARIMA")
      for (p in params) {
        np <- np + 1
        print(paste("Test: Combin",np," / ", nparams))
        print(p)
        reg_calib <- reg.calib[,p,drop=F]
        if (!all(stri==0)) reg_calib <- as.matrix(data.frame(stri, reg_calib))    
        amc <- auto.arima(ts.data, d = ndiff,ic = criterion, trace = F, method = "ML", seasonal = F, xreg = reg_calib)
        diag.model <- diagnose.model(amc, ts.data, xreg = reg.calib, simple = F, MAX.COEF = MAX.P.CLI, lag = LAG.MAX, label = p)
        ct <- coeftest(amc)
        col_idx <- ncol(reg_calib)
        cor.tst <- cor.test(as.numeric(ts.data),reg_calib[,col_idx],method="spearman",alternative = "two.sided",exact=T, conf.level=0.95)
        amc.eval <- c(diag.model,
                      ct[nrow(ct),4],
                      ifelse(all(stri==0),"-",ct[nrow(ct)-1,4]),
                      as.numeric(cor.tst$estimate),
                      cor.tst$p.value)
        lines(amc$fitted, col="blue")
        #res.pvalue <- shapiro.test(residuals(amc))$p.value
        # rmse.calib <- abs(as.numeric(amc.eval[3]))
        # rmse.valid <- abs(as.numeric(amc.eval[8]))
        #if (#(abs(as.numeric(df.out[cur.am,9])) > rmse.valid) &&(amc.eval[7]> 0.05) #&& ) 
        print(paste0("amc.eval5 - ",amc.eval[5]))
        #if (as.numeric(amc.eval[5]) > 0.05) {
          print("adding to df.out")
          df.out[nrow(df.out)+1,] <- c(iso_code,amc.eval)
        #} #retain if BoxJenkins is significant
      }
    } else {
      print("Aucune combinaison de paramètres sensible")
    }
  }
}

df.out2 <- subset(df.out, ((Model == "ARIMA") | ((rho.pvalue <= 0.05) & (Model != "ARIMA"))))
df.out3 <- subset(df.out, (((Qtest.calib <= 0.05) & (rho.pvalue <= 0.05) & (Model != "ARIMA"))))

View(df.out2)
write.csv(df.out2, file = paste0("models/models_pm_stri_daily.csv"), row.names = F)
