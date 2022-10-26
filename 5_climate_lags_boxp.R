  rm(list=ls())
  setwd("D:/MOD_COVID19/MOD")
  Sys.setenv(TZ = "UTC")
  
  library(stringr)
  library(ggplot2)
  library(ggstatsplot)
  library(patchwork)
  library(statsExpressions)
  
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
  
  df <- read.csv("models/models_pm_stri_daily_f.csv", header = T, sep = ",", dec = ".")
  df <- df[df$Model != "ARIMA",]
  df <- add_param(df)
  
  i <- 0
  plist <- list()
  params <- c("pr", "tdew", "tmin", "tmoy", "tmax", "wspd","rh","ah","insol")
  pnames <- c("Rainfall", "Dew point temperature", "Minimum temperature", "Average temperature",
             "Maximum temperature", "Wind speed", "Relative humidity", "Absolute humidity", "Insolation")
  
  for (p in params) {
    #p <- "pr"
    i <- i+1
    lag.pos <- df[((df$rho.spearman>0) & (df$cli.par==p)),]
    lag.neg <- df[((df$rho.spearman<0) & (df$cli.par==p)),]
    nn <- max(nrow(lag.pos), nrow(lag.neg))
    df.lag <- data.frame(cbind.na(lag.pos$cli.lag, lag.neg$cli.lag))
    colnames(df.lag) <- c("positive", "negative")
    df.lag <- stack(df.lag)
    
    # pl <- ggplot(data = df.lag, aes(x=ind, y=values, fill=ind)) +
    #   geom_boxplot() + xlab("") + ylab("days")+
    #   geom_jitter(shape=16, position=position_jitter(0.1))+
    #   labs(title = paste0(letters[i],") ", pnames[i], " (",p,")"), fill="association")
    wt <- wilcox.test(jitter(df.lag[df.lag$ind=="positive",]$values), jitter(df.lag[df.lag$ind=="negative",]$values),
                alternative = "two.sided", conf.level = 0.95, exact = T)
    subtitle <- paste0("W = ",round(wt$statistic, digits=1), 
                       ", p-value = ", round(wt$p.value, digits=4))
    
    pl <- ggbetweenstats(
      data = df.lag,
      x = ind,
      y = values,
      xlab = "",
      ylab = "lag [days]",
      plot.type = "box",
      type = "np",
      pairwise.display = "s", k = 1,
      pirwise.comparisons = T,
      # centrality.type = "nonparametric",
      # centrality.plotting = "median",
      centrality.label.args = list(size = 7, nudge_x = 0.2),
      conf.level = 0.95,
      ggsignif.args = list(textsize = 20, tip_length = 0.01),
      centrality.point.args = aes(col = "black", size = 4),
      point.args = list(size = 3),
      title = paste0(letters[i],") ", pnames[i], " (",p,")"),
      package = "ggsci",
      palette = "nrc_npg"
    ) + theme(plot.title = element_text(size=20),
              plot.subtitle = element_text(size=16, face = "italic"),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 20)) +
      labs(subtitle=subtitle) + 
      guides(color = guide_legend(override.aes = list(size=10)))
    
    plist[[i]] <- pl
    
  }
  
  grob <- wrap_plots(plist, ncol=3, guides = "collect") & 
    theme(axis.text = element_text(size=14)) &
    theme(legend.position = 'bottom', legend.key.size = unit(1,"cm"), 
          legend.text = element_text(size=20)) 
  
  ggsave(filename = "graph/cli_lags.png", grob, dpi = 500, scale = 1, width = 15, height = 15)
