setwd("D:/MOD_COVID19/MOD/")
Sys.setenv(TZ = "UTC")

library(COVID19)
library(zoo)
library(xts)
library(ggplot2)
library(patchwork)

s_date <- "2020-01-01"
e_date <- "2022-09-30"
N_DAYS <- as.numeric((as.Date(e_date) - as.Date(s_date)) + 1)

df <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")
countries <- rownames(df) <- df$Code_ISO
regions <- unique(df$Regions)

pop <- read.csv("data/pop_tot.csv", header = T, sep = ",", dec = ".")
rownames(pop) <- pop$Code_ISO
pop$Code_ISO <- NULL
colnames(pop) <-c("pop")

calc_data <- function(df, countries, regions, pop, N_DAYS, s_date, e_date, cumulative) {
  
  df.cases <- sapply(regions,function(x) NULL)
  for (region in regions) {
    #region <- "AN"
    reg_countries <- df[df$Regions==region,"Code_ISO"]
    df.cases[[region]] <- data.frame(matrix(nrow=N_DAYS, ncol = length(reg_countries)))
    colnames(df.cases[[region]]) <- reg_countries
  }
  
  for (iso_code in countries) {
    #iso_code <- "ZAF"
    print(iso_code)
    region <- df[iso_code,"Regions"]
    covid_cases <- read.csv(paste0("data/covid_daily/",iso_code,"_COVID.csv"), header = T, sep = ",", dec = ".")[,2]
    #covid_cases <- floor(na.approx(covid_cases))
    if (!cumulative) covid_cases <- append(covid_cases[1], diff(covid_cases))
    covid_cases <- (covid_cases/pop[iso_code,"pop"])*1e6
    cc <- append(rep(NA,N_DAYS-length(covid_cases)), covid_cases)
    df.cases[[region]][[iso_code]] <- cc
  }
  
  df.sum <- data.frame(matrix(nrow = N_DAYS, ncol = 0))
  for (region in names(df.cases)) {
    sums <- rowSums(df.cases[[region]], na.rm = T)
    df.sum <- cbind(df.sum, sums)
  }
  colnames(df.sum) <- names(df.cases)
  dates <- seq(as.Date(s_date),as.Date(e_date), by = "day")
  
  dates <- rep(dates, length(df.sum))
  df.sum <- data.frame(dates, stack(df.sum))
  colnames(df.sum) <- c("Dates","Cases","Regions")
  #df.sum$Regions <- factor(df.sum$Regions, levels=c("SAf","NAf","EAf","WAf","CAf"))  
  
  return (df.sum)
}

df.cum <- calc_data(df,countries,regions,pop,N_DAYS,s_date,e_date, cumulative=T)
df.diff <- calc_data(df,countries,regions,pop,N_DAYS,s_date,e_date, cumulative=F)

alpha <- 0.45
#colors  <- rainbow(length(regions))
colors  <- c("#FF0000","#CCFF00","#00FF66","#0066FF","#CC00FF")

pcum <- ggplot(df.cum, aes(x=Dates, y=Cases, fill=Regions)) + 
  geom_area(alpha=alpha) +
  xlab("") + ylab("Confirmed cases (per million people)") + 
  labs(title = "a) Cumulative confirmed cases per region in Africa")+
  scale_fill_manual(values=colors)

pdiff <- ggplot(df.diff, aes(x=Dates, y=Cases, fill=Regions)) + 
  stat_smooth(data = df.diff, alpha = alpha, span = 1/7, geom = 'area', method = 'loess') +
  xlab("") + ylab("New cases (per million people)") +
  labs(title = "b) Daily new confirmed cases per region in Africa",
         caption = "Period : January 1, 2020 to September 30, 2022 \nSource : JHU/CSSE")  +
  scale_fill_manual(values=colors)

grob <- pcum + pdiff + plot_layout(nrow=2, guides = "collect") & theme(legend.position = 'bottom')

ggsave(filename = "graph/cases_per_regions.png", grob, dpi = 500, scale = 1, width = 20, height = 22, unit = "cm")
print("finished")