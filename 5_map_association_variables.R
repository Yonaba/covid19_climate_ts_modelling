setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)
library(gridExtra)
library(dplyr)
library(stringr)
library(patchwork)

add_param <- function(df) {
  cli.par <- str_replace(df$Model,"ARIMAX/","")
  cli.par <- str_split(cli.par,"_",simplify = T)
  cli.par <- cli.par[,1]
  df$cli.par <- cli.par
  return (df)
}

world <- ne_countries(scale = "medium", returnclass = "sf")

df <- read.csv("models/models_pm_stri_daily_f.csv", header = T, sep = ",", dec = ".")
df <- df[!is.na(df$Coef),]
df.pos <- df[df$rho.spearman > 0,]
df.neg <- df[df$rho.spearman < 0,]

df.pos <- df.pos[order(df.pos$Code_ISO, -df.pos$rho.spearman),]
df.neg <- df.neg[order(df.neg$Code_ISO, df.neg$rho.spearman),]

#df.pos <- distinct(df.pos, Code_ISO, .keep_all = T)
#df.neg <- distinct(df.neg, Code_ISO, .keep_all = T)

#rownames(df.pos) <- df.pos$Code_ISO
#rownames(df.neg) <- df.neg$Code_ISO

df.pos <- add_param(df.pos)
df.neg <- add_param(df.neg)
colnames(df.pos)[1] <- colnames(df.neg)[1] <- "iso_a3"

africa <- data.frame(read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")$Code_ISO)
colnames(africa) <- "iso_a3"
africa <- merge(world, africa, by = "iso_a3")

colours <- c("#8FBDDD","#E8F7B8","#E06F27","#C44A23","#D13140","#539FB7","#68AF62","#CCA28E","#8E063B")
names(colours) <- c("pr","tdew","tmin","tmoy","tmax","wspd","rh","ah","insol")
pname <- c("Rainfall", "Dew point temperature", "Minimum temperature", "Average temperature",
           "Maximum temperature", "Wind speed", "Relative humidity", "Absolute humidity", "Insolation")

make_plot_list <- function(df, param_list, pname, africa) {
  pl.list = list()
  i <- 0
  for (param in names(param_list)) {
    i <- i+1
    countries <- df[df$cli.par == param,]
    countries <- distinct(countries, iso_a3, .keep_all = T)
    countries <- merge(africa, countries, by = "iso_a3", all.x = T) 
    
    pl <- ggplot(data = countries) +
      geom_sf(aes(fill = rho.spearman)) +
      annotation_scale(location = "bl", line_width = 0.5) +
      annotation_north_arrow(location = "tr", which_north = "true",
                             style = north_arrow_fancy_orienteering) +
      coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
      labs(title = paste0(letters[i],") ", pname[i], " (",param,")"), fill = expression(paste("Spearman's ", rho)))
    
    pl.list[[i]] <- pl
  }
  return (pl.list)
}

pl.pos <- make_plot_list(df.pos, colours, pname, africa)
pl.neg <- make_plot_list(df.neg, colours, pname, africa)

grob.pos <- wrap_plots(pl.pos, ncol=3, guides = "collect") & 
  scale_fill_continuous(limits=c(0,0.35), na.value=NA) &
  theme(legend.position = 'bottom', legend.key.size = unit(1,"cm"))

grob.neg <- wrap_plots(pl.neg, ncol=3, guides = "collect") & 
  scale_fill_continuous(limits=c(-0.35,0),na.value=NA) &
  theme(legend.position = 'bottom', legend.key.size = unit(1,"cm"))

ggsave(filename = "graph/param_clim_pos.png", grob.pos, dpi = 500, scale = 1.25, width = 8, height = 9)
ggsave(filename = "graph/param_clim_neg.png", grob.neg, dpi = 500, scale = 1.25, width = 8, height = 9)
print("finished")
