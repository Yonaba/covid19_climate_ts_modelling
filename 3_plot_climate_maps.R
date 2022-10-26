setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(sf)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(rgeos)
library(gridExtra)
library(dplyr)
library(stringr)
library(patchwork)
library(paletteer)

param <- c("pr", "tdew", "tmin", "tmoy", "tmax", "wspd","wspd10","rh","ah","insol","ps")
countries <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")$Code_ISO
ddf <- data.frame(matrix(nrow=0, ncol=length(param)+1))
colnames(ddf) <- c("iso_a3",param)

for (iso_code in countries) {
  #iso_code <- "BFA"
  df <- read.csv(paste0("data/weekly/",iso_code,"_DAT.csv"), header = T, sep = ",", dec = ".")
  df <- df[,3:ncol(df)]
  pr <- df[,1]
  df <- colMeans(df)
  df["pr"] <- sum(pr)  
  df <- c(iso_code,as.numeric(df))
  ddf[nrow(ddf)+1,] <- df
}

rownames(ddf) <- countries
ddf[, c(2:ncol(ddf))] <- sapply(ddf[, c(2:ncol(ddf))], as.numeric)
ddf["ESH",] <- ddf["MAR",]
ddf["ESH","iso_a3"] <- "ESH"

world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- merge(world, ddf, by = "iso_a3")

make_plot <- function(data,var,breaks, transfun, rev, title, ftitle, pal) {
  
  colours <- paletteer_c(pal, length(breaks)-1) 
  if (rev) colours <- rev(colours)
  
  pl <- ggplot(data = data, aes(fill = .data[[var]])) +
    geom_sf() +
    annotation_scale(location = "bl", line_width = 0.5) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
    labs(title = title, fill=ftitle) +
    scale_fill_gradientn(colours = colours, trans = transfun,
                         breaks = breaks, labels = breaks)
  
  return(pl)
  
}

pr <- make_plot(africa, "pr", c(0,100,1000,2500,5000,10000), "sqrt",F, "a) Cumulative rainfall (pr)", "pr (mm)","ggthemes::Blue")
tdew <- make_plot(africa, "tdew", c(0,5,10,15,20,25), "identity",F, "b) Dew point temperature (tdew)", "tdew (°C)", "ggthemes::Blue-Green Sequential")
tmin <- make_plot(africa, "tmin", c(5,10,15,20,25), "identity",F, "c) Minimum temperature (tmin)", "tmin (°C)","ggthemes::Orange")
tmoy <- make_plot(africa, "tmoy", c(10,15,20,25,30,35), "identity",F, "d) Average temperature (tmoy)", "tmoy (°C)","ggthemes::Orange-Gold")
tmax <- make_plot(africa, "tmax", c(15,20,25,30,35), "identity",F, "e) Maximum temperature (tmax)", "tmax (°C)","ggthemes::Red")
wspd <- make_plot(africa, "wspd", c(1,2,3,4,5), "identity",F, "f) Wind speed (wspd)", "wspd (m/s)","ggthemes::Blue-Teal")
#wspd10 <- make_plot(africa, "wspd10", c(1,2,3,4,5,6), "identity", F, "Wind speed at 10 m (wspd10)", "wspd10 (m/s)","ggthemes::Classic Blue")
rh <- make_plot(africa, "rh", c(25,40,55,70,85), "identity",F, "g) Relative air humidity (rh)", "rh (%)","ggthemes::Green-Blue Diverging")
ah <- make_plot(africa, "ah", c(5,10,15,20), "identity",F, "h) Absolute air humidity (ah)", "ah (%)","ggthemes::Orange-Blue Diverging")
insol <- make_plot(africa, "insol", c(15,18,21,24), "identity",T, "i) Insolation (insol)", "insol (MJ/m²/d)","grDevices::Heat")
#ps <- make_plot(africa, "ps", c(75,80,85,90,95,100), "identity",F, "Surface pressure (kPa)", "ps (%)","grDevices::BurgYl")

grob <- (pr + tdew + tmin + tmoy + tmax + wspd + rh + ah + insol) + plot_layout(ncol=3)

ggsave(filename = "graph/clim_africa.png", grob, dpi = 500, width = 30, height = 25, scale = 0.55)

print("finished")
