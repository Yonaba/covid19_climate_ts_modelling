setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(viridis)
library(sf)

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(gridExtra)

world <- ne_countries(scale = "medium", returnclass = "sf")

df <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")
countries <- df$Code_ISO

df.conf <- read.csv("data/covid_cases_data_t.csv", header = T, sep = ",", dec = ".")
colnames(df.conf)[1] <- "Code_ISO"
rownames(df.conf) <- countries

df.conf["ESH",] <- df.conf["MAR",]
df.conf["ESH",]$Code_ISO <- "ESH"

dates <- c("June_2020", "December_2020","June_2021", "December_2021", "June_2022", "September_2022")
colnames(df.conf) <- c("iso_a3",dates)

conf.africa <- merge(world, df.conf, by = "iso_a3")

#dev.off()

breaks.conf <- c(1,100,10000,500000)
legend.y <- 0.3

p1 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = June_2020)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "June 30, 2020", fill="") +
  theme(legend.position = c(0.12, legend.y), 
        legend.background = element_rect(fill="lightgrey"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))


p2 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = December_2020)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "December 31, 2020",fill="") +
  theme(legend.position = c(0.12, legend.y), 
        legend.background = element_rect(fill="lightgrey"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))

p3 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = June_2021)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "June 30, 2021",fill="") +
  theme(legend.position = c(0.12, legend.y), 
        legend.background = element_rect(fill="lightgrey"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))

p4 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = December_2021)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "December 31, 2021",fill="") +
  theme(legend.position = c(0.12, legend.y), 
        legend.background = element_rect(fill="lightgrey"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))

p5 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = June_2022)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "June 30, 2022",fill="") +
  theme(legend.position = c(0.12, legend.y), 
        legend.background = element_rect(fill="lightgrey"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))

p6 <- ggplot(data = conf.africa) +
  geom_sf(aes(fill = September_2022)) +
  scale_fill_continuous(low="white", high="red", na.value = "white",
                        breaks = breaks.conf, trans = "log", labels = breaks.conf,
                        limits = c(head(breaks.conf,1), tail(breaks.conf,1))) + 
  annotation_scale(location = "bl", line_width = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-20, 52), ylim = c(-35,35)) +
  labs(title = "September 30, 2022",fill="") +
  theme(legend.position = c(0.12, legend.y), 
        legend.background = element_rect(fill="lightgrey"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))

grob.cases <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

ggsave(filename = "graph/cloropleth_cases.png", grob.cases, dpi = 500, scale = 1.85,
       width = 25, height = 15, units = "cm")

dev.off()
print("finished")