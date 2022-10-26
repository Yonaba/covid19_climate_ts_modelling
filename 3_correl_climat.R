Sys.setenv(TZ = "UTC")
setwd("D:/MOD_COVID19/MOD/")

library(rstatix)
library(ggcorrplot)
library(patchwork)

countries <- read.csv("data/coord_africa.csv",header = TRUE, sep = ",", dec = ".")
rownames(countries) <- iso_codes <- countries$Code_ISO
params <- c("pr","tdew","tmin","tmoy","tmax","wspd","wspd10","rh","ah","insol","ps")

list.regions.data <- list.regions.cor <- list.regions.pval <- 
  sapply(unique(countries$Regions),function(x) NULL)

for (name in names(list.regions.data)) {
  list.regions.data[[name]] <- list.regions.cor[[name]] <- 
    list.regions.pval[[name]] <- data.frame(matrix(nrow = 0, ncol = length(params)))
  
  colnames(list.regions.data[[name]]) <- colnames(list.regions.cor[[name]]) <- 
    colnames(list.regions.pval[[name]]) <- params
}

for (iso_code in iso_codes) {
  #iso_code <- "BFA"
  print(iso_code)
  iso_region <- countries[iso_code,"Regions"]
  df <- read.csv(paste0("data/weekly/",iso_code,"_DAT.csv"),
                 header = TRUE, sep = ",", dec = ".")
  df <- df[,3:ncol(df)]
  colnames(df) <- params
  
  df$wspd10 <- NULL
  df$ps <- NULL
  
  list.regions.data[[iso_region]] <- rbind(list.regions.data[[iso_region]], df)
}

for (region in names(list.regions.data)) {
  #region <- "AN"
  print(paste("Processing Spearman:",region))
  cor.mat <- data.frame(cor_mat(list.regions.data[[region]], method = "spearman", 
                     alternative = "two.sided", conf.level = 0.95))
  cor.pmat <- data.frame(cor_pmat(list.regions.data[[region]], method = "spearman", 
                     alternative = "two.sided", conf.level = 0.95)) 
  rownames(cor.mat) <- rownames(cor.pmat) <- cor.mat$rowname
  cor.mat$rowname <- cor.pmat$rowname <- NULL
  
  for (i in 1:length(cor.pmat)) cor.pmat[i,i] <- 1 # Force diagonal to be blank  
  
  list.regions.cor[[region]] <- cor.mat
  list.regions.pval[[region]] <- cor.pmat
}

labels <- c("a) Northern Africa (NAf)",
            "b) Central Africa (CAf)",
            "c) Western Africa (WAf)",
            "d) Southern Africa (SAf)",
            "e) Eastern Africa (EAf)")

labels <- data.frame(labels)
rownames(labels)<- names(list.regions.cor)

plist <- list()
i <- 0
for (region in c("NAf","WAf","CAf","EAf","SAf")) {
  i <-i+1
  cor.plot <- ggcorrplot(list.regions.cor[[region]], method = "square",hc.order = F, type = "lower",
                         title = labels[region,1],
                         legend.title = "Spearman's \u03C1",
                         outline.color = "white", lab = T,
                         lab_size = 3, p.mat = list.regions.pval[[region]], 
                         sig.level = 0.05, insig = "blank", show.diag = F, 
                         ggtheme = ggplot2::theme_gray)
    theme(legend.key.size = unit(1.75, "cm"))
  #plot(cor.plot)
  plist[[i]] <- cor.plot
  #ggsave(filename = paste0("graph/corr_climate_",region,".png"), dpi = 500, scale = 1.75)  
}

plist[[6]] <- guide_area()

grob <- wrap_plots(plist, ncol=3, guides = "collect")

ggsave(filename = "graph/corr_climate.png", grob, dpi = 500, scale = 1.25, width = 25, height = 20, unit = "cm")





