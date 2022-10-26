setwd("D:/MOD_COVID19/MOD")
Sys.setenv(TZ = "UTC")

library(ape)

df <- read.csv("moran/data_moran2.csv", header = T, sep = ",", dec = ".")
pop <- read.csv("data/coord_africa.csv", header = T, sep = ",", dec = ".")$Pop

m.dist <- as.matrix(dist(cbind(df$Lon, df$Lat)))
m.dist.inv <- 1/m.dist
diag(m.dist.inv) <- 0

df.raw <- df[,4:ncol(df)]
df.raw <- (df.raw/1e6)*pop
df <- df[,4:ncol(df)]

df.out <- df.raw.out <- data.frame(matrix(nrow=0, ncol=4))
colnames(df.out) <- colnames(df.raw.out) <- c("observed","expected","sd","p.value")

for (i in 1:ncol(df)) {
  df.out[nrow(df.out)+1,] <- Moran.I(df[,i], m.dist.inv)
  df.raw.out[nrow(df.raw.out)+1,] <- Moran.I(df.raw[,i], m.dist.inv)
}

dates <- c("2020-06-30", "2020-12-31",
           "2021-06-30", "2021-12-31",
           "2022-06-30", "2022-09-30")
df.out <- data.frame(dates,df.out)
write.csv(df.out, file = paste0("models/moran.csv"), row.names = F)
print("finished")
