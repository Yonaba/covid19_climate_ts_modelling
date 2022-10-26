rescale.vector <- function(x, minv, maxv) {
  if (all(x==0)) return (x)
  return ((((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))*(maxv-minv)) + minv)
}

rescale.df <- function(df, minv, maxv) {
  df.out <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  for (col in colnames(df)) {
    rcol <- (((df[,col] - min(df[,col], na.rm = T))/
                (max(df[,col], na.rm = T)-min(df[,col], na.rm = T)))*
               (maxv - minv)) + minv
    df.out <- cbind(df.out, rcol)
  }
  colnames(df.out) <- colnames(df)
  rownames(df.out) <- rownames(df)
  return (df.out)
}

# fonction pour arrondi au multiple
round_up <- function(x,to=10) { to*(x%/%to + as.logical(x%%to))}

prewhiten <- function (x, y, ndiff, criterion) {
  fit <- auto.arima(y, d = ndiff, ic = criterion, trace = F, method = "ML", seasonal = F)
  res.y <- residuals(fit)
  res.x <- residuals(Arima(x, model = fit))
  return (cbind(res.x, res.y))
}

get.sig.lags <- function(x, y, ndiff, criterion, LAG.MAX) {
  res <- prewhiten(x, y, ndiff, criterion)
  res.ccf <- ccf(res[,1], res[,2], na.action = na.omit, type = "correlation", 
                 lag.max = LAG.MAX, plot = F)
  u.ci <-  qnorm((1 + 0.95)/2)/sqrt(res.ccf$n.used)
  lags <- cbind(-LAG.MAX:0,head(res.ccf$acf[,,1], LAG.MAX+1))
  sig.lags <- c()
  for (i in NROW(lags):1) {
    if ((abs(lags[i,2])) > u.ci) sig.lags <- append(sig.lags, lags[i,1])
  }
  return (sig.lags)
}