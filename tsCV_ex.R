
# correction de la fonction tsCV {forecast}
tsCV2 <- function (y, forecastfunction, h = 1, window = NULL, xreg = NULL, initial = 0, ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  if (initial >= n) 
    stop("initial period too long")
  tsp(e) <- tsp(y)
  if (!is.null(xreg)) {
    xreg <- ts(as.matrix(xreg))
    if (NROW(xreg) != length(y)) 
      stop("xreg must be of the same size as y")
    xreg <- ts(rbind(xreg, matrix(NA, nrow = h, ncol = NCOL(xreg))), 
               start = start(y), frequency = frequency(y))
  }
  if (is.null(window)) 
    indx <- seq(1 + initial, n - 1L)
  else indx <- seq(window + initial, n - 1L, by = 1L)
  for (i in indx) {
    y_subset <- subset(y, start = ifelse(is.null(window), 
                                         1L, ifelse(i - window >= 0L, i - window + 1L, stop("small window"))), 
                       end = i)
    if (is.null(xreg)) {
      fc <- try(suppressWarnings(forecastfunction(y_subset, 
                                                  h = h, ...)), silent = TRUE)
    }
    else {
      xreg_subset <- subset(xreg, start = ifelse(is.null(window), 
                                                 1L, ifelse(i - window >= 0L, i - window + 1L, 
                                                            stop("small window"))), end = i)
      xreg_future <- subset(xreg, start = i + 1, end = i + 
                              h)
      fc <- try(suppressWarnings(forecastfunction(y_subset, 
                                                  h = h, xreg = xreg_subset, newxreg = xreg_future,...)), 
                silent = TRUE)
    }
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  }
  else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}

#fonction de cross-validation
aafc <- function(y, h, order, xreg = NULL, newxreg = NULL) {
  #print(paste("call aafc", order))
  fit <- Arima(y, order = order, xreg=xreg, method="ML")
  #print(paste("Fit",fit))
  #print(paste("Fit:",paste(arimaorder(fit),collapse=" "),length(y)))
  forecast(fit, xreg = newxreg, h = h)
}

cvf <- function(data, aafc, h = 1, initial = 0, xreg = NULL, order) {
  n <- length(data)
  am_cv <- tsCV2(data, aafc, h = h, initial = initial, xreg = xreg, order = order)
  acc <- accuracy(ts(data[-1] - am_cv[-n]), ts(data[-1]))
  return (acc[,c("ME","MAE","RMSE","MAPE")])
}

diagnose.model <- function(model, data, xreg = NULL, simple = T, MAX.COEF = NULL, lag = 100, label = NULL) {
  model.type <- ifelse(simple,"ARIMA",paste0("ARIMAX/",paste0(label, collapse = ".")))
  model.params <- paste0("(",paste0(arimaorder(model),collapse=","),")")
  acc <- accuracy(model$x, model$fitted)
  model.rmse <- acc[2]
  model.mae <- acc[3]
  #model.mpe <- acc[4]
  #model.mape <- acc[5]
  model.Q <- checkresiduals(model, lag = lag, plot = F)$p.value

  m.coefs <- rep(NA, MAX.COEF)
  if (!simple) {
    m.coefs <- tail(coeftest(amc)[,1],length(p))
    if (length(m.coefs) < MAX.COEF) m.coefs <- append(m.coefs,rep(NA,MAX.COEF - length(m.coefs)))
    m.coefs <- as.numeric(m.coefs)
  }
  return (c(model.type, model.params, model.rmse, model.mae, 
            model.Q, m.coefs))
}

