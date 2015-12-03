# Bootstrap Panel Quantile Regression

# example
library(plm); library(quantreg);
wd = "C:/Users/Riki/Dropbox/USGS/Work/bpqr/"
data = read.csv(paste(wd, "bpqr_sample_data.csv",sep=""))
data = pdata.frame(data, index=c("Station","Year"))
formula = log10(Original.Peaks) ~ Urban.Frac + Precip
tau = c(.25,.5,.75)
bpqr(formula, data, B = 10)


bpqr <- function(formula, data, B = 100, firm = 1, method="within", tau = .5){
  #cl <- match.call()
  #mf <- match.call(expand.dots = FALSE)

  # Re-shape to wide to resample individual with their times
  key <- names(attr(data,"index"))
  value <- all.vars(formula)
  dt <- data[,c(key,value)]

  firm.var <- key[firm]
  time.var <- key[-firm]

  widedt	<- reshape(dt, idvar=firm.var, timevar=time.var, direction="wide")

  n <- length(unique(data[,firm.var]))   # n panel samples
  K <- ncol(model.matrix(formula,data))  # parameters

  param <- array(0,dim=c(B,K,length(tau))) #initialize output data

  pb <- winProgressBar(title = "progress bar", min = 0, max = B, width = 300)
  Sys.time() -> start

  for (i in 1:B){
    # Generate random indices out of the n individuals
    samp <- sample(1:n,n,replace=T); # for balanced and unbalanced data

    wide_rdt <- widedt[samp,]    # bootstrap data
    wide_rdt[,firm.var] <- 1:n     # rename id to prevent duplicates

    # Reshape the bootstrap data back to long version to apply estimator
    rdt <- data.frame(na.omit(reshape(wide_rdt, direction="long")))
    names(rdt) <- c(key,value)
    rdt <- rdt[order(rdt[firm.var], rdt[time.var]),]
    row.names(rdt) <- paste(rdt[,firm.var],rdt[,time.var],sep=".")
    rpdt    <- pdata.frame(rdt, index = key, drop.index = T, row.names = T)

    # panel quant reg
    z <- pqr(formula, data=rpdt)

    # write result
    theta   <- coef(z)

    if(length(tau) == 1) param[i,,1] <- coef(z) else
    if(length(tau) > 1) {for(j in 1:length(tau)) param[i,,j] <- coef(z)}

    Sys.sleep(0.1); setWinProgressBar(pb, i, title=paste( round(i/B*100, 0),"% done"))
  }
  close(pb)
  Sys.time() -> end
  cat("Bootstrap Complete. \nTime elapsed: ", round(difftime(end, start, units="secs"),3) ,"seconds")

  colnames(param) <- names(theta)
  return(param)
}





