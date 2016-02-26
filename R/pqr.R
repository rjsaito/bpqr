# Panel Quantile Regression

# this function currently only handles fixed effects panel regression
# @formula
# @parameter data - must be pdata


# example
 wd = "C:/Users/Riki/Dropbox/USGS/Work/bpqr/"
 data = read.csv(paste(wd, "bpqr_sample_data.csv",sep=""))
 data = pdata.frame(data, index=c("Station","Year"))
 formula = log10(Original.Peaks) ~ Urban.Frac + Precip
 tau = c(.25,.5,.75)
 pqr(formula, data, tau=tau)

pqr <- function(formula, data, method="within", tau=.5){
  require(plm); require(quantreg);

  pmod  <- plm(formula,data=data,model=method)
  hatalpha.i  <- cbind(fixef(pmod));
  #firm <- sub("^(.*)[-].*", "\\1", row.names(data))
  firm <- attr(data,"index")[,1]
  firm.fe <- merge(cbind(table(firm)),cbind(hatalpha.i), by="row.names")
  intercept <- sum(row.prod(firm.fe[,-1]))/nrow(data)  #weighted mean of fixed effects
  hatalpha0 <- rep(hatalpha.i, table(firm)) - intercept

  mf <- model.frame(formula, data)
  y <- model.response(mf, "numeric")
  #x <- model.matrix(formula, data)
  y.new <- y - hatalpha0     #new y values adjusted for fixed effects
  fm <- update.formula(formula, as.formula('y.new ~ .'))    #use eval instead?
  df <- data.frame(y.new,data)   #might need to edit
  z <- rq(fm, data=df, tau)
  z$fe <- hatalpha.i
  z
}








