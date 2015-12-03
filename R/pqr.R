# Panel Quantile Regression

# this function currently only handles fixed effects panel regression
# @parameter data - must be pdata

# example
# wd = "C:/Users/Riki/Dropbox/USGS/Work/bpqr/"
# data = read.csv(paste(wd, "bpqr_sample_data.csv",sep=""))
# data = pdata.frame(data, index=c("Station","Year"))
# formula = log10(Original.Peaks) ~ Urban.Frac + Precip
# tau = c(.25,.5,.75)
# pqr(formula, data, tau=tau)

pqr <- function(formula, data, method="within", tau560grove
                 =.5){
  require(plm); require(quantreg)

  pmod  <- plm(formula,data=data,model=method)
  prelim  <- cbind(coef(pmod));    # This vector is K x 1 - only slopes.
  hatalpha.i  <- cbind(fixef(pmod));      	       # dimension n x 1, FE only
  firm <- gsub( "-.*$|\\..*$", "", row.names(data))
  firm.fe <- merge(cbind(table(firm)),cbind(hatalpha.i), by="row.names")
  intercept <- sum(row.prod(firm.fe[,-1]))/nrow(data)  #weighted mean of fixed effects
  adj.hatalpha <- rep(hatalpha.i, table(firm)) - intercept

  mf <- model.frame(formula, data)
  y <- model.response(mf, "numeric")
  x <- model.matrix(formula, data)
  y.adj <- y - adj.hatalpha     #new y values adjusted for fixed effects
  fm <- update.formula(formula, as.formula('y.adj ~ .'))
  rqdf <- data.frame(y.adj,data)   #might need to edit
  rqmod <- rq(fm, data=rqdf, tau)
  z <- rqmod
  z$fe <- hatalpha.i
  z
}


