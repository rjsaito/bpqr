# Panel Quantile Regression

# @parameter data - must be pdata

# example
# wd = "C:/Users/Riki/Dropbox/USGS/Work/bpqr/"
# sample_dat = read.csv(paste(wd, "bpqr_sample_data.csv",sep=""))
eq = log10(Original.Peaks) ~ Urban.Frac + Precip
library(plm)
pdat = pdata.frame(sample_dat, index=c("Station","Year"))
pqr(qr, pdat)


pqr <- function(formula, data, method="within"){
  mod  <- plm(formula,data=data,model=method)
  prelim  <- cbind(coef(mod));    # This vector is K x 1 - only slopes.
  hatalpha.i  <- cbind(fixef(mod));      	       # dimension n x 1

  firm <- gsub( "-.*$", "", row.names(data))
  firm.fe <- merge(cbind(table(firm)),cbind(hatalpha.i), by="row.names")

  intercept <- sum(row.prod(firm.fe[,-1]))/nrow(data)  #weighted mean of fixed effects
  adj.hatalpha <- rep(hatalpha.i, table(firm)) - intercept

  mf <- model.frame(formula, data)
  mt <- attr(mf, "terms")

  y <- model.response(mf, "numeric")
  y.adj <- y - adj.hatalpha     #new y values adjusted for fixed effects

  #p1, if empty

  x <- model.matrix(mt, mf, contrasts)
  yx <- cbind(y.adj,x)
  z <- plm(y.adj~., data=yx, model=method)

# remove precip
# precip.adj=dataframe[,3]-dataframe[,4]*within$coefficients[1]
FE.adj=dataframe$Peak

# run new quant reg without precip
hattheta   <- coef(rq(Peak~Urban.Frac+Precip,taus2,data=dataframe));
theta=cbind(hattheta);
