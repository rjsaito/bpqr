wd = "C:/Users/Riki/Dropbox/USGS/Work/bpqr/"

#bpqr
sample_dat = read.csv(paste(wd, "bpqr_sample_data.csv",sep=""))

library(plm)
library(quantreg)
library(matrixStats)



#only fixed effect
#ids must be "firm","year"

formula = log(Original.Peaks) ~ Urban.Frac + Precip
#getAnywhere(lm.fit)

#use x and y instead of formula?
bpqr.fit <- function(formula, ids, B = 1000, effect="within", taus = c(.5,.8,.9,.96,.98,.99,.995,.998))
  #cl <- match.call()
  #mf <- match.call(expand.dots = FALSE)

  n <- nrow(data)
  pdata <- pdata.frame(data, index=ids, drop.index=T)
  pmod <- plm(formula, data = pdata, model=effect)

  hatalpha.i <- cbind(fixef(pmod))
  firm.size <- cbind(table(data[,ids[1]]))
  sumprod.hatalpha.i <- row.prod(merge(hatalpha.i, firm.size, by="row.names")[,-1])
  intercept <- sumprod.hatalpha.i/n
  hatalpha <- rep(hatalpha.i-intercept,firm.size)

  newdata <- model.frame(formula, data)
  newdata[,1] <- newdata[,1] - hatalpha      # Transformed Y

  # run quant reg
  ntau = length(taus); nvar <- ncol(newdata)
  coef <- matrix(0,ntau,nvar)
  for(i in 1:length(taus)) coef[i,] <- coef(rq(formula,taus[i],data=newdata))

  #---------------------- Bootstrap SE ----------------------------------
  library(splitstackshape)
  R <- 1000
  K <-length(df)-2;
  d.f=data.frame[,-c(4:5,8)]

  # Re-shape to wide to resample individual with their times
  names(d.f)[5]="UF"
  newdf	<-reshape(d.f,idvar="firm",timevar="year",direction="wide");

  bst.list=list(NULL)
  for(i in 1:length(taus2)){
    assign(paste("bst.tau",tau[i],sep=""), matrix(0,R,K))
    bst.list[[i]] =  get(paste("bst.tau",tau[i],sep=""))
  }

  n=length(unique(data.frame$firm))
  pb <- winProgressBar(title = "progress bar", min = 0,max = R, width = 300)

  Sys.time() -> start
  for (j in 1:R){
    # Generate random indices out of the n individuals
    ind<-sample(1:n,n,replace=T); # for balanced data

    # Get the bootstrap data
    bdata<- newdf[ind,]; # bootstrap data
    # rename firms to prevent duplicates
    bdata$firm=rep(c(1:n))

    # Reshape the bootstrap data back to long version to apply estimator
    # for unbalanced panel
    rs.bdata<-na.omit(Reshape(bdata,id.vars="firm",var.stubs=c("Peak","Precip","UF"),sep="."))
    rs.bdata$time <- rs.bdata$time+1944
    setnames(rs.bdata,c("time","UF"),c("year","Urban.Frac"))
    rownames(rs.bdata)=NULL
    nrow(rs.bdata)

    # quant reg
    data    <- pdata.frame(rs.bdata, index = c("firm", "year"), drop.index = T, row.names = T);
    within  <- plm(Eqfor,data=data,model="within");
    prelim  <- cbind(coef(within));    # This vector is K x 1 - only slopes.

    hatalpha.i  <- cbind(fixef(within));      	                  # dimension n x 1
    sumprod.hatalpha=sum(hatalpha.i*as.vector(table(rs.bdata$firm)))
    intercept <- sumprod.hatalpha/nrow(rs.bdata)
    hatalpha    <- array(,dim=c(nrow(rs.bdata),1))

    for (i in 1:n) {
      seg.subs = which(rs.bdata$firm==i)
      hatalpha[seg.subs,1] = hatalpha.i[i] - intercept
    }

    dataframe <- rs.bdata
    # remove fixed effects
    dataframe$Peak <- rs.bdata$Peak-hatalpha;     # Transformed Y

    # remove precip
    # precip.adj=dataframe[,3]-dataframe[,4]*within$coefficients[1]
    FE.adj=dataframe$Peak

    # run new quant reg without precip
    hattheta   <- coef(rq(Peak~Urban.Frac+Precip,taus2,data=dataframe));
    theta=cbind(hattheta);

    for(i in 1:length(taus2)){
      bst.list[[i]][j,] = as.vector(theta[,i])
    }
    Sys.sleep(0.1); setWinProgressBar(pb, j, title=paste( round(j/R*100, 0),"% done"))
  }
  close(pb)
  Sys.time() -> end
  time=end-start

  cat(" Version  :",outversion,"\n","Bootstrap: Done","\n")







