---
title: "A Vignette to Bootstrap Panel Quantile Regression"
author: "Riki Saito"
date: "December 3, 2015"
output: html_document
---


First, we will view the summary of the example data used in this vignette.



```
## Loading required package: Formula
## Loading required package: SparseM
## 
## Attaching package: 'SparseM'
## 
## The following object is masked from 'package:base':
## 
##     backsolve
```

```
##     Station        Segment           Year          Precip        
##  5438500:  70   Min.   :0.000   1968   : 109   Min.   :0.000024  
##  5439500:  70   1st Qu.:1.000   1969   : 109   1st Qu.:0.717553  
##  5447500:  70   Median :1.000   1970   : 109   Median :1.170675  
##  5545750:  70   Mean   :1.121   1971   : 109   Mean   :1.270443  
##  5540500:  69   3rd Qu.:1.000   1972   : 109   3rd Qu.:1.597410  
##  5542000:  69   Max.   :4.000   1966   : 108   Max.   :6.455270  
##  (Other):3646                   (Other):3411                     
##    Urban.Frac      Present.Urban.Frac Original.Peaks 
##  Min.   :0.00000   Min.   :0.0000     Min.   :    3  
##  1st Qu.:0.06173   1st Qu.:0.1958     1st Qu.:  185  
##  Median :0.25994   Median :0.5798     Median :  502  
##  Mean   :0.33843   Mean   :0.5263     Mean   : 1251  
##  3rd Qu.:0.58403   3rd Qu.:0.8220     3rd Qu.: 1270  
##  Max.   :0.98398   Max.   :1.0000     Max.   :25400  
## 
```

Let us first examine the relationship of the response with the predictor, and the boxplot of the response by "firm" levels to motivate the use of a panel quantile regression.


```r
# Motivation for method
plot(data$Urban.Frac, log10(data$Original.Peaks), xlab="Urban.Frac",ylab="log10(Peaks)")
```

![plot of chunk unnamed-chunk-2](https://github.com/rjsaito/bpqr/blob/master/Vignette_files/figure-html/unnamed-chunk-2-1.png) 

```r
boxplot(log10(Original.Peaks) ~ Station, data)
```

![plot of chunk unnamed-chunk-2](https://github.com/rjsaito/bpqr/tree/master/Vignette_files/figure-html/unnamed-chunk-2-2.png) 


We can argue the use of a quantile regression from the first plot, and the use of a panel regression from the second plot. Now let us try applying a panel quantile regression on this data. The first step is to perform a fixed effects panel regression on this data.


```r
formula = log10(Original.Peaks) ~ Urban.Frac;
plm  <- plm(formula,data=data,model="within");
prelim  <- cbind(coef(plm));    # This vector is K x 1 - only slopes.

hatalpha.i  <- cbind(fixef(plm));      	                  # dimension n x 1
all.firm <- gsub( "-.*$|\\..*$", "", row.names(data))
all.firm.fe <- merge(cbind(table(all.firm)),cbind(hatalpha.i), by="row.names")
intercept <- sum(row.prod(all.firm.fe[,-1]))/nrow(data)  #weighted mean of fixed effects
adj.hatalpha <- rep(hatalpha.i, table(all.firm)) - intercept
hist(hatalpha.i)
```

![plot of chunk unnamed-chunk-3](https://github.com/rjsaito/bpqr/tree/master/Vignette_files/figure-html/unnamed-chunk-3-1.png) 

From the panel regression, we can see the varying intercepts of different "firms". We will remove this heterogeneity in the response, and the apply quantile regression.



```r
tau = c(.01,.05,seq(.1,.9,by=.1),.95,.99)
mf <- model.frame(formula, data)
y <- model.response(mf, "numeric")
x <- model.matrix(formula, data)
y.adj <- y - adj.hatalpha     #new y values adjusted for fixed effects
fm <- update.formula(formula, as.formula('y.adj ~ .'))
index = names(attr(data,"index"))
rqdf <- pdata.frame(cbind(data,y.adj),index=index)   #might need to edit
z <- rq(fm, data=rqdf, tau)
#z <- rq(y.adj~., data=data.frame(y.adj,x[,-1]), tau)
z$fe <- hatalpha.i
z
```

```
## Call:
## rq(formula = fm, tau = tau, data = rqdf)
## 
## Coefficients:
##             tau= 0.01 tau= 0.05 tau= 0.10 tau= 0.20 tau= 0.30 tau= 0.40
## (Intercept) 1.7574098 2.0192620 2.1836002 2.3292450 2.4237670 2.4982138
## Urban.Frac  0.8245286 0.7126698 0.5965053 0.5179786 0.4781038 0.4449967
##             tau= 0.50 tau= 0.60 tau= 0.70 tau= 0.80 tau= 0.90 tau= 0.95
## (Intercept) 2.5540395 2.6077825 2.6744599 2.7555898 2.8575315 2.9627968
## Urban.Frac  0.4336063 0.4237011 0.4076257 0.3699474 0.3657407 0.3708973
##             tau= 0.99
## (Intercept) 3.2196072
## Urban.Frac  0.2976004
## 
## Degrees of freedom: 4064 total; 4062 residual
```

```r
plot(data$Urban.Frac, y.adj, xlab="Urban.Frac")
```

![plot of chunk unnamed-chunk-4](https://github.com/rjsaito/bpqr/tree/master/Vignette_files/figure-html/unnamed-chunk-4-1.png) 

```r
plot(data$Urban.Frac, y.adj)
for(i in seq_len(length(tau))) abline(coef(z)[1,i],coef(z)[2,i])
```

![plot of chunk unnamed-chunk-4](https://github.com/rjsaito/bpqr/tree/master/Vignette_files/figure-html/unnamed-chunk-4-2.png) 


Now with bootstrapping



```
## Bootstrap Complete. 
## Time elapsed:  16.564 seconds
```

![plot of chunk unnamed-chunk-5](https://github.com/rjsaito/bpqr/tree/master/Vignette_files/figure-html/unnamed-chunk-5-1.png) 
