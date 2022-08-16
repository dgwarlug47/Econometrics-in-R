library(AER)
library(stargazer)
data("GrowthDJ")
dat <- GrowthDJ

# Q1)
fit <- lm(gdpgrowth~gdp60, dat)

# yes the slope of gdp60 is negative but it is not very significant since we
# can't easily reject the null hyphotesis that the slope is zero, we can't
# reject with 10% significance

se <- sqrt(diag(vcov(fit)))

t_test <- fit$coefficients[2]/se[2]
# -1.314309. same value as the t value of the summary output

(pt(t_test, length(dat$oil) - 2))
# 9.5% p value, we to reject the hyphotesis that the slope is smaller than zero with 10%
# significance 10%.

stargazer(fit, type="text")

# ===============================================
#   Dependent variable:    
#   ---------------------------
#   gdpgrowth         
# -----------------------------------------------
#   gdp60                        -0.00003          
# (0.00002)         
# 
# Constant                     4.172***          
#   (0.192)          
# 
# -----------------------------------------------
#   Observations                    116            
# R2                             0.015           
# Adjusted R2                    0.006           
# Residual Std. Error      1.858 (df = 114)      
# F Statistic             1.727 (df = 1; 114)    
# ===============================================
#   Note:               *p<0.1; **p<0.05; ***p<0.01

# The slope of gdp60 seems to be very close to zero with a high margin
# of safety, because of estimated stand error and also
# because of the p-values.

# Whereas the intercept is clearly large (around 4), but the value estimated
# is not reliable because it has a high variance.

# The adjusted R square seems pretty low, thus this model doesn't seem very useful.

# Q 2)

plot(gdpgrowth~gdp60, data=dat, main="gdpgrowth versus gdpg60")
abline(fit)

# Like the Solow growth model's predictions, it seems that gdpgrowth decreases
# when the GDP in the 60s increases. Note however that as discussed in the previous
# question it is not possible to reject the null hyphotesis that the slope is zero
# therefore if there is a negative correlation, it looks like it is quite small,
# in fact close to zero. Thus there is an existing negative relationship, but
# it is quite weak

# Q 3)

dat$gdp60[56] # this is the outlier of the linear regression

datWithoutOutlier <- dat[-c(56), ]

# part 1)

fitWithoutOutlier <- lm(gdpgrowth~gdp60, datWithoutOutlier)

# yes the slope of gdp60 is negative but it is not very significant since we
#  easily reject the null hyphotesis that the slope is zero, even thouth we
# reject with 10% significance

seWithoutOutlier <- sqrt(diag(vcov(fitWithoutOutlier)))

t_testWithoutOutlier <- fitWithoutOutlier$coefficients[2]/seWithoutOutlier[2]
# -1.287452

(pt(t_testWithoutOutlier, length(datWithoutOutlier$oil) - 2)) 
# 10.02% significance

stargazer(fitWithoutOutlier, type="text")

# ===============================================
#   Dependent variable:    
#   ---------------------------
#   gdpgrowth         
# -----------------------------------------------
#   gdp60                         -0.0001          
# (0.0001)          
# 
# Constant                     4.317***          
#   (0.256)          
# 
# -----------------------------------------------
#   Observations                    115            
# R2                             0.014           
# Adjusted R2                    0.006           
# Residual Std. Error      1.860 (df = 113)      
# F Statistic             1.658 (df = 1; 113)    
# ===============================================
#   Note:               *p<0.1; **p<0.05; ***p<0.01

# The slope of gdp60 seems to be very close to zero with a high margin
# of safety, because of estimated stand error and also
# because of the p-values. Note however that the slope is much bigger (in absolute value)
# compared to previous regression including the outlier

# Whereas the intercept is clearly large (around 4), and the variance of the
# estimator intercept is much lower compared to the previous regression

# The adjusted R square seems pretty low, thus this model doesn't seem very useful.

# part 2)

plot(gdpgrowth~gdp60, data=datWithoutOutlier, main="gdpgrowth versus gdpg60 without outlier")
abline(fitWithoutOutlier)

# Like the Solow growth model's predictions, it seems that gdpgrowth decreases
# when the GDP in the 60s increases. Note however that as discussed in the previously
# that the slope is very close to zero
# Thus if there is an existing negative relationship,
# it is quite weak

# Q 4)

dat <- datWithoutOutlier

datFromOECD <- subset(dat, oecd == 'yes')
datNotFromOECD <- subset(dat, oecd == 'no')

# part 1)

fitFromOECD <- lm(gdpgrowth~gdp60, datFromOECD)

# the slope of gdp60 is still negative the p-value is very small now,
# thus we can easily reject the null hyphotesis that the slope is zero

seFromOECD <- sqrt(diag(vcov(fitFromOECD)))

t_testFromOECD <- fitFromOECD$coefficients[2]/seFromOECD[2]
# -4.235412, same as the summary outpt

(pt(t_testFromOECD, length(datFromOECD$oil) - 2)) 
# 0.02% p value


fitNotFromOECD <- lm(gdpgrowth~gdp60, datNotFromOECD)

# yes the slope of gdp60 is negative but is not very significant
# since we can't reject the null hyphotesis even if we choose a very
# high significance level

seNotFromOECD <- sqrt(diag(vcov(fitNotFromOECD)))

t_testNotFromOECD <- fitNotFromOECD$coefficients[2]/seNotFromOECD[2]
# -0.28

(pt(t_testNotFromOECD, length(datNotFromOECD$oil) - 2)) 
# 38% p value

stargazer(fitFromOECD, fitNotFromOECD, type="text")

# =============================================================
#   Dependent variable:           
#   -----------------------------------------
#   gdpgrowth                
# (1)                  (2)        
# -------------------------------------------------------------
#   gdp60                     -0.0002***            -0.00003     
# (0.0001)             (0.0001)     
# 
# Constant                   5.510***             4.188***     
#   (0.418)              (0.311)      
# 
# -------------------------------------------------------------
#   Observations                  22                   93        
# R2                          0.473                0.001       
# Adjusted R2                 0.446                -0.010      
# Residual Std. Error    0.740 (df = 20)      2.029 (df = 91)  
# F Statistic         17.939*** (df = 1; 20) 0.079 (df = 1; 91)
# =============================================================
#   Note:                             *p<0.1; **p<0.05; ***p<0.01

# the R square is much larger for the OECD countries, thus the gdp at 1960
# seems to be a much better predictor for those types of countries

# part 2)

## col=2 for oecd and 1 otherwise
col <- (dat$oecd=="yes")+1

## the pch=20 creates bullets instead of empty points
plot(gdpgrowth~gdp60, dat, col=col, main="gdp growth versus gpdp60 in OECD and non OECD countries",
     xlab="GDP(1960)", ylab="Growth(60-85)", pch=20, lwd=2)
legend("topright", c("non-OECD","OECD"), col=1:2, lty=1,
       lwd=2, pch=20)
abline(fitFromOECD, col="red")
abline(fitNotFromOECD, col="black")

# It seems that Solow Growth model is much more true for OECD countries rather
# than the non OECD countries, this is easily verified by the graph, since the
# the linear regresison of countries
# from OECD have much more negative slope rather than countries not from OECD

# Q 5)

datAmongOilProducers <- subset(dat, oil == 'yes')

fitAmongOilProducers<- lm(gdpgrowth~gdp60, datAmongOilProducers)

# yes the slope of gdp60 is negative but we can't easily reject the null
# rejec the null hyphotesis that the slope is not zero

seAmongOilProducers <- sqrt(diag(vcov(fitAmongOilProducers)))

t_testAmongOilProducers <- fitAmongOilProducers$coefficients[2]/seAmongOilProducers[2]
# -0.5935868

(pt(t_testAmongOilProducers, length(datAmongOilProducers$oil) - 2))
# 28% p value, not enough to reject that the slope is zero with significance
# 10%, thus not enough evidence to support convergence of oil producers

# Q 6)

datWithGoodLiteracy <- subset(dat, literacy60 > 70)

# part 1)

fitWithGoodLiteracy <- lm(gdpgrowth~gdp60, datWithGoodLiteracy)

# yes the slope of gdp60 is negative and it is quite significant (20%)
# we can reject the null hyphotesis that the slope of gdp60 is zero with
# very low significance-levels

seWithGoodLiteracy <- sqrt(diag(vcov(fitWithGoodLiteracy)))

t_testWithGoodLiteracy <- fitWithGoodLiteracy$coefficients[2]/seWithGoodLiteracy[2]
# -3.271373

(pt(t_testWithGoodLiteracy, length(datWithGoodLiteracy$oil) - 2)) 
# 0.12% p value

# stargazer(fitWithGoodLiteracy, type="text")
# 
# ===============================================
#   Dependent variable:    
#   ---------------------------
#   gdpgrowth         
# -----------------------------------------------
#   gdp60                       -0.0003***         
#   (0.0001)          
# 
# Constant                     5.667***          
#   (0.558)          
# 
# -----------------------------------------------
#   Observations                    34             
# R2                             0.251           
# Adjusted R2                    0.227           
# Residual Std. Error       1.472 (df = 32)      
# F Statistic           10.702*** (df = 1; 32)   
# ===============================================
#   Note:               *p<0.1; **p<0.05; ***p<0.01

# The slope of gdp60 seems to be 3x larger (in absolute value) for high literacy countries
# compared to all countries without the outlier. With almost the same variance for the slope


# part 2)

plot(gdpgrowth~gdp60, data=datWithGoodLiteracy, main="gdpgrowth versus gdpg60 in countries with good literacy")
abline(fitWithGoodLiteracy)

# Like the Solow growth model's predictions, it seems that gdpgrowth decreases
# when the GDP in the 60s increases. Note however that as discussed in the previous
# question it is possible to reject by a large margin the null hyphotesis
# that the slope is zero, therefore there is a strong negative correlation.


# Q 7)

datWithHighInvestiment <- subset(dat, invest > 23)

datWithLowInvestiment <- subset(dat, invest < 15)

# part 1)
fitWithHighInvestiment <- lm(gdpgrowth~gdp60, datWithHighInvestiment)

# yes the slope of gdp60 is negative and it is quite significant,
# thus we can reject the null hyphotesis even with very low significance-levels

seWithHighInvestiment <- sqrt(diag(vcov(fitWithHighInvestiment)))

t_testWithHighInvestiment <- fitWithHighInvestiment$coefficients[2]/seWithHighInvestiment[2]
# -3.25315 

(pt(t_testWithHighInvestiment, length(datWithHighInvestiment$oil) - 2)) 
# 0.12%, thus we can safely reject the null hyphotesis that
# the slope is zero


fitWithLowInvestiment <- lm(gdpgrowth~gdp60, datWithLowInvestiment)

# in fact the there is the only example so far where the opposite of 
# Solow's growth is model, that is a positve relationship between
# growth and GDP.

seWithLowInvestiment <- sqrt(diag(vcov(fitWithLowInvestiment)))

t_testWithLowInvestiment <- fitWithLowInvestiment$coefficients[2]/seWithLowInvestiment[2]
# 0.03

(pt(t_testWithLowInvestiment, length(datWithLowInvestiment$oil) - 2)) 
# 51% p value, thus we can't reject the hyphotesis that the slope is zero
# against the hyphotesis that is less than zero

stargazer(fitWithHighInvestiment, fitWithLowInvestiment, type="text")

# part 2)

# =============================================================
#   Dependent variable:           
#   -----------------------------------------
#   gdpgrowth                
# (1)                  (2)        
# -------------------------------------------------------------
#   gdp60                     -0.0003***            0.00000      
# (0.0001)             (0.0001)     
# 
# Constant                   6.052***             3.252***     
#   (0.562)              (0.351)      
# 
# -------------------------------------------------------------
#   Observations                  34                   47        
# R2                          0.249               0.00002      
# Adjusted R2                 0.225                -0.022      
# Residual Std. Error    1.650 (df = 32)      1.772 (df = 45)  
# F Statistic         10.583*** (df = 1; 32) 0.001 (df = 1; 45)
# =============================================================
#   Note:                             *p<0.1; **p<0.05; ***p<0.01

#
# 1 is black, 2 is red
both <- rbind(datWithHighInvestiment, datWithLowInvestiment)
col <- c(rep(c(1), length(datWithHighInvestiment$oil)),  rep(c(2), length(datWithLowInvestiment$oil)))

## the pch=20 creates bullets instead of empty points
plot(gdpgrowth~gdp60, both, col=col, main="Convergence",
     xlab="GDP(1960)", ylab="Growth(60-85)", pch=20, lwd=2)
legend("topright", c("high investiment","low investiment"), col=1:2, lty=1,
       lwd=2, pch=20)
abline(fitWithHighInvestiment, col="black")
abline(fitWithLowInvestiment, col="red")

# Seems that Solow's growth model is more true for countries
# with high investiment, since with countries with a 
# low investiment even have positive slopes, which is exactly
# the opposite of the Solow growth model

# Q 8) we can conclude that the Solow growth model is neither
# always false nor always true, it turns out that if the country
# has major difficulties such as low investiment, low literacy
# or no oil, it seems that it is too "hard" to make it grow
# thus the slope of the gdp60 is sometimes even positive, or at
# least we can't reject the hyphoteis that the slope is not zero. 
# On the other hand when countries have those good traits, then they have
# momentum to grow, thus the Slow growth model is true.

# Q 9) 

fit1 <- lm(gdp60~literacy60, dat)

fit2 <- lm(log(gdp60)~literacy60, dat)

fit3 <- lm(I(log(gdp60))~literacy60, dat)

stargazer(fit1, fit2, type="text")

# ==========================================================
#   Dependent variable:     
#   ----------------------------
#   gdp60       log(gdp60)  
# (1)            (2)     
# ----------------------------------------------------------
#   literacy60                      58.941***      0.020***   
#   (5.531)        (0.002)   
# 
# Constant                         181.478       6.635***   
#   (332.116)       (0.095)   
# 
# ----------------------------------------------------------
#   Observations                       101            101     
# R2                                0.534          0.624    
# Adjusted R2                       0.530          0.620    
# Residual Std. Error (df = 99)   1,958.700        0.559    
# F Statistic (df = 1; 99)        113.581***    164.163***  
#   ==========================================================
#   Note:                          *p<0.1; **p<0.05; ***p<0.01

# Comments: it seems that every percent increase in literacy of the population
# increases the gdp60 by in average 58 dollars, also every percent increase in the literacy
# increases in average the gdp60 by 2 percent

plot(gdp60~literacy60, data=dat, main="gdp60 versus literacy60")
abline(fit1)

plot(log(gdp60)~literacy60, data=dat, main="log gdp60 versus literacy60")
abline(fit2)

# option 1) countries with better literacy rate, have a more skilled workforce
# thus a more productive work force too, which impacts positively in the gdp per capita

# option 2) when a country increases its GDP per capita, the average person
# has more financial resources, thus has more time to invest in their own
# education and also their children education as well.

# option 3) most countries that have a high gdp per capita at the 60s
# belong to europe, and also most countries that have a high literacy
# rate are from Europe too. European countries have high GDP
# because of the colonialism. Also European Countries have a high literacy
# rate because their governments believe that a high literacy would be
# very important to establish a society of greate culture.

