library(AER)
library(stargazer)
library(car)
library(wooldridge)
data('k401ksubs')
dat <- k401ksubs

# i)
dat <- subset(dat, marr==0 & fsize > 1 & male==0)

res <- lm(nettfa~inc+age, data = dat)

(summary(res))

# Call:
#   lm(formula = nettfa ~ inc + age, data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -122.66  -11.12   -2.28    5.46  957.12 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -38.08715    5.29517  -7.193 1.13e-12 ***
#   inc           1.06972    0.08815  12.135  < 2e-16 ***
#   age           0.50835    0.11938   4.258 2.22e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 38.25 on 1170 degrees of freedom
# Multiple R-squared:  0.133,	Adjusted R-squared:  0.1315 
# F-statistic: 89.73 on 2 and 1170 DF,  p-value: < 2.2e-16

stargazer(res, type="text")

# ===============================================
#   Dependent variable:    
#   ---------------------------
#   nettfa           
# -----------------------------------------------
#   inc                          1.070***          
#   (0.088)          
# 
# age                          0.508***          
#   (0.119)          
# 
# Constant                    -38.087***         
#   (5.295)          
# 
# -----------------------------------------------
#   Observations                   1,173           
# R2                             0.133           
# Adjusted R2                    0.132           
# Residual Std. Error     38.247 (df = 1170)     
# F Statistic          89.733*** (df = 2; 1170)  
# ===============================================
#   Note:               *p<0.1; **p<0.05; ***p<0.01

# one year more of age, increases the financial wealth by in average 500 dollars
# a thousand more dollars of income increases in average the wealth by 1000 dollars

# I am surprised that in average people only save 500 dollars every year,
# since their wealth increases in average by only 500 dollars

# ii)

# the intercept predicts that if a person has 0 years and no income it would have
# a financial wealth of -38 thousand dollars. This estimate is clearly wrong,
# because when you are born your wealth is 0 (if you don't count the wealth of)
# your parents
# the incorrect intercept is a result from the fact that few samples have 
# small age and no income.

# iii)

dat$newY = dat$nettfa -0.8*dat$age

res1 <- lm(newY~inc+age, data = dat)


(b1 <- coef(res1))

(s1 <- sqrt(diag(vcov(res1))))

(t1 <- b1/s1)

qt(.975, nobs(res1)-3)

(pt(t1[3], nobs(res1) - 3))

# we can't reject with 1% significance

# iv)

res2 <- lm(nettfa~inc, data=dat)

# both values are very similar, because income and age are somewhat independent
# after you reach a certain age. Thus after you remove the age from the regression
# their effects don't get captured by the income

# v)

linearHypothesis(res, c("age=0.0", "inc=0.0"))
# 
# Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
# 1   1172 1974019                                  
# 2   1170 1711494  2    262525 89.733 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# we can easily reject the null hyphotesis at 5% since the p-value was less
# than 2.2e-16

# Question 2)
# i)
data('htv')
dat <- htv

dat <- subset(dat, west==1)

res <- lm(educ~motheduc+fatheduc+abil+I(abil^2), data =dat)

summary(res)

# Call:
#   lm(formula = educ ~ motheduc + fatheduc + abil + I(abil^2), data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.4458 -1.4620 -0.0741  1.1566  5.9833 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.96622    0.86004   9.263  < 2e-16 ***
#   motheduc     0.22266    0.07434   2.995 0.003083 ** 
#   fatheduc     0.09390    0.04507   2.084 0.038447 *  
#   abil         0.34157    0.09271   3.684 0.000294 ***
#   I(abil^2)    0.07606    0.02398   3.171 0.001753 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.942 on 203 degrees of freedom
# Multiple R-squared:  0.3588,	Adjusted R-squared:  0.3462 
# F-statistic:  28.4 on 4 and 203 DF,  p-value: < 2.2e-16

stargazer(res, type="text")

# ==============================================
#   Dependent variable:
#   ---------------------------
#   educ
# -----------------------------------------------
#   motheduc                     0.223***
#   (0.074)
# 
# fatheduc                      0.094**
#   (0.045)
# 
# abil                         0.342***
#   (0.093)
# 
# I(abil2)                     0.076***
#   (0.024)
# 
# Constant                     7.966***
#   (0.860)
# 
# -----------------------------------------------
#   Observations                    208
# R2                             0.359
# Adjusted R2                    0.346
# Residual Std. Error      1.942 (df = 203)
# F Statistic           28.399*** (df = 4; 203)
# ===============================================
#   Note:               *p<0.1; **p<0.05; ***p<0.01

linearHypothesis(res, "I(abil^2)=0")
# the p-value of this hyphotesis is less sthan 1%, thus we can easily
# reject it

# ii)

linearHypothesis(res, "motheduc=fatheduc")
# p-value is 21%, thus it is not easy to reject the null hyphotesis

# iii)

res1 <- lm(educ~motheduc+I(motheduc+fatheduc)+abil+I(abil^2), data = dat)

summary(res1)$coefficients[,4]

# (Intercept)               motheduc I(motheduc + fatheduc) 
# 2.932357e-17           2.101218e-01           3.844695e-02 
# abil              I(abil^2) 
# 2.939591e-04           1.752787e-03 

# just as before the p-value remains 21%, and it can't easily be rejected

# iv)

linearHypothesis(res, "motheduc=fatheduc")
# p-value is 21%, thus it is not easy to reject the null hyphotesis

# v)
res2 <- lm(educ~tuit18+tuit17+motheduc+fatheduc+abil+I(abil^2), data =dat)

linearHypothesis(res2, c("tuit18=0.0", "tuit17=0.0"))

# we can reject the null hyphotesis with significance 5%,
# thus yes they are jointly significant

# vi)

(cor(dat$tuit17, dat$tuit18))
# [1] 0.9844963

# it is preferred to use the average because when you have multicolinearity
# in a model, this reduces drastically the precision of the estimates

res3 <- lm(educ~I((tuit18+tuit17)/2)+motheduc+fatheduc+abil+I(abil^2), data =dat)

# the coefficient of the average is equal to the sum of the coefficients of each tuitiion
# year.

# vii) it doesn't make sense when interpreted from a casual perspective,
# since the increase of unit of tuition increases the education level by 0.08.
# Because more expensive tuition makes sense that the education level would be
# lower since, education becomes more expesive and thus less people would want
# to educate
