library(AER)
library(stargazer)
library(car)
data("GrowthDJ")
dat <- GrowthDJ

# removing outlier

dat$gdp60[56] # this is the outlier of the linear regression
dat <- dat[-c(56), ]

# a)

res <- lm(gdpgrowth~gdp60+I(gdp60^2), data=dat)

linearHypothesis(res, "I(gdp60^2)=0")

se <- sqrt(diag(vcov(res)))

t_test <- res$coefficients[3]/se[3]
# -2.122037 

(pt(t_test, length(dat$oil) - 2))*2
# 0.03592589

f <- function(x){
  predict(res, data.frame(gdp60 = x))
}

c(f(2000), f(3000), f(4000), f(5000), f(6000), f(7000), f(8000))
# 4.285420 4.428507 4.480613 4.441735 4.311874 4.091031 3.779205 

plot(gdpgrowth~gdp60, data=dat)
curve(f(x), add=TRUE)

# the convergence of gdpgrowth only starts after a certain level of
# gdp60, around 5000

# the estimated average growth is maximized around 4072 gdp60 and the maximum
# average growth is 4.48

# test the Solow prediction is harder in this scenario because
# the Solow prediction is "monotone decreasing" that is more gdp60 means less growth,
# whereas our model/regression is not "monotone", in other words, there is a part where the gdpgrowth is
# decreasing and another part that is increasing, thus our model
# is not always decreasing, which makes it harder to compare with the Solow prediction.
# We would have to ponder for example if the time that the model/regression is decreasing
# is much more than it is increasing.

# b)


res0 <- lm(gdpgrowth~gdp60, data=dat)
res1 <- lm(gdpgrowth~gdp60+literacy60, data=dat)
res2 <- lm(gdpgrowth~gdp60+literacy60+invest, data=dat)
res3 <- lm(gdpgrowth~gdp60+literacy60+invest+oecd, data=dat)

# there is clearly a large difference when we add the literacy60, since the
# the significance of the slope of gdp60 increases a lot (the p-value of the null
# hyhphotesis that slope is zero decreased a lot).
# Note as well that the literacy60 is quite significant in res1,
# but when we add invest and oecd it's p-value increases a lot
# Note as well that the oecd variable doesn't seem very significant, since it's p-value is quite
# large, also note that when we add the oecd variable it increases the gdp60 p-value making
# it less signifcant

get_p_value <- function(res){
  se <- sqrt(diag(vcov(res)))
  
  t_test <- res$coefficients[2]/se[2]
  # -1.314309. same value as the t value of the summary output
  
  return (pt(t_test, length(dat$oil) - 2))
}

c(get_p_value(res0), get_p_value(res1), get_p_value(res2), get_p_value((res3)))
# 0.1002281283 0.0026753698 0.0003107753 0.0059098432 

# just like we saw in assignment3, the gdp60 significance increases
# a lot when we restrict our models to scenarios where variables
# such as literacy60, invest are constant. Thus the Solow prediction
# becomes more accurate under those scenarios.

c(summary(res0)$r.squared, summary(res0)$adj.r.squared)
# [1] 0.014456376 0.005734751

c(summary(res1)$r.squared, summary(res1)$adj.r.squared)
# [1] 0.09992964 0.08156086

c(summary(res2)$r.squared, summary(res2)$adj.r.squared)
# [1] 0.2522361 0.2291094

c(summary(res3)$r.squared, summary(res3)$adj.r.squared)
# [1] 0.2801016 0.2501058


# according to both r squared in adj rsquared the best model
# is res3. Since the number of samples is quite high, compared to the
# number of parameters, the adjusted rsquared is quite similar to
# the values of the raw r squared.


# r squared


f4 <- function(x){
  x*res2$coefficients[2] + mean(dat$literacy60, na.rm=TRUE)*res2$coefficients[3] + mean(dat$invest, na.rm=TRUE)*res2$coefficients[4] + res2$coefficients[1]
}

plot(gdpgrowth~gdp60, data=dat)
curve(f4(x), add=TRUE)

# the reason why the other three models have less observations is because since the
# other three models regress over more parameters, more rows will be ignored
# because of the presence of NA, thus less rows will be used for the regression
# Yes this a problem, specially if there are many NA because than the more parameters
# we add the less observations we will have to do the regression

#c) 

res <- lm(gdpgrowth~gdp60+I(gdp60^2)+literacy60+invest, data=dat)

linearHypothesis(res, "I(gdp60^2)=0")

se <- sqrt(diag(vcov(res)))

t_test <- res$coefficients[3]/se[3]
# -0.4888696 

(pt(t_test, length(dat$oil) - 2))*2
# 0.6258414
# thus the p-value is much larger compared to a), thus we have less confidence that the quadratic term
# is significant compared to a)

f <- function(x){
  x*res$coefficients[2] +x*x*res$coefficients[3] + mean(dat$literacy60, na.rm=TRUE)*res$coefficients[4] + mean(dat$invest, na.rm=TRUE)*res$coefficients[5] + res$coefficients[1]
}

c(f(2000), f(3000), f(4000), f(5000), f(6000), f(7000), f(8000))
# 4.367279 4.147428 3.905608 3.641821 3.356065 3.048342 2.718650 
# clearly the gdpgrowth goes down much more faster compared to a)

plot(gdpgrowth~gdp60, data=dat)
curve(f(x), add=TRUE)

# unlike in a) the convergence of gdpgrowth happens for all values of gdp60

# the estimated average growth is maximized at gdp60=0

# test the Solow prediction is much easier in this scenario compared to a) because
# the Solow prediction is "monotone decreasing" that is more gdp60 means less growth, but
# and that is exactly the trend that our model/regresion in c) follows, thus 
# our regression in c) is compatible with the Solow prediction



