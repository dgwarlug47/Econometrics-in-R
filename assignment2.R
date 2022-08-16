library(wooldridge)
library(stargazer)
data("sleep75")
all_dat <- sleep75
all_dat$sleep <- all_dat$sleep/60/7
dat <- subset(all_dat, marr == 1 & smsa ==1 & gdhlth == 1)

# 2 a)

histogram_and_normal_dist <- function(samples, title) {
  hist(samples, main=title, freq=F)
  curve(dnorm(x, mean(samples), sd(samples)), min(samples), max(samples), add=TRUE, color="violet")
}

stargazer(dat[,c("age", "educ", "male", "sleep")], type="text")

# ==========================================
#   Statistic  N   Mean  St. Dev.  Min   Max  
# ------------------------------------------
#   age       208 39.279  10.556   23     64  
# educ      208 13.567  2.541     8     17  
# male      208 0.635   0.483     0     1   
# sleep     208 7.709   1.014   4.143 10.893
# ------------------------------------------

histogram_and_normal_dist(dat$sleep, "histogram of sleep hours per day")

# answer: The average sleep is 7.7 hours, and standard deviation is not very high,
# thus most people will sleep between around 7.7 +-1 hours.
# The sleep distribution looks moderately normal, because it is moderately left skewed



# 2 b)
m <- mean(dat$sleep)
n <- nrow(dat)
s <- sd(dat$sleep)/sqrt(n)
crt.t <- qt(.975, n-1) # critical value of the t-distribution
crt.n <- qnorm(.975) # critical value of the N(0,1)
c(m-crt.t*s, m+crt.t*s) # for the t distribution
# [1] 7.570444 7.847596
c(m-crt.n*s, m+crt.n*s) # for the normal distribution
# [1] 7.571254 7.846786

# answer: The t distribution and the normal distribution
# produce relatively similar confidence intervals. Because we have many samples (208),
# and as n gets larger the t distribution converge to a normal distribution.
# Note however that due to the low standard deviation and the fact that there are many
# samples (208) the interval of confidence is quite small.

# 2 c)
m <- mean(dat$sleep)
n <- nrow(dat)
s <- sd(dat$sleep)
# t-test
t <- (m-8)/(s/sqrt(n))
# [1] -4.139703
## critical values from t (5%)
qt(0.05, n-1) ## we reject the null hyphotesis
## [1] -1.652248

pt(-abs(t), n -1)
# [1] 2.530422e-05, we reject the null hyphotesis

# 2 d)

male = subset(dat, male == 1)
female = subset(dat, male == 0)

stargazer(male[,c("age", "educ", "male", "sleep")], type="text")

# ==========================================
#   Statistic  N   Mean  St. Dev.  Min   Max  
# ------------------------------------------
#   age       132 39.788  10.768   23     64  
# educ      132 13.591  2.571     8     17  
# male      132 1.000   0.000     1     1   
# sleep     132 7.645   1.085   4.143 10.893
# ------------------------------------------

stargazer(female[,c("age", "educ", "male", "sleep")], type="text")

# ========================================
#   Statistic N   Mean  St. Dev.  Min   Max 
# ----------------------------------------
#   age       76 38.395  10.188   23    62  
# educ      76 13.526  2.506     8    17  
# male      76 0.000   0.000     0     0  
# sleep     76 7.820   0.871   5.607 9.750
# ----------------------------------------

# answer: females tend to sleep a bit more in average than males,
# and the variance of the sleep hour of females is also smaller compared to males.
#

# 2 e)

v_male <- var(male$sleep)

v_female <- var(female$sleep)

test <- v_male/v_female

n1 <- nrow(male)

n2 <- nrow(female)

(qf(.95, n1-1, n2-1)) # Critical value: We same variance hyphotesis
# [1] 1.415713

1-pf(test, n1-1, n2-1) ## p-value: We reject the hyphotesis
# [1] 0.01908717


# 2 f)
m_male <- mean(male$sleep)
m_female <- mean(female$sleep)

v_male <- var(male$sleep)
v_female <- var(female$sleep)

n_male = nrow(male)
n_female = nrow(female)

## Diff.l variance
s.d <- sqrt(v_male/n_male+v_female/n_female)
t <- (m_female-m_male)/s.d
# [1] 1.27546
pv <- 1-pnorm(t) ## You can almost reject with a significace level of 10%
# [1] 0.1010732
qnorm(.90) # confirmed with the critical value

t.test(male$sleep, female$sleep, alternative="less")

# Welch Two Sample t-test
# 
# data:  male$sleep and female$sleep
# t = -1.2755, df = 184.55, p-value = 0.1019
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 0.05194006
# sample estimates:
#   mean of x mean of y 
# 7.644931  7.820332

# 2 g)

v_sleep <- var(dat$sleep)
n <- nrow(dat)
(test <- (n-1)*v_sleep/1)
# [1] 212.7263
(crit <- qchisq(.95, n-1)) # critical value
## [1] 241.5657
## We don't reject as the test is less than the critical value
## confirmed by the p-value
1-pchisq(test, n-1)
# [1] 0.3776823


# 2 h)

v_sleep <- var(dat$sleep)
right <- qchisq(.975, n-1)
left <- qchisq(.025, n-1)
up <- (n-1)*v_sleep/left
down <- (n-1)*v_sleep/right
c(down=down, up=up)

# down        up 
# 0.8552201 1.2583826 

# 2 i)

v <- vector()
sleep <- dat$sleep # just to simplify the variable name
n <- length(sleep)
for (i in 1:1000)
  v[i] <- var(sleep[sample(n, size=n, replace=TRUE)])
SD <- sd(v)
## [1] 0.1179105
test <- (var(sleep)-1)/SD
## [1] 0.2346105
qnorm(.95) # critical value, so we fail to reject
## [1] 1.644854
1-pnorm(test) # p-value; we don't reject
## [1] 0.4072555

c(var(sleep) - 1.96*SD, var(sleep) + 1.96*SD)
# [1] 0.7838056 1.2715205

# I believe 1000 is enough since it is 4 times the amount of data points we have
