load(file='TeachingRatings.rda')
dat <- TeachingRatings


# Q 1)
res <- lm(eval~beauty+I(beauty^2), data=dat)

# we shouldn't add the beauty squared factor, because we can't reject the null
# hyphotesis that its slope is not zero

# one more unit of beauty increases in average the evaluation by 0.14
# one more unit of beauty^2 decreases in average the evaluation by 0.03

# Q 2)

m <- mean(dat$beauty)
ape <- lm(eval~beauty+I(beauty^2-2*m*beauty), data=dat)

(coef(ape)[2])
# 0.1467102 

# Q 3)

v <- vcov(res)
m <- mean(dat$beauty)
s <- sqrt(v[2,2]+4*m^2*v[3,3]+4*m*v[2,3])
b <- coef(res)

ape <- b[2]+2*b[3]*mean(dat$beauty)
crit <- qt(.975, res$df)
c(ape-s*crit, ape+s*crit)

# Q 4)
ape <- lm(eval~beauty+I(beauty^2-2*m*beauty), data=dat)

confint(ape, "beauty")

s <- sqrt(vcov(ape)[2,2])
b <- coef(ape)
df <- res$df

tstar <- qt(.975, df)
# The lower bound
low <- b[2] - tstar*s
low
## 0.07752359 

# the upper bound
up <- b[2] + tstar*s
up
# 0.2158968


# Q 5)
res <- lm(eval~beauty+age+I(age*beauty), data=dat)

# one more unit of beauty decreases the evaluation in average by
# 0.33
# one more unit of ages increases the evaluation by a very small amount
# one more unit of age*beauty increases the evaluation by 0.01

# it seems that the age is not significant, since the p-value
# is quite large. But the interaction term is quite significant,
# which means that age itself is not a predictor but age times beauty
# but in fact it is not trivial in a first analysis if the interaction
# is significant because one depends of the other or if only because beauty
# is signifcant

# Q 6)

m <- mean(dat$age)
ape <- lm(eval~beauty+age+I(beauty*age-m*beauty), data=dat)

(coef(ape)[2])
# 0.1517305

# Q 7) 

v <- vcov(res)
m <- mean(dat$beauty)
s <- sqrt(v[2,2]+4*m^2*v[3,3]+4*m*v[2,3])
b <- coef(res)

ape <- b[2]+2*b[3]*mean(dat$beauty)
crit <- qt(.975, res$df)
c(ape-s*crit, ape+s*crit)
# -0.63309918 -0.04522587

# Q 8)

ape <- lm(eval~beauty+age+I(beauty*age-m*beauty), data=dat)

confint(ape, "beauty")

s <- sqrt(vcov(ape)[2,2])
b <- coef(ape)
df <- res$df

tstar <- qt(.975, df)
# The lower bound
low <- b[2] - tstar*s
low
## -0.6330992 

# the upper bound
up <- b[2] + tstar*s
up
# -0.04522587

# Q 9)

dat$male <- as.numeric(dat$gender == "male")
dat$native <- as.numeric(dat$native == "yes")

res1 <- lm(eval~beauty+age+I(beauty*age), data=dat)

summary(res1)$adj.r.squared
# [1] 0.05123299

res2 <- lm(eval~beauty+age+I(age^2)+I(beauty*age), data=dat)

summary(res2)$adj.r.squared
# [1] 0.04921442

res3 <- lm(eval~beauty+age+I(beauty*age)+I(beauty^2), data=dat)

summary(res3)$adj.r.squared
# [1] 0.0493375

res4 <- lm(eval~beauty+age+I(beauty*age)+I(beauty^2), data=dat)

summary(res4)$adj.r.squared
# [1] 0.0493375

res5 <- lm(eval~beauty+age+I(beauty*age)+I(male), data=dat)

summary(res5)$adj.r.squared
# [1] 0.08563304

res6 <- lm(eval~beauty+age+I(beauty*age)+I(male)+I(native), data=dat)

summary(res6)$adj.r.squared

# [1] 0.1083137

# res6 is the linear regression with the best adjusted r squared
# all parameters of this linear regression are "useful" in the
# sense that they all have a high signifcance except the age

# one more unit of beauty decreases in average the evaluation by 0.4
# one more unit of beauty*age increases the evaluation by 0.11
# being male increases the evaluation by 0.22 in average
# being native increases the evaluation by 0.36 in average

# Q 10)

res <- lm(eval~I(beauty-1.5)+I(age-40)+I(male-1)+I(native-1),
          data=dat)

res2 <- lm(eval~I(beauty)+I(age)+I(male)+I(native),
          data=dat)

(p <- coef(res)[1])
#    4.340116


crit <- qt(.975, res$df)
s <- sqrt(vcov(res)[1,1])
c(p-s*crit, p+s*crit)
# 4.218323    4.461909 

# Q 11)

res <- lm(eval~I(beauty-1.5)+I(age-40)+I(male-0)+I(native-1),
          data=dat)

(p <- coef(res)[1])
#    4.129812 


crit <- qt(.975, res$df)
s <- sqrt(vcov(res)[1,1])
c(p-s*crit, p+s*crit)

# 4.014937    4.244687

# for females the average prediction and the confidence interval
# are both smaller than with male

# Q 12)

newdata <- data.frame(age=40, beauty=1.5, male=1, native=1)

res <- lm(eval~I(beauty-1.5)+I(age-40)+I(male-1)+I(native-1),
          data=dat)

predict(res, newdata=newdata, interval="confidence",level=0.95)

# fit      lwr      upr
# 1 4.340116 4.218323 4.461909

newdata <- data.frame(age=40, beauty=1.5, male=0, native=1)

res <- lm(eval~I(beauty-1.5)+I(age-40)+I(male-0)+I(native-1),
          data=dat)
predict(res, newdata=newdata, interval="confidence",level=0.95)

#        fit      lwr      upr
# 1 4.129812 4.014937 4.244687


# Question 2

load(file='CPS1985.rda')
dat <- CPS1985

datam <- subset(dat, gender=="male")
datan <- subset(dat, gender=="female")

resm <- lm(wage~education+experience+I(experience^2), data=datam)
b <- coef(resm)
(aMax <- -b[3]/(2*b[4]))


aVec <- vector()
n <- nrow(datam)
for (i in 1:1000)
{
  ind <- sample(n, size=n, replace=TRUE) # select the rows randomely
  data <- datam[ind,] # get the new bootstrap sample
  res2 <- lm(wage~education+experience+I(experience^2), data=data) # re-estimate with new sample
  b2 <- coef(res2)
  aVec[i] <- -b2[3]/(2*b2[4]) # store the aMax in a vector
}
(se <- sd(aVec)) #


(tstar <- qt(.975, nobs(resm)-3))


c(down = aMax-tstar*se, up = aMax+tstar*se)

# down.experience   up.experience 
# 25.05566        45.72982

resn <- lm(wage~education+experience+I(experience^2), data=datan)
b <- coef(resn)
(aMax <- -b[3]/(2*b[4]))


aVec <- vector()
n <- nrow(datan)
for (i in 1:1000)
{
  ind <- sample(n, size=n, replace=TRUE) # select the rows randomely
  data <- datan[ind,] # get the new bootstrap sample
  res2 <- lm(wage~education+experience+I(experience^2), data=data) # re-estimate with new sample
  b2 <- coef(res2)
  aVec[i] <- -b2[3]/(2*b2[4]) # store the aMax in a vector
}
(se <- sd(aVec)) #


(tstar <- qt(.975, nobs(resn)-3))


c(down = aMax-tstar*se, up = aMax+tstar*se)

# down.experience   up.experience 
#       -430.8293        539.6558 
