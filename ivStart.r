library(optmatch)
library(RItools)

install.packages(c('AER','sandwich'))
library(AER)
#library(sandwich)

##### make fake data
set.seed(613)
n=100000
sameSex=rbinom(n,1,.5)

U=rbinom(n,1,.2)
dadInc=rnorm(n)
momBA=rbinom(n,1,.4)

oneMoreLP0=.5*sameSex+U+.2*dadInc-.5*momBA-1
oneMore0=rbinom(n,1,plogis(oneMoreLP0))

oneMore1=oneMore0+ifelse(oneMore0==0,
                         rbinom(n,1,.5), 0)
oneMore = ifelse(sameSex==1, oneMore1, oneMore0 )
table(oneMore0, oneMore1)

workLP=.5-U-.1*dadInc+.3*momBA

# does mother work if the kids are different sexes
work0=rbinom(n,1,plogis(workLP))
table(work0)

# does mother work if the kids are same sex
work1=work0*ifelse(oneMore1==oneMore0, 1, rbinom(n,1,.5))

table(oneMore0, oneMore1, work1, work0)

table(work1, work0)

work=ifelse(sameSex==1,work1,work0)

dat=data.frame(sameSex,oneMore,momBA,dadInc,work)
#rm( list = setdiff( ls(), "dat"))

table(dat$oneMore)
table(dat$momBA)
table(dat$oneMore)
table(work0, work1)

### asumptions
# 1 - IV is random (yes, see line 11)
# 2 - IV effects treatment (yes, see line 16)


# true ITT 
mean(work1-work0)


# true late 
mean(work1[oneMore1!=oneMore0] - work0[oneMore1!=oneMore0])

mean(work1 - work0)/mean(oneMore1-oneMore0)


###### ESTIMTAING #####
itt <- mean(work[sameSex ==1])-mean(work[sameSex ==0])
t.test(work~sameSex, data= dat)


#### prop. compliers 

### estaimne CATE (or LATE)

###### Two stages least squares ######
stage1 = lm(oneMore~sameSex)
summary(stage1)

# fitted falue form the stage one regression
Dhat = fitted(stage1) 
#Dhat1 = predict(stage1) 

stage2 = lm(work~Dhat)
summary(stage2)

# but we need to account for the fact that Dhat has measurement error
TwoStageDsq=ivreg(work~oneMore|sameSex, data = dat)
summary(TwoStageDsq)

summary(TwoStageDsq, vcov. = vcovHC) # adjust for standard errors
