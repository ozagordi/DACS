library(plyr)
load("loansData.rda")
# remove the observation with very little information
loansData <- subset(loansData, !is.na(loansData$Monthly.Income))

# Data cleaning, percent is now numeric, months and so on
loansData$Interest.Rate.percent <-
  as.numeric(sub("%", "", loansData$Interest.Rate))
loansData$Interest.Rate <- NULL
loansData$Loan.Length.months <-
  as.numeric(sub("months", "", loansData$Loan.Length))
loansData$Loan.Length <- NULL
loansData$Debt.To.Income.Ratio.percent <-
  as.numeric(sub("%", "", loansData$Debt.To.Income.Ratio))
loansData$Debt.To.Income.Ratio <- NULL

loansData$Request.Income.Ratio <-
  loansData$Amount.Requested/loansData$Monthly.Income

# Transform FICO range in numeric based on the first figure
x <- strsplit(as.character(loansData$FICO.Range), split="-")
loansData$FICO.numeric <- as.numeric(laply(.data=x, .fun=function(z) z[[1]][1]))

# Several exploratory plots

# interest goes down with FICO
plot(loansData$FICO.Range, loansData$Interest.Rate.percent)

# debt to income quite normally distributed
hist(loansData$Debt.To.Income.Ratio.percent)
plot(loansData$Debt.To.Income.Ratio.percent, loansData$Interest.Rate.percent)

# interest goes up with debt to income
fit_di <- lm(Interest.Rate.percent ~ Debt.To.Income.Ratio.percent,
             data=loansData)
summary(fit_di)
abline(fit_di)
# no clear pattern of interest w.r.t. employment length
plot(loansData$Employment.Length, loansData$Interest.Rate.percent)
aovel <- aov(Interest.Rate.percent ~ Employment.Length, data=loansData)
summary(aovel)

# interest depends on loan purpose
plot(loansData$Loan.Purpose, loansData$Interest.Rate.percent)
aovlp <- aov(Interest.Rate.percent ~ Loan.Purpose, data=loansData)
summary(aovlp)

# no dependance on state
aovst <- aov(Interest.Rate.percent ~ State, data=loansData)
summary(aovst)

# Home ownership has one observation defined as NONE, set to NA
loansData$Home.Ownership[loansData$Home.Ownership == 'NONE'] <- NA
plot(loansData$Home.Ownership, loansData$Interest.Rate.percent)

# anova sees something
fitaov2 <- aov(Interest.Rate.percent ~ Home.Ownership, data=loansData)
summary(fitaov2)

# renting increases of 0.7 w.r.t. to mortgage
fitlm2 <- lm(Interest.Rate.percent ~ Home.Ownership, data=loansData)
summary(fitlm2)

# log of monthly income is better distributed
hist(loansData$Monthly.Income)
hist(log(loansData$Monthly.Income))

# remove outliers in the monthly income
plot(loansData$Monthly.Income, loansData$Interest.Rate.percent)#, xlim=c(0, 30000))
loansDatanew <- subset(loansData, Monthly.Income < 30000)

# there is some effect of the monthly income
plot(log(loansData$Monthly.Income), loansData$Interest.Rate.percent)
fitmi <- lm(loansData$Interest.Rate.percent ~ log(loansData$Monthly.Income))
summary(fitmi)
abline(fitmi)

# amount requested seems to have an effect
plot(loansData$Amount.Requested, loansData$Interest.Rate.percent)
fitam <- lm(Interest.Rate.percent ~ Amount.Requested, data=loansData)
summary(fitam)
abline(fitam)

# Transform FICO range in numeric based on the first figure
x <- strsplit(as.character(loansData$FICO.Range), split="-")
loansData$FICO.numeric <- as.numeric(laply(.data=x, .fun=function(z) z[[1]][1]))

# no need to include interaction term
fitlmi <- lm(Interest.Rate.percent ~ FICO.numeric * log(Monthly.Income),
             data=loansData)
summary(fitlmi)

# so we fit without interaction
fitlmi <- lm(Interest.Rate.percent ~ FICO.numeric + log(Monthly.Income),
             data=loansData)
summary(fitlmi)

# FICO and income are not strongly correlated
cor(loansData$Monthly.Income, loansData$FICO.numeric)
cor(log(loansData$Monthly.Income), loansData$FICO.numeric)

# effect of the amount requested
fitam2 <- lm(Interest.Rate.percent ~ Amount.Requested * FICO.numeric,
             data=loansData)
summary(fitam2)

#effect of ratio amount/income
fitam3 <- lm(Interest.Rate.percent ~ Request.Income.Ratio * FICO.numeric,
             data=loansData)
summary(fitam3)

library(effects)
plot(effect("Request.Income.Ratio:FICO.numeric", fitam3,
            list(FICO.numeric=c(650, 700, 750))), multiline=TRUE)

