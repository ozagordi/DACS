library(plyr)
library(effects)
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


# effect of the amount requested
fitam2 <- lm(Interest.Rate.percent ~ Amount.Requested * FICO.numeric,
             data=loansData)
summary(fitam2)

plot(effect("Amount.Requested:FICO.numeric", fitam2,
            list(FICO.numeric=c(640, 700, 830))),
    multiline=TRUE,
    main="Effect of amount requested\nand FICO score on interest rate",
    xlab="Amount requested [$]",
    ylab="Interest rate [%]",
    cex.lab=2.0)
