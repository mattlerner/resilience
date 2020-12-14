library(zoo)

###### Variables to be set ######
n <- 1000 # vector size. this is logistical.

annual.probability.of.event <- 0.1
term.of.bond.in.years <- 5
number.of.policyholders <- 10

policy.amount <- 1000000000 # 1b
policy.amount.sd <- 100000000 #100m

###### Variables derivative of set variables ######

# normal distribution of losses for now
loss.curve <- rnorm(n, policy.amount, policy.amount.sd)

###### Key functions ###### 


#### Get EP Curve from loss curve
loss.amount <- seq(from = 0, to = max(loss.curve), length.out=10000)
ep.curve <- sapply(loss.amount, function(x) { return(mean(loss.curve > x)) }, simplify="vector")
ep <- data.frame(amount=loss.amount, prob=ep.curve)

#### Calculate AAL and sd.loss
ep.negative <- ep[ep$amount >= policy.amount,]
id <- order(ep.negative$amount)
x <- ep.negative$amount
y <- ep.negative$prob
AUC <- sum(diff(x[id])*rollmean(y[id],2))
aal <- AUC
sd.loss <- sd(ep$amount)

#### Premium calculations

exposure <- number.of.policyholders
pure.premium <- aal/exposure
y <- 0.1      # desired portfolio yield
z <- 1.645    # z-score for prob that actual result will require more surplus allocated
R = (y * z)/(1 + y)  # reluctance factor
risk.load <- (R * sd.loss)/exposure # replace with CV?

# Components of variable expense and profit - values taken from example in source D.1
commissions <- 0.2
premium.tax <- 0.04
underwriting.profit.provision <- 0.05
trend.adjustment <- 0 # this is in the example but we leave it out for now
fixed.expense = 25

# final calculations
variable.expense.and.profit <- (commissions + premium.tax + underwriting.profit.provision) + trend.adjustment
average.rate <- (pure.premium + risk.load + fixed.expense) / (1 - variable.expense.and.profit)

print(paste("Average annual rate per policyholder:",average.rate))

###### References  ###### 
# D. Insurance References
# 1. [AIR guide](https://www.cia-ica.ca/docs/default-source/2018/218103e.pdf) on CAT bonds and pricing.
# 2. [An example](https://www.chicagofed.org/publications/chicago-fed-letter/2018/405)
# 3. [An analysis of the market price of CAT bonds](https://www.casact.org/pubs/forum/09spforum/02Bodoff.pdf)
# 4. [Fundamentals of catastrophe modeling (presentation)](https://www.casact.org/education/rpm/2010/handouts/cmwa-hess.pdf)
# 5. [Overview of deductible ratemaking](https://www.soa.org/globalassets/assets/Files/Research/Projects/research-2016-gi-deductible-ratemaking.pdf)
# 6. [Worked example of CAT bond modeling](https://www.researchgate.net/publication/318990000_Uncertainty_Quantification_in_the_Design_of_Earthquake_Catastrophe_Bonds)