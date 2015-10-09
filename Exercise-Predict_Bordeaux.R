###############  Multivariate linear regression with the Bordeaux dataset  ###############

## Bordeaux dataset contains 72 Bordeaux wine samples, with variables:
## Wine brand
## Price
## ParkerPoints and CoatesPoints: the evaluation given by two institutes
## Binary dummy variables, indicating the features of producing areas and whether
## the wine is reputed.

data <- read.csv("http://nycdatascience.com/slides/XRIntermediate/part2/data/Bordeaux.csv")
library(car)

# Look at a scatterplot of each non-binary variable
scatterplotMatrix(data[,2:4])
# Only look at the diagonal (distributions of each variable), and
# above diagonal (relationships between the variables).

# Now look at the relationship between Price(variable of interest) and
# each of the dummy(binary) variables on a series of boxplots
par(mfrow=c(2,3))
for(i in 5:9) {
      boxplot(as.formula(paste0('Price~',names(data)[i])),
              data=data,
              ylab="Price",
              xlab=names(data)[i])
}
par(mfrow=c(1,1))
# Looks like we should disregard the Pomerol variable because Price hardly
# changes whether this is a 0 or a 1

# Convert dummy variables to factor type, leave the others as is
data2 <- data.frame(data[2:4],sapply(data[,5:9],factor))

# Since Price variable is not normally distributed, need to transform/normalize
# Boxcox results will tell us how to do that. BUT WHY DO THIS?
boxCox(Price~.,data=data2)
# Since max of log-likelihood is at a lambda near zero, y becomes log(y)
# and our model becomes:
model <- lm(log(Price)~.,data=data2) 

# Plotting the model produces 4 graphs to look at and assess.
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
# The straight line on the NormalQQ shows the values were properly normalized
# Others show there are not any obvious outliers

# Plot the fitted price vs observed price to see how good the model looks
plot(model$fitted,log(data2$Price)) # model$fitted is already log scale
abline(0,1)
# Shows and almost perfect prediction!

# But let's continue to study it, to see if all variables are necessary,
# ie, test the significance of the parameters and VIF.
# Method 1: Analysis of Variance Table
anova(model)
# The last 2 columns (Pr and *) show that Coates and P95 are questionable
# as to whether they are necessary in the model.
# Method 2: Variance Inflation Factor
vif(model)
# Parker and P95 both show a high VIF
# Since P95 was flagged by both of these methods, there is a strong case to
# remove it from the model.
model2 <- update(model, .~.-P95andAbove)
anova(model2)

# Let's compare the two models to validate removal of the P95 variable
anova(model,model2)
AIC(model,model2)
# AIC shows model 2 is better (lower value = better model)
BIC(model,model2)
# BIC shows model 2 is better (lower value = better model)
# It's unanimous. Model 2 is better, which validates

####################################################################

# For this dataset, answer the following questions:

# 1. Which evaluation (Parker or Coates) has more influence on
#    the prices of wines?

summary(model2)
# Looking at the Coefficients-Esimate for Parker and Coates, we see:
# Increasing ParkerPoints by one unit will cause the price rise by 13.7%.
# Increasing CoatesPoints by one unit will cause the price rise by 9.6%.

# 2. Which wine is the most overpriced and underpriced:
#    IOW, best and worst bang for the buck?

# Find the samples with largest Price deviation from the model.
# Want to look at residuals of the given data because they show how
# far each data point is from the fitted model.
resid <- model2$residuals
fitted <- model2$fitted

# For the most overpriced (worst deal)
highp <- which.max(resid)
cbind(data2[highp,1:2],fit=exp(fitted[highp]))
# exp is exponent , or antilog
# fitted[highp] is the expected value (according to the model) of that
# highest residual
# Answer is wine #58
data[58,1]
# Brand is Tertre-Roteboeuf

# For the most underpriced (best deal)
lowp <- which.min(resid)
cbind(data2[lowp,1:2],fit=exp(fitted[lowp]))
# Answer is wine #61
data[61,1]
# Brand is La Fleur-Petrus