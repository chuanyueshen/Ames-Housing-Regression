library("AmesHousing")
data = ames_raw
geo = ames_geo
train = merge(data,geo,by = "PID")
train = train[-c(1,2)]


### Data Pre-processing
library(ggplot2)
library(rpart)
# Replace NA with "None" when a particular feature is not there

# Summarize NA data
sapply(train[,1:82], function(x) sum(is.na(x)))
# Delete the varaible with the same value excessive 80% (Alley, PoolQC, Fence, Misc Feature)
train1 = train[-c(6,72,73,74)]

# MasVnrType and MasVnrArea
summary(as.factor(train1$`Mas Vnr Type`))
# See if "None" corresponds to 0 area
table(train1$`Mas Vnr Area`[train$`Mas Vnr Type`== "None"])
# Assign the type of other 5 observations to NA
train1$`Mas Vnr Type` <- ifelse(train1$`Mas Vnr Area` == 0, "None", train1$`Mas Vnr Type`)
# NA's are less than 5% of total samples, delete them later

# BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF
table(is.na(train1$`Bsmt Qual`))
table(is.na(train1$`Bsmt Cond`))
table(is.na(train1$`Bsmt Exposure`))
table(is.na(train1$`BsmtFin Type 1`))
table(is.na(train1$`BsmtFin Type 2`))
idx1 <- is.na(train1$`Bsmt Qual`) & is.na(train1$`Bsmt Cond`) & is.na(train1$`Bsmt Exposure`) & is.na(train1$`BsmtFin Type 1`) & is.na(train1$`BsmtFin Type 2`)
train1$`Bsmt Qual`[which(idx1 == TRUE)] <- "None"
train1$`Bsmt Cond`[which(idx1 == TRUE)] <- "None"
train1$`Bsmt Exposure`[which(idx1 == TRUE)] <- "None"
train1$`BsmtFin Type 1`[which(idx1 == TRUE)] <- "None"
train1$`BsmtFin Type 2`[which(idx1 == TRUE)] <- "None"
# Check rest NA's in 5 properties, less than 5%, delete later
train1$`BsmtFin SF 1`[which(train1$`BsmtFin Type 1` == "None")] <- 0
train1$`BsmtFin SF 2`[which(train1$`BsmtFin Type 2` == "None")] <- 0
train1$`Bsmt Unf SF`[which(idx1 == TRUE)] <- 0
train1$`Total Bsmt SF`[which(idx1 == TRUE)] <- 0

# GarageType, GarageYrBlt, GarageFinish, GarageQual, GarageCond, GarageCars, GarageArea
table(is.na(train1$`Garage Type`))
table(is.na(train1$`Garage Finish`))
table(is.na(train1$`Garage Yr Blt`))
table(is.na(train1$`Garage Qual`))
table(is.na(train1$`Garage Cond`))
idx2 <- is.na(train1$`Garage Type`) & is.na(train1$`Garage Yr Blt`) & is.na(train1$`Garage Finish`) & is.na(train1$`Garage Qual`) & is.na(train1$`Garage Cond`)
table(idx2)
# Set None based on idx2
train1$`Garage Type`[which(idx2 == TRUE)] <- "None"
train1$`Garage Qual`[which(idx2 == TRUE)] <- "None"
train1$`Garage Cond`[which(idx2 == TRUE)] <- "None"
train1$`Garage Finish`[which(idx2 == TRUE)] <- "None"
train1$`Garage Yr Blt`[is.na(train1$`Garage Yr Blt`)] = median (na.omit(train1$`Garage Yr Blt`))
# GarageCars, GarageArea. Only one NA, replace with median
table(is.na(train1$`Garage Cars`) | is.na(train1$`Garage Area`))
train1$`Garage Cars`[is.na(train1$`Garage Cars`)]=median(na.omit(train1$`Garage Cars`))
train1$`Garage Area`[is.na(train1$`Garage Area`)]=median(na.omit(train1$`Garage Area`))

# Electrical
# only one missing, delete later
table(is.na(train1$Electrical))

# LotFrontage
table(is.na(train1$`Lot Frontage`))
train1$`Lot Frontage`[is.na(train1$`Lot Frontage`)]=median(na.omit(train1$`Lot Frontage`))

# FireplaceQu
table(is.na(train1$`Fireplace Qu`))
table(train1$Fireplaces)
# Matches well
# Assign None to NA
train1$`Fireplace Qu`[is.na(train1$`Fireplace Qu`)] <- "None"

train2=na.omit(train1)


### Categorical variables & Numeric variables
# Numerical varialble
num <- train2[c(3,4,18,19,25,33,35,36,37,42,43,44,45,46,47,48,49,50,51,53,55,58,60,61,65,66,67,68,69,70,71,72,73,77,78)]
# Categorical variable
cat = train2[-c(3,4,18,19,25,33,35,36,37,42,43,44,45,46,47,48,49,50,51,53,55,58,60,61,65,66,67,68,69,70,71,72,73,77,78)]
cat_name = colnames(train2[-c(3,4,18,19,25,33,35,36,37,42,43,44,45,46,47,48,49,50,51,53,55,58,60,61,65,66,67,68,69,70,71,72,73,77,78)])
for (name in cat_name) {
  cat[[name]] =as.character(cat[[name]])
}


### Colinearity
round(cor(num), dig=2)
tmp <- cor(num)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
num <- num[,!apply(tmp,2,function(x) any(abs(x) >= 0.9))]
train3 = cbind(num,cat)


### Tranformation of outcome
library(MASS)
model1=lm(SalePrice~.,data=train3)
bc=boxcox(model1, lambda = seq(-2, 2, 1/10))
l=bc$x[which.max(bc$y)]%/%0.5


### Viarable selection
model0=lm(SalePrice~1,data=train3)
# AIC
model2.1=step(model0, scope=list(lower=model0, upper=model1), direction = "forward") # 49 variables_22 num & 27 cate
model2.2=step(model1, direction = "both") # 51 variables_23 num & 28 cate
model2.3=step(model1, direction = "backward") # the sames as model2.2
# BIC
n=nrow(train3)
model3.1=step(model0, scope=list(lower=model0, upper=model1), direction = "forward",k=log(n)) # 27 varaibles_15 num & 12 cate
model3.2=step(model1, direction = "both",k=log(n)) # 26 variables_14 num & 12 cate
model3.3=step(model1, direction = "backward",k=log(n)) # the same as model3.2


### Training error
MSE_2.1=mean(model2.1$residuals^2)
MSE_2.2=mean(model2.2$residuals^2)
MSE_3.1=mean(model3.1$residuals^2)
MSE_3.2=mean(model3.2$residuals^2)


### ANOVA
anova(model2.2,model2.1) # Use the smaller one (model2.1)
anova(model2.1,model3.1) # Use the larger one (model2.1)
anova(model2.1,model3.2) # Use the larger one (model2.1)
# Use model2.1


### Diagnostic
library(faraway)
p=ncol(train3)
# High-leverage points
lev=influence(model2.1)$hat
lev[lev>2*p/n]

# Outliers 
jack=rstudent(model2.1);
cutoff = abs(qt(0.05/(2*n),n-p-1))
k=which(jack > cutoff)
# 15 outliers

# High influential points
cook = cooks.distance(model2.1)
g=which(cook > 1 | cook == "NaN")
# 2 high influential points
halfnorm(cook,labs=row.names(train4$SalePrice),ylab="Cook's distances")

# Delete outliers and high influential points
train4=train3
train4=train4[-c(k,g),]
model4=lm(formula = SalePrice ~ `Overall Qual` + `Gr Liv Area` + Neighborhood + 
            `BsmtFin SF 1` + `Roof Matl` + `MS SubClass` + `Bsmt Exposure` + 
            `Overall Cond` + `Year Built` + `Misc Val` + `Condition 2` + 
            `Sale Condition` + `Kitchen Qual` + `Garage Area` + `Lot Area` + 
            `Screen Porch` + `Bsmt Qual` + `Exterior 1st` + `Condition 1` + 
            Fireplaces + `Land Contour` + Functional + `BsmtFin SF 2` + 
            `Bsmt Unf SF` + `Pool Area` + `Mas Vnr Area` + `Land Slope` + 
            `Year Remod/Add` + `Lot Config` + `Mas Vnr Type` + `2nd Flr SF` + 
            `Exter Qual` + `Bsmt Full Bath` + `Full Bath` + Latitude + 
            `BsmtFin Type 2` + `BsmtFin Type 1` + `Bedroom AbvGr` + `Garage Yr Blt` + 
            Street + `Half Bath` + `House Style` + `Lot Shape` + `Garage Cars` + 
            `Fireplace Qu` + `Roof Style` + `Garage Qual` + `Mo Sold` + 
            `Yr Sold`, data = train4)


### Assumption check
# Fitted versus Residuals Plot
plot(fitted(model4), resid(model4), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model4")
abline(h = 0, col = "darkorange", lwd = 2)
# For any fitted value, the residuals seem roughly centered at 0, indicating that the linearity assumption is not violated. However, it is also observed that the spread of the residuals is wider when fitted value is larger than 3e+05. The constant variance assumption is violated here.

# Breusch-Pagan Test
library(car)
ncvTest(model4)
# The p-value is less than 0.05, thus reject the null of homoscedasticity. The constant variance assumption is violated, matching our findings with a fitted versus residuals plot.

# Histograms
par()
hist(resid(model4),
     xlab   = "Residuals",
     main   = "Histogram of Residuals, model4",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
# Check for normality assumption. It does have a rough bell shape, but also has a very sharp peak, thus is not clear whether the model satisfies normality assumption. For this reason we will usually use more powerful tools such as Q-Q plots and the Shapiro-Wilk test for assessing the normality of errors. 

# Q-Q plot
qqnorm(resid(model4), main = "Normal Q-Q Plot, model4", col = "darkgrey")
qqline(resid(model4), col = "dodgerblue", lwd = 2)
# The suspect Q-Q plot suggests that the errors may not follow a normal distribution.

# Shapiro-Wilk Test
shapiro.test(resid(model4))
# The small p-value indicates that there is only a small probability the data sampled from a normal distribution.

