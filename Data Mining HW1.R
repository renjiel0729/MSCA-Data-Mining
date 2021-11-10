dataPath <- "~/Desktop/"
data <- read.csv(paste(dataPath,'GermanCredit.csv', sep = '/'), header=TRUE)
data

# Delete the column named "Class"
df = subset(data, select = -c(Class,CheckingAccountStatus.none,CreditHistory.Critical, Purpose.Vacation, 
                              Purpose.Other, SavingsAccountBonds.Unknown,EmploymentDuration.Unemployed,
                              Personal.Male.Married.Widowed,Personal.Female.Single,OtherDebtorsGuarantors.Guarantor,
                              Property.Unknown, OtherInstallmentPlans.None,Housing.ForFree,
                              Job.Management.SelfEmp.HighlyQualified) )
original <- lm(Amount~.,data=df)
summary(original)

# Run 1000 Times and store coefficients, R square training, and R square holdout. 
i<-1
linear<-function(i){
    set.seed(i*5)
    sample_size <- floor(0.632 * nrow(df))
    train_ind <- sample(seq_len(nrow(df)), size = sample_size)
    train <- df[train_ind, ]
    test <- setdiff(1:nrow(df),train)
    xtest <- df[test,]
    ytest <- df[test,"Amount"]
    lm1000 <- lm(Amount~.,data=train)
    lmpredict <- predict(lm1000,newdata=xtest)
    train_rsq <- summary(lm1000)$r.squared
    test_rsq <- cor(ytest,lmpredict)^2
    result <- c(i=as.numeric(i), coefficient=lm1000$coefficient, Rsquare_train=train_rsq, Rsquare_holdout=test_rsq)
    return(result)
}
result1000 <- as.data.frame(t(sapply(1:1000,linear)))
result1000

# Choose Age, House Rent, and Property RealEstate to generate plots. 
hist(result1000$coefficient.Age,main='Age')
hist(result1000$coefficient.Housing.Rent,main='House Rent')
hist(result1000$coefficient.Property.RealEstate,main='Property RealEstate')

# Plot the distribution of R squared in train
hist(result1000$Rsquare_train,main='Rsquare_train')

# Percentage of decrease of R square.
change_rsquare <- ((result1000$Rsquare_train - result1000$Rsquare_holdout)/result1000$Rsquare_train) *100
change_rsquare
hist(change_rsquare,main='Percentage of R square change')

# Mean
col_mean = apply(result1000,2,mean)
col_mean

# Standard Deviation
col_sd = apply(result1000,2,sd)
col_sd

# Compare the percentage of difference between mean of coefficients vs original coefficients.
coef_change <- ((col_mean - original$coefficient) / original$coefficient) *100
coef_change

# Calculate the confidence Interval
lower <- col_mean + qnorm(.025)*col_sd
upper <- col_mean + qnorm(.975)*col_sd
width <- upper - lower*sqrt(.632)
CInew <- cbind(lower,upper,width)
CInew

# Full model CI.
fullmodelCI <- confint(original)
fullmodelwidth <- fullmodelCI[,2] - fullmodelCI[,1]
fullmodelresult <- cbind(fullmodelCI,fullmodelwidth)
fullmodelresult

# Calculate how many CI are broader or tighter
countCI <- ifelse(width > fullmodelresult[,3],1,0)
countCI
sum(countCI)
