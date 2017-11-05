# Data analysis of the Demonetisation data set
library(ggplot2)
library(MASS)
library(boot)

attach(Demon)

# converting all the "not yes" to "No"

Demonitisation[Demonitisation == 'not Yes'] <- 'No'
Data_ <- Demon[,-8]
Data <- cbind(Data_, Demonitisation)

DataSum = summary(Data)

# converting continuous variable age into categorical variable for visualisation.
row <- which(age == 1130)
Data <- Data[-47,]
attach(Data)

agecat <- cut(age,c(18,38,58,78,98,136), labels = c("18-38", "38-58","58-78", "78-98","98-above"))
incomecat <- cut(monthly.income, c(-1,13000,23000,33000,43000,53000,63000,85000,500000), labels = c("below 13k","13k-23k","23k-33k","33k-43k","43k-53k","53k-63k","63k-85k","85-above"))

Data_T <- cbind(Data, incomecat,agecat) # Transformed data which contains more categorical variable
Data_N <- Data[!is.na(Urban),]
#Urban_N <- Urban[!is.na(Urban)]





############################# Exploratory data analysis on the Demon data set############################

# 1. Bar graph showing the count of no of people in support and against of demonitisation
p1 <- qplot(x = Demonitisation, data = Data, fill = sex, geom="bar") # distinction on basis of sex
p2 <- qplot(x = Demonitisation, data = Data[!is.na(Urban),], fill = Urban[!is.na(Urban)], geom = "bar") # distinction on basis of Urban or Rural
p3 <- qplot(x = Demonitisation, data = Data, fill= Residence,geom = "bar") # distinction on basis of residence
p4 <- qplot(x = Demonitisation, data = Data_T, fill = agecat,geom = "bar") # distinction on basis of age group
p5 <- qplot(x = Demonitisation, data = Data_T, fill = incomecat,geom = "bar")



p6 <- qplot(x=sex, y = monthly.income, data=Data, fill=sex, geom = "boxplot")# Visualising whether there is substantial difference in women and men income.


p7 <- qplot(x=sex, y=monthly.income, data = Data) + geom_jitter(aes(color=sex)) #Distribution showing the monthly income of male and female in different range.


p8 <- qplot(x = Demonitisation, data = Data[!is.na(Urban),], fill = sex, facets = .~Urban[!is.na(Urban)] )+ geom_bar()#Visualisation of demonitisation outcome
# by Urban and rural people with proportion of sex attribute.

p9 <- qplot(x = Demonitisation, data = Data_T, facets = .~incomecat, fill = sex ) + geom_bar() # visualisation of Demonitisation outcome provided by different
# income group people in society classfied as poor, lower middle, middle, uppermiddle and rich.


q <- qplot(x=monthly.income, data = Data) + geom_histogram(fill="steelblue") # Visualising the distribution of income with histogram. 
q_trans <- qplot(x = log(monthly.income + 10)) + geom_histogram() # Long transform to convert earning into gaussian distribution.


q1 <-  qplot(x = monthly.income, data = Data_T, facets = .~sex) + geom_histogram() # visualising the distribution of income in mena and women category.

q1_trans <- qplot(x = log(10+monthly.income), data = Data, facets = .~sex) + geom_histogram() # taking log tranform to make the distribution normal in both category.


t1 <- qplot(x = Urban, y = monthly.income, data = Data, fill=Urban) + geom_boxplot()# visualising the difference in income of urban and rural people.

r1 <- qplot(x = age, y = monthly.income,data = Data, colour = factor(sex)) +geom_point() +  geom_smooth(col="darkblue") #Visualising whether there is a relationship
# between age and monthly income through scatter plot and fitting a smoothing curve through to observe trend in data.

r1_trans <- qplot(x=age,y=monthly.income, data=Data, facets = .~sex) + geom_point(col="steelblue") + geom_smooth(col="red") #Visualising the trend of age with income
# in both categories.



 




########################################################################## Problem_2: ####################################################################
######################################################## Escape ##############################################################33
# Calculating the population parameter
set.seed(1)
x <- monthly.income
n = 5000 # number of samples to be collected
sample_mean = rep(NA, n) # vector to store all the sample means
for (i in 1:n){
  sample_mean[i] = mean(sample(monthly.income, 100))
}

population_mean_1 = mean(sample_mean) # From the central limit theorem

qplot(x=sample_mean) + geom_histogram(col="lightgreen",fill="steelblue")+geom_vline(xintercept = population_mean_1, lwd = 2, col="red")

# Applying bootstrap method to meausre the population standard deviation

boot.fun = function(data, index){
  return(mean(data[index]))
}

bootobj = boot(x, boot.fun, R = 1000)
population_mean_2 = bootobj$t0 # Mean of population calculated using the bootstrap sampling.
mean_sd = sd(bootobj$t)
CI = boot.ci(bootobj, conf = 0.95) #confidence interval using the bootstrapping.

CI_1 = population_mean_1 + c(-1,1)*2*mean_sd # 95% confidence interval for mean using the central limit theorem.


boot.fun1 = function(data, index){ # function for estimating the population standard deviance.
  return(sd(data[index]))
}
psd = boot(x,boot.fun1, R = 1000)$t0 # population standard deviation estimation using bootstrap rersampling.
#******************************************* Continue Onwards #######################################3*****************************
############################################################## problem no: 2 using the assumptions provided #################################################3

################################ Outlier Detection and removal to estimate the population parameter###################################
Q1 = quantile(monthly.income, 1/4)
Q2  = quantile(monthly.income, 3/4)

IR = quantile(monthly.income, 3/4) - quantile(monthly.income,1/4)
upper_limit = Q2 + 3/2*IR
lower_limit = Q1 - 3/2*IR
MaxIncome = max(monthly.income)
q <- cut(monthly.income,c(0,lower_limit,upper_limit,MaxIncome), c("LowerOutlier", "Normal", "UpperOutliers"))

b1 <- qplot(y = monthly.income, x = c("Income") ,geom="boxplot", xlab = "Income")+geom_point(aes(col = q))# boxplot the see the outliers.
j1 <- qplot(y = monthly.income, x = c("Income"), xlab = "Income")  + geom_jitter(aes(col = q)) + geom_point(aes(col = q)) # Jitterplot to see the outliers





# Mehtod 1: Removing the data after 95th percentile to remove ouliers.
out <- quantile(monthly.income, 0.95)
monthly.income_ <- monthly.income[monthly.income < out]
data_h1 = data.frame(x = monthly.income_)
h1 <- ggplot(data = data_h1, aes(x=monthly.income_, y = ..density..)) + geom_histogram(fill = "steelblue", alpha = 1/2) + geom_density(col="red", lwd=1.5) # Histogram showing data distribution after removing the lower and upper percentile vlaue data.

# Method 2: Manually set outliers range by closely observing the histogram.
s3 <- qplot(x = monthly.income) + geom_histogram(fill= "steelblue", alpha = 1/2, col ='lightgreen') # Histogram to check the outliers.
Income_new <- monthly.income[monthly.income < 250000] # Outliers selected manually after observing the histogram.
data_s4 = data.frame(x = Income_new)
s4 <- ggplot(data=data_s4, aes(x=Income_new, y = ..density..)) + geom_histogram( alpha = 1/2, col = "green", fill="steelblue") + geom_density(col="red", lwd=1.5)

# Method 3: Removing the outliers using the interquantile range method
Income_ <- monthly.income[monthly.income < upper_limit  ]
data_h2 = data.frame(income = Income_)
h2 <- ggplot(data = data_h2 ,aes(x = income, y= ..density.. )) + geom_histogram(col="steelblue", alpha = 1/2, col='green') + geom_density(col="red", lwd = 1.5)


################################### Drawing a theoritical vs sample population gamma distribution#########################

var <- var(Income_new)
mean <- mean(Income_new)

beta = mean/var # shape of gamma distribution
alpha = mean*beta # rate of the gamma distribution

# plotting gamma distributions using the calcualted parameter
x = seq(-10,100000, by = 2)
d <- dgamma(x, shape = alpha, rate = beta)
p1 <- qplot(x = x, y = d) + geom_line(col = "steelblue")

p2 <- ggplot(data=Data, aes(x=monthly.income, y=..density..)) + geom_histogram(fill="darkblue",alpha=1/2) + geom_density(col="red",lwd=3/2, alpha=1/5)
require(gridExtra)
grid.arrange(p1,p2, ncol=2)

########################################## Validating our assumptions throught he quantile quantile plot#############################
n = length(Income_new)
probablities = 1:n/(n+1)
t_quantiles = qgamma(probablities, shape = alpha, rate = beta)
theoretical_quantile = sort(t_quantiles)
population_quantile = sort(Income_new)

quantile_plot <- qplot(y = population_quantile, x = theoretical_quantile) + geom_point(col="darkblue", size= 3) + geom_abline(slope = 1, intercept = 0, col="steelblue", lwd=1.5)
# The above plot validate our assumption of distribution being gamma as the line is almost straight.

#############################################################################******************************#############################################3





p_beta = population_mean_1/psd^2 #beta of population calculated using central limit theorem
p_alpha = population_mean_1*p_beta # alpha of population calculated using central limit theorem


##########(c)Simulate (simulation size 5000) .....###################################################
simulations = 5000
prob = rgamma(simulations,shape = alpha, rate=beta)
perc_60 = quantile(prob, 0.6)
h <- hist(prob,breaks=5000,plot=FALSE)
cuts <- cut(h$breaks, c(-100,perc_60,'Inf'))
plot(h, col=cuts)
cuts1 = cut(prob, c('-Inf',perc_60,'Inf'),c("Before 60the percentile", "after 60th percentile"))
data_prob = data.frame(simul=prob)

h_simul <- ggplot(data=data_prob, aes(x=simul, y = ..density..))  +geom_density(aes(fill= cuts1),col="red", lwd = 1.5)
j_simul <- qplot(y = prob, x = c("simulations"), xlab = "Simulations") + geom_jitter(aes(col=cuts1), size = 0.05) 













