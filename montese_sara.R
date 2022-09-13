
#a. Give a description of the data set.
my_data <- read.table(file.choose(), sep=",")
my_data<-my_data$V8


#Structure of the dataset
class(my_data)
str(my_data)

#b. Normality check

#1) Use a visualisation of the data like boxplot (histogram is less reliable)
my_boxplot<-boxplot(my_data)

#2). use a quantile-quantile plot, putting 'normal' quantiles against sample quantiles
qqnorm(my_data)
qqline(my_data, col="red")

#From the plot we notice that the  data falls along the dotted line (expected quantiles of a normal distribution)

#3). The most widely used statistical test for normality is the so-called Shapiro-Wilk test
shapiro.test(my_data)

#
# We find a p-value of 0.0001683 < 0.05
# The test is significant, we reject that null hypothesis and so accept the alternative hypothesis that the data are 
#not sampled from a Gaussian population.



#Remove the two outliers and re-do the normality-check
outliers <-my_boxplot$out
my_data <- my_data[-which(my_data %in% outliers)]
#
shapiro.test(my_data)

#The conclusion of the test doenst't change.



#c. Use R to perform a broad exploratory data analysis of these population data:
# ------------------------------
# Measures of central tendency
# ------------------------------
summary(my_data)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(my_data)
#Mode: 150
#Median:138
#Mean:137

# ------------------------------
# Measures of disperson
# ------------------------------
#
(n <- length(my_data))
#Population variance
(variance<-var(my_data) * (n-1)/n)

# Population standard deviation
(st_dev<-sqrt(variance) )


# Coefficient of variation 
(cv <- st_dev / mean(my_data) * 100)

#- A visualization, including a histogram and a (notched) box plot.

# ------------------------------
# Histogram
# ------------------------------

my_freq_distr <- hist(my_data, breaks = "Sturges", main = paste(" Maximum Heart Rate achieved"), xlab = "MaxHR", col="green")
# Check the structure of the object
str(my_freq_distr)

# ------------------------------
# Boxplot
# ------------------------------
#
boxplot(my_data, notch=TRUE)



#d. Now draw a random sample of size n = 25 from data_montese_1,
#and use R to perform a similar broad exploratory data analysis of these sample data.

sample_data = my_data[sample(51:75)]



# ------------------------------
# Measures of central tendency
# ------------------------------
summary(sample_data)
getmode(sample_data)
#Mode: 170
#Median:140
#Mean:138.8

# ------------------------------
# Measures of disperson
# ------------------------------
#
(n <- length(sample_data))
#Sample variance
(variance<-var(sample_data))

# Sample standard deviation
(st_dev<-sd(sample_data))


# Coefficient of variation 
(cv <- st_dev / mean(sample_data) * 100)

#- A visualisation, including a histogram and a (notched) box plot.
# ------------------------------
# Histogram
# ------------------------------

my_freq_distr <- hist(sample_data, breaks = "Sturges", main = paste(" Maximum Heart Rate achieved"), xlab = "MaxHR", col="red")
# Check the structure of the object
str(my_freq_distr)


# ------------------------------
# Boxplot
# ------------------------------
#
# Use the function boxplot() to display a boxplot of the set of data
boxplot(sample_data, notch=TRUE)

#e. Interpret the results of both analyses in a short textual report.






#----------------------------------------------------------------

# PART C
#a. The datasets represent the total government expenditure on education, from 1970 to 2019, for Italy and United Kindom 
#(given as a share of GDP).


data_it <- read.table(file.choose(), sep=",") #data_montese_2.csv
data_it<-data_it$V4 #select only the gpa share field

boxplot(data_it)
#removing outliers...
outliers <-boxplot(data_it)$out
newdata_it <- data_it[-which(data_it %in% outliers)]


data_uk <- read.table(file.choose(), sep=",") #data_montese_3.csv
data_uk<-data_uk$V4 #select only the gpa share field

boxplot(data_uk)


# Verify whether the normality assumption is reasonable.
# with very small data-sets, the shapiro test is the only real option
shapiro.test(newdata_it)
shapiro.test(data_uk)
#
# Conclusion: the p-values are bigger than 0.05, therefore the tests are not significant. 
# We do not reject the hypothesis of normality.
#
 boxplot(data_uk, newdata_it, names=c("UK","IT"))
#the boxplot shows that the median expenditure in education of the UK is greater than in Italy
#Besides, the interquartile range and the range of the data set is greater for the UK than Italy,
#(as shown by the length of the boxes and the distances between the ends of the two whiskers for each boxplot).


#Numerical summary of the datasets
 summary(newdata_it)
 summary(data_uk)
 
#Hypothesis test H0: mean_IT is equal to mean_UK

t.test(newdata_it, data_uk, var.equal = TRUE)
#p-value:3.353e-07<0.05
#A small p-value means that there is stronger evidence in favor of the alternative hypothesis. As expected, the two means are different.


#------------------------------------------------------------

#D. 
#the dataset contains, for each Italian region, two features: 
#% Population with at least 1 dose of vaccine (at 13/21/21)
#% Population with a degree

#Total observations: 21
data <- read.table(file.choose(), sep=",") #data_montese_4.csv

#Hypothesis: the % of vaccinated people in a certain region depends on the education level of the region

plot(data$V3, data$V2, xlab = "% Population with degree", ylab = "% Population with at least 1 dose of vaccine (at 13/21/21)", pch=16)
cor(data$V3, data$V2)
#rho: 0.4336502 -> The coefficient indicates a weak positive relationship between the variables


# the regression model:
lm_vaccine <- lm(data$V2~data$V3)
abline(lm_vaccine, col="red")
# the slope is equal to
(slope <- cov(data$V2,data$V3)/var(data$V3))
# and the y-intercept is
(y_intercept <-mean(data$V2) - slope*mean(data$V3))

# The regression line's equation: % vaccinated population = 0.4052854 * % population with degree + 68.45962


# What will be the % of vaccinated people, if the region's education level is 10% of the popultation? 
(prediction <- slope*10 + y_intercept)

# What will be the % of vaccinated people, if the region's education level is 18% of the popultation? 
(prediction <- slope*18 + y_intercept)
#The % of vaccinated people slightly grows for increasing values of % population with degree in the region, as expected

#==========================================
# Analysis of the linear regression model
#==========================================
summary(lm_vaccine)

#- The p value for the slope tells us that the independent variable doesn't have strong statistical predictive capability (*)
#- the R-squared indicates that only about 20% of the dependent variable is predicted by the independent variable - which is a weak

#- Although R-squared can give you an idea of how strongly associated the predictor variables are with the response variable,
#  it doesn't provide a formal statistical test for this relationship. This is why the F-Test is useful since it is a formal statistical test. 
#  Since the p-value is almost equal to 0.05, the F-test is considered on the borderline of statistical significance, and 
#so the correlation between the independent variable and dependent variable.



# ANOVA
anova(lm_vaccine)
#
# 95% confidence intervals for the slope and y-intercept
#
confint(lm_vaccine)
#

#Interpret residuals 
shapiro.test(resid(lm_vaccine))
#p-value: 0.544 -> The p-value of the Shapiro-Wilk Test is greater than 0.05, the test is not significant. We have no reason to reject 
#the null hypothesis that the residuals are normally distributed.


# residuals vs. fitted plot
#
plot(fitted(lm_vaccine),resid(lm_vaccine))
abline(0,0)



# The fitted vs residuals plot is very useful to visualize the suitability of the linear regression model.
#The residuals do not really "bounce randomly" around the 0 line. This may suggests that the assumption that the relationship is linear is not very reasonable.
#The residuals do not form an approximate "horizontal band" around the 0 line. This suggests that the variances of the error 
#terms are not equal.
#There are some residuals that "stand out" from the basic random pattern of residuals. This suggests that there could be some outliers. 

#In conclusion, by analyzing the residual plot I can't validate the regression model. This could depend on the small size of the dataset.