##########################################################################
#                        Name: Zachary Hunt                              #
#                        Task: Lab 6                                     #
#                        Date: 11/11/2018                                #
##########################################################################

# Uploading the data
data <- read.csv(file.choose())
# Making sure it uploading correctly
summary(data)
head(data)
# Looks like it uploaded well
# In case there are null values
data <- na.omit(data)

##########################################################################
#                           Question 1                                   #
##########################################################################
# Is there a significant difference in crime rates among different regions
# in the U.S.? If so, which regions appear to be different?

# Checking for normality
hist(data$crime[data$region=="W"], main="West", xlab="Crime Rate")
hist(data$crime[data$region=="MW"], main="Midwest", xlab="Crime Rate")
hist(data$crime[data$region=="S"], main="South", xlab="Crime Rate")
hist(data$crime[data$region=="NE"], main ="Northeast", xlab="Crime Rate")
# Considering the relatively small sample size, the data is normal enough
# to run an Anova comparing crime rates and the different regions
a1<-aov(data$crime~data$region)
summary(a1)
# With a p value of 0.237, the crime rates don't seem to differ among regions


##########################################################################
#                           Question 2                                   #
##########################################################################
#Is there a significant difference in poverty among different regions in 
#the U.S.? If so, which regions appear to be different?

# Checking for normality
hist(data$poverty[data$region=="W"], main="West", xlab="% in poverty")
hist(data$poverty[data$region=="MW"], main="Midwest", xlab="% in poverty")
hist(data$poverty[data$region=="S"], main="South", xlab="% in poverty")
hist(data$poverty[data$region=="NE"], main ="Northeast", xlab="% in poverty")
# Considering the relatively small sample size, the data is normal enough
# to run an Anova comparing poverty between the different regions
a2<-aov(data$poverty~data$region)
summary(a2)
# With a p value of 4.38e-07 there seems to be a significant difference 
# among poverty and the different regions. To figure out which regions
# differ, a posthoc test will be run
posthoc1<-TukeyHSD(a2)
posthoc1
# With a p value of 0.0000924, the South and Midwest are different when it
# comes to poverty rates. With a p value of 0.0000012, the South and
# Northeast appear to be different among poverty rates. With a P value of
# 0.0000370, the South and West appear to be different among poverty rates.
# Essentially, the South is the outlier and is different from all other regions.

##########################################################################
#                           Question 3                                   #
##########################################################################
# Are there some regions where more people are educated? If so, which ones?

# Checking for normality
hist(data$pcths[data$region=="W"], main="West", xlab="% educated (Highschool or beyond)")
hist(data$pcths[data$region=="MW"], main="Midwest", xlab="% educated (Highschool or beyond)")
hist(data$pcths[data$region=="S"], main="South", xlab="% educated (Highschool or beyond)")
hist(data$pcths[data$region=="NE"], main ="Northeast", xlab="% educated (Highschool or beyond)")
# Considering the relatively small sample size, the data is normal enough
# to run an Anova comparing poverty between the different regions
a3<-aov(data$pcths~data$region)
summary(a3)
# With a p value of 1.81e-11 there seems to be a significant difference among
# regions when it comes to education. To figure out which regions differ,
# a posthoc will be run
posthoc2<-TukeyHSD(a3)
posthoc2
# With a p value of 0.0000001, the South and Midwest appear to be significantly
# different when it comes to education. With a p value of 0.0000004, the South and
# Northeast appear to be significantly different. Finally, with a p value of 0.0,
# the South and West are significantly different while comparing education rates.
# The South is significantly different from all other regions.

##########################################################################
#                           Question 4                                   #
##########################################################################
#Is crime correlated with the poverty? With single parents? With education?

# looking for linearity between crime and poverty
plot(data$crime, data$poverty, main="Crime and Poverty", 
     xlab="Crime", ylab="Poverty", pch=16)
abline(lm(data$poverty ~ data$crime), col="black")
# When plotting crime against poverty, the relationship is linear. Thus,
# a correlation test can be run. Now I need to check for normality
hist(data$crime, main = "Crime", xlab = "Crime rate", ylab = "Frequency")
hist(data$poverty, main = "Poverty", xlab = "Poverty rate", ylab = "Frequency")
# The data isn't normal, thus I will run a Spearman test
cor.test(data$poverty, data$crime, method="spearman")
# With a correlation coefficient of 0.3535487, there seems to be a 
# weak positive relationship between crime and poverty

# looking for linearity between crime and single parent households
plot(data$crime, data$single, main="Crime and Single Parents ", 
     xlab="Crime", ylab="Single Parents", pch=16)
abline(lm(data$single ~ data$crime), col="black")
# When plotting crime against single parent household, the relationship is linear.
# Thus, a correlation test can be run. Now I need to check for normality
hist(data$crime, main = "Crime", xlab = "Crime rate", ylab = "Frequency")
hist(data$single, main = "Single Parent Households", xlab = "% Single Parents", ylab = "Frequency")
# The data is fairly normal, thus a Pearson's correlation test can be run
cor.test(data$single, data$crime)
# With a correlation coefficient of 0.6486796, there seems to be a strong 
# positive relationship between crime and single parent households

# looking for linearity between crime and education
plot(data$crime, data$pcths, main="Crime and Education", 
     xlab="Crime", ylab="High School Education or Beyond", pch=16)
abline(lm(data$pcths ~ data$crime), col="black")
# When plotting crime against single parent household, the relationship is linear.
# Thus, a correlation test can be run. First I need to check for normality
hist(data$crime, main = "Crime", xlab = "Crime rate", ylab = "Frequency")
hist(data$pcths, main = "Education", xlab = "High School Education or Beyond", ylab = "Frequency")
# The data is fairly normal, thus a Pearson's correlation test can be run
cor.test(data$pcths, data$crime)
# With a correlation coefficient of -0.2967173, there seems to be a weak negative
# relationship between crime and education

##########################################################################
#                           Question 5                                   #
##########################################################################
# Does the relationship between crime and poverty differ by region? Does
# the relationship between crime and single parents differ by region?

# As found previously, when comparing crime across regions the data was
# reltively normal considering sample size, and so were poverty and 
# single parenthood. The relationship between poverty and crime was linear
# along with the relationship between crime and single parenthood.
# First I will subset the data
south <- subset(data, region == 'S')
midwest <- subset(data, region == 'MW')
west <- subset(data, region == 'W')
northeast <- subset(data, region == 'NE')

# Testing correlation between crime and poverty across regions
cor.test(south$crime, south$poverty) # p = 0.7955, r = 0.0762914
cor.test(midwest$crime, midwest$poverty) # p = 0.02926, r = 0.6265435
cor.test(west$crime, west$poverty) # p = 0.2018, r = 0.3788357 
cor.test(northeast$crime, northeast$poverty) # p = 0.2028, r = 0.4162872


# Testing correlation between crime and single parenthood across regions
cor.test(south$crime, south$single) # p = 0.2241, r = 0.3470331
cor.test(midwest$crime, midwest$single) # p = 0.001078, r = 0.8204508
cor.test(west$crime, west$single) # p = 0.0006478, r = 0.8171788 
cor.test(northeast$crime, northeast$single) # p = 0.02472, r = 0.6678387


##########################################################################
#                           Question 6                                   #
##########################################################################
# Plot the relationship between crime and poverty. Make the symbol differ
# by region. Finally, add a line showing the relationship between crime and
# poverty by region (e.g., 4 lines, one for each region). Make the line type
# or color differ by region.



plot(data$crime, data$poverty, pch=16, col=data$region, main="Crime and Poverty", 
     xlab="Crime (per 100,000 people)", ylab="Poverty (% population under poverty line)")
abline(lm(data$poverty[data$region=="NE"]~data$crime[data$region=="NE"]), col="red")
abline(lm(data$poverty[data$region=="S"]~data$crime[data$region=="S"]), col="green")
abline(lm(data$poverty[data$region=="MW"]~data$crime[data$region=="MW"]), col="black")
abline(lm(data$poverty[data$region=="W"]~data$crime[data$region=="W"]), col="blue")
legend("topleft", c("NE", "S", "MW", "W"), col=c("red", "green", "black", "blue"), pch=16)





##########################################################################
# To answer the research question
plot(data$crime, data$single, pch=16, col=data$region, main="Crime and Single Parenthood", 
     xlab="Crime (per 100,000 people)", ylab="Single Parenthood (% population that are single parents)")
abline(lm(data$single[data$region=="NE"]~data$crime[data$region=="NE"]), col="red")
abline(lm(data$single[data$region=="S"]~data$crime[data$region=="S"]), col="green")
abline(lm(data$single[data$region=="MW"]~data$crime[data$region=="MW"]), col="black")
abline(lm(data$single[data$region=="W"]~data$crime[data$region=="W"]), col="blue")
legend("topleft", c("NE", "S", "MW", "W"), col=c("red", "green", "black", "blue"), pch=16)










