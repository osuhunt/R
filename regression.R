#######################################
#       Name: Zachary Hunt            #
#       Task: Lab 8 / Regression      #
#       Date: 11/30/2018              #
#######################################

# "Moving to Fitness Program" in Colorado - proving that
# living in Colorado makes you healthier

# assuming alpha = 0.05 for all tests

# importing the data
data <- read.csv(file.choose())
# Making sure it uploading correctly
names(data)
summary(data)
head(data)
# Looks like it uploaded well

########################
#      Question 1      #
########################

# At the start of the MtFP do the treatment and control groups
# (i.e. movers and stayers) have statistically significant
# differences in BMI? Why is it important for a treatment and
# a control group to be equal at the start of a study? 

# subsetting
movers <- subset(data, Movers == 1)
stayer <- subset(data, Movers == 0)
# looking for normality
hist(movers$Start_BMI, xlab="Start BMI", ylab="Frequency", main="Treatment group / Movers")
hist(stayer$Start_BMI, xlab="Start BMI", ylab="Frequency", main="Control group / Stayers")
# looks great
# checking if variances are equal
var.test(movers$Start_BMI, stayer$Start_BMI)
# The p value is 0.9504 so the variances are equal, "TRUE"
# I can run a two sampled t test now, unpaired as the data is coming from
# different areas
t.test(movers$Start_BMI, stayer$Start_BMI, var.equal=TRUE, paired=FALSE)
# with a p value of 0.3462, the null of no difference cannot be 
# rejected, thus the starting BMI for both groups are not statistically
# different from one another. It's important that the treatment and 
# control groups are equal at the start of this study because if they
# are already statistically different at the beginning, having people
# move to Colorado to prove healthiness won't show any difference from
# the start to the end even if moving really does improve health. Also,
# if the movers are already statistically significanty different in
# BMI at the start, say they have a signicantally lower BMI than the
# stayers, than it would bias the study and not really prove much at 
# the end if they still have a higher BMI.

########################
#      Question 2      #
########################

# For both the treatment group (the movers) and the control group
# (the stayers) is BMI different at the end of the study when 
# compared to the beginning? If so, did the mean BMI increase or
# decrease? 

# checking for normality
hist(movers$End_BMI, xlab="End BMI", ylab="Frequency", main="Treatment group / Movers")
hist(stayer$End_BMI, xlab="End BMI", ylab="Frequency", main="Control group / Stayers")
# looks great
# checking if variances are equal
var.test(movers$Start_BMI, movers$End_BMI)
var.test(stayer$Start_BMI, stayer$End_BMI)
# with p values of 5.37e-12 and < 2.2e-16 respectively, the null
# of no difference can be rejected thus the variances are not equal
# now running a paired (same group, different times) 
t.test(movers$Start_BMI, movers$End_BMI, var.equal=FALSE, paired=TRUE)
t.test(stayer$Start_BMI, stayer$End_BMI, var.equal=FALSE, paired=TRUE)
# with a p value of < 2.2e-16 for the mover group, the null of
# no difference can be rejected meaning the starting and ending
# BMI are statistically different among movers. With a p value of 
# < 2.2e-16 for the group that stayedm the null of no difference can
# be rejected meaning the starting and ending BMI are statistically
# different among the group that stayed.

# checking how they changed
mean(movers$Start_BMI) #27.26
mean(movers$End_BMI) #29.16
mean(stayer$Start_BMI) #27.08
mean(stayer$End_BMI) #29.07
# in both the moving and staying group, the ending BMI increased
# from the starting BMI.


########################
#      Question 3      #
########################

# Did BMI change for those who moved vs. those who stayed?  If so, 
# did the mean BMI increase or decrease?  NOTE: You will have to 
# create a new variable, which is the change in BMI between the start
# and end of the program.  Then use this to compare between the two
# groups. 

# creating difference variables
BMIchangemovers <- (movers$End_BMI - movers$Start_BMI)
BMIchangestyaer <- (stayer$End_BMI - stayer$Start_BMI)
# checking if the variances are equal
var.test(BMIchangemovers, BMIchangestyaer)
# with a p value of 0.4698, the null of no difference cannot be rejected
# thus the variances are equal
# running an unpaired two sample t test
t.test(BMIchangemovers, BMIchangestyaer, var.equal=TRUE, paired=FALSE)
# with a p value of 0.692, the null of no difference cannot be rejected
# thus there is not a significant change in BMI for those who moved vs
# those who styaed. However, although it's not significant, it was shown
# in Q2 that it did increase in both cases.

########################
#      Question 4      #
########################

# Run a regression to describe the relationship between BMI and 
# physical activity at the end of the study.  How much of the 
# variation in BMI can be described by physical activity?

# checking for normality
hist(data$PhysA_End, xlab = "Average Daily Physical Acticivty (End)", ylab = "Frequency", main = "Physical Activty at the End")
hist(data$End_BMI, xlab = "BMI", ylab = "Frequency", main = "BMI at the End")
# looks great
# checking for linearity
plot(data$End_BMI, data$PhysA_End, xlab = "BMI", ylab = "Avg Physical Activity", main = "End of study BMI v Physical Activity")
# not an obvious linear relationship, but there may be a slight one
# checking for evidence that they are related
cor.test(data$End_BMI, data$PhysA_End)
# with an r of -.09, there seems to be a weak / no negative relationship
# now running a regression test to determine how physical activity influences BMI
ols1 <- lm(data$End_BMI ~ data$PhysA_End, data = data) # dependent ~ independent
summary(ols1)
# the output shows that as the BMI increases by 1, physical activity decreases
# by 0.013529 minutes. This makes sense as one would expect less physical
# activity to result in a higher BMI. With an adjusted R-squared value
# of 0.007766, 0.7766% of the variation in BMI can be attributed to 
# physical activity

# using residulas to examine model fit
par(mfrow = c(2, 2))
plot(ols1, las = 1)
# the model fits well with a small amount of heteroskedasticity


########################
#      Question 5      #
########################

# Run a regression that describes the relationship between BMI and 
# physical activity at the end of the study and include the variable
# for treatment group.  When you consider both things, how much of 
# the variation in BMI can you describe?

# running a multiple regression test to determine how physical activity influences BMI
# with movers variable
ols1 <- lm(data$End_BMI ~ data$PhysA_End + data$Movers, data = data) # dependent ~ independent
summary(ols1)
# the output shows that as the BMI increases by 1, physical activity decreases
# by 0.001748 minutes. With an adjusted R-squared value
# of 0.00729 , 0.729% of the variation in BMI can be attributed to 
# physical activity


