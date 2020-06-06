## National Age and Gender ##
library(car)
library(MASS)
library(tseries)

NatAge <- NationalAgeRate_DataSet_Final_Version_
NatAge <- subset(NatAge, Gender != 'ambos')
NatAge <- subset(NatAge, select= c(5,3,2,7,8,4,1,6))


head(NatAge)
pairs(NatAge[,c(-3,-7:-8)])
cor(NatAge[,c(-3,-7:-8)])

#From the pairs plot we can see a lot of apparent linear correlation between Deaths and all other variables. However, we can also see a lot of correlation
#amongst the variables themselves. This might be due to the fact that a fraction of Hospitalized, is in the ICU, a fraction of Cases are Hospitalized, etc.
#From the cor values we can see Cases-Hospitalized, Cases-ICU, and Hospitalized ICU have a high r value. Correlation with day doesn't seem to be too strong. 
#So, it seems we have a big issue with multicollinearity. Moreover, it seems all our variables are driven by time, as it is a time series. Therefore, we could
#end up with spurious regression, if we proceed. If we want to include these other variables, we need to remove the effect of Days from them and make them 
#stationary, something which is beyond the limits of the course. Another option is not to use these variables in our model.
#We can try to do both.

#This test shows data is non-stationary

adf.test(NatAge$Deaths)
adf.test(NatAge$ConfirmedCases)
adf.test(NatAge$Hospitalized)
adf.test(NatAge$ICU)

boxplot(Deaths ~ AgeRange + Gender, NatAge)

#There seems to be clear differences between Elderly and Young and Adult. Some potential differences between Male and Female
#Trying to put it all into a model, and see the results.


natage.lm <- lm(Deaths ~ ConfirmedCases + Hospitalized + ICU + Days + factor(Gender) + factor(AgeRange), NatAge)
summary(natage.lm)

#Simply trying to make a model with all the variables, gives unexpected signs in the coefficients and infalted std.errors for most variables. 
#However, we get a very high Rsq value and very significant p-values which are usually indicators of spurious regression.

#If we look at the Variance Inflation factors for the explanatory variables of this model, we see high levels of multicollinearity between Cases, Hosp, and ICU,
# as well as some relation to Days
vif(natage.lm)

#We can conduct an Augmented Dickey-Fuller Test to look at the residuals of this model, and see whether we have spurious regression going on or not. The test tests
#for stationarity. Non-stationarity in the residuals indicates spurious regression, which means we are given very significant results for relationships between 
#variables that might not be related at all, but that give apparent relation due to their distribution with time.


adf.test(natage.lm$residuals)

#Very high p-value indicates spurious regression. So, we now need to decide, whether we keep Days as an explanatory variable for deaths, or we try and eliminate the
#effect of time and achieve stationary data. We can do both:

#Firstly, removing Cases, Hospitalized and ICU as they are all related to time, as is Deaths.

plot(Deaths ~ Days, NatAge)
points(NatAge$Days[NatAge$AgeRange == "Young"],(NatAge$Deaths[NatAge$AgeRange == 'Young']))
points(NatAge$Days[NatAge$AgeRange == "Adult"],(NatAge$Deaths[NatAge$AgeRange == 'Adult']), col = 'blue')
points(NatAge$Days[NatAge$AgeRange == "Elderly"],(NatAge$Deaths[NatAge$AgeRange == 'Elderly']), col = 'red')

points(NatAge$Days[NatAge$Gender == "hombres"],(NatAge$Deaths[NatAge$Gender == 'hombres']), pch = 3)
legend()

#It is unclear whether the relationship here is linear or not, it seems Days and Deaths should be exponential in a pandemic like Coronavirus. Nevertheless, we can try
# ot make a linear model and look at the residuals

natage.lm <- lm(Deaths~Days + factor(AgeRange) + factor(Gender), NatAge)
summary(natage.lm)

par(mfrow = c(1,2))
plot(natage.lm, which = c(1,2))
shapiro.test(natage.lm$residuals)
dev.off()

#We can see there is a clear pattern in the residuals, which indicates non-linearity, so our data doesn't seem to be linear. By taking the log of the
#Response variable, we address an exponential effect which we suspect the Deaths will have with the increase of time.

logDeaths <- log(NatAge$Deaths)
NatAge <- cbind(NatAge, logDeaths)
plot(logDeaths ~ Days, NatAge)
plot(log(Deaths) ~ Days, NatAge, type = 'n')
points(NatAge$Days[NatAge$AgeRange == "Young"],log(NatAge$Deaths[NatAge$AgeRange == 'Young']))
points(NatAge$Days[NatAge$AgeRange == "Adult"],log(NatAge$Deaths[NatAge$AgeRange == 'Adult']), col = 'blue')
points(NatAge$Days[NatAge$AgeRange == "Elderly"],log(NatAge$Deaths[NatAge$AgeRange == 'Elderly']), col = 'red')

points(NatAge$Days[NatAge$Gender == "hombres"],log(NatAge$Deaths[NatAge$Gender == 'hombres']), pch = 3)


boxplot(logDeaths ~ factor(Gender) + factor(AgeRange), NatAge)

#We can try and create the model again, with this new transformation
natage.lm <- lm(logDeaths~Days + factor(AgeRange) + factor(Gender), NatAge)
summary(natage.lm)

par(mfrow = c(1,2))
plot(natage.lm, which = c(1,2))
shapiro.test(natage.lm$residuals)
dev.off()

#There still seems to be some interaction that is not accounted for. Moreover, we have some outliers which seem to
#be violating our homoskedasticity and maybe our normal assmptions.

plot(logDeaths ~ Days, NatAge)

#If we look at the plot again it seems the data from the first 5-10 days seem to lose the linearity with the rest
#of the data. At the beginning, we had taken data from 7 days after the lockdown, as the average time for symptoms
#to appear is between 5 and 7 days. However, we see that in the first days of our data, we might still be getting 
#some effects from the pre-lockdown cases.Lets extend our buffer to two weeks after the lockdown, and see how that
#affects our model

NatAge <- subset(NatAge, Days > 7)

natage.lm <- lm(logDeaths~Days + factor(AgeRange) + factor(Gender), NatAge)
summary(natage.lm)

par(mfrow = c(1,2))
plot(natage.lm, which = c(1,2))
shapiro.test(natage.lm$residuals)
dev.off()

#As we can see, removing those outliers, we get a better model, which meets normality assumptions as well as homoskedasticity
#with the exception of some remaining outliers. However, it seems we still have some interaction in the residuals as we have 
#3 bands of residuals with a negative slope. So lets include some possible interactions in our model

natage.full <- lm(logDeaths~Days * factor(AgeRange) * factor(Gender), NatAge)
natage.null <- lm(logDeaths~1, NatAge)
natage.stepfwd <- stepAIC(natage.null, scope = list(lower = natage.null, upper = natage.full),
                          direction = "forward")

summary(natage.stepfwd)

par(mfrow = c(1,2))
plot(natage.stepfwd, which = c(1,2))
shapiro.test(natage.stepfwd$residuals)
dev.off()

#Other option:

#We can try and do this by taking the differences day to day for all the observations, like this we will hopefully get rid of the effects of Days in the data.
#We did that in Excel 

NatAge <- NationalAgeRate_DataSet_Final_Version_

NatAge <- subset(NatAge, AgeRange != 'Total')
NatAge <- subset(NatAge, Gender != 'ambos')
NatAge <- subset(NatAge, DailyCases > 0)
NatAge <- subset(NatAge, DailyICU > 0)
NatAge <- subset(NatAge, DailyHospitalized > 0)
NatAge <- subset(NatAge, Days > 7)
NatAge <- subset(NatAge, Days != 35)

NatAge <- subset(NatAge, select= c(5,9:12, 1, 6, 4))

pairs(NatAge[,c(-2,-6:-7)])

adf.test(NatAge$DailyDeaths)
adf.test(NatAge$DailyCases)
adf.test(NatAge$DailyHospitalized)
adf.test(NatAge$DailyICU)

#Once again, we seem to have a very accelerated, or exponential growth, so we can take the logarithm of deaths to see if 
#we can find a more linear plot
logDeaths <- log(NatAge$Deaths)

pairs(cbind(logDeaths,(NatAge[,c(3:5,8)])))

#We can see our deaths have a logarithmic relationship with 
pairs(cbind(logDeaths,log(NatAge[,c(3:5,8)])))
cor(cbind(logDeaths,log(NatAge[,c(3:5,8)])))

#It now seems we have gotten rid of the effect of Days in the other explanatory variables. 
#However, we still seem to have multicollinearity as Cases-Hospitalized, Hospitalized-ICU and Cases-ICU
#All seem to be correlated, shown by the plots and the correlation coeffs.

natage.lm <- lm(logDeaths ~ log(DailyCases) + log(DailyICU) + log(DailyHospitalized) + Days + factor(AgeRange) + factor(Gender), NatAge)
summary(natage.lm)

vif(natage.lm)
#We still have slightly High VIFs, but nothing to do with the values from before, lets take out Hospitalized and see how it affects our model
natage.lm <- lm(logDeaths ~ log(DailyCases) + log(DailyICU) + Days + factor(AgeRange) + factor(Gender), NatAge)
summary(natage.lm)

vif(natage.lm)

par(mfrow = c(1,2))
plot(natage.lm, which = c(1,2))
shapiro.test(natage.lm$residuals)

#Now that we have decent VIFs, there seeems there could be some interaction in the residuals
natage.full <- lm(logDeaths ~ log(DailyCases) * log(DailyICU) * Days * factor(AgeRange) * factor(Gender) , NatAge)
natage.null <- lm(logDeaths ~ 1, NatAge)


natage.stepfwd <- stepAIC(natage.null, scope = list(lower = natage.null, upper = natage.full),
                          direction = "forward")
summary(natage.stepfwd)


par(mfrow = c(1,2))
plot(natage.stepfwd, which = c(1,2))
shapiro.test(natage.stepfwd$residuals)
adf.test(natage.stepfwd$residuals)


##ANOVA Models ##

#Now that we have seen that Deaths seem to be higher in Elderly men
boxplot((DeathRate) ~ factor(AgeRange), NatAge)
boxplot((DeathRate) ~ factor(Gender), NatAge)

interaction.plot(factor(NatAge$AgeRange), factor(NatAge$Gender), (NatAge$DeathRate))
tapply((NatAge$DeathRate), list(NatAge$Gender, NatAge$AgeRange), mean)

#It seems there is an interaction as the lines aren't completely parallel.

natage.aov <- aov(DeathRate ~ factor(AgeRange) + factor(Gender)+factor(AgeRange)*factor(Gender), NatAge)
summary(natage.aov)

#We see there is a significant interaction, so the results seem to be shaped by the interaction

par(mfrow = c(1,2))
plot(natage.aov, which = c(1,2))

dev.off()

#Maybe we can skip directly to this, as we made this column, and we know Death and Confirmed are exponential
#However, if you think about it, Deaths and ConfirmedCases which make up DeathRate are both exponential, in this time series, so deathrate should be too.

boxplot(log(DeathRate) ~ factor(Gender)+factor(AgeRange) , NatAge)
tapply(log(NatAge$DeathRate), list(NatAge$Gender, NatAge$AgeRange), mean)

interaction.plot(factor(NatAge$AgeRange), factor(NatAge$Gender), log(NatAge$DeathRate))

natage.aov <- aov(log(DeathRate) ~   factor(AgeRange) + factor(Gender), NatAge)
summary(natage.aov)

par(mfrow = c(1,2))
plot(natage.aov, which = c(1,2))
shapiro.test(natage.aov$residuals)
dev.off()

#Residuals vs Fitted shows interaction...

natage.aov <- aov(log(DeathRate) ~ factor(Gender)*factor(AgeRange), NatAge)
summary(natage.aov)

table.means(natage)


NatAgeMen <- subset(NatAge, Gender = 'hombres')
NatAgeWomen <- subset(NatAge, Gender = 'mujeres')

natage.aovmen <- aov(log(DeathRate) ~ factor(AgeRange), NatAgeMen)
summary(natage.aovmen)


TukeyHSD(natage.aovmen)

natage.aovw <- aov(log(DeathRate) ~ factor(AgeRange), NatAgeWomen)
summary(natage.aovw)

TukeyHSD(natage.aovw)

par(mfrow = c(1,2))
plot(natage.aov, which = c(1,2))
shapiro.test(natage.aov$residuals)

dev.off()


