load(file.path(projecthome, "data", "clinicaltrial.Rdata")) # load data
str(clin.trial) 
install.packages("dplyr")
library(dplyr)
library(gplots)
plotmeans(  formula = mood.gain ~ drug,  # plot mood.gain by drug
            data = clin.trial,           # the data frame
            xlab = "Drug Administered",  # x-axis label
        http://127.0.0.1:11139/graphics/plot_zoom_png?width=939&height=796    ylab = "Mood Gain",          # y-axis label
            n.label = FALSE              # don't display sample size
)

#H0: MeanJoy = MeanPlacebo = MeanAnx
#H1: MeanJoyu =/= MeanPlacebo =/= MeanAnx
#double blind means that both doctors and participants don't know what pill was given to the participant

clin.trial %>%
  group_by(drug) %>%
  dplyr::summarise(ave_mood_gain = mean(mood.gain))




outcome <- clin.trial$mood.gain

group <- clin.trial$drug

gp.means <- tapply(outcome,group,mean)

gp.means <- gp.means[group]
gp.means
dev.from.gp.means <- outcome - gp.means
squared.devs <- dev.from.gp.means ^2


Y <- data.frame( group, outcome, gp.means,
                 dev.from.gp.means, squared.devs )


print(Y, digits = 2)

SSw <- sum(squared.devs)
SSw
#we want to keep the variance as low as possible to get the most similar results (drugs to students example)

gp.means <- tapply(outcome,group,mean)

grand.mean <- mean(outcome)
grand.mean

dev.from.grand.mean <- gp.means - grand.mean
dev.from.grand.mean


squared.devs <- dev.from.grand.mean ^2

gp.sizes <- tapply(outcome,group,length)

wt.squared.devs <- gp.sizes * squared.devs


Y <- data.frame( gp.means, grand.mean, dev.from.grand.mean, 
                 squared.devs, gp.sizes, wt.squared.devs )
print(Y, digits = 2)





SSb <- sum( wt.squared.devs )
print( SSb )

Msb = SSb/2
Msb

MSw= SSw/15
MSw

F_stat = Msb/MSw
F_stat


pf(18.6, df1 = 2, df2 = 15, lower.tail = FALSE)
pf(3.6823, df1 = 2, df2 = 15, lower.tail = FALSE)

#Now we can run the ANOVA and reject the null hypothesis

#function for ANOVA vv
 #formula is checking the impact of a drug on a mood.gain for the data clin.trial
aov(formula = mood.gain ~ drug, data = clin.trial) 

my.anova <- aov( mood.gain ~ drug, clin.trial ) 
#residuals is just an error
#DRUG is the between subject.

summary(my.anova)
#if you want pvalue you have to run the summary of ANOVA


#effect size
SStot <- SSb + SSw          # total sums of squares
eta.squared <- SSb / SStot  # eta-squared value
print( eta.squared )

library(lsr)
posthocPairwiseT( x = my.anova, p.adjust.method = 'none' )
posthocPairwiseT( x = my.anova, p.adjust.method = 'bonferroni' ) # adjusted p-value = original p-value * number of tests
posthocPairwiseT( x = my.anova, p.adjust.method = 'holm' ) #  the biggest p-value remains unchanged, the second biggest p-value is doubled, the third biggest p-value is tripled, and so on


TukeyHSD(my.anova,p.adj = "none") # little bit different but very similary p values to the above

### ANOVA assumptions

library(car)
leveneTest(my.anova)

leveneTest(y = mood.gain ~ drug, data = clin.trial)   # y is a formula in this case
leveneTest(y = clin.trial$mood.gain, group = clin.trial$drug)   # y is the outcom


# Checking the normality assumption
my.anova.residuals <- residuals( object = my.anova )   # extract the residuals
hist( x = my.anova.residuals ) 
qqnorm( y = my.anova.residuals )   
shapiro.test( x = my.anova.residuals )


### #LINEAR REGRESSION

# The following data set is fictitious, but based on real events. 
# Suppose I’m curious to find out how much my infant son’s sleeping habits affect my mood. 
# Let’s say that I can rate my grumpiness very precisely, 
# on a scale from 0 (not at all grumpy) to 100 (grumpy as a very, very grumpy old man). 
# And, lets also assume that I’ve been measuring my grumpiness, 
# my sleeping patterns and my son’s sleeping patterns for quite some time now. 
# Let’s say, for 100 days. And, being a nerd, I’ve saved the data as a file called parenthood.Rdata.

library(ggplot2)
load("parenthood.Rdata")
parenthood

parenthood %>% 
  ggplot(aes(y=dan.grump,x=dan.sleep)) +
  geom_point(size = 2) +
  geom_smooth(method=lm,se=FALSE)


regression.1 <- lm( formula = dan.grump ~ dan.sleep,  
                    data = parenthood )  

print( regression.1 )
standardCoefs( regression.1 )

X <- parenthood$dan.sleep  # the predictor
Y <- parenthood$dan.grump  # the outcome

Y.pred <- -8.94 * X  +  125.97

SS.resid <- sum((Y - Y.pred)^2 )
print( SS.resid )


SS.tot <- sum((Y- mean(Y))^2)
print(SS.tot)

R.squared <- 1 - (SS.resid / SS.tot)
print( R.squared )

### Regression and correlation

r <- cor(X, Y)  # calculate the correlation
print( r^2 )  

cor.test( x = parenthood$dan.sleep, y = parenthood$dan.grump )
### Formula for t-statistics for cor test if r * square root N-2/1-r squared





r * sqrt((100-2)/(1-r^2))

sum(abs(regression.1$residuals))/100


### note that t value = Estimate/Std Erroe

### Diagnosing regression model

hist( x = residuals( regression.1 ),   # data are the residuals
      xlab = "Value of residual",      # x-axis label
      main = "",                       # no title 
      breaks = 20                      # lots of breaks
)


yhat.2 <- fitted.values( object = regression.1 )
plot( x = yhat.2, 
      y = parenthood$dan.grump,
      xlab = "Fitted Values",
      ylab = "Observed Values" 
)


plot(x = regression.1, which = 1)
plot( x = regression.1, which = 2)


## Multiple linear regression

regression.2 <- lm( formula = dan.grump ~ dan.sleep + baby.sleep,  
                    data = parenthood )
summary(regression.2)



## Which model to choose?
full.model <- lm( formula = dan.grump ~ dan.sleep + baby.sleep + day,  
                  data = parenthood  )

step( object = full.model,     # start at the full model
      direction = "backward"   # allow it remove predictors but not add them
)

## Extra task: Explain the following command and the results
summary(lm(mood.gain ~ drug, clin.trial))


### Exercise


### Fit (and interpret) the regression model for the following data:

# A common observation in ecology is that species diversity decreases as you get further
# from the equator. To see whether this pattern could be seen on a small scale, 
# we used data from the Audubon Society's Christmas Bird Count, 
# in which birders try to count all the birds in a 15-mile diameter area during one winter day.
# We looked at the total number of species seen in each area on the Delmarva Peninsula during 
# the 2005 count. Latitude and number of bird species are the two measurement variables;
# location is the hidden nominal variable.
Input <- ("Town                 State  Latitude  Species
  'Bombay Hook'          DE     39.217    128
  'Cape Henlopen'        DE     38.800    137
  'Middletown'           DE     39.467    108
  'Milford'              DE     38.958    118
  'Rehoboth'             DE     38.600    135
  'Seaford-Nanticoke'    DE     38.583     94
  'Wilmington'           DE     39.733    113
  'Crisfield'            MD     38.033    118
  'Denton'               MD     38.900     96
  'Elkton'               MD     39.533     98
  'Lower Kent County'    MD     39.133    121
  'Ocean City'           MD     38.317    152
  'Salisbury'            MD     38.333    108
  'S Dorchester County'  MD     38.367    118
  'Cape Charles'         VA     37.200    157
  'Chincoteague'         VA     37.967    125
  'Wachapreague'         VA     37.667    114")
(data.set <- read.table(textConnection(Input), header = TRUE))
plot(Species ~ Latitude, data = data.set, pch = 20)
cor.test(~ Species + Latitude, data = data.set) # Significancy of Pearson's coeff




model <- lm(Species ~ Latitude+State, data = data.set)
hist(model$residuals)
shapiro.test(model$residuals)





plot(Species ~ Latitude, data = data.set, pch = 20,
     xlab = "Latitude", ylab = "Number of Species",
     main = "Species vs. Latitude")
abline(model, col = "red")


summary(model)

######################################


#OLS

setwd("~/Desktop/stats for ml/OLS GLM")
library(haven)
df<- read_sav("Air-Traffic_Controllers.sav")
View(df)

summary(df$Group)

df$Group <- factor(df$Group)
summary(df$Group)

levels(df$Group) <- c("Non-smoking", "Delayed Smoking", "Active Smoking")
summary(df$Group)

library(stats)
analysis <- aov(Memory_Err~Group, data = df)
summary(analysis)

qf(0.025, 2, 42, lower.tail = FALSE)

#Visualize the differences between group means - HOMEWORK


TukeyHSD(analysis)

TukeyHSD(analysis, p.adj = "bonferroni") #fdr - False Discovery Rate

#Visualization of Post hoc test
par()
old.par <- par(mar = c(4, 15, 2, 2))
par(old.par)
plot(TukeyHSD(analysis, p.adj = "bonferroni", conf.level=.95), las = 2)


#Repeated measures ANOVA
df <- read_sav("one-way ANOVA repeated measures.sav")
summary(df)
head(df) #wide format

#changing from wide to long
install.packages("tidyr")
library(tidyr)
df_long = df%>%
  gather("before", "after_5_min", "after_10_min",key = time, value = score)

head(df)  
head(df_long)

repeated <- aov(score~time+Error(ID/time), data = df_long)
summary(repeated)

#do not do independent ANOVA when you have repeated measures design!!! 
#always include error term
#install.packages()
library("emmeans")
emm <- emmeans(repeated, ~time)
pairs(emm, adjust = "bonferroni")

library(ggpubr)
ggline(df_long, x = "time", y = "score", 
       add = "mean_ci")


#Regression and ANOVA are in the same group of General Linear Models
#Normal distribution of data (DV)
#Linear relationship
reg.analysis <- lm(Memory_Err~Group, data = df)
summary(df)
summary(reg.analysis)

#R squred for simple regression and adjusted R squared for multiple
#How much variance in the dependent variable you explain by using predictors
#Report it in the percentages.

library(dplyr)
df %>% group_by(Group) %>%
  summarize(Mean = mean(Memory_Err),
            SD = sd(Memory_Err))


#Interaction Analysis (ANCOVA) 
library(ggplot2)
library(MASS)
head(anorexia)

anorexia["WtGain"] = anorexia["Postwt"] - anorexia["Prewt"]

head(anorexia)

ggplot(anorexia, aes(x = Treat, y = WtGain)) + 
  geom_point() 

#Regression OLS - assumptions to meet (linearity, normality)
anorexia_lin_model <- lm(WtGain~Treat, data = anorexia)
summary(anorexia_lin_model)

anorexia$Treat <- factor(anorexia$Treat)

contrasts(anorexia$Treat)


anorexia_with_interaction <- lm(WtGain~Treat+Prewt+Treat*Prewt, data = anorexia)
summary(anorexia_with_interaction)

ggplot(anorexia, aes(x = WtGain, y = Prewt, colour = Treat)) + 
  geom_point(size = 2.5) +
  geom_smooth(method = lm, se = FALSE, aes(linetype = Treat))

#relationship in Control condition between two variables was different 
#than in cognitive behavioral and family treatment.


#3d scatterplot 
head(Boston)
reg_analysis <- lm(medv~age, data = Boston)
ggplot(Boston, aes(x = age, y = medv)) + 
  geom_point()

reg_analysis_2_var <- lm(medv~age+crim, data = Boston)
summary(reg_analysis_2_var)
ggplot(Boston, aes(x = age, y = medv, color = crim)) + 
  geom_point()

library("scatterplot3d")

scatterplot3d(Boston$medv, Boston$crim, Boston$age, type = "h", pch = 19)

require(ggiraph)
require(ggiraphExtra)
require(plyr)

ggPredict(reg_analysis_2_var, interactive = TRUE)

plot(reg_analysis_2_var)
plot(reg_analysis_2_var, 4)
plot(reg_analysis_2_var, 5)

library(car)
durbinWatsonTest(reg_analysis_2_var) #insignificant - assumption is met

#heteroscedasticity
ncvTest(reg_analysis_2_var) #insignificant - assumption is met



#GENERALIZED LINEAR MODELS
#Normality, linearity assumptions are not required

reg_analysis_glm <- glm(medv~age+crim, data = Boston, family='gaussian')
summary(reg_analysis_2_var) #here we have used lm ()
summary(reg_analysis_glm)

#if you have gaussian distribution, use LM (more efficient)


#Logistic
install.packages("AER")
library(AER)
data(Affairs)
head(Affairs)
summary(Affairs)

#creating new variable for Affairs
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0

summary(Affairs$ynaffair)

Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels = c(0,1), 
                           labels = c("No", "Yes"))


summary(Affairs$ynaffair)
table(Affairs$ynaffair)

fit_full <- glm(ynaffair~.-affairs, data = Affairs, family = binomial()) #you can also define link
summary(fit_full) 

#install.packages("rsq")
#library(rsq)
#rsq(fit_full) 
#R squared - amount of variance our model explains in the dependent variable



fit_full <- glm(ynaffair~.-affairs, data = Affairs, family = binomial()) #you can also define link
summary(fit_full)

fit_reduced <- glm(ynaffair~age+yearsmarried+religiousness+rating, data = Affairs, family = binomial()) #you can also define link
summary(fit_reduced) 

anova(fit_reduced, fit_full, test = "Chisq")  
#reduced model fit data as good as full model - insignificant Chi2 (0.2108)

#How to select best predictors using automatic method
regression_stepwise <- step(fit_full, direction = "backward")

#For logistic regression we compare AIC - the quality of the logistic model
summary(regression_stepwise)

#Interpretation of coefficients

coef(regression_stepwise) #log (odds)
exp(coef(regression_stepwise))
#The odds of the affairs:
#increase for each year married (because exp of coefficient is higher than 1)
#decrease for religiousness and rating (because exp of coefficient is lower than 1)

#in case if categorical variable is significant you have coefficient for the gender male - 
#the odds of affair increase for males compared to females (exp coefficient is higher than 1)


#Correction for overdispertion
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness + rating + gender, family = quasibinomial(), data = Affairs) #this is a correction for overdispertion (quasibinomial)
pchisq(summary(fit.od)$dispersion * fit.od$df.residual,
       fit.od$df.residual, lower = F)

#p value for the test should be insignificant
#p = 0.3291751 -> no overdispertion is present
#we can use the model from family binomial () no need to use quasi ()

#Visualize the model
#3 features is possible ggPredict(name_of_model, interactive = TRUE)


#Poisson regression - for counts (like number of seizures, number of likes)
data (breslow.dat, package = "robust")
names(breslow.dat)

summary(breslow.dat[c(6,7,8,10)])

#Initial exploration of the data EDA
ggplot(breslow.dat, aes(x = sumY)) +
  geom_histogram()
#Data is skewed

ggplot(breslow.dat, aes(y = sumY, x = Trt)) +
  geom_boxplot()
#possible outliers, different variances in groups, non-normal distribution

fit <- glm(sumY~Base+Age+Trt, data = breslow.dat, family=poisson())
summary(fit) #
coef(fit) #log mean
#One  year increase in age is linked to log mean increase of number 
#of seizures by 0.23. Difficult to understand
exp(coef(fit) )
#Base and age are linked to increase in seizures
#For drug group we have decrease in comparisong to placebo
#If the age increase by 1 -> seizures are multiplied by 1.023 (bigger number)

#Correction for overdispertion
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type="poisson") #presence of overdispertion
#significant p value -> overdispertion present


fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
              family=quasipoisson())
summary(fit.od)










