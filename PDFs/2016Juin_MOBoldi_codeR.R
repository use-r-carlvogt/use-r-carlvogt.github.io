require(lme4)
require(lmerTest)
require(car)
require(lattice)

data1 <- read.table("Data1.csv", header=TRUE, sep=",")

## ################################
## 1-way anova: factor=juge
anova(lm(acidité~juge, data=data1))
summary(aov(acidité~juge, data=data1))

## 1-way anova: factor=vin
anova(lm(acidité~vin, data=data1))
summary(aov(acidité~vin, data=data1))

## random factor: juge
mod1 <- lmer(acidité ~ 1 + (1|juge), data=data1)
summary(mod1)
anova(mod1) ## nothing, normal

rand(mod1) ## test random term using LRT.

# comparison
mod1 <- lmer(acidité ~ 1 + (1|juge), data=data1, REML=FALSE)
l1 <- logLik(mod1)
l2 <- logLik(mod0 <- lm(acidité ~ 1, data=data1))
2*(l1 - l2)
1-pchisq(2*(l1 - l2), df=1)
1-pchisq(42, df=1)
rand(mod1)

## ###################################
## Two-way anova

## vin fixed, juge rdm
mod2 <- lmer(acidité ~ vin + (1|juge), data=data1)
summary(mod2)

anova(mod2)
?anova.merMod

rand(mod2)

## Using aov
summary(aov(acidité ~ vin + Error(juge), data=data1))

## Compare to only fixed effect
mod2.2 <- lm(acidité ~ vin + juge, data=data1)
anova(mod2.2)

## ####################################
## Multiple comparisons of wines

require(multcomp) ## see also summary mod2, correlation matrix
summary(glht(mod2, linfct=mcp(vin="Tukey")))

summary(glht(mod2.2, linfct=mcp(vin="Tukey")))

## ####################################
## Longitudinal: sleepstudy

?sleepstudy

xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)", aspect = "xy")

## model with rdm intercept
fm1 <- lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)
anova(fm1)
rand(fm1)

## model with rdm slope
fm2 <- lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
summary(fm2)
anova(fm2)
rand(fm2)
confint(fm2)

?confint.merMod ## profile likelihood method
confint(fm2, method="Wald")
confint(fm2, method="boot") ## note: .sig02 == correlation

## code for ??
fm2.2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), data=sleepstudy)
fm2.2 <- lmer(Reaction ~ Days + (Days||Subject), data=sleepstudy)
summary(fm2.2)
anova(fm2.2)
rand(fm2.2)

## #############################
## Hierarchical model

head(Pastes)
?Pastes

boxplot(strength~batch, data=Pastes)
boxplot(strength~cask, data=Pastes)
boxplot(strength~sample, data=Pastes)

hm <- lmer(strength ~ (1|batch/cask), data=Pastes)
hm.2 <- lmer(strength ~ (1|batch) + (1|batch:cask), data=Pastes) ## idem
summary(hm)
rand(hm)

hm2 <- lmer(strength ~ (1|batch), data=Pastes)

## What is ??
hm3 <- lmer(strength ~ (1|batch)+(1|cask), data=Pastes)  
rand(hm3)
