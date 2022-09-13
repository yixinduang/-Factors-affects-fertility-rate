library(readxl)
library(GGally)
library(tidyverse)
library(ggplot2)

p3 <- read_excel("D:/xin/306/project/p31.xlsx")
p3$region <- as.factor(p3$region)
boxplot(fr~region, data=p3)
##Different region have different mean of fr.

#ggpairs(p3,aes(color=region))
##not linear: GDP, income, population 
full <- lm(fr~., data = p3)
summary(full)
plot(fitted(full), resid(full))

full2 <- lm(fr~.+I(GDP^2)+I(incom^2)+I(Population^2), data = p3)
summary(full2)
plot(fitted(full2), resid(full2))


#model one-------------------------------------------------------------

reg <- lm(fr~.+I(GDP^2)+I(Population^2), data = p3)
summary(reg)
plot(fitted(reg), resid(reg))
##Since the income^2 is not significant and the adjR^2 increases. so we do not need the income^2 term.

#Check m_rate vs fr
lm0 <- lm(fr~region, data=p3)
summary(lm0)

lm <- lm(fr~region+m_rate, data=p3)
summary(lm)
plot(fitted(lm), resid(lm))



#model two------------------------------------------------------------

##m_rate is not significant therefore, we can ignore it and refit the model.
reg2 <- lm(fr~.+I(GDP^2)+I(Population^2)-m_rate, data = p3)
summary(reg2)
plot(fitted(reg2), resid(reg2))

##Check incom vs fr
lm1 <-lm(fr~region+incom, data=p3)
summary(lm1)
plot(fitted(lm1), resid(lm1))



#model three----------------------------------------------------------
##not significant, delete income.
reg3 <- lm(fr~.+I(GDP^2)+I(Population^2)-m_rate-incom, data = p3)
summary(reg3)
plot(fitted(reg3), resid(reg3))

##Check EMP
lm2 <- lm(fr~region+EMP, data=p3)
summary(lm2)
plot(fitted(lm2), resid(lm2))
##Since EMP is significant but in full model, it becomes non-significant, therefore, we can assume that there is multicollinearity.
##We need to check the correlation
## Add correlation matrix




#model four-----------------------------------------------------------
reg4 <- lm(fr~.+I(GDP^2)+I(Population^2)-m_rate-incom-EMP, data = p3)
summary(reg4)
plot(fitted(reg4), resid(reg4))

#correlation
pdata=data.frame(fertility=p3$fr,gdp=p3$GDP,gdp_square=(p3$GDP^2),income=p3$incom,marrage_rate=p3$m_rate,employment_rate=p3$EMP,population=p3$Population,population_square=(p3$Population^2))

cor(pdata)

#col between GDP and Poplulation




#model five-----------------------------------------------------------

reg6 <- lm(fr~.+I(GDP/Population)+I((GDP-mean(GDP))^2)+I((Population-mean(Population))^2)-m_rate-incom-GDP-Population, data = p3)
summary(reg6)
plot(fitted(reg6), resid(reg6))





#model six-----------------------------------------------------------
reg7 <- lm(fr~.+I(GDP/Population)+I((GDP/Population-mean(GDP/Population))^2)+I((Population-mean(Population))^2)-m_rate-incom-GDP, data = p3)
summary(reg7)
plot(fitted(reg7), resid(reg7))



#####################################