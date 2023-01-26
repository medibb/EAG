# git commit -am "msg"
# git push - u origin master

### Data wrangling

rm() #변?????????

## Excel 
library(readxl) 
eag1 <- read_excel(path = "C:/Users/Jaehyun/OneDrive - 고신대학교/0. project/EAG/data/EAG.xlsx",sheet = 1, col_names=TRUE)
eag2 <- read_excel(path = "C:/Users/Jaehyun/OneDrive - 고신대학교/0. project/EAG/data/EAG.xlsx",sheet = 2, col_names=TRUE)
eag2AE <- subset(eag2, eag2$move == 1)
eag1abs <- abs(eag1)

eag2$channel1 <- ifelse(eag1$channel1 == boxplot(eag1$channel1~eag1$move)$out, NA, eag1$channel1)
eag2$channel2 <- ifelse(eag1$channel2 == boxplot(eag1$channel2~eag1$move)$out, NA, eag1$channel2)
eag2$channel3 <- ifelse(eag1$channel3 == boxplot(eag1$channel3~eag1$move)$out, NA, eag1$channel3)
eag2$channel4 <- ifelse(eag1$channel4 == boxplot(eag1$channel4~eag1$move)$out, NA, eag1$channel4)
eag2$channel5 <- ifelse(eag1$channel5 == boxplot(eag1$channel5~eag1$move)$out, NA, eag1$channel5)
eag2$channel6 <- ifelse(eag1$channel6 == boxplot(eag1$channel6~eag1$move)$out, NA, eag1$channel6)
eag2$channel7 <- ifelse(eag1$channel7 == boxplot(eag1$channel7~eag1$move)$out, NA, eag1$channel7)
eag2$channel8 <- ifelse(eag1$channel8 == boxplot(eag1$channel8~eag1$move)$out, NA, eag1$channel8)


eag3$channel1 <- ifelse(eag1$channel1 > -10000, ifelse(eag1$channel1 > 10000, NA, eag1$channel1),NA)
eag3$channel2 <- ifelse(eag1$channel2 > -10000, ifelse(eag1$channel2 > 10000, NA, eag1$channel2),NA)
eag3$channel3 <- ifelse(eag1$channel3 > -10000, ifelse(eag1$channel3 > 10000, NA, eag1$channel3),NA)
eag3$channel4 <- ifelse(eag1$channel4 > -10000, ifelse(eag1$channel4 > 10000, NA, eag1$channel4),NA)
eag3$channel5 <- ifelse(eag1$channel5 > -10000, ifelse(eag1$channel5 > 10000, NA, eag1$channel5),NA)
eag3$channel6 <- ifelse(eag1$channel6 > -10000, ifelse(eag1$channel6 > 10000, NA, eag1$channel6),NA)
eag3$channel7 <- ifelse(eag1$channel7 > -10000, ifelse(eag1$channel7 > 10000, NA, eag1$channel7),NA)
eag3$channel8 <- ifelse(eag1$channel8 > -10000, ifelse(eag1$channel8 > 10000, NA, eag1$channel8),NA)


#proportional value table generation
eag1AP <- data.frame(0,0,0,0,0,0,0,0)
colnames(eag1AP)<- c('channel1','channel2','channel3','channel4','channel5','channel6','channel7','channel8')

  for (i in 1:60){
    eag1AP <- rbind(eag1AP, prop.table(eag1abs[i,2:9]))
  }
eag1AP <- eag1AP[-1,]
eag1AP <- cbind(eag1AP, eag1[10])




#plot by channel
par(mfrow = c(10, 2))
for (i in 1:20){
  plot(x=c(1:8),y=eag3[i,2:9], type="l", ylim=c(-10000,10000),ann=F, axes=F, col=sample(1:255))
}

par(mfrow = c(1, 1))
boxplot(eag3[2:9])#boxplot

#plot by channel with absolute & proportion values
par(mfrow = c(10, 2))
for (i in 1:20){
  plot(x=c(1:8),y=eag1AP[i,1:8], type="l", ylim=c(0,1),ann=F, col=sample(1:255))
}

# ggplot
ggplot(eag2) +
 aes(x = amplitude, y = channel, colour = channel) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_gradient() +
 theme_minimal() +
 facet_wrap(vars(move))

par(mfrow = c(1, 1))
boxplot(eag1AP[1:8])


library(moonBook)
mytable(move~., data=eag1, digits=3)
mytable(channel~amplitude, data=eag2AE, digits=3)
mytable(move~., data=eag1AP, digits=3)
mytable(move~., data=eag2, digits=3)
mytable(move~., data=eag3, digits=3)



#anova
res=aov(channel4~move,data = eag3)

library(agricolae)
#post_hoc
print(LSD.test(res,"move",p.adj="bonferroni",group=F)$comparison)



write.csv(eag, "MM.csv")   #Change name!!


### Data ???검

## Power analysis
install.packages("pwr")
library(pwr)
mean.1 <- mean(Data$`S(R)`[Data$KL_over2==0])
mean.2 <- mean(Data$`S(R)`[Data$KL_over2==1])
sd.1 <- sd(Data$`S(R)`[Data$KL_over2==0])
sd.2 <- sd(Data$`S(R)`[Data$KL_over2==1])

effectSize <- abs(mean.1-mean.2)/(sqrt((sd.1^2-sd.2^2)/2))

pwr.t.test(d=effectSize, power = .8, sig.level = .05, type = "two.sample", alternative = "two.sided")

#distribution find.
install.packages("MASS")
library(propagate)
fitDistr(Data3$`flat(R)`, plot = "hist", distsel = c(1:20))

# remove outlier
boxstat <- boxplot(AmpBtp~age+sex, data = MM) #draw plot
boxstat <- boxstat$out[boxstat$out < mean(boxstat$stats[3,])] #remove one-sided(lower) outlier
MMout   <- unique(rbind(MMout,subset(MM, MM$AmpBtp %in% boxstat))) #save outlier

# ????????
x <-Data3$Age
mean(x)
sd(x)



### Statistic analysis

# T-test
install.packages("moonBook")
library(moonBook)
mytable(age+sex~., data=MM, digits=3)
mytable(sex+age~., data=MM)
mytable(age+sex~., data=MM2, digits=3)
mytable(sex+age~., data=MM2)

moonBook::densityplot(~, data=)
shapiro.test()
shapiro.test()
wilcox.test(~, data=)

var.test()
t.test(~, data=, var.equal=TRUE) # Student's t test
t.test(~, data=, var.equal=FALSE) # Welch's test

#GLMM
install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)

## two-way ancova lm(depVar ~ covar1 + covar2  + ... + covar# + IndVar*IndVar, data=data)
output <- lm(MM$AreaNeg ~ Height + Muscle + Fat + Gender*Age, data = MM)
output <- lm(MM$DurNeg ~ BMI + MSK + Gender*Age, data = MM)
output <- lm(MM$AmpPtp ~ BMI + MSK + Gender*Age, data = MM)
output <- lm(MM$Lon ~ Side + Muscle + Fat + MSK + Height + Weight + SI + sex*age, data = MM)
summary.aov(output)

  ##etas
res = 161.66
15.59 / (res + 15.59)
0.01 / (res +0.01)
0.76 / (res +0.76)
0.01 / (res +0.01)

 #ANCOVA post-hoc
install.packages("emmeans") 
library(emmeans)




### correlation analysis ###
plot(A~B) #????????? ??????
cor(MM[,c(7:11,19)], use='complete.obs', method='pearson') # only r-value

## r value with p value: corr.test
install.packages("psych")
library(psych)
corr.test(MM[,c(2:6,18)], use = 'complete', method = 'pearson') #p-value & digit = 2


## 그림 그리??? + chart, r-value, p-value
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(MM[,c(2:6,18)], histogram=, pch="+") #18 = SI




#post hocs on the adjusted means
#effect("IV column", output)
install.packages("effects")
library(effects)
effect("Age", output)
effect("Gender", output)

MM$Age <-as.factor(MM$Age)
MM$Gender <-as.factor(MM$Gender)

male <- subset(MM, Gender == 1)
female <- subset(MM, Gender == 2)
young <- subset(MM, Age == 1)
old <- subset(MM, Age == 2)

male.out = lm(male$Lat ~ Height + Age, data = male)
female.out = lm(female$Lat ~ Height + Age, data = female)
young.out = lm(young$Lat ~ Height + Gender, data = young)
old.out = lm(old$Lat ~ Height + Gender, data = old)

install.packages("multcomp")
library(multcomp)
male_post = glht(male.out, linfct=mcp(Age = "Tukey"))
female_post = glht(female.out, linfct=mcp(Age = "Tukey"))
young_post = glht(young.out, linfct=mcp(Gender = "Tukey"))
old_post = glht(old.out, linfct=mcp(Gender = "Tukey"))
summary(male_post)
summary(female_post)
summary(young_post)
summary(old_post)

install.packages("ggplot2")
library(ggplot2)
graphdata = na.omit(MM)
bargraph = ggplot(graphdata, aes(Gender, Lat, fill = Age))
ggplot(graphdata, aes(Gender, Lat, fill = Age))


# glm #gamma distibution
out2 <- glm(`flat(R)`~Age+Sex+BMI+FlexPain0+Mcmurray+KL_over2+swelling, family=Gamma(link = log), data=Data3)
summary(out2)
anova(out2)

#multivariable analysis, lmer
require(lme4)
require(lmerTest)
eff <- lmer(MM$AmpBtp ~ age + sex + side + Muscle + Fat + MSK + BMI + Height + Weight +(1|Motor), data = MM) #multivariable
coef(summary(eff))[ , "Estimate"] # lmer???? estimate ????


#latex transform
install.packages("moonBook")
library(moonBook)
---------------
  
  anova_test(MM$Lat ~ Height + age*gender)
  
  
