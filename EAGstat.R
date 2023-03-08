# git commit -m "msg", if it dosen't work? git add first
# git push or #git push - u origin "main/master"

### Data wrangling
#eag1:all channels by person, eag2:amplitudes by channel & person
library(readxl)
eag1 <- read_excel(path = "./data/EAG.xlsx",sheet = 1, col_names=TRUE)
eag4 <- read_excel(path = "./data/EAG.xlsx",sheet = 2, col_names=TRUE)
eag2AE <- subset(eag2, eag2$move == 1) #AE; active extension; move == 1

#eag2 generation(remove outlier by IQR), code error for channel6 & 7
eag2$channel1 <- ifelse(eag1$channel1 == boxplot(eag1$channel1~eag1$move)$out, NA, eag1$channel1)
eag2$channel2 <- ifelse(eag1$channel2 == boxplot(eag1$channel2~eag1$move)$out, NA, eag1$channel2)
eag2$channel3 <- ifelse(eag1$channel3 == boxplot(eag1$channel3~eag1$move)$out, NA, eag1$channel3)
eag2$channel4 <- ifelse(eag1$channel4 == boxplot(eag1$channel4~eag1$move)$out, NA, eag1$channel4)
eag2$channel5 <- ifelse(eag1$channel5 == boxplot(eag1$channel5~eag1$move)$out, NA, eag1$channel5)
eag2$channel6 <- ifelse(eag1$channel6 == boxplot(eag1$channel6~eag1$move)$out, NA, eag1$channel6)
eag2$channel7 <- ifelse(eag1$channel7 == boxplot(eag1$channel7~eag1$move)$out, NA, eag1$channel7)
eag2$channel8 <- ifelse(eag1$channel8 == boxplot(eag1$channel8~eag1$move)$out, NA, eag1$channel8)

#eag3 generation(remove outlier roughly)
eag3 <- eag1
eag3$channel1 <- ifelse(eag1$channel1 > -10000, ifelse(eag1$channel1 > 10000, NA, eag1$channel1),NA)
eag3$channel2 <- ifelse(eag1$channel2 > -10000, ifelse(eag1$channel2 > 10000, NA, eag1$channel2),NA)
eag3$channel3 <- ifelse(eag1$channel3 > -10000, ifelse(eag1$channel3 > 10000, NA, eag1$channel3),NA)
eag3$channel4 <- ifelse(eag1$channel4 > -10000, ifelse(eag1$channel4 > 10000, NA, eag1$channel4),NA)
eag3$channel5 <- ifelse(eag1$channel5 > -10000, ifelse(eag1$channel5 > 10000, NA, eag1$channel5),NA)
eag3$channel6 <- ifelse(eag1$channel6 > -10000, ifelse(eag1$channel6 > 10000, NA, eag1$channel6),NA)
eag3$channel7 <- ifelse(eag1$channel7 > -10000, ifelse(eag1$channel7 > 10000, NA, eag1$channel7),NA)
eag3$channel8 <- ifelse(eag1$channel8 > -10000, ifelse(eag1$channel8 > 10000, NA, eag1$channel8),NA)


#flex, ext df 
sheets <- excel_sheets("./data/EAG.xlsx")
sheets <- sheets[7:length(sheets)]
data <- lapply(sheets, function(x) read_excel("./data/EAG.xlsx", col_names=FALSE, sheet = x, range = "A3:Z3"))
result <- do.call(rbind, data)
flex <- result[,1:24]
flex[] <- lapply(flex, as.numeric)

data <- lapply(sheets, function(x) read_excel("./data/EAG.xlsx", col_names=FALSE, sheet = x, range = "A4:Z4"))
result <- do.call(rbind, data)
ext <- result[,1:24]
ext[] <- lapply(ext, as.numeric)


flex <- cbind(id = c(1:20), flex = 1, flex)
ext <- cbind(id = c(1:20), flex = 2, ext)
eag4 <- rbind(flex,ext)


#flexprop, extprop df
A <- flex[, 2:9]
B <- A / rowSums(abs(A)) 
C <- flex[, 10:17]
D <- C / rowSums(abs(C)) 
E <- flex[, 18:25]
F <- E / rowSums(abs(E))
G <- cbind(B, D, F) 
flexprop <- G
flexprop <- cbind(id = c(1:20), flex = 1, flexprop)

A <- ext[, 2:9]
B <- A / rowSums(abs(A)) 
C <- ext[, 10:17]
D <- C / rowSums(abs(C)) 
E <- ext[, 18:25]
F <- E / rowSums(abs(E))
G <- cbind(B, D, F) 
extprop <- G
extprop <- cbind(id = c(1:20), flex = 2, extprop)

eag5 <- rbind(flexprop,extprop)
rm(A, B,C,D,E,F,G)


#flexna, extna / na <- outlier by row
library(dplyr)
q <- apply(flex, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
iqr <- q[3, ] - q[1, ]
upper <- q[3, ] + 1.5 * iqr
lower <- q[1, ] - 1.5 * iqr
flex_na <- flex
flex_na[] <- lapply(flex_na, function(x) ifelse(x > upper | x < lower, NA, x))
flex_na <- cbind(id = c(1:20), flex = 1, flex_na)

q <- apply(ext, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
iqr <- q[3, ] - q[1, ]
upper <- q[3, ] + 1.5 * iqr
lower <- q[1, ] - 1.5 * iqr
ext_na <- ext
ext_na[] <- lapply(ext_na, function(x) ifelse(x > upper | x < lower, NA, x))
ext_na <- cbind(id = c(1:20), flex = 2, ext_na)
rm(q,iqr,upper,lower)


eag6 <- rbind(flex_na,ext_na)


#flexnaprop, extnaprop df / na.rm=TRUE!!
A <- flex_na[, 3:10]
B <- A / apply(A, 1, function(x) sum(abs(x), na.rm = TRUE))
C <- flex_na[, 11:18]
D <- C / apply(C, 1, function(x) sum(abs(x), na.rm = TRUE))
E <- flex_na[, 19:26]
F <- E / apply(C, 1, function(x) sum(abs(x), na.rm = TRUE))
G <- cbind(B, D, F) 
flex_naprop <- G
flex_naprop <- cbind(id = c(1:20), flex = 1, flex_naprop)

A <- ext_na[, 3:10]
B <- A / apply(A, 1, function(x) sum(abs(x), na.rm = TRUE))
C <- ext_na[, 11:18]
D <- C / apply(C, 1, function(x) sum(abs(x), na.rm = TRUE))
E <- ext_na[, 19:26]
F <- E / apply(E, 1, function(x) sum(abs(x), na.rm = TRUE))
G <- cbind(B, D, F) 
ext_naprop <- G
ext_naprop <- cbind(id = c(1:20), flex = 2, ext_naprop)

eag7 <- rbind(flex_naprop,ext_naprop)
rm(A,B,C,D,E,F,G)


# movement atv = 1, psv = 2, sqt = 3

A <- flex_na[, 3:10]
B <- flex_na[, 11:18]
C <- flex_na[, 19:26]
colnames(B) <- colnames(A)
colnames(C) <- colnames(A)
D <- rbind(A, B, C)
move <- rep(1:3, each =20)
ID <- rep(1:20, 3)
flex <- 1
E <- cbind(ID, move, flex, D)

A <- ext_na[, 3:10]
B <- ext_na[, 11:18]
C <- ext_na[, 19:26]
colnames(B) <- colnames(A)
colnames(C) <- colnames(A)
D <- rbind(A, B, C)
move <- rep(1:3, each =20)
ID <- rep(1:20, 3)
flex <- 2
F <- cbind(ID, move, flex, D)

eag2 <- rbind(E, F)

require(rrtable)
#statistics by angle
library(ztable)
library(xtable)
library(rrtable)
mytable(flex~., data=eag4, digits=3)
mytable(flex~., data=eag5, digits=3)
mytable(flex~., data=eag6, digits=3)
mytable(flex~., data=eag7, digits=3)
mytable(flex~., data=abs(eag6), digits=3)
mytable(flex~., data=abs(eag7), digits=3)
A <- mytable(move+flex~., data=eag2, digits=3)
mytable(flex+move~., data=eag2, digits=3)

table2pptx("mytable(flex~., data=eag4, digits=3)", echo=TRUE,append = FALSE)
table2pptx("mytable(flex~., data=eag5, digits=3)", echo=TRUE,append = TRUE)
table2pptx("mytable(flex~., data=eag6, digits=3)", echo=TRUE,append = TRUE)
table2pptx("mytable(flex~., data=eag7, digits=3)", echo=TRUE,append = TRUE)
table2pptx("mytable(flex~., data=abs(eag6), digits=3)", echo=TRUE,append = TRUE)
table2pptx("mytable(flex~., data=abs(eag7), digits=3)", echo=TRUE,append = TRUE)
table2pptx(A, echo=TRUE,append = TRUE)

#### Raw data analysis####

#boxplot with eag3; because eag1 has to larger outlier
par(mfrow = c(1, 1))
boxplot(eag3[2:9])

#t-test
library(moonBook)
mytable(move~., data=eag3, digits=3)
#post_hoc from anova; LSD test
library(agricolae)
res=aov(channel8~move,data = eag3)
print(LSD.test(res,"move",p.adj="bonferroni",group=F)$comparison)

#channel 1,2,4,5,7,8 were significant

#channel2 boxplot by move
ggplot(eag3) +
  aes(x = "", y = channel6, group = move) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()


#overlapping code for later
# par(mfrow = c(4, 2))
# for (i in 1:8){
# ggplot(eag3) +
#   aes(x = "", y = eag3[i,], group = move) +
#   geom_boxplot(fill = "#112446") +
#   theme_minimal()
# }



#### Absolute proportional data analysis####

#eag1AP generation; proportional data
eag1abs <- abs(eag1) #eag1  절대값

eag1AP <- data.frame(0,0,0,0,0,0,0,0)
colnames(eag1AP)<- c('channel1','channel2','channel3','channel4','channel5','channel6','channel7','channel8')

  for (i in 1:60){
    eag1AP <- rbind(eag1AP, prop.table(eag1abs[i,2:9]))
  }
eag1AP <- eag1AP[-1,]
eag1AP <- cbind(eag1AP, eag1[10])


#boxplot for all channel eag1AP
par(mfrow = c(1, 1))
boxplot(eag1AP[1:8])


#t-test by movement
library(moonBook)
mytable(move~., data=eag1, digits=3)
mytable(move~., data=eag1abs, digits=3) #absolute
mytable(move~., data=eag1AP, digits=3) #absolute & proportion
#post_hoc from anova; LSD test
library(agricolae)
res=aov(channel2~move,data = eag1AP)
print(LSD.test(res,"move",p.adj="bonferroni",group=F)$comparison)

#channel2 boxplot by move
ggplot(eag1AP) +
  aes(x = "", y = channel2, group = move) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()

##results from eag1AP was channel2; move1 vs move3


#eag4; data by channel & person & move



#plot by channel
par(mfrow = c(10, 2))
for (i in 1:20){
  plot(x=c(1:8),y=eag3[i,2:9], type="l", ylim=c(-10000,10000),ann=F, axes=F, col=sample(1:255))
}


#plot by channel with eag1AP(absolute & proportion values)
par(mfrow = c(10, 2))
for (i in 1:20){
  plot(x=c(1:8),y=eag1AP[i,1:8], type="l", ylim=c(0,1),ann=F, col=sample(1:255))
}




mytable(channel~amplitude, data=eag2AE, digits=3)

mytable(move~., data=eag2, digits=3)
mytable(move~., data=eag3, digits=3)

install.packages("agricolae")









write.csv(eag, "eag.csv")   #Change name!!



























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
  
  
