setwd("D:/Cours MOOC/Introduction à la statistique avec R/Applications") #pour le répertoire
diab <- read.csv("diabetes.csv")
str(diab)
table(diab$Outcome)
hist(diab$Age)
hist(diab$Glucose)
hist(diab$Insulin)
hist(diab$BloodPressure)
hist(diab$Pregnancies)

boxplot(diab$Age, xlab="age")
boxplot(diab$Age~diab$Outcome)
boxplot(diab$BloodPressure~diab$Outcome)
plot(jitter(diab$Age), jitter(diab$BloodPressure))

cor(diab$Age, diab$Outcome, use="complete.obs")
cor(diab$Glucose, diab$Outcome, use="complete.obs")
cor(diab$BloodPressure, diab$Outcome, use="complete.obs")

summary(diab)
by(diab$Age, diab$Outcome, sd, na.rm=TRUE)
t.test(diab$Age~diab$Outcome,var.equal=TRUE)
by(diab$BloodPressure, diab$Outcome, sd, na.rm=TRUE)
by(diab$Glucose, diab$Outcome, sd, na.rm=TRUE)
wilcox.test(diab$Glucose, diab$Outcome)
t.test(diab$Glucose~diab$Outcome,var.equal=TRUE)
by(diab$SkinThickness, diab$Outcome, sd, na.rm=TRUE)
by(diab$Insulin, diab$Outcome, sd, na.rm=TRUE)
cor.test(diab$Glucose, diab$Outcome)
cor.test(diab$BloodPressure, diab$Outcome)
t.test(diab$Glucose~diab$Outcome,var.equal=TRUE)
t.test(diab$Pregnancies~diab$Outcome,var.equal=TRUE)
t.test(diab$BMI~diab$Outcome,var.equal=TRUE)
cor.test(diab$Insulin, diab$Outcome)


mod2 <- lm(Outcome~Age+Glucose+Pregnancies, data=diab)
summary(mod2)
hist(resid(mod2))

library(corrplot)
corrplot(cor(diab[,c("Age","Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Outcome")], use="complete.obs"), method = "circle")

library(psy)
mdspca(diab)
sphpca(diab, v=55)
expliquer <- "Outcome"
explicatives <-  c("Age","Pregnancies","Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction")
fpca(data = diab, y=expliquer, x=explicatives, partial="No")

cah <- hclust(dist(t(scale(diab))), method="ward.D") #le "t" pour classer les variables et non pas les individus
plot(cah,main="classification hiérarchique")
