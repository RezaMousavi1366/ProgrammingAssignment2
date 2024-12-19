# Load packages
library(nortest)
library(lmtest)

# Load data
data <- read.csv(file="D:/Reza/MyFiles/Course/StatProject/Horse.csv",header=T);
Breed = data$Breed;
Height = data$Height;
Weight = data$Weight;
Gender = data$Gender
BinaryGender = data$Binary.Gender 
Age = data$Age;
TravelTime = data$End.Time
Diet = data$Diet

# Height Variable (Figure 1)
png(file="D:/Reza/MyFiles/Course/StatProject/Height.png", width = 700, height = 435);
par(mfrow=c(1,2));
boxplot(Height,col="white", lwd = 1, main="Height Distribution", cex.main=1);
qqnorm(Height, pch = 1, main="Normal Q-Q Plot for Height", cex.main=1, frame = TRUE);
qqline(Height, col = "black", lwd = 1);
dev.off()

Mean = mean(Height);
Median = median(Height);
Max = max(Height);
Min = min(Height);
IQRT = IQR(Height);
STD = sd(Height);

print(Mean);
print(Median);
print(Max);
print(Min);
print(IQRT);
print(STD)

# Weight Variable
Mean = mean(Weight);
Median = median(Weight);
Max = max(Weight);
Min = min(Weight);
IQRT = IQR(Weight);
STD = sd(Weight);

print(Mean);
print(Median);
print(Max);
print(Min);
print(IQRT);
print(STD)

# Age Variable (Figure 2)
png(file="D:/Reza/MyFiles/Course/StatProject/Age.png", width = 700, height = 435);
par(mfrow=c(1,2));
boxplot(Age,col="white", lwd = 1, main="Age Distribution", cex.main=1);
qqnorm(Age, pch = 1, main="Normal Q-Q Plot for Age", cex.main=1, frame = TRUE);
qqline(Age, col = "black", lwd = 1);
dev.off()

Mean = mean(Age);
Median = median(Age);
Max = max(Age);
Min = min(Age);
IQRT = IQR(Age);
STD = sd(Age);

print(Mean);
print(Median);
print(Max);
print(Min);
print(IQRT);
print(STD)

# Travel time Variable (Figure 3)
png(file="D:/Reza/MyFiles/Course/StatProject/Travel time.png", width = 700, height = 435);
par(mfrow=c(1,2));
Num_Bins <- ceiling(sqrt(length(data[,1])));
hist(TravelTime, main="Horse race",xlab="Travel Time",breaks=Num_Bins, col="white", cex.main=1);
qqnorm(TravelTime, pch = 1, main="Normal Q-Q Plot for Travel time", cex.main=1, frame = TRUE);
qqline(TravelTime, col = "black", lwd = 1);
dev.off()

Mean = mean(TravelTime);
Median = median(TravelTime);
Max = max(TravelTime);
Min = min(TravelTime);
IQRT = IQR(TravelTime);
STD = sd(TravelTime);

print(Mean);
print(Median);
print(Max);
print(Min);
print(IQRT);
print(STD)

# Height and Weight Relationship (Figure 4)
png(file="D:/Reza/MyFiles/Course/StatProject/scatterHeight&Weight.png", width = 630, height = 435);
plot(Height,Weight,xlab="Height",ylab="Weight")
LRM = lm(Weight ~ Height)
abline(LRM,col="red")
dev.off()
Corre <- cor(Height, Weight, method = "pearson")
# Anderson-Darling Test
ad.test(LRM$residuals)
# Breusch-Pagan Test
bptest(LRM)

# Age and Weight Relationship (Figure 5)
png(file="D:/Reza/MyFiles/Course/StatProject/scatterAge&Weight.png", width = 630, height = 435);
plot(Age,Weight,xlab="Age",ylab="Weight")
LRM = lm(Weight ~ Age)
abline(LRM,col="red")
dev.off()
Corre <- cor(Age,Weight, method = "pearson")
# Anderson-Darling Test
ad.test(LRM$residuals)
# Breusch-Pagan Test
bptest(LRM)

# Logistic regression test
LogM <- glm(BinaryGender ~ Weight, family=binomial)
summary(LogM)

# Contingency Table and Chi-squared Test (Table I)
tab <- table(Breed, Gender)
tab <- cbind(tab,rowSums(tab))
tab <- rbind(tab,colSums(tab))
print(tab)
chisq.test(Breed, Gender, correct = FALSE)

# Box plots travel time for different diets (Figure 6)
png(file="D:/Reza/MyFiles/Course/StatProject/boxPlotsTravelTime.png", width = 630, height = 435);
boxplot(TravelTime[Diet=="Chaffe"],TravelTime[Diet=="Com.Con"],
        TravelTime[Diet=="Grain"],col="white", lwd = 1, xlab= "Diet", 
        ylab="Travel Time",
        names = c("Chaffe", "Commercial Concentrate", "Grain"));
dev.off()

# Anova Test (Table II)
AnovaAnalysis <- anova(lm(TravelTime~Diet))

# Tukey's HSD (Figure 7)
png(file="D:/Reza/MyFiles/Course/StatProject/TukeysHSD.png", width = 630, height = 435);
plot(TukeyHSD(aov(lm(TravelTime~Diet))))
dev.off()