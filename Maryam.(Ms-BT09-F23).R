x<-read.csv("smoking_dataset.csv")
str(x)

x$gender <- as.factor(x$gender)
x$Urine.protein <- as.factor(x$Urine.protein )
x$tartar <- as.factor(x$tartar)
x$oral <- as.factor(x$oral)
x$hearing.left. <- as.factor(x$hearing.left.)
x$hearing.right. <- as.factor(x$hearing.right.)
x$dental.caries <- as.factor(x$dental.caries)
x$smoking <- as.factor(x$smoking)

str(x)
is.na(x)
y<-na.omit(x)

library(ggplot2)
ggplot(y,aes(y=age))+geom_boxplot()
IQR_age<-IQR(y$age)
IQR_age
lower_bound<-quantile(y$age,0.25)-1.5*IQR_age
upper_bound<-quantile(y$age,0.25)+1.5*IQR_age
z<-y[y$age>=lower_bound&x$age<=upper_bound,]
ggplot(z,aes(y=age))+geom_boxplot()

ggplot(y,aes(y=height.cm.))+geom_boxplot()
IQR_height.cm.<-IQR(y$height.cm.)
IQR_height.cm.
lower_bound<-quantile(y$height.cm.,0.25)-1.5*IQR_height.cm.
upper_bound<-quantile(y$height.cm.,0.25)+1.5*IQR_height.cm.
z<-y[y$height.cm.>=lower_bound&x$height.cm.<=upper_bound,]
ggplot(z,aes(y=height.cm.))+geom_boxplot()

ggplot(y,aes(y=weight.kg.))+geom_boxplot()
IQR_weight.kg.<-IQR(y$weight.kg.)
IQR_weight.kg.
lower_bound<-quantile(y$weight.kg.,0.25)-1.5*IQR_weight.kg.
upper_bound<-quantile(y$weight.kg.,0.25)+1.5*IQR_weight.kg.
z<-y[y$weight.kg.>=lower_bound&x$weight.kg.<=upper_bound,]
ggplot(z,aes(y=weight.kg.))+geom_boxplot()

ggplot(y,aes(y=waist.cm.))+geom_boxplot()
IQR_waist.cm.<-IQR(y$waist.cm.)
IQR_waist.cm.
lower_bound<-quantile(y$waist.cm.,0.25)-1.5*IQR_waist.cm.
upper_bound<-quantile(y$waist.cm.,0.25)+1.5*IQR_waist.cm.
z<-y[y$waist.cm.>=lower_bound&x$waist.cm.<=upper_bound,]
ggplot(z,aes(y=waist.cm.))+geom_boxplot()


ggplot(y,aes(y=eyesight.left.))+geom_boxplot()
IQR_eyesight.left.<-IQR(y$eyesight.left.)
IQR_eyesight.left.
lower_bound<-quantile(y$eyesight.left.,0.25)-1.5*IQR_eyesight.left.
upper_bound<-quantile(y$eyesight.left.,0.25)+1.5*IQR_eyesight.left.
z<-y[y$eyesight.left.>=lower_bound&x$eyesight.left.<=upper_bound,]
ggplot(z,aes(y=eyesight.left.))+geom_boxplot()

ggplot(y,aes(y=eyesight.right.))+geom_boxplot()
IQR_eyesight.right.<-IQR(y$eyesight.right.)
IQR_eyesight.right.
lower_bound<-quantile(y$eyesight.right.,0.25)-1.5*IQR_eyesight.right.
upper_bound<-quantile(y$eyesight.right.,0.25)+1.5*IQR_eyesight.right.
z<-y[y$eyesight.right.>=lower_bound&x$eyesight.right.<=upper_bound,]
ggplot(z,aes(y=eyesight.right.))+geom_boxplot()


ggplot(y,aes(y=systolic))+geom_boxplot()
IQR_systolic<-IQR(y$systolic)
IQR_systolic
lower_bound<-quantile(y$systolic,0.25)-1.5*IQR_systolic
upper_bound<-quantile(y$systolic,0.25)+1.5*IQR_systolic
z<-y[y$systolic>=lower_bound&x$systolic<=upper_bound,]
ggplot(z,aes(y=systolic))+geom_boxplot()

ggplot(y,aes(y=relaxation))+geom_boxplot()
IQR_relaxation<-IQR(y$relaxation)
IQR_relaxation
lower_bound<-quantile(y$relaxation,0.25)-1.5*IQR_relaxation
upper_bound<-quantile(y$relaxation,0.25)+1.5*IQR_relaxation
z<-y[y$relaxation>=lower_bound&x$relaxation<=upper_bound,]
ggplot(z,aes(y=relaxation))+geom_boxplot()

ggplot(y,aes(y=fasting.blood.sugar))+geom_boxplot()
IQR_fasting.blood.sugar<-IQR(y$fasting.blood.sugar)
IQR_fasting.blood.sugar
lower_bound<-quantile(y$fasting.blood.sugar,0.25)-1.5*IQR_fasting.blood.sugar
upper_bound<-quantile(y$fasting.blood.sugar,0.25)+1.5*IQR_fasting.blood.sugar
z<-y[y$fasting.blood.sugar>=lower_bound&x$fasting.blood.sugar<=upper_bound,]
ggplot(z,aes(y=fasting.blood.sugar))+geom_boxplot()

ggplot(y,aes(y=Cholesterol))+geom_boxplot()
IQR_Cholesterol<-IQR(y$Cholesterol)
IQR_Cholesterol
lower_bound<-quantile(y$Cholesterol,0.25)-1.5*IQR_Cholesterol
upper_bound<-quantile(y$Cholesterol,0.25)+1.5*IQR_Cholesterol
z<-y[y$Cholesterol>=lower_bound&x$Cholesterol<=upper_bound,]
ggplot(z,aes(y=Cholesterol))+geom_boxplot()

ggplot(y,aes(y=triglyceride))+geom_boxplot()
IQR_triglyceride<-IQR(y$triglyceride)
IQR_triglyceride
lower_bound<-quantile(y$triglyceride,0.25)-1.5*IQR_triglyceride
upper_bound<-quantile(y$triglyceride,0.25)+1.5*IQR_triglyceride
z<-y[y$triglyceride>=lower_bound&x$triglyceride<=upper_bound,]
ggplot(z,aes(y=triglyceride))+geom_boxplot()


ggplot(y,aes(y=HDL))+geom_boxplot()
IQR_HDL<-IQR(y$HDL)
IQR_HDL
lower_bound<-quantile(y$HDL,0.25)-1.5*IQR_HDL
upper_bound<-quantile(y$HDL,0.25)+1.5*IQR_HDL
z<-y[y$HDL>=lower_bound&x$HDL<=upper_bound,]
ggplot(z,aes(y=HDL))+geom_boxplot()

ggplot(y,aes(y=LDL))+geom_boxplot()
IQR_LDL<-IQR(y$LDL)
IQR_LDL
lower_bound<-quantile(y$LDL,0.25)-1.5*IQR_LDL
upper_bound<-quantile(y$LDL,0.25)+1.5*IQR_LDL
z<-y[y$LDL>=lower_bound&x$LDL<=upper_bound,]
ggplot(z,aes(y=LDL))+geom_boxplot()


ggplot(y,aes(y=hemoglobin))+geom_boxplot()
IQR_hemoglobin<-IQR(y$hemoglobin)
IQR_hemoglobin
lower_bound<-quantile(y$hemoglobin,0.25)-1.5*IQR_hemoglobin
upper_bound<-quantile(y$hemoglobin,0.25)+1.5*IQR_hemoglobin
z<-y[y$hemoglobin>=lower_bound&x$hemoglobin<=upper_bound,]
ggplot(z,aes(y=hemoglobin))+geom_boxplot()


ggplot(y,aes(y=serum.creatinine))+geom_boxplot()
IQR_serum.creatinine<-IQR(y$serum.creatinine)
IQR_serum.creatinine
lower_bound<-quantile(y$serum.creatinine,0.25)-1.5*IQR_serum.creatinine
upper_bound<-quantile(y$serum.creatinine,0.25)+1.5*IQR_serum.creatinine
z<-y[y$serum.creatinine>=lower_bound&x$serum.creatinine<=upper_bound,]
ggplot(z,aes(y=serum.creatinine))+geom_boxplot()

ggplot(y,aes(y=AST))+geom_boxplot()
IQR_AST<-IQR(y$AST)
IQR_AST
lower_bound<-quantile(y$AST,0.25)-1.5*IQR_AST
upper_bound<-quantile(y$AST,0.25)+1.5*IQR_AST
z<-y[y$AST>=lower_bound&x$AST<=upper_bound,]
ggplot(z,aes(y=AST))+geom_boxplot()


ggplot(y,aes(y=ALT))+geom_boxplot()
IQR_ALT<-IQR(y$ALT)
IQR_ALT
lower_bound<-quantile(y$ALT,0.25)-1.5*IQR_ALT
upper_bound<-quantile(y$ALT,0.25)+1.5*IQR_ALT
z<-y[y$ALT>=lower_bound&x$ALT<=upper_bound,]
ggplot(z,aes(y=ALT))+geom_boxplot()


ggplot(y,aes(y=Gtp))+geom_boxplot()
IQR_Gtp<-IQR(y$Gtp)
IQR_Gtp
lower_bound<-quantile(y$Gtp,0.25)-1.5*IQR_Gtp
upper_bound<-quantile(y$Gtp,0.25)+1.5*IQR_Gtp
z<-y[y$Gtp>=lower_bound&x$Gtp<=upper_bound,]
ggplot(z,aes(y=Gtp))+geom_boxplot()

str(z)

mean(z$age)
mean(z$height.cm.)
mean(z$weight.kg.)
mean(z$waist.cm.)
mean(z$eyesight.left.)
mean(z$eyesight.right.)
mean(z$systolic)
mean(z$relaxation)
mean(z$fasting.blood.sugar)
mean(z$Cholesterol)
mean(z$triglyceride)
mean(z$HDL)
mean(z$LDL)
mean(z$hemoglobin)
mean(z$serum.creatinine)
mean(z$AST)
mean(z$ALT)
mean(z$Gtp)


median(z$age)
median(z$height.cm.)
median(z$weight.kg.)
median(z$waist.cm.)
median(z$eyesight.left.)
median(z$eyesight.right.)
median(z$systolic)
median(z$relaxation)
median(z$fasting.blood.sugar)
median(z$Cholesterol)
median(z$triglyceride)
median(z$HDL)
median(z$LDL)
median(z$hemoglobin)
median(z$serum.creatinine)
median(z$AST)
median(z$ALT)
median(z$Gtp)

sd(z$age)
sd(z$height.cm.)
sd(z$weight.kg.)
sd(z$waist.cm.)
sd(z$eyesight.left.)
sd(z$eyesight.right.)
sd(z$systolic)
sd(z$relaxation)
sd(z$fasting.blood.sugar)
sd(z$Cholesterol)
sd(z$triglyceride)
sd(z$HDL)
sd(z$LDL)
sd(z$hemoglobin)
sd(z$serum.creatinine)
sd(z$AST)
sd(z$ALT)
sd(z$Gtp)

var(z$age)
var(z$height.cm.)
var(z$weight.kg.)
var(z$waist.cm.)
var(z$eyesight.left.)
var(z$eyesight.right.)
var(z$systolic)
var(z$relaxation)
var(z$fasting.blood.sugar)
var(z$Cholesterol)
var(z$triglyceride)
var(z$HDL)
var(z$LDL)
var(z$hemoglobin)
var(z$serum.creatinine)
var(z$AST)
var(z$ALT)
var(z$Gtp)

ggplot(z,aes(y=age))+geom_histogram()
ggplot(z,aes(y=height.cm.))+geom_histogram()
ggplot(z,aes(y=weight.kg.))+geom_histogram()
ggplot(z,aes(y=waist.cm.))+geom_histogram()
ggplot(z,aes(y=eyesight.left. ))+geom_histogram()
ggplot(z,aes(y=eyesight.right.))+geom_histogram()
ggplot(z,aes(y=systolic ))+geom_histogram()
ggplot(z,aes(y=relaxation  ))+geom_histogram()
ggplot(z,aes(y=fasting.blood.sugar))+geom_histogram()
ggplot(z,aes(y=Cholesterol ))+geom_histogram()
ggplot(z,aes(y=triglyceride))+geom_histogram()
ggplot(z,aes(y= HDL ))+geom_histogram()
ggplot(z,aes(y= LDL ))+geom_histogram()
ggplot(z,aes(y=hemoglobin  ))+geom_histogram()
ggplot(z,aes(y=serum.creatinine))+geom_histogram()
ggplot(z,aes(y=AST ))+geom_histogram()
ggplot(z,aes(y=ALT ))+geom_histogram()
ggplot(z,aes(y=Gtp))+geom_histogram()


ggplot(z,aes(y=age))+geom_density()
ggplot(z,aes(y=height.cm.))+geom_density()
ggplot(z,aes(y=weight.kg.))+geom_density()
ggplot(z,aes(y=waist.cm.))+geom_density()
ggplot(z,aes(y=eyesight.left.))+geom_density()
ggplot(z,aes(y=eyesight.right.))+geom_density()
ggplot(z,aes(y=systolic ))+geom_density()
ggplot(z,aes(y=fasting.blood.sugar ))+geom_density()
ggplot(z,aes(y=Cholesterol ))+geom_density()
ggplot(z,aes(y=relaxation ))+geom_density()
ggplot(z,aes(y=triglyceride ))+geom_density()
ggplot(z,aes(y=HDL ))+geom_density()
ggplot(z,aes(y=LDL ))+geom_density()
ggplot(z,aes(y=hemoglobin ))+geom_density()
ggplot(z,aes(y=serum.creatinine ))+geom_density()
ggplot(z,aes(y=AST  ))+geom_density()
ggplot(z,aes(y=ALT  ))+geom_density()
ggplot(z,aes(y=Gtp   ))+geom_density()


ggplot(z,aes(x=gender))+geom_bar()
ggplot(z,aes(x=hearing.left.))+geom_bar()
ggplot(z,aes(x=hearing.right.))+geom_bar()
ggplot(z,aes(x=Urine.protein))+geom_bar()
ggplot(z,aes(x=dental.caries))+geom_bar()
ggplot(z,aes(x=tartar))+geom_bar()
ggplot(z,aes(x=smoking))+geom_bar()

contingency_table<-table(z$gender)
print(contingency_table)

contingency_table<-table(z$hearing.left.)
print(contingency_table)

contingency_table<-table(z$hearing.right.)
print(contingency_table)

contingency_table<-table(z$Urine.protein)
print(contingency_table)

contingency_table<-table(z$dental.caries)
print(contingency_table)

contingency_table<-table(z$tartar)
print(contingency_table)

contingency_table<-table(z$smoking)
print(contingency_table)

table(z$smoking,z$gender)
table(z$smoking,z$hearing.left.)
table(z$smoking,z$hearing.right.)
table(z$smoking,z$Urine.protein)
table(z$smoking,z$dental.caries)
table(z$smoking,z$tartar)


ggplot(z, aes(x = smoking, fill = gender)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking and gender",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen")) +
theme_minimal()


ggplot(z, aes(x = smoking, fill = hearing.left.)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking and hearing.left.",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen")) +
theme_minimal()

ggplot(z, aes(x = smoking, fill = hearing.right.)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking and hearing.right.",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen")) +
theme_minimal()

ggplot(z, aes(x = smoking, fill = Urine.protein)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking and Urine.protein",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen","darkgrey","blue","pink","red")) +
theme_minimal()

ggplot(z, aes(x = smoking, fill = dental.caries)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking and dental.caries",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen")) +
theme_minimal()

ggplot(z, aes(x = smoking, fill = tartar)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking and tartar",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen")) +
theme_minimal()

ggplot(z, aes(x = smoking, fill = smoking)) +
geom_bar(position = "dodge", color = "black", stat = "count") +
labs(title = "Bar Plot of smoking",
x = "smoking", y = "Count") +
scale_fill_manual(values = c("skyblue", "lightgreen")) +
theme_minimal()


ggplot(z[complete.cases(z$age),],aes(x=smoking,y=age))+
geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
labs(title ="boxplot of age by smoking",
x="smoking",
y="age")

ggplot(z[complete.cases(z$height.cm.),],aes(x=smoking,y=height.cm.))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of height.cm. by smoking",
       x="smoking",
       y="height.cm.")

ggplot(z[complete.cases(z$weight.kg.),],aes(x=smoking,y=weight.kg.))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of weight.kg. by smoking",
       x="smoking",
       y="weight.kg.")

ggplot(z[complete.cases(z$waist.cm.),],aes(x=smoking,y=waist.cm.))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of waist.cm. by smoking",
       x="smoking",
       y="waist.cm.")

ggplot(z[complete.cases(z$eyesight.left.),],aes(x=smoking,y=eyesight.left.))+
  geom_boxplot(fill="skyblue",color="black",)+theme_minimal()+
  labs(title ="boxplot of eyesight.left. by smoking",
       x="smoking",
       y="eyesight.left.")

ggplot(z[complete.cases(z$eyesight.right.),],aes(x=smoking,y=eyesight.right.))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of eyesight.right. by smoking",
       x="smoking",
       y="eyesight.right.")

ggplot(z[complete.cases(z$systolic),],aes(x=smoking,y=systolic))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of systolic by smoking",
       x="smoking",
       y="systolic")

ggplot(z[complete.cases(z$relaxation),],aes(x=smoking,y=relaxation))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of relaxation by smoking",
       x="smoking",
       y="relaxation")

ggplot(z[complete.cases(z$fasting.blood.sugar),],aes(x=smoking,y=fasting.blood.sugar))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of fasting.blood.sugar by smoking",
       x="smoking",
       y="fasting.blood.sugar")

ggplot(z[complete.cases(z$Cholesterol),],aes(x=smoking,y=Cholesterol))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of Cholesterol by smoking",
       x="smoking",
       y="Cholesterol")
ggplot(z[complete.cases(z$triglyceride ),],aes(x=smoking,y=triglyceride ))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of triglyceride  by smoking",
       x="smoking",
       y="triglyceride ")

ggplot(z[complete.cases(z$HDL),],aes(x=smoking,y=HDL))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of HDL by smoking",
       x="smoking",
       y="HDL")

ggplot(z[complete.cases(z$LDL),],aes(x=smoking,y=LDL))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of LDL by smoking",
       x="smoking",
       y="LDL")

ggplot(z[complete.cases(z$hemoglobin),],aes(x=smoking,y=hemoglobin))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of hemoglobin by smoking",
       x="smoking",
       y="hemoglobin")

ggplot(z[complete.cases(z$serum.creatinine),],aes(x=smoking,y=serum.creatinine))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of serum.creatinine by smoking",
       x="smoking",
       y="serum.creatinine")

ggplot(z[complete.cases(z$AST),],aes(x=smoking,y=AST))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of AST by smoking",
       x="smoking",
       y="AST")

ggplot(z[complete.cases(z$ALT),],aes(x=smoking,y=ALT))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of ALT by smoking",
       x="smoking",
       y="ALT")

ggplot(z[complete.cases(z$Gtp),],aes(x=smoking,y=Gtp))+
  geom_boxplot(fill="skyblue",color="black")+theme_minimal()+
  labs(title ="boxplot of Gtp by smoking",
       x="smoking",
       y="Gtp")


model <- glm(smoking ~ age, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ gender, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ height.cm., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ weight.kg., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ waist.cm., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ eyesight.left., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ eyesight.right., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ systolic, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ relaxation, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ fasting.blood.sugar, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ hearing.left., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ hearing.right., data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ Cholesterol, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ triglyceride, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ HDL, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ LDL, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ hemoglobin, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ Urine.protein, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ serum.creatinine, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ AST, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ ALT, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ Gtp, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ dental.caries, data = z, family = "binomial")
summary(model)

model <- glm(smoking ~ tartar, data = z, family = "binomial")
summary(model)

set.seed(123)
sample_index <- sample(1:nrow(z), 0.7 * nrow(z)) 
train_data <- z[sample_index, ] 
test_data <- z[-sample_index, ] 

model<-glm(smoking ~ gender+hemoglobin+systolic+fasting.blood.sugar+ALT+weight.kg.+tartar+Cholesterol+dental.caries+triglyceride+serum.creatinine+Gtp+height.cm., data = train_data , family = "binomial")
summary(model)

prediction<-predict(model,newdata=test_data,type="response")

predicted_classes<-ifelse(prediction>0.5,1,0)

actual_classes <- test_data$smoking
accuracy <- mean(predicted_classes == actual_classes)
cat("Accuracy on Test Set:", accuracy, "\n")

library(randomForest)
set.seed(123)
sample_index <- sample(1:nrow(z), 0.7 * nrow(z)) 
train_data <- z[sample_index, ] 
test_data <- z[-sample_index, ] 
train_labels <- train_data[, ncol(train_data)]
test_labels <- test_data[, ncol(test_data)]
train_data <- train_data[, -ncol(train_data)]
test_data <- test_data[, -ncol(test_data)]
rf_model <- randomForest(x = train_data, y = train_labels, ntree = 1000, seed = 123)
y_pred <- predict(rf_model, newdata = test_data)
accuracy <- sum(y_pred == test_labels) / length(test_labels)
cat("Accuracy:", accuracy, "\n")
var_importance <- importance(rf_model)
print("Variable Importance:")
print(var_importance)

