install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
install.packages("dplyr")
install.packages("randomForest")
install.packages("corrplot")
install.packages("plyr")
install.packages("histogram")
test <- read.csv("C:/Users/Lenovo/Downloads/titanic/test.csv")
train <- read.csv("C:/Users/Lenovo/Downloads/titanic/train.csv")
full  <- dplyr::bind_rows(train, test) # test + train
options( warn = -1 )
str(full)
summary(full)
library(ggplot2)
#_____________________________Exploratory Analysis______________
# Age vs Survived
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  +     geom_histogram(bins=30) + 
  +     theme_classic() +
  +     xlab("Age") +
  +     scale_fill_discrete(name = "Survived") + 
  +     ggtitle("Age vs Survived")
# Sex vs Survived
ggplot(full[1:891,], aes(Sex, fill = factor(Survived))) + 
  +     geom_bar(stat = "count", position = 'dodge')+
  +     theme_classic() +
  +     xlab("Sex") +
  +     ylab("Count") +
  +     scale_fill_discrete(name = "Survived") + 
  +     ggtitle("Sex vs Survived")
#Sex vs Survived vs Age
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  +     geom_histogram(bins=30) + 
  +     theme_classic() +
  +     xlab("Age") +
  +     ylab("Count") +
  +     facet_grid(.~Sex)+
  +     scale_fill_discrete(name = "Survived") + 
  +     theme_classic()+
  +     ggtitle("Age vs Sex vs Survived")
# Pclass vs Survived
ggplot(full[1:891,], aes(Pclass, fill = factor(Survived))) + 
  +   geom_bar(stat = "count")+
  +   theme_classic() +
  +   xlab("Pclass") +
  +   facet_grid(.~Sex)+
  +   ylab("Count") +
  +   scale_fill_discrete(name = "Survived") + 
  +   ggtitle("Pclass vs Sex vs Survived")
#Pclass vs Sex vs Age
ggplot(full[1:891,], aes(x = Age, y = Sex)) + 
  +   geom_jitter(aes(colour = factor(Survived))) + 
  +   theme_classic()+
  +   theme(legend.title = element_blank())+
  +   facet_wrap(~Pclass) + 
  +   labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  +   scale_fill_discrete(name = "Survived") + 
  +   scale_x_continuous(name="Age",limits=c(0, 81))
#Fare vs Pclass
ggplot(full[1:891,], aes(x = Fare, y = Pclass)) + 
  +     geom_jitter(aes(colour = factor(Survived))) + 
  +     theme_classic()+
  +     theme(legend.title = element_blank())+
  +     labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
  +     scale_fill_discrete(name = "Survived") + 
  +     scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))
Data Processing
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
#Reassign rare titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
# Reassign mlle, ms, and mme, and rare
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% royalty]  <- 'Royalty'
full$Title[full$Title %in% officer]  <- 'Officer
full$Surname <- sapply(full$Name,  
+                        function(x) strsplit(x, split = '[,.]')[[1]][1])
#graph title
ggplot(full[1:891,], aes(Title,fill = factor(Survived))) +
+     geom_bar(stat = "count")+
+     xlab('Title') +
+     ylab("Count") +
+     scale_fill_discrete(name = " Survived") + 
+     ggtitle("Title vs Survived")+
+     theme_classic()
#Family 
# Family Size
full$Fsize <- full$SibSp + full$Parch + 1
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
+   geom_bar(stat='count', position='dodge') +
+   scale_x_continuous(breaks=c(1:11)) +
+   xlab('Family Size') +
+   ylab("Count") +
+   theme_classic()+
+   scale_fill_discrete(name = "Survived") + 
+   ggtitle("Family Size vs Survived")
#FsizeD
full$FsizeD[full$Fsize == 1] <- 'Alone'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'Small'
full$FsizeD[full$Fsize > 4] <- 'Big'
mosaicplot(table(full$FsizeD, full$Survived), main='FsizeD vs Survived', ylab="Survived",xlab="FsizeD",col = hcl(c(50, 120)),)
Processing Embarked (Replace missing values by most common value = S)
full[c(62, 830), 'Embarked']
 full$Embarked[c(62, 830)] <- 'S'
ggplot(full[1:891,], aes(Pclass, fill = factor(Survived))) + 
+   geom_bar(stat = "count")+
+   theme_classic() +
+   xlab("Pclass") +
+   ylab("Count") +
+   facet_wrap(~Embarked) + 
+   scale_fill_discrete(name = "Survived") + 
+   ggtitle("Embarked vs Pclass vs Survived")
Processing Fare (Replace missing value by Pclass = 3 's median)
full[1044, ]
ggplot(full[full$Pclass == '3', ], 
       +        aes(x = Fare)) +
  +     geom_density(fill = 'lightgrey', alpha=0.4) + 
  +     geom_vline(aes(xintercept=median(Fare, na.rm=T)),
                   +                colour='darkred', linetype='dashed', lwd=1) +
  +     xlab('Fare') +
  +     ggtitle("Pclass = 3")+
  +     ylab("Density") +
  +     scale_x_continuous(labels=dollar_format()) +
  +     theme_classic()
Processing Age (Replace missing values by Title's median)
title.age <- aggregate(full$Age,by = list(full$Title), FUN = function(x) median(x, na.rm = T))
 full[is.na(full$Age), "Age"] <- apply(full[is.na(full$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])
#Na value count
sum(is.na(full$Age))
 #New Variable : Child (From Age)
full$Child[full$Age < 18] <- 'Child'
> full$Child[full$Age >= 18] <- 'Adult'
> 
> ggplot(full[1:891,][full[1:891,]$Child == 'Child', ], aes(Sex, fill = factor(Survived))) + 
+   geom_bar(stat = "count") + 
+   xlab("Sex") +
+   ylab("Count") +
+   facet_wrap(~Pclass)+
+   scale_fill_discrete(name = "Survived") +
+   ggtitle("Child vs Sex vs Pclass vs Survived")+
+   theme_classic()
table(full$Child, full$Survived)
#Factor
full$Child  <- factor(full$Child)
full$Sex  <- factor(full$Sex)
 full$Embarked  <- factor(full$Embarked)
 full$Title  <- factor(full$Title)
 full$Pclass  <- factor(full$Pclass)
 full$FsizeD  <- factor(full$FsizeD)
 #Data without Cabin & Ticket 
 full1 <- full[,-9]
 full_mod <- full1[,-10]
#Modeling with Random Forest
 train <- full_mod[1:891,]
 test <- full_mod[892:1309,]
library('randomForest')
set.seed(123)
> rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title + 
+  FsizeD + Child, data = train)
Confusion Matri
#prediction
rf.fitted = predict(rf_model)
ans_rf = rep(NA,891)
for(i in 1:891){
+   ans_rf[i] = as.integer(rf.fitted[[i]]) - 1
+ }
# Résultat
 table(ans_rf)
print(rf_model)
OOB Error and GiniOOB Error and Gini 
plot(rf_model, ylim=c(0,0.36), main = 'RF_MODEL')
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)





