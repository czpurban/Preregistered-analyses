if (!require("pacman")) install.packages("pacman")
pacman::p_load(car, lme4, dplyr)


set.seed(111)
# Simulating data ------------------------------------

data<-data.frame(id = rep(1:201, each=48), response =  sample(1:6, 201*48, replace=T),
                         attitude = rep(rnorm(201, 0, 1.5), each = 96), 
                         male = rep(sample(c(0,1),201, replace = T), each = 96),
                         response1 =  sample(1:6, 201*96, replace=T),
                         response2 =  sample(1:6, 201*96, replace=T)
                         )
                                                  
i<-1
while (i<201*48+1){
  data$item[i:(i+47)] <- c(sample(1:48, 24, replace=F),sample(49:96, 24, replace=F))
  data$manipulated[i:(i+47)]<-c(sample(c(rep(0,21),rep(1,3)), 24, replace=F),
                                        sample(c(rep(0,21),rep(1,3)), 24, replace=F))
  data$trial[i:(i+47)] <- sample(c(1:48),48,replace=F) 
  i<-i+48
}

data$behavioral<-ifelse(data$item<48, 1 ,0)


# Manipulating pretest and posttest data ------------------------------------
data$experience[data$manipulated==1]<-c(0:5)

# Assigning value of experience, i.e., how many manipulated items s/he has already seen
i<-1
while (i <length (data[,1])+1) {
  k<-0
  for (j in 1:48) {
    if (data$manipulated[i]==0) {
      data$experience[i]<-k
    } else {k<-data$experience[i]+1}
    i<-i+1
  }
}

data$manipContrast<-NA
data$manipContrast<-ifelse (data$manipulated==1 & data$response1 < 4, 1, data$manipContrast)
data$manipContrast<-ifelse (data$manipulated==1 & data$response1 >= 4, -1, data$manipContrast)
data$manipContrast<-ifelse (data$manipulated==0, 0, data$manipContrast)
# data$accepted<-rbinom(48*201, 1, prob = 0.8)  

#merging pre-post test data; keeping only pretest data that match posttest data
#data<-merge (data, data, by.x = c("id", "item"), by.y = c("id", "item"), all.x=F, all.y=T)

cor(data)>0.7 #checking correlations between IVs 

#centering of variables that enter interactions
data$attitude.centr<-as.numeric(scale(data$attitude, center=T, scale=F))
# data$manipulated.centr<-as.numeric(scale(data$manipulated, center=T, scale=F))
data$experience.centr<-as.numeric(scale(data$experience/6, center=T, scale=F)) #scaling
data$behavioral.centr<-as.numeric(scale(data$behavioral, center=T, scale=F))
data$trial.centr<-as.numeric(scale(data$trial/48, center=T, scale=F))
# data$manipUp.centr<-as.numeric(scale(data$manipUp, center=T, scale=F))
# data$manipDown.centr<-as.numeric(scale(data$manipDown, center=T, scale=F))
# data$accepted.centr<-as.numeric(scale(data$accepted, center=T, scale=F))
data$response1ZeroSum<-as.factor(data$response1)
contrasts(data$response1ZeroSum) = contr.sum(6)
data$response1LinContr <- data$response1-3

#DV
# data$change<-ifelse(data$response.x==data$response.y, 0, 1)

data$score <-  
    data$id/201 + 
    data$item/96 +
    data$behavioral + 
    data$attitude +
    # data$manipUp.centr - 
    # data$manipDown.centr + 
    # data$behavioral.centr +
    data$manipContrast +
    data$attitude +
    data$male +
    data$trial/48 +
    data$experience.centr/6 +
    data$response1/6

fivenum(data$score)
                    
data$extremePerc<-rbinom(48*201,1, prob = 0.5+data$score/120)


# Analysis  ------------------------------------

# model se sum-to-zero kontrasty pro response1
model1 <- glmer(extremePerc ~ (1 + manipContrast | id) 
                + (1 + response1ZeroSum + manipContrast | item)
                + response1ZeroSum
                + attitude.centr
                + manipContrast
                + male + experience.centr, 
                data = data, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)

# model s response1 jako spojitou proměnnou 
model2 <- glmer(extremePerc ~ (1 + manipContrast | id) 
                + (1 + response1 + manipContrast | item)
                + response1
                + attitude.centr
                + manipContrast
                + male + experience.centr, 
                data = data, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2)

# model s lineárními kontrasty pro response1
model3 <- glmer(extremePerc ~ (1 + manipContrast | id) 
                + (1 + response1LinContr + manipContrast | item)
                + response1LinContr
                + attitude.centr
                + manipContrast
                + male + experience.centr, 
                data = data, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model3)

# where: 
# id...participant identifier;
# item...item identifiers;
# response1.zeroSum... pretest response (1-6) recoded with sum-to-zero contrast coding;
# response1.... pretest response (1-6);
# response1LinContr...linear contrasts for pretest reponses (1=-3, 2 = -2, 3 = -1, 4 = 1
# 5 = 2, 6 = 3);
# behavioral.centr...dummy indicator of behavioral items (centered);
# attitude.centr...environmental attitude (from pretest; centered);
# male...dummy indicator of males; 
# trial.centr...order of trial (order of trial divided by the total number of trials and centered);
# experience.centr...number of trials that have been manipulated (number of trials divided by six and centered).
