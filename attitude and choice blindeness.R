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
data$extreme<-ifelse(data$response==1 | data$response==6, 1, 0)
data$central<-ifelse(data$response==3 | data$response==4, 1, 0)
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

#merging pre-post test data; keeping only pretest data that match posttest data
#data<-merge (data, data, by.x = c("id", "item"), by.y = c("id", "item"), all.x=F, all.y=T)

cor(data)>0.7 #checking correlations between IVs 

#centering of variables that enter interactions
data$attitude.centr<-as.numeric(scale(data$attitude, center=T, scale=F))
data$manipulated.centr<-as.numeric(scale(data$manipulated, center=T, scale=F))
data$experience.centr<-as.numeric(scale(data$experience/6, center=T, scale=F)) #scaling
data$behavioral.centr<-as.numeric(scale(data$behavioral, center=T, scale=F))
data$trial.centr<-as.numeric(scale(data$trial/48, center=T, scale=F))

#DV
# data$change<-ifelse(data$response.x==data$response.y, 0, 1)
data$change<-NA
data$score<-(10+data$manipulated.centr*4+data$behavioral+data$attitude+
                    abs(123.5-data$id)/201+ #efekty lidí nejsou jejich id
                    data$item+data$male+
                    abs(data$trial-19.3)/48+ #efekty položek nejsou jejich identifikátory
                    data$experience/6+data$extreme+
                    data$central)/700
for (i in 1:length(data[,1])) {
data$change[i]<-rbinom(1,1,data$score[i])}

table(data$change)

# Analysis  ------------------------------------

#toto je kompletní model, který ale dává singularitu
model1 <- glmer(change~ (1 + manipulated.centr+behavioral.centr|id) + (1 + manipulated.centr|item) +
                + attitude.centr*manipulated.centr #tests H6
                + manipulated.centr*behavioral.centr #tests H7
                #+ manipulated.centr*trial.centr # možná vyhodit?
                + manipulated.centr*experience.centr + male + extreme + central, 
                data = data, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)

#toto je nejlepší model, který nehází singularitu
model1 <- glmer(change ~ (1 |id) + (1 + manipulated |item)
                + attitude.centr*manipulated.centr #tests H6
                + manipulated.centr*behavioral.centr #tests H7
                + manipulated.centr*trial.centr # možná vyhodit?
                + manipulated.centr*experience.centr 
                + male 
                + extreme 
                + central
                , data = data, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)


# where: 
# id...participant identifier;
# item...item identifiers;
# manipulated.centr...dummy indicator of manipulated items (centered);
# behavioral.centr...dummy indicator of behavioral items (centered);
# attitude.centr...environmental attitude (from pretest; centered);
# male...dummy indicator of males; 
# extreme...dummy indicator of responses 1 or 6 in the pretest;
# cental... dummy indicator of responses 3 or 4 in the pretest;
# trial.centr...order of trial (order of trial divided by the total number of trials and centered);
# experience.centr...number of trials that have been manipulated (number of trials divided by six and centered).
