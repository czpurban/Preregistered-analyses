if (!require("pacman")) install.packages("pacman")
pacman::p_load(car, lme4, dplyr)


set.seed(111)
# Simulating pretest and posttest data ------------------------------------
pretestData<-data.frame(id = rep(1:201, each=96) , item = rep (1:96, 201), 
                        behavioral = rep(rep(0:1, each=48), 201),
                        attitude = rep(rnorm(201, 0, 1.5), each = 96), 
                        male = rep(sample(c(0,1),201, replace = T), each = 96),
                        response =  sample(1:6, 201*96, replace=T)
                        )



posttestData<-data.frame(id = rep(1:201, each=48), response =  sample(1:6, 201*48, replace=T))
                                                  
i<-1
while (i<201*48+1){
  posttestData$item[i:(i+47)] <- c(sample(1:48, 24, replace=F),sample(49:96, 24, replace=F))
  posttestData$manipulated[i:(i+47)]<-c(sample(c(rep(0,21),rep(1,3)), 24, replace=F),
                                        sample(c(rep(0,21),rep(1,3)), 24, replace=F))
  posttestData$trial[i:(i+47)] <- sample(c(1:48),48,replace=F) 
  i<-i+48
}

# Manipulating pretest and posttest data ------------------------------------
pretestData$extreme<-ifelse(pretestData$response==1 | pretestData$response==6, 1, 0)
pretestData$central<-ifelse(pretestData$response==3 | pretestData$response==4, 1, 0)
posttestData$experience[posttestData$manipulated==1]<-c(0:5)

# Assigning value of experience, i.e., how many manipulated items s/he has already seen
i<-1
while (i <length (posttestData[,1])+1) {
  k<-0
  for (j in 1:48) {
    if (posttestData$manipulated[i]==0) {
      posttestData$experience[i]<-k
    } else {k<-posttestData$experience[i]+1}
    i<-i+1
  }
}

#merging pre-post test data; keeping only pretest data that match posttest data
prepostData<-merge (pretestData, posttestData, by.x = c("id", "item"), by.y = c("id", "item"), all.x=F, all.y=T)

cor(prepostData)>0.7 #checking correlations between IVs 

#centering of variables that enter interactions
prepostData$attitude.centr<-as.numeric(scale(prepostData$attitude, center=T, scale=F))
prepostData$manipulated.centr<-as.numeric(scale(prepostData$manipulated, center=T, scale=F))
prepostData$experience.centr<-as.numeric(scale(prepostData$experience/6, center=T, scale=F)) #scaling
prepostData$behavioral.centr<-as.numeric(scale(prepostData$behavioral, center=T, scale=F))
prepostData$trial.centr<-as.numeric(scale(prepostData$trial/48, center=T, scale=F))

#DV
# prepostData$change<-ifelse(prepostData$response.x==prepostData$response.y, 0, 1)
prepostData$change<-NA
prepostData$score<-(10+prepostData$manipulated.centr*4+prepostData$behavioral+prepostData$attitude+
                    abs(123.5-prepostData$id)/201+ #efekty lidí nejsou jejich id
                    prepostData$item+prepostData$male+
                    abs(prepostData$trial-19.3)/48+ #efekty položek nejsou jejich identifikátory
                    prepostData$experience/6+prepostData$extreme+
                    prepostData$central)/700
for (i in 1:length(prepostData[,1])) {
prepostData$change[i]<-rbinom(1,1,prepostData$score[i])}

table(prepostData$change)

# Analysis  ------------------------------------

#toto je kompletní model, který ale dává singularitu
model1 <- glmer(change~ (1 + manipulated.centr+behavioral.centr|id) + (1 + manipulated.centr|item) +
                + attitude.centr*manipulated.centr #tests H6
                + manipulated.centr*behavioral.centr #tests H7
                #+ manipulated.centr*trial.centr # možná vyhodit?
                + manipulated.centr*experience.centr + male + extreme + central, 
                data = prepostData, family = binomial(link = "logit"), 
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
                , data = prepostData, family = binomial(link = "logit"), 
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
