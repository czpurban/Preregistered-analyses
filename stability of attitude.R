if (!require("pacman")) install.packages("pacman")
pacman::p_load(car, lme4, dplyr)

# Simulating pretest and posttest data ------------------------------------
pretestData<-data.frame(id = rep(1:201, each=96) , item = rep (1:96, 201), response =  sample(1:6, 201*96, replace=T), 
                        attitude = rnorm(201*96,0,1.5), behavioral = rep(rep(0:1, each=48), 201))
pretestData$male<-sample(c(0,1),201*96, replace = T)


posttestData<-data.frame(id = rep(1:201, each=48), 
                         response =  sample(1:6, 201*48, replace=T) 
                         )
i<-1
while (i<201*48+1){
  posttestData$item[i:(i+47)] <- sample(1:96, 48, replace=F) 
  posttestData$manipulated[i:(i+47)]<-sample(c(rep(0,42), rep(1,6)), replace = F)
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
prepostData$change<-ifelse(prepostData$response.x==prepostData$response.y, 0, 1)

# Analysis  ------------------------------------

model1 <- glmer(change~ (1 + manipulated.centr+behavioral.centr|id) + (1 + manipulated.centr|item) +
                + attitude.centr*manipulated.centr #tests H6
                + manipulated.centr*behavioral.centr #tests H7
                #+ manipulated.centr*trial.centr # možná vyhodit?
                + manipulated.centr*experience.centr + male + extreme + central, 
                data = prepostData, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)
edit(prepostData)

#máme problém se singularitou; 
isSingular(model1, tol = 1e-05)

model1 <- glmer(change~ (1|id) + (1|item) +
                  + attitude.centr*manipulated.centr #tests H6
                + manipulated.centr*behavioral.centr #tests H7
                #+ manipulated.centr*trial.centr # možná vyhodit?
                + manipulated.centr*experience.centr + male + extreme + central, 
                data = prepostData, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)
#poøád problémy se singularitou

model1 <- glmer(change~ (1|id) +
                  + attitude.centr*manipulated.centr #tests H6
                + manipulated.centr*behavioral.centr #tests H7
                #+ manipulated.centr*trial.centr # možná vyhodit?
                + manipulated.centr*experience.centr + male + extreme + central, 
                data = prepostData, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)
#tady už problém se singularitou není


summary(prepostData)

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
