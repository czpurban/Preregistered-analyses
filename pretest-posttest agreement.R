#This R script analyzes the agreement between pretest and posttest responses
# to evaluative and behavioral attitude items.

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
  posttestData$trial[i:(i+47)] <- sample(c(1:48),48,replace=F) 
  i<-i+48
}

# Manipulating pretest and posttest data ------------------------------------
pretestData$extreme<-ifelse(pretestData$response==1 | pretestData$response==6, 1, 0)
pretestData$central<-ifelse(pretestData$response==3 | pretestData$response==4, 1, 0)

#merging pre-post test data; keeping only pretest data that match posttest data
prepostData<-merge (pretestData, posttestData, by.x = c("id", "item"), by.y = c("id", "item"), all.x=F, all.y=T)

prepostData$score<-(prepostData$item/96 + prepostData$id/201 +
                    prepostData$trial/96 + prepostData$behavioral + 
                    prepostData$extreme + prepostData$central +
                    prepostData$attitude + prepostData$male)/80 #max je 10%, min. je 4%

# DV = is there any change between the pretest and postest?
prepostData$anyChange<-rbinom(48*201,1, prob = 0.1 + prepostData$score)
#fivenum(prepostData$change)

# DV = how much of the change is there between the pretest and the postest
prepostData$sizeChange<-trunc(as.numeric(scale(prepostData$score))*3)
prepostData$sizeChange[prepostData$anyChange==0]<- NA
prepostData$sizeChange[prepostData$sizeChange==0]<-1
prepostData$sizeChange[prepostData$sizeChange>5]<-5
prepostData$sizeChange[prepostData$sizeChange<(-5)]<--5
table(prepostData$sizeChange)

#centering of variables that enter interactions
prepostData$attitude.centr<-as.numeric(scale(prepostData$attitude, center=T, scale=F))
prepostData$behavioral.centr<-as.numeric(scale(prepostData$behavioral, center=T, scale=F))
prepostData$trial.centr<-as.numeric(scale(prepostData$trial/48, center=T, scale=F))

#This model tests whether behavioral items are less likely to be changed between pretest and posttest.
model1 <- glmer(anyChange ~ (1 |id) + (1 + behavioral.centr |item)
               + behavioral.centr #testing hyp. that behavioral are more stable
               + trial.centr 
               + male + attitude.centr, 
               data = prepostData, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)

#This model tests whether the size of the chenge between pretest and posttest is smaller for behavioral items.
model2 <- lmer(sizeChange ~ (1 |id) + (1 + behavioral.centr |item)
                + behavioral.centr #testing hypothesis that behavioral result in smaller change
                + trial.centr 
                + male + attitude.centr, 
                data = prepostData, REML = F, control = lmerControl(optimizer ="Nelder_Mead"))
summary(model2)

# where:
# anyChange...is there any change between pretest and posttest (dummy indicator);
# sizeChange...what is the size of the change between pretest and posttest (takes values -5 to +5);
# id...participant identifier;
# item...item identifiers;
# behavioral.centr...dummy indicator of behavioral items (centered);
# attitude.centr...environmental attitude (from pretest; centered);
# male...dummy indicator of males; 
# trial.centr...order of trial (order of trial divided by the total number of trials and centered).
