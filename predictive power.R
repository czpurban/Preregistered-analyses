# Testing and comparing predictive power of environmental attitude
# measured with evaluative items (attitude 1) and behavioral items (attitude 2).

if (!require("pacman")) install.packages("pacman")
pacman::p_load(car, lme4, boot)


set.seed(111)
# Simulating pretest and posttest data ------------------------------------
productData<-data.frame(id = rep(1:201, each=32), pair = rep (1:32, 201),
                        greenProduct = rep(rep(1:16), 201*16),
                        greenLeft = sample(c(0:1), 32*201, replace = T),
                        attitude1 = rep(rnorm(201, 0, 1.5), each = 32),
                        attitude2 = rep(rnorm(201, 0, 1.5), each = 32),
                        male = rep(sample(c(0,1), 201, replace=T), each = 32)) 
i<-1                        
while (i<201*32+1){
  productData$trial[i:(i+31)] <- sample(c(1:32),32,replace=F) 
  i<-i+32
}

productData$trial<-as.numeric(scale(productData$trial))
productData$atitude1<-as.numeric(scale(productData$attitude1))
productData$atitude2<-as.numeric(scale(productData$attitude2))

productData$score<-rowSums(productData)/3000
productData$greenChoice<-rbinom(201*32, 1, 0.5+productData$score)

edit(productData)

# Analysis  ------------------------------------
# We first look at whether attitude 1 and attitude 2 predict 
# choice of green products

model1 <- glmer(greenChoice ~ (1 | id) + (1 | greenProduct/pair)
                          + trial + greenLeft + male
                          + attitude1,
                data = productData, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model1)

model2 <- glmer(greenChoice ~ (1 | id) + (1 | greenProduct/pair)
                + trial + greenLeft + male
                + attitude2,
                data = productData, family = binomial(link = "logit"), 
                glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model2)


# To test whether attitude 1 and attitude 2 have similar effect on 
# proenvironmental behavior, we test the equality of beta weights for attitude1 and attitude2.
# We get SE for their difference using bootstrap (see also:
#https://stackoverflow.com/questions/40952679/r-testing-equivalence-of-coefficients-in-multivariate-multiple-regression)

b_b <- function(dataset, indices) {
  data<-dataset[indices,]
  model1 <- glmer(greenChoice ~ (1 | id) + (1 | greenProduct/pair)
                  + trial + greenLeft + male + attitude1,
                  data = data, family = binomial(link = "logit"), 
                  glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  
  model2 <- glmer(greenChoice ~ (1 | id) + (1 | greenProduct/pair)
                  + trial + greenLeft + male + attitude2,
                  data = data, family = binomial(link = "logit"), 
                  glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  
  return(fixef(model1)[5] - fixef(model2)[5])
}
# with 1000 resamplings, this model will run for 18 hours on my notebook (i5-8265U CPU, 1.6 GHz, 16 GB RAM)
# the number of resamplings, R, has to be set to at least 1000

results <- boot(data=productData, statistic=b_b, R=10)
results


