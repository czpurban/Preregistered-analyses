library(stringdist)
library(dplyr)
library(ordinal)

#data<-read.delim("C:/Users/urban.j/Disk Google/CUK/---projekty/GACR Campbell II/Sdileny disk/Lab experiment 1/data/data posttest/extrahovana data bez pilotu/Character results.txt")

green_items <- read.delim("C:/Users/urban.j/Disk Google/CUK/---projekty/GACR Campbell II/Sdileny disk/Lab experiment 1/textová data/green_items_all.csv", 
                            header=FALSE, quote="", stringsAsFactors=FALSE)
#items 1-50 are one-off, items 51-100 are repeated

immoral_vignettes <-  read_excel("C:/Users/urban.j/Downloads/immoral.xlsx", 
                                              col_names = FALSE)
immoral_vignettes <- pull(immoral_vignettes)

names<-c("Petr","Pavel", "Jiří","Jakub","Tomáš","Lukáš","Ondřej","Marek")

for (i in 1:length(names)){
  data$item1<-gsub(names[i], "AAA", data$item1)
  data$item2<-gsub(names[i], "AAA", data$item2)
  data$item3<-gsub(names[i], "AAA", data$item3)
  data$item4<-gsub(names[i], "AAA", data$item4)
}

data$id<-as.numeric(data$id)
table(data$id)

#coding trials
data$trial<-NA
for (i in 1:length(unique(data$id))) {
  data$trial[data$id==i]<-c(1:8)}
data$round<-data$trial/8-0.5

#coding content of green vignettes
position1<-amatch(as.character(data$item1), as.character(green_items$V1),maxDist=5)
position2<-amatch(as.character(data$item2), as.character(green_items$V1),maxDist=5)
position3<-amatch(as.character(data$item3), as.character(green_items$V1),maxDist=5)
position4<-amatch(as.character(data$item4), as.character(green_items$V1),maxDist=5)

itemsCoded<-cbind(position1, position2, position3, position4)

# This is just a check that we identify items correctly
# itemsCodedNA<-is.na(itemsCoded)
# fillers<-rowSums(itemsCodedNA)
# table(data$condition, fillers)

data$type.one.green<-NA
data$type.repeated.green<-NA
for (i in 1:length(itemsCoded[,1])) {
  itemsID<-itemsCoded[i,]
  data$type.one.green[i]<-sum(itemsID[itemsID %in% c(1:50)], na.rm=T)
  data$type.repeated.green[i]<-sum(itemsID[itemsID %in% c(51:100)], na.rm=T)
  }

data$one.green<-ifelse (data$type.one.green==0, 0, 1)
data$repeated.green<-ifelse (data$type.repeated.green==0, 0, 1)

for (i in 1:length(names)){
  data$immoral<-gsub(names[i], "AAA", data$immoral)
}

data$immoral<-amatch(as.character(data$immoral), as.character(immoral_vignettes),maxDist=10)

#table(data$immoral, useNA = "always") #checking that we have identified all correctly

table(data$behavior) # 4 responses are missing (="")
table(data$behavior=="") # checking the missing responses
data$behavior[data$behavior==""]<-NA
data$behavior <- factor(data$behavior, levels = c("Velmi nemorální","Celkem nemorální","Spíše nemorální",
                                                  "Spíše morální", "Celkem morální", "Velmi morální"))

table(data$person) # 4 responses are missing (="")
table(data$person=="") # checking the missing responses
data$person[data$person==""]<-NA
data$person <- factor(data$person, levels = c("Velmi nemorální","Celkem nemorální","Spíše nemorální",
                                                  "Spíše morální", "Celkem morální", "Velmi morální"))

# Analyses using preregistered scripts
model1.behavior <- clmm(behavior ~ repeated.green + one.green + round + 
                          (1|immoral)+(1|type.repeated.green)+(1|type.one.green)+(1+repeated.green+one.green|id),
                          data=data,link="logit",threshold="flexible")
summary(model1.behavior)

model1.person <- clmm(person~repeated.green+one.green+ round+
                          (1|immoral)+(1|type.repeated.green)+(1|type.one.green)+(1+repeated.green+one.green|id),
                          data=data,link="logit",threshold="flexible")
summary(model1.person)

edit(data)
