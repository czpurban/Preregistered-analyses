file="https://drive.google.com/uc?authuser=0&id=1jZ9gO7PlUTWyo4aNbNYiW-4-OVk5fBu3&export=download"
data <- read.delim(file)
id<-as.numeric(data$id)
table(id)
unique(id)

#tady vidím jednak 213 participantù a dále nìkolik prvních participantù (pilot) mìlo jen 7 trialù v charakteru. Chyba?

