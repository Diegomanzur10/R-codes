


#Load raw data
train=read.csv("train.csv", header=TRUE)
test= read.csv("test.csv", header=TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived=data.frame(Survived=rep("None", nrow (test)), test[,])

# Combine data set

data.combined=rbind(train,test.survived)

# A bit about R data types (e.g., factors)

str(data.combined)

data.combined$Survived= as.factor(data.combined$Survived)
data.combined$Pclass= as.factor(data.combined$Pclass)


#Take a look a gross survival rates
table(data.combined$Survived)


#Distribution across classes
table(data.combined$Pclass)


# Load up ggplot2 package to use for visualization
# install.packages("ggplot2")
library(ggplot2)

#Hipothesis - Rich folks survived at a higher rate
train$Pclass= as.factor(train$Pclass) #Pclass from train data frame and we transform Survived into a factor variable to create uniques values
train$Survived= as.factor(train$Survived)

ggplot(train,aes(x=Pclass, fill=Survived)) + 
  geom_bar()+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")

# Examine the first few names in the training data set
head(as.character(train$Name))

# How many uniques names are there across both train & test ds?
length(unique(as.character(data.combined$Name)))

#Two duplicated names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names=as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# Next, take a look at the records in the combined ds

data.combined[which(data.combined$Name %in% dup.names),]

# What is up with the "Miss.." and "Mr.." thing?
library(stringr)

# Any correlation with other variables (e.g. Sibps)
misses=data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlate with age

mrses=data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males=data.combined[which(train$Sex=="male"),]
males[1:5,]

# Expand upon the relationship between "Survived" and "Pclass" by adding the new "Title" variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction

extractTitle = function(Name) {
  if (length(grep("Miss.",Name))>0){
    return ("Miss.")
    } else if (length(grep("Master.",Name))>0){
      return ("Master.")
      } else if (length(grep("Mrs.",Name))>0){
        return ("Mrs.")
        } else if (length(grep("Mr.",Name))>0){
          return ("Mr.")
          } else {
            return("Other")
          }
  }

titles= NULL

for (i in 1:nrow(data.combined)){
  titles= c(titles,extractTitle(data.combined[i,"Name"]))
}

data.combined$Title= as.factor(titles)