#install.packages("plyr")
#install.packages("dplyr")

library(plyr); library(dplyr)

# reading the x_test variables names

setwd("D:/mpessoa/curso/R/03 Getting and Cleaning Data/project/UCI HAR Dataset")
features = read.table("features.txt")

### standart variable names ####

features$V2 <- tolower(features$V2)
features$V2 <- gsub("-","",features$V2)
features$V2 <- sub("\\()","",features$V2)
features$V2 <- sub("\\)","",features$V2)
features$V2 <- sub("\\(","",features$V2)
features$V2 <- gsub("\\,","",features$V2)



##########################################################################


# reading test  dataset
setwd("D:/mpessoa/curso/R/03 Getting and Cleaning Data/project/UCI HAR Dataset/test")

subject_test = read.table("subject_test.txt")
x_test = read.table("X_test.txt")
y_test = read.table("Y_test.txt")


# reading train dataset
setwd("D:/mpessoa/curso/R/03 Getting and Cleaning Data/project/UCI HAR Dataset/train")

subject_train = read.table("subject_train.txt")
x_train = read.table("X_train.txt")
y_train = read.table("Y_train.txt")


## create  variables to merge the dataset

library(dplyr)

subject_test <- mutate(subject_test,
                  id=seq(1,2947,by=1))

x_test <- mutate(x_test,
                       id=seq(1,2947,by=1))

y_test <- mutate(y_test,
                       id=seq(1,2947,by=1))


subject_train <- mutate(subject_train,
                       id=seq(1,7352,by=1))

x_train <- mutate(x_train,
                 id=seq(1,7352,by=1))

y_train <- mutate(y_train,
                 id=seq(1,7352,by=1))


### padronizando variaveis

###############################################################################
#### step 04 - Appropriately labels the data set with descriptive variable names  ###
###############################################################################

subject_test <- rename(subject_test, id_subject = V1)
y_test <- rename(y_test, id_activity = V1)


subject_train <- rename(subject_train, id_subject = V1)
y_train <- rename(y_train, id_activity = V1)


## naming the x_test and x_train variables

# cleaning variables names

names<- features[,2]

colnames(x_test) <- names
colnames(x_test)[562] <- "id"

colnames(x_train) <- names
colnames(x_train)[562] <- "id"


###############################################################################
#### step 01 - Merges the training and the test sets to create one data set ###
###############################################################################


#groups: 1.training 2.test

# merge the tests dataset


temp = merge(subject_test,y_test,by.x="id",by.y="id",all=TRUE)
temp$group <- ("test")

mergedtest = merge(temp,x_test,by.x="id",by.y="id",all=TRUE)
rm(temp)

# merge the training dataset

temp = merge(subject_train,y_train,by.x="id",by.y="id",all=TRUE)
temp$group <- ("training")

mergedtrain = merge(temp,x_train,by.x="id",by.y="id",all=TRUE)
rm(temp)

# making only one dataset with test and traing data


uci_har_dataset <- rbind(mergedtest,mergedtrain)

###############################################################################
#### step 02 - Extracts only the measurements on the mean and standard deviation for each measurement  ###
###############################################################################


names_var <- colnames(uci_har_dataset)

vmean <-grep ("mean", names_var)
std   <-grep ("std", names_var)


uci_har_dataset <- uci_har_dataset[,grepl("mean", colnames(uci_har_dataset)) | grepl("std", colnames(uci_har_dataset)) 
                            | grepl("id", colnames(uci_har_dataset)) | grepl("id_subject", colnames(uci_har_dataset))
                            | grepl("id_activity", colnames(uci_har_dataset)) | grepl("group", colnames(uci_har_dataset))]


###############################################################################
#### step 03 - Extracts only the measurements on the mean and standard deviation for each measurement  ###
###############################################################################

uci_har_dataset$id_activity <- factor(uci_har_dataset$id_activity,
                    levels = c(1,2,3,4,5,6),
                    labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))



###############################################################################
#### step 04 - Appropriately labels the data set with descriptive variable names  ###
###############################################################################

# the step 04 was made from 71 to 89 lines

colnames(uci_har_dataset)

###############################################################################
#### step 05 - creates a second, independent tidy data set with the average of 
### each variable for each activity and each subjec  ###
###############################################################################


step_05 <- group_by(uci_har_dataset, id_activity, id_subject)

library (reshape2)

datamelt <- melt(uci_har_dataset, id=c("id_activity", "id_subject"),
            measure.vars=c("tbodyaccmeanx","tbodyaccmeany","tbodyaccmeanz","tbodyaccstdx","tbodyaccstdy","tbodyaccstdz","tgravityaccmeanx",
                           "tgravityaccmeany"  ,"tgravityaccmeanz"                 , "tgravityaccstdx"                   ,"tgravityaccstdy"                   ,"tgravityaccstdz"                  
  ,"tbodyaccjerkmeanx"                , "tbodyaccjerkmeany"                 ,"tbodyaccjerkmeanz"                 ,"tbodyaccjerkstdx"                 
  ,"tbodyaccjerkstdy"                 , "tbodyaccjerkstdz"                  ,"tbodygyromeanx"                    ,"tbodygyromeany"                   
  ,"tbodygyromeanz"                   , "tbodygyrostdx"                     ,"tbodygyrostdy"                     ,"tbodygyrostdz"                    
  ,"tbodygyrojerkmeanx"               , "tbodygyrojerkmeany"                ,"tbodygyrojerkmeanz"                ,"tbodygyrojerkstdx"                
  ,"tbodygyrojerkstdy"                , "tbodygyrojerkstdz"                 ,"tbodyaccmagmean"                   ,"tbodyaccmagstd"                   
  ,"tgravityaccmagmean"               , "tgravityaccmagstd"                 ,"tbodyaccjerkmagmean"               ,"tbodyaccjerkmagstd"               
  ,"tbodygyromagmean"                 , "tbodygyromagstd"                   ,"tbodygyrojerkmagmean"              ,"tbodygyrojerkmagstd"              
  ,"fbodyaccmeanx"                    , "fbodyaccmeany"                     ,"fbodyaccmeanz"                     ,"fbodyaccstdx"                     
  ,"fbodyaccstdy"                     , "fbodyaccstdz"                      ,"fbodyaccmeanfreqx"                 ,"fbodyaccmeanfreqy"                
  ,"fbodyaccmeanfreqz"                , "fbodyaccjerkmeanx"                 ,"fbodyaccjerkmeany"                 ,"fbodyaccjerkmeanz"                
  ,"fbodyaccjerkstdx"                 , "fbodyaccjerkstdy"                  ,"fbodyaccjerkstdz"                  ,"fbodyaccjerkmeanfreqx"            
  ,"fbodyaccjerkmeanfreqy"            , "fbodyaccjerkmeanfreqz"             ,"fbodygyromeanx"                    ,"fbodygyromeany"                   
  ,"fbodygyromeanz"                   , "fbodygyrostdx"                     ,"fbodygyrostdy"                     ,"fbodygyrostdz"                    
  ,"fbodygyromeanfreqx"               , "fbodygyromeanfreqy"                ,"fbodygyromeanfreqz"                ,"fbodyaccmagmean"                  
  ,"fbodyaccmagstd"                   , "fbodyaccmagmeanfreq"               ,"fbodybodyaccjerkmagmean"           ,"fbodybodyaccjerkmagstd"           
  ,"fbodybodyaccjerkmagmeanfreq"      , "fbodybodygyromagmean"              ,"fbodybodygyromagstd"               ,"fbodybodygyromagmeanfreq"         
  ,"fbodybodygyrojerkmagmean"         , "fbodybodygyrojerkmagstd"           ,"fbodybodygyrojerkmagmeanfreq"      ,"angletbodyaccmeangravity"         
  ,"angletbodyaccjerkmeangravitymean)", "angletbodygyromeangravitymean"     ,"angletbodygyrojerkmeangravitymean" ,"anglexgravitymean"                
  ,"angleygravitymean"                ,"anglezgravitymean"))


head(datamelt)
  
step_05 <- dcast(datamelt, id_subject + id_activity  ~ variable, mean)
step_05

setwd("D:/mpessoa/curso/R/03 Getting and Cleaning Data/project/UCI HAR Dataset")
write.table(step_05, file="step_05.txt", sep=" ", row.name=FALSE)
