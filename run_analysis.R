###########################################################################
## run_analysis.R
###########################################################################
##
## Programming Assignment for Coursera Data Science Specializatio, Getting and Cleaning Data
## https://class.coursera.org/getdata-012
##
## data set: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
##
## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
###########################################################################

###########################################################################
## Required libraries
###########################################################################

library(utils)          ## needed for the unzip function
library(reshape2)       ## needed for the melt and dcast functions
library(DataCombine)    ## needed for the replace function, install.packages("DataCombine")



############################################################################
## Constants
############################################################################

## use constants to prevent typos

smartphoneDatasetWebsiteUrl <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"
dataFileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataDestinationZipfile <- "./getdata-projectfiles-UCI HAR Dataset.zip"

trainingSetSourceUrl <- "./UCI HAR Dataset/train/X_train.txt"
trainingActivitySetSourceUrl <- "./UCI HAR Dataset/train/y_train.txt"
trainingSubjectSetSourceUrl <- "./UCI HAR Dataset/train/subject_train.txt"
testSetSourceUrl <- "./UCI HAR Dataset/test/X_test.txt"
testActivitySetSourceUrl <- "./UCI HAR Dataset/test/y_test.txt"
testSubjectSetSourceUrl <- "./UCI HAR Dataset/test/subject_test.txt"
featuresSourceUrl <- "./UCI HAR Dataset/features.txt"
activityLabelsSourceUrl <- "./UCI HAR Dataset/activity_labels.txt"


trainingSetUrl <- "/.X_train.txt"
trainingActivitySetUrl <- "./y_train.txt"
trainingSubjectSetUrl <- "./subject_train.txt"

testSetUrl <- "./X_test.txt"
testActivitySetUrl <- "./y_test.txt"
testSubjectSetUrl <- "./subject_test.txt"

templatesUrl <- "./templates/"
featuresUrl <- "./features.txt"
featuresMappingsFilename <- "featuresMappings.txt"
featuresMappingsUrl <-  paste0(templatesUrl, featuresMappingsFilename)
featuresRegexFilename <-   "featuresRegex.txt"
featuresRegexUrl <-  paste0(templatesUrl, featuresRegexFilename)

codebookUrl <-  "./Codebook.md"
codebookTemplateFilename <- "Codebook_template.md"
codebookTemplateUrl <-  paste0(templatesUrl, codebookTemplateFilename)
readmeUrl <-  "./README.md"
readmeTemplateFilename <- "README_template.md"
readmeTemplateUrl <-  paste0(templatesUrl, codebookTemplateFilename)


activityLabelsUrl <- "./activity_labels.txt"

subjectVariableName <- "subject.id"
activityVariableName <- "activity.id"

activityId <- "id"
activityLabel <- "activity.name"

myMeansUrl <- "./MyMeans.txt"

###########################################################################
## Debug settings
###########################################################################

## When developing code it is easier and faster to work with a subset of the data sets 
## If useDataSubSet is TRUE then we use only 10 rows of the data sets
## If useDataSubSet is FALSE, we read the complete data sets
useDataSubSet <- FALSE

## For testing reduce the number of columns and columns to calculate the final means for 
## activities and subjects
## If useMeltSubSet is TRUE then we use limited amount of rows/columns
## If useMeltSubSet is FALSE, we use all available rows/columns
useMeltSubSet <- FALSE

## For testing you can check if the written tidy dataset can be read in the R system
## If checkReadingTidyDataSet is TRUE then we will read the tidyDataset file into a new data frame tidyDataset
checkReadingTidyDataSet <- TRUE

###########################################################################
## Download and unzip data file
###########################################################################

## Check if the file getdata-projectfiles-UCI HAR Dataset.zip has been downloaded already
## If not, then we will download the file
if (!"getdata-projectfiles-UCI HAR Dataset.zip" %in% list.files("./")) {
    download.file(dataFileUrl, destfile= dataDestinationZipfile, method = "curl")
} 

## Check if the subdirectory "UCI HAR Dataset" exists
## If not, then we will have to unzip the downloaded file
if (!"UCI HAR Dataset" %in% list.files("./")) {
    unzip (dataDestinationZipfile, exdir = ".")
} 

## now copy the data filesfiles to the working directory
## Normally, would not do this, but seems to be a requirement in the assignment
## to be able to read the data files from working directory.
##
## QUOTE 
## The code should have a file run_analysis.R in the main directory 
## that can be run as long as the Samsung data is in your working directory.
## UNQUOTE


file.copy(trainingSetSourceUrl, trainingSetUrl)
file.copy(trainingActivitySetSourceUrl, trainingActivitySetUrl)
file.copy(trainingSubjectSetSourceUrl, trainingSubjectSetUrl)
file.copy(testSetSourceUrl, testSetUrl)
file.copy(testActivitySetSourceUrl, testActivitySetUrl)
file.copy(testSubjectSetSourceUrl, testSubjectSetUrl)
file.copy(featuresSourceUrl, featuresUrl)
file.copy(activityLabelsSourceUrl, activityLabelsUrl)


###########################################################################
## Read the training and the test sets from disk into data frames
## We read as well the activity and subject data
## For speeding up development and debugging we might limit the number 
## of rows (nrows) by setting the useDataSubSet property
## When using a subset the end results will not be correct so we print a warning message
###########################################################################

if (useDataSubSet)
{
    print("Warning: using sub sets of test and training data for debugging!")
    subsetRows <- 10
} else
{
    subsetRows <- -1
}
testData <- read.table(testSetUrl, nrows=subsetRows, stringsAsFactors = FALSE)
testActivityData <- read.table(testActivitySetUrl, nrows=subsetRows, stringsAsFactors = FALSE)
testSubjectData <- read.table(testSubjectSetUrl, nrows=subsetRows, stringsAsFactors = FALSE)

trainingData <- read.table(trainingSetUrl, nrows=subsetRows, stringsAsFactors = FALSE)
trainingActivityData <- read.table(trainingActivitySetUrl, nrows=subsetRows, stringsAsFactors = FALSE)
trainingSubjectData <- read.table(trainingSubjectSetUrl, nrows=subsetRows, stringsAsFactors = FALSE)

 
if (useDataSubSet)
{
    ## Modify the activity data sets a bit so we get more activity types in the set for debugging
    ## else wise we only see the STANDING activity
    testActivityData[] <- rep(1:5, each=2) 
    trainingActivityData[] <-  rep(1:5, each=2) 
    
} 

###########################################################################
## 1. Merges the training and the test sets to create one data set.
###########################################################################

## Add the Subject data to the test data frame, this will create a new column
testData[,subjectVariableName] <- testSubjectData[,1]

## Add the Activity data to the test data frame, this will create a new column
testData[,activityVariableName] <- testActivityData[,1]

## Add the Subject data to the training data frame, this will create a new column
trainingData[,subjectVariableName] <- trainingSubjectData[,1]

## Add the Activity data to the training data frame, this will create a new column
trainingData[,activityVariableName] <- trainingActivityData[,1]

## Create a new data frame with the training data appended to the test data
df<-rbind(testData,trainingData)
#str(df)

###########################################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
###########################################################################

## Read the features names from the supplied documentation
featuresLabels <- read.table(featuresUrl)

## Set the names of the df data frame using the features labels plus
## the two added variables subject and activity
names(df) <- c(as.character(featuresLabels[,2]),subjectVariableName , activityVariableName)

## Create a regex expression to filter on the mean, standard deviation, subject and activity
## we ignore the other variables
includeVariables <- paste0("mean()|std()|",subjectVariableName,"|",activityVariableName)

## Now create a sub set of the df data frame using the regex expression
## the subset will only contain the mean, standard deviation, subject and activity
dfSubset <- df[ , grep(includeVariables, names(df)) ]
# str(dfSubset)

###########################################################################
## 3. Uses descriptive activity names to name the activities in the data set
###########################################################################

## Read the activity labels and give these variables a meaningful name
activityLabels <- read.table(activityLabelsUrl, stringsAsFactors = FALSE)
names(activityLabels) <- c(activityId, activityLabel)

## Merge the dfSubset data frame with the activity labels to add the descriptive activity names
dfMerged= merge(dfSubset, activityLabels, by.x=activityVariableName, by.y=activityId, all=FALSE)

## We no longer need the original activity id variable
dfMerged[,activityVariableName] <- NULL

###########################################################################
## 4. Appropriately labels the data set with descriptive variable names.
###########################################################################

## Check if the features mapping file exists
## If not, then we will create a basic template for the features mapping file
## The feature mappings file will be used to create descriptive variable names
## and to fill the Code Book with the vraiable names and descriptions
## The basic template can be opened in a text editor and updated 

if (!featuresMappingsFilename %in% list.files(templatesUrl)) {
    print("Features Mappings not found, creating new file")
    
    if (!featuresRegexFilename %in% list.files(templatesUrl)) {
        print("Features Regex not found, creating new file")
        featuresRegexData <- data.frame("find"="tbody", "replace.with"="body.")
        write.table(featuresRegexData, file =featuresRegexUrl, sep= ";", quote=FALSE, row.names = FALSE)
        
    } else{
        
        featuresRegexData <- read.table(featuresRegexUrl, sep= ";", header = TRUE, stringsAsFactors = FALSE)
    }
    ## Create a data frame we can use to create a basic template for the features mapping file
    ## We create a variable with the original feature name and two variables to hold 
    ## the descriptive variable name and a description we can use for creating the Code Book
    ## As standard we use lower case for the descriptive names
    featuresRegexData <- read.table(featuresRegexUrl, sep= ";", header = TRUE, stringsAsFactors = FALSE)
    
    featuresMappingsData <- data.frame("feature"=names(dfMerged)
                                       , "feature.descriptive"=tolower(names(dfMerged))
                                       , feature.description ="description")
    # Create descriptive names by using a mapping data from featuresRegex.txt
    featuresMappingsData <- FindReplace(data = featuresMappingsData, Var = "feature.descriptive"
                                        , replaceData = featuresRegexData, from = "find", to = "replace.with"
                                        , exact = FALSE)
    featuresMappingsData
    
    ## Create the basic template features mapping file, you can edit this file later with text editor if needed
    write.table(featuresMappingsData, file =featuresMappingsUrl
                 ,sep= ";" , quote=FALSE, row.names = FALSE)
    
} else {
    featuresMappingsData <- read.table(featuresMappingsUrl
                                       ,sep= ";" ,header = TRUE, stringsAsFactors = FALSE)
}

## We can now use the features mapping file to rename the variables into descriptive variables
## We use the match() function create a lookup function
## http://stackoverflow.com/questions/22475400/r-replace-values-in-data-frame-using-lookup-table
names(dfMerged) <- featuresMappingsData[match(names(dfMerged), featuresMappingsData[,1]),2]


###########################################################################
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
###########################################################################

if (useMeltSubSet)
{
    ## for testing reduce the number of columns and columns:
    print("Warning: using sub sets of Melt for debugging!")
    dfMerged <- dfMerged[c(1,2, 3,18, 19,20), c(1,2, 80,81)]
}


## Info about using melt() and dcast():
## http://www.statmethods.net/management/reshape.html
## http://seananderson.ca/2013/10/19/reshape.html

dfMelted <- melt(dfMerged, id.vars = c(subjectVariableName, activityLabel))

## Note: you cannot use: dcast(dfMelted, activityLabel+subjectVariableName   ~ variable, mean)
##       the following works, but you get ugly column names: dcast(dfMelted, get(activityLabel) + get(subjectVariableName)  ~ variable, mean)
##       the do.call() option provides the best option in this case.
##       see: http://stackoverflow.com/questions/20145606/cast-reshape-using-variables-instead-of-the-columns-name
myMeans <- do.call("dcast", args = list(data = dfMelted, formula = paste(activityLabel, '+',subjectVariableName,'~ variable'), mean))

# Write the MyMeans data frame to disk
write.table(myMeans,file=myMeansUrl,row.names=FALSE)


## You can test if you can read the tidyDataset and how it looks
if (checkReadingTidyDataSet)
{
    tidyDataset <- read.table(myMeansUrl ,header = TRUE )
}

###########################################################################
## Create cookbook
###########################################################################


## Create list of features with descriptions to niclude in the cookbook



features <- apply(featuresMappingsData, 1, function(x) {
    paste0("**", x[2], "**\n\n",x[3], "\n\n")
    
})
featuresList <-paste0(features, collapse = '')



## http://en.wikibooks.org/wiki/R_Programming/Text_Processing#Reading_and_writing_text_files
## http://gastonsanchez.com/Handling_and_Processing_Strings_in_R.pdf
## http://www.homogenisation.org/admin/docs/RWfiles.pdf

templatesUrl <- "./templates/"
codebookUrl <-  "./Codebook.md"
codebookTemplateFilename <- "Codebook_template.md"
codebookTemplateUrl <-  paste0(templatesUrl, codebookTemplateFilename)
 
## note: It is worth pointing out that this code won't work for compressed files. 
## In that case, the number of bytes returned by file.info(filename)$size will 
## not match the actual content that will be read in memory, which we expect to be larger.
# source: http://stackoverflow.com/questions/9068397/import-text-file-as-single-character-string
text <- readChar(codebookTemplateUrl,file.info(codebookTemplateUrl)$size)

find <- "<!---Template warning-->(\\S|\\s)*<!---\\/Template warning-->"
replace <- paste("<!---This file is auto generated by run_analysis\r\nDo not edit this file, instead edit",codebookTemplateUrl,"-->")
text <-sub(pattern = find, replacement = replace, x = text)

find <- "<!---GeneratedFeatures--><!---\\/GeneratedFeatures-->"
replace <- featuresList
text <-sub(pattern = find, replacement = replace, x = text)



find <- "<!---SmartPhoneWebsiteUrl--><!---\\/SmartPhoneWebsiteUrl-->"
replace <- paste0("[Smartphone data set web site](",smartphoneDatasetWebsiteUrl,")")
text <-sub(pattern = find, replacement = replace, x = text)

find <- "<!---DataFileUrl--><!---\\/DataFileUrl-->"
replace <- paste0("[getdata-projectfiles-UCI HAR Dataset.zip](",dataFileUrl,")")
text <-sub(pattern = find, replacement = replace, x = text)

find <- "<!---DateTimeCreated--><!---\\/DateTimeCreated-->"
replace <- format(Sys.time(), "%a %b %d %H:%M:%S %Y")
text <-sub(pattern = find, replacement = replace, x = text)

writeChar(text, codebookUrl,eos = NULL)


###########################################################################
## Create README
###########################################################################


readmeUrl <-  "./README.md"
readmeTemplateFilename <- "README_template.md"
readmeTemplateUrl <-  paste0(templatesUrl, readmeTemplateFilename)

text <- readChar(readmeTemplateUrl,file.info(readmeTemplateUrl)$size)

find <- "<!---Template warning-->(\\S|\\s)*<!---\\/Template warning-->"
replace <- paste("<!---This file is auto generated by run_analysis\r\nDo not edit this file, instead edit",readmeTemplateUrl,"-->")
text <-sub(pattern = find, replacement = replace, x = text)

find <- "<!---DataFileUrl--><!---\\/DataFileUrl-->"
replace <- paste0("Data source: [getdata-projectfiles-UCI HAR Dataset.zip](",dataFileUrl,")")
text <-sub(pattern = find, replacement = replace, x = text)

find <- "<!---DateTimeCreated--><!---\\/DateTimeCreated-->"
replace <- format(Sys.time(), "%a %b %d %H:%M:%S %Y")
text <-sub(pattern = find, replacement = replace, x = text)

writeChar(text, readmeUrl,eos = NULL)


## print warnings if debug settings are used
if (useDataSubSet)
{
    print("Warning: using sub sets of test and training data for debugging!")
    warning("Warning: using sub sets of test and training data for debugging!")
}
if (useMeltSubSet)
{
    print("Warning: using sub sets of test and training data for debugging!")
    warning("Warning: using sub sets of Melt for debugging!")
}