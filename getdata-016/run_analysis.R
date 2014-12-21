## getdata-016 Course Project

## 0. Download and unzip data

# First, load libraries that will be required during the execution of this code
require(dplyr)
require(reshape2)
require(sqldf)
require(tidyr)

# Check to see if local directort exists, and if not create it
if (!file.exists("~/datasciencecoursera/getdata-016")){
  dir.create("~/datasciencecoursera/getdata-016")
}

# Make the getdata-016 directory the working directory
setwd("~/datasciencecoursera/getdata-016")

# Download data file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", 
              destfile = "getdataproject.zip", 
              method = "curl")

# Unzip data files
unzip("getdataproject.zip", junkpaths = TRUE)

# Create a list of features
features <- read.table("features.txt")

# Get rid of "(" and ")" characters and replace them with something distinctive
# Makes life easier later
features[,2] <- gsub("\\)|\\(", "~", features[,2])

# Read in training and test sets
X_train <- read.fwf("X_train.txt", widths = rep(16, 561), col.names = features[,2], buffersize = 10)
X_test <- read.fwf("X_test.txt", widths = rep(16, 561), col.names = features[,2], buffersize = 10)
y_train <- read.table("y_train.txt", col.names = "activity")
y_test <- read.table("y_test.txt", col.names = "activity")
subject_train <- read.table("subject_train.txt", col.names = "subject")
subject_test <- read.table("subject_test.txt", col.names = "subject")


## 1. Merge training and test sets to create a single data set

# Combine training and test sets
X_full <- rbind(X_train, X_test)
y_full <- rbind(y_train, y_test)
subject_full <- rbind(subject_train, subject_test)


## 2. Extract only the mean and standard deviations for each measurement

# Extract only the features for mean or standard deviation for measurements
X_full_extract <- X_full[,grep("mean~~|std~~", names(X_full))]

# Merge the data sets into one set that includes the features, activity codes, and subject
full_set <- cbind(subject_full, y_full, X_full_extract)


## 3. Create descriptive activity names

# Swap out the activity codes for human-readable names
activity_names <- read.table("activity_labels.txt", col.names = c("activity", "description"))
full_set[,"activity"] <- sqldf("select activity_names.description 
                       from full_set 
                       inner join activity_names
                       on full_set.activity = activity_names.activity")


## 4. Create descriptive variable names

# The names are standardized, so we can substitute descriptive text for existing text
names(full_set) <- sub("BodyBody", "Body", names(full_set))
names(full_set) <- sub("^t", "time_domain_measurement_of_the_", names(full_set))
names(full_set) <- sub("^f", "frequency_domain_measurement_of_the_", names(full_set))
names(full_set) <- sub("Body", "body_component_", names(full_set))
names(full_set) <- sub("Gravity", "gravity_component_", names(full_set))
names(full_set) <- sub("Acc", "accelerometer_signal_", names(full_set))
names(full_set) <- sub("Jerk", "jerk_", names(full_set))
names(full_set) <- sub("Gyro", "gyroscope_signal_", names(full_set))
names(full_set) <- sub("Mag", "magnitude_", names(full_set))
names(full_set) <- sub("-mean~~", "mean", names(full_set))
names(full_set) <- sub("-std~~", "standard_deviation", names(full_set))
names(full_set) <- sub("-X$", "_of_the_X_axis", names(full_set))
names(full_set) <- sub("-Y$", "_of_the_Y_axis", names(full_set))
names(full_set) <- sub("-Z$", "_of_the_Z_axis", names(full_set))


## 5. Build tidy dataset

# First need a variable column
molten_set <- melt(full_set, id.vars = c("subject", "activity"))

# Now we split the combined variable into separate variable columns
molten_set$domain <- "time"
molten_set[grep("frequency_domain", molten_set[, "variable"]), "domain"] <- "frequency"
molten_set$domain <- as.factor(molten_set$domain)

molten_set$component <- "body"
molten_set[grep("gravity", molten_set[, "variable"]), "component"] <- "gravity"
molten_set$component <- as.factor(molten_set$component)

molten_set$sensor <- "accelerometer"
molten_set[grep("gyroscope", molten_set[, "variable"]), "sensor"] <- "gyroscope"
molten_set$sensor <- as.factor(molten_set$sensor)

molten_set$transform <- "jerk"
molten_set[grep("magnitude", molten_set[, "variable"]), "transform"] <- "magnitude"
molten_set[grep("signal_mean|signal_standard", molten_set[, "variable"]), "transform"] <- "no transform"
molten_set$transform <- as.factor(molten_set$transform)

molten_set$summary <- "mean"
molten_set[grep("standard_deviation", molten_set[, "variable"]), "summary"] <- "standard deviation"
molten_set$summary <- as.factor(molten_set$summary)

molten_set$axis <- "X"
molten_set[grep("of_the_Y_axis", molten_set[, "variable"]), "axis"] <- "Y"
molten_set[grep("of_the_Z_axis", molten_set[, "variable"]), "axis"] <- "Z"
molten_set[grep("magnitude_mean|magnitude_standard", molten_set[, "variable"]), "axis"] <- "no axis"
molten_set$axis <- as.factor(molten_set$axis)

# Get rid of the variable column and we have our tidy set (with some rearranging)
tidy_set <- molten_set[, c(1, 2, 5, 6, 7, 8, 9, 10, 4)]

# Now create the summaries by each variable
tidy_set <- tbl_df(tidy_set)
tidy_set <- group_by(tidy_set, subject, activity, domain, component, sensor, transform, summary, axis)
tidy_set <- summarise(tidy_set, mean(value))

# Finally, output the table as a file
write.table(tidy_set, file = "tidy_set.txt", row.names = FALSE, sep = "\t")
