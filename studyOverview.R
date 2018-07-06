require(synapser)
require(tableone)
require(plyr)
require(dplyr)
require(ggplot2)
synLogin()

## SOME VARIABLES THAT ARE SET FOR USE THROUGHOUT
releasedVersions <- c("version 1.0.3, build 72",
                      "version 1.0.5, build 75",
                      "version 1.0.6, build 76",
                      "version 1.1.0, build 82")

#####
## GET BACKGROUND SURVEY
#####
bg <- synTableQuery("SELECT * FROM syn11855399 WHERE uploadDate > '2018-03-14' AND uploadDate < '2018-06-30' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
aa <- as.data.frame(bg)
## USE ONLY RELEASED VERSIONS OF THE APP
aa <- aa[ which(aa$appVersion %in% releasedVersions), ]

## CENSOR AGE
aa$answers.age[ which(aa$answers.age > 89) ] <- 89

## CENSOR HEIGHT
## lower than 4 feet or higher than 7 foot six inches set as missing (very unlikely)
## lower than 5 feet or higher than 6 foot six inches censored at those values
aa$answers.height[ which(aa$answers.height < 48) ] <- NA
aa$answers.height[ which(aa$answers.height < 60) ] <- 60
aa$answers.height[ which(aa$answers.height > 90) ] <- NA
aa$answers.height[ which(aa$answers.height > 78) ] <- 78

## CENSOR WEIGHT
## lower than 80 or higher than 500 pounds set as missing (very unlikely)
## lower than 100 or higher than 300 pounds censored at those values
aa$answers.weight[ which(aa$answers.weight < 80) ] <- NA
aa$answers.weight[ which(aa$answers.weight < 100) ] <- 100
aa$answers.weight[ which(aa$answers.weight > 500) ] <- NA
aa$answers.weight[ which(aa$answers.weight > 300) ] <- 300

## KEEP MOST RECENT VERSION OF THE SURVEY
aa$createdOn <- as.POSIXct(aa$createdOn/1000, origin="1970-01-01")
aa <- aa[ order(aa$createdOn, decreasing = TRUE), ]
aa <- aa[ !duplicated(aa$healthCode), ]
aa <- aa[ order(aa$createdOn), ]

## GET DATA FROM BASELINE BP
bt <- synTableQuery("SELECT * FROM syn11860964 WHERE uploadDate > '2018-03-14' AND uploadDate < '2018-06-30' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
cc <- as.data.frame(bt)
## USE ONLY RELEASED VERSIONS OF THE APP
cc <- cc[ which(cc$appVersion %in% releasedVersions), ]
cc <- cc[ order(cc$createdOn, decreasing = TRUE), ]
cc <- cc[ !duplicated(cc$healthCode), ]
cc <- cc[ order(cc$createdOn), ]
cc <- cc[, c("healthCode", "answers.cuff")]
aa <- merge(aa, cc, by="healthCode", all.x=TRUE, all.y=FALSE)

#####
## LOOK AT USAGE PATTERNS
#####
uTab <- synTableQuery("SELECT * FROM syn11855393 WHERE uploadDate > '2018-03-14' AND uploadDate < '2018-06-30' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
uu <- as.data.frame(uTab)
uu <- uu[ which(uu$appVersion %in% releasedVersions), ]
uu$createdOn <- as.POSIXct(uu$createdOn/1000, origin="1970-01-01")
uu$createdOnDate <- as.Date(uu$createdOn)

## ADD TIME SINCE ENTERED STUDY
uu <- ddply(uu, .(healthCode), mutate,
            timeOn = createdOn - min(createdOn),
            daysOn = createdOnDate - min(createdOnDate))

## SUMMARIZE TO ONE PER HEALTHCODE
upp <- ddply(uu, .(healthCode), summarize,
             nBP = sum(originalTable == "Blood Pressure-v6"),
             nMorning = sum(originalTable %in% c("Morning-v4", "Morning-v5")),
             nMAB = sum(originalTable %in% c("Body and Mind-v6", "Body and Mind-v7")),
             nEvening = sum(originalTable %in% c("Night-v8", "Night-v9")),
             daysOnStudy = as.numeric(max(timeOn), units="days"))

## GET RIDE OF THEORETICAL DUPLICATE BP MEASUREMENTS
checkinTables <- c("Morning-v4", "Morning-v5", "Body and Mind-v6", "Body and Mind-v7", "Night-v8", "Night-v9")
allActivities <- uu[ !(uu$originalTable %in% c("Baseline-v2", checkinTables)), ]
allCheckins <- uu[ uu$originalTable %in% checkinTables, ]
allCheckins <- ddply(allCheckins, .(healthCode, createdOnDate), summarize,
                     morning = any(originalTable %in% c("Morning-v4", "Morning-v5")),
                     afternoon = any(originalTable %in% c("Body and Mind-v6", "Body and Mind-v7")),
                     evening = any(originalTable %in% c("Night-v8", "Night-v9")))
allCheckins <- ddply(allCheckins, .(healthCode), mutate,
                     daysOn = createdOnDate - min(createdOnDate))

#####
## PLOT FUNCTIONS
#####
plotThreeWeeks <- function(df){
  tmp <- unique(df[, c("healthCode", "daysOn")])
  tmp <- tmp[ tmp$daysOn > 0 & tmp$daysOn < 22, ]
  tt <- as.data.frame(table(tmp$daysOn)/length(unique(tmp$healthCode)))
  twp <- ggplot(tt, aes(x=Var1, y=Freq)) +
    geom_histogram(stat="identity") +
    xlab("days on study") +
    ylab("proportion") +
    theme_minimal()
  return(twp)
}

plotTimeOfDay <- function(df){
  tmp <- df
  ## APPROXIMATION OF LOCAL TIME
  tmp$hourOfDay <- floor(as.integer(format(tmp$createdOn, "%H")) + tmp$createdOnTimeZone/100)
  tmp$hourOfDay[ which(tmp$hourOfDay < 0) ] <- tmp$hourOfDay[ which(tmp$hourOfDay < 0) ] + 24
  tmp$hourOfDay[ which(tmp$hourOfDay >= 24) ] <- tmp$hourOfDay[ which(tmp$hourOfDay >= 24) ] - 24
  tt <- as.data.frame(table(tmp$hourOfDay)/nrow(tmp))
  tod <- ggplot(tt, aes(x=Var1, y=Freq)) +
    geom_histogram(stat="identity") +
    xlab("local hour of the day") +
    ylab("proportion") +
    theme_minimal()
  return(tod)
}

plotDayOfWeek <- function(df){
  tmp <- df
  ## APPROXIMATION OF LOCAL TIME
  tmp$dayOfWeek <- factor(format(tmp$createdOn, "%a"), levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  tt <- as.data.frame(table(tmp$dayOfWeek)/nrow(tmp))
  dow <- ggplot(tt, aes(x=Var1, y=Freq)) +
    geom_histogram(stat="identity") +
    xlab("day of the week") +
    ylab("proportion") +
    theme_minimal()
  return(dow)
}


#####
## DEMOGRAPHICS OVERVIEW
#####
catVars <- c("answers.sex", 
             "answers.ethnicity.Black or African American", 
             "answers.ethnicity.Asian or Pacific Islander",
             "answers.ethnicity.White",
             "answers.ethnicity.American Indian or Alaska Native",
             "answers.ethnicity.Other",
             "answers.education",
             "answers.tobacco",
             "answers.health")
contVars <- c("answers.age", "answers.height", "answers.weight")
tableOne <- CreateTableOne(vars=c(catVars, contVars), data=aa, factorVars=catVars)
print(tableOne, nonnormal=contVars)

## PLOT OUTPUTS
plotDayOfWeek(allActivities)
plotDayOfWeek(aa)
