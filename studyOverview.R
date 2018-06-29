require(synapser)
synLogin()

#####
## GET BACKGROUND SURVEY
#####
bg <- synTableQuery("SELECT * FROM syn11855399 WHERE uploadDate > '2018-03-14' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
aa <- bg$asDataFrame()

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
aa <- aa[ order(aa$createdOn, decreasing = TRUE), ]
aa <- aa[ !duplicated(aa$healthCode), ]
aa <- aa[ order(aa$createdOn), ]

#####
## DEMOGRAPHICS OVERVIEW
#####

## GENDER
table(aa$answers.sex, useNA = "always")
table(aa$answers.sex, useNA = "always")/nrow(aa)*100
## ETHNICITY
table(aa$`answers.ethnicity.Black or African American`, useNA = "always")
table(aa$`answers.ethnicity.Black or African American`, useNA = "always")/nrow(aa)*100
table(aa$`answers.ethnicity.Asian or Pacific Islander`, useNA = "always")
table(aa$`answers.ethnicity.Asian or Pacific Islander`, useNA = "always")/nrow(aa)*100
table(aa$answers.ethnicity.White, useNA = "always")
table(aa$answers.ethnicity.White, useNA = "always")/nrow(aa)*100
table(aa$`answers.ethnicity.American Indian or Alaska Native`, useNA = "always")
table(aa$`answers.ethnicity.American Indian or Alaska Native`, useNA = "always")/nrow(aa)*100
table(aa$answers.ethnicity.Other, useNA = "always")
table(aa$answers.ethnicity.Other, useNA = "always")/nrow(aa)*100
## EDUCATION
table(aa$answers.education, useNA = "always")
table(aa$answers.education, useNA = "always")/nrow(aa)*100
## TOBACCO USE
table(aa$answers.tobacco, useNA = "always")
table(aa$answers.tobacco, useNA = "always")/nrow(aa)*100
## EXERCISE
# table(aa$answers.exercise, useNA = "always")
# table(aa$answers.exercise, useNA = "always")/nrow(aa)*100
## HEALTH STATUS
table(aa$answers.health, useNA = "always")
table(aa$answers.health, useNA = "always")/nrow(aa)*100

## CONTINUOUS VALUES
summary(aa$answers.age)
summary(aa$answers.height)
summary(aa$answers.weight)
