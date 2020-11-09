require(synapser)
require(ggplot2)
synLogin()

# qq <- synQuery('SELECT id, name FROM table WHERE projectId=="syn11540066"')
# qq[ order(qq$table.name), ]

releasedVersions <- c("version 1.0.3, build 72",
                      "version 1.0.5, build 75",
                      "version 1.0.6, build 76",
                      "version 1.1.0, build 82",
                      "version 1.1.4, build 86")

bg <- synTableQuery("SELECT * FROM syn11855399 WHERE uploadDate > '2018-03-14' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
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


table(aa$answers.sex, useNA = "always")/nrow(aa)
summary(aa$answers.age)

bt <- synTableQuery("SELECT * FROM syn11860964 WHERE uploadDate > '2018-03-14' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
cc <- as.data.frame(bt)
## USE ONLY RELEASED VERSIONS OF THE APP
cc <- cc[ which(cc$appVersion %in% releasedVersions), ]
cc <- cc[ order(cc$createdOn, decreasing = TRUE), ]
cc <- cc[ !duplicated(cc$healthCode), ]
cc <- cc[ order(cc$createdOn), ]
cc <- cc[, c("healthCode", "answers.cuff")]
aa <- merge(aa, cc, by="healthCode", all.x=TRUE, all.y=FALSE)
aa$cuffCalibration <- aa$answers.cuff != "None"
names(aa) <- gsub(" ", "_", names(aa), fixed=TRUE)

table(cc$answers.cuff, useNA = "always")/nrow(cc)


# sss <- aa[ grep("Samsung", aa$phoneInfo, fixed=TRUE), ]
# 
# thisThing <- strsplit(sss$phoneInfo, "-", fixed=TRUE)
# res <- sapply(thisThing, function(x){
#   if(length(x) < 2){
#     return(NULL)
#   } else{
#     return(substr(x[[2]], nchar(x[[2]]), nchar(x[[2]])))
#   }
# })
# res <- unlist(res)
# table(res)
