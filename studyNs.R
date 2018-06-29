require(synapseClient)
require(ggplot2)
synapseLogin()

# qq <- synQuery('SELECT id, name FROM table WHERE projectId=="syn11540066"')
# qq[ order(qq$table.name), ]

bg <- synTableQuery("SELECT * FROM syn11855399 WHERE uploadDate > '2018-03-14' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
aa <- bg@values
aa$answers.age[ which(aa$answers.age > 89) ] <- 89
aa <- aa[ order(aa$createdOn, decreasing = TRUE), ]
aa <- aa[ !duplicated(aa$healthCode), ]
aa <- aa[ order(aa$createdOn), ]


table(aa$answers.sex, useNA = "always")/nrow(aa)
summary(aa$answers.age)
# hist(aa$answers.age, breaks="Scott")

# pp <- ggplot(data=aa, aes(answers.age, fill="red")) +
#   geom_histogram(binwidth = 1)
# show(pp)

bt <- synTableQuery("SELECT * FROM syn11860964 WHERE uploadDate > '2018-03-14' AND dataGroups IS NULL AND userSharingScope='ALL_QUALIFIED_RESEARCHERS'")
cc <- bt@values
cc <- cc[ order(cc$createdOn, decreasing = TRUE), ]
cc <- cc[ !duplicated(cc$healthCode), ]
cc <- cc[ order(cc$createdOn), ]

table(cc$answers.cuff, useNA = "always")/nrow(cc)

