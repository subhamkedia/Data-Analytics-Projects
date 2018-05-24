setwd("~/Desktop/CBS")

library(ggplot2)
login = read.csv("login.csv")
demo = read.csv("part_demo.csv")
head(demo)
colnames(demo)[1] <- "user_id"
colnames(login)[1] <- "course_id"
head(login)
#login = login[,-6]
login = login[,-1]
head(login)
demo = na.omit(demo)
login = na.omit(login)

func = unique(demo$Function)
industry = unique(demo$Industry)
country = unique(demo$Mailing.Country)
count_func = table(demo$Function)
plot(count_func)
count_industry = table(demo$Industry)
plot(count_industry)
count_country = table(demo$Mailing.Country)
plot(count_country)
plot_industry = ggplot(demo, aes(x = demo$Industry)) + geom_bar() + geom_bar(color = "white", fill=rgb(0.1,0.4,0.5,0.7))
plot_country = ggplot(demo, aes(x = demo$Mailing.Country)) + geom_bar() + geom_bar(color = "white", fill=rgb(0.2,0.7,0.1,0.4))
plot_function = ggplot(demo, aes(x = demo$Function)) + geom_bar() + geom_bar(color = "white", fill=rgb(0.5,0.5,0.5,0.5))
plot_country + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot_function + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot_industry + theme(axis.text.x = element_text(angle = 90, hjust = 1))

activity = unique(login$activity_type)
engage = unique(login$engageable_type)
engage_id = unique(login$engageable_id)
org_id = unique(login$organization_id)
count_activity = table(login$activity_type)
plot(count_activity)
count_engage = table(login$engageable_type)
plot(count_engage)
count_engage_id = table(login$engageable_id)
plot(count_engage_id)
count_org = table(login$organization_id)
plot(count_org)
plot_activity = ggplot(login, aes(x = login$activity_type)) + geom_bar() + geom_bar(color = "white", fill=rgb(0.2,0.3,0.4,0.5))
plot_organiz = ggplot(login, aes(x = login$organization_id)) + geom_bar() + geom_bar(color = "white", fill=rgb(0.1,0.2,0.3,0.4))
plot_engage = ggplot(login, aes(x = login$engageable_type)) + geom_bar() + geom_bar(color = "white", fill=rgb(0.5,0.6,0.7,0.8))
plot_engage + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot_organiz + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot_activity + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###### Finding out how many users are present from each organization ##########
matrix1 = matrix(nrow = length(unique(login$organization_id)), ncol = 2)
for (j in 1:nrow(matrix1)){
    mask1 = (login$organization_id == j)
    x = length(unique(login[mask1,]$User_ID))
    matrix1[j, 1] = j
    matrix1[j, 2] = x
  }

####### Converting the matrix to data frame #######
head(matrix1)
mat = as.data.frame(matrix1)

######### Plotting Number of users from each organization #############
plt = plot(x=mat$V1, y=mat$V2, xlab="Organization", ylab="Number of Users from each Organization", type="histogram")

######## finding out which organization contributes the maximum number of users #######
mat <- mat[order(mat$V2),] 
