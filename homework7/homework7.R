# This script will create the functions, KNN, PCA, UMAP
# Let's start with KNN
# First the dataset
View(iris)

library(caret)
set.seed(1010)

indxTrain <- createDataPartition(y = iris$Species,p = 0.75,list = FALSE)
training <- iris[indxTrain,]
testing <- iris[-indxTrain,]

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train(Species ~ ., data = training, 
                method = "knn", trControl = ctrl,
                preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit)

max(knnFit$results["Accuracy"])

knn_fit <- knn3(Species ~ ., data = training, k = 11)

y_hat_knn <- predict(knn_fit, newdata = testing, type = "class")

confusionMatrix(y_hat_knn, testing$Species)$overall["Accuracy"]

plot(y_hat_knn)

# lets try it again bro
train_knn <- train(Species ~ ., method = "knn", data = testing)
y_hat_knn <- predict(train_knn, testing, type = "raw")
confusionMatrix(y_hat_knn, testing$Species)$overall[["Accuracy"]]

ggplot(train_knn, highlight = TRUE)

set.seed(2008)
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Species ~ ., method = "knn",
                      data = training,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)


# PCA

pca <- prcomp(iris[, 1:4], scale = TRUE)
pca

plot(pca)


pcaData <- as.data.frame(pca$x[, 1:2]) # extract first two columns and convert to data frame
pcaData <- cbind(pcaData, iris$Species) # bind by columns
colnames(pcaData) <- c("PC1", "PC2", "Species") # change column names

library(ggplot2)
ggplot(pcaData) +
  aes(PC1, PC2, color = Species, shape = Species) + # define plot area
  geom_point(size = 2) # adding data points


percentVar <- round(100 * summary(pca)$importance[2, 1:2], 0) # compute % variances
ggplot(pcaData, aes(PC1, PC2, color = Species, shape = Species)) + # starting ggplot2
  geom_point(size = 2) + # add data points
  xlab(paste0("PC1: ", percentVar[1], "% variance")) + # x label
  ylab(paste0("PC2: ", percentVar[2], "% variance")) + # y label
  ggtitle("Principal component analysis (PCA)") + # title
  theme(aspect.ratio = 1) # width and height ratio

data.frame(pca$x[,1:2], Species=iris$Species) %>%
  ggplot(aes(PC1,PC2, fill = Species))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

# UMAP
library(umap)
umap_iris <- umap(iris[,1:4])
data.frame(umap_iris$layout, Species=iris$Species) %>%
  ggplot(aes(X1,X2, color = Species, shape = Species))+
  geom_point() +
  coord_fixed(ratio = 1)



# Now for the basketball package
basketball_data <- as_tibble(
  read.fwf(
    'http://kenpom.com/cbbga22.txt', 
    widths=c(11,24,3,23,4,4,21), 
    strip.white = T
  )
)

mutated_table <- mutate_function(basketball_data)
arranged_table <- arrange_function(mutated_table)
selected_table <- remove_function(mutated_table)
table_2022 <- func_2022_games(selected_table)
SUU_table <- team_function(table_2022, "Southern Utah")

# now lets mutate a new column to get average total points

basketball_data <- basketball_data %>% 
  select(-V6, -V7)
basketball_data <- basketball_data %>% 
  mutate_function()

names(basketball_data)[1] <- "Date"
names(basketball_data)[2] <- "AwayTeam"
names(basketball_data)[3] <- "AwayScore"
names(basketball_data)[4] <- "HomeTeam"
names(basketball_data)[5] <- "HomeScore"

# okay, now lets start doing the homework
levels(basketball_data$AwayTeam)
basketball_data$AwayTeam <- as.factor(basketball_data$AwayTeam)
basketball_data$HomeTeam <- as.factor(basketball_data$HomeTeam)
basketball_data$Venue <- as.factor(basketball_data$Venue)
basketball_data$Date <- as.Date(basketball_data$Date, format = "%m/%d/%Y")
basketball_data <- basketball_data[,-8]

# to get average points scored
# we want to filter the team then
# sum all scored points
# then take an average 
data_test<-basketball_data
sum_suu_scored <- basketball_data %>% 
  filter(grepl("Southern Utah",AwayTeam)) %>%
  summarize(mean(sum_suu_scored$AwayScore))
data_test$avg_away_suu_score <- sum_suu_scored$`mean(AwayScore)`

data <- basketball_data %>% 
  filter(grepl("Southern Utah",AwayTeam)) %>%
  mutate(win = ifelse(AwayScore > HomeScore,1,0))
data2 <- basketball_data %>%
  filter(grepl("Southern Utah",HomeTeam)) %>%
  mutate(win = ifelse(HomeScore > AwayScore,1,0))
data <- rbind(data,data2)

win_percentage <- sum(data$win)/nrow(data)


basketball_data$AwayTeam <- as.character(basketball_data$AwayTeam)
basketball_data %>%
  group_by(AwayTeam) %>%
  summarize(avg_score = mean(AwayScore))


aggregate(x = basketball_data$AwayScore,                # Specify data column
          by = list(basketball_data$AwayTeam),              # Specify group indicator
          FUN = mean)

# got this to work
kenpom22_conf <- kenpom22_conf.1
away_avg_score <- kenpom22_conf %>%                                       
  group_by(vis) %>%   
  # mutate(avg_away_score = mean(AwayScore))
 summarise_at(vars(score2),
               list(name = mean))

#so now we can merge
basketball_data <- merge(basketball_data, away_avg_score, by = "AwayTeam")
basketball_data <- basketball_data[ , c("Date", "AwayTeam", "AwayScore", "HomeTeam", "HomeScore", "Venue", "score_difference", "name")]
names(basketball_data)[8] <- "avg_away_score"

# Home
home_avg_score <- kenpom22_conf %>%                                       
  group_by(home) %>%   
  # mutate(avg_away_score = mean(AwayScore))
  summarise_at(vars(score1),
               list(name = mean))

#so now we can merge
basketball_data <- merge(basketball_data, home_avg_score, by = "HomeTeam")
basketball_data <- basketball_data[ , c("Date", "AwayTeam", "AwayScore", 
                                        "HomeTeam", "HomeScore", "Venue", 
                                        "score_difference", "avg_away_score", 
                                        "name")]
names(basketball_data)[8] <- "avg_away_score"

basketball_data %>%
  arrange(HomeTeam)


# Well this works so we'll go ahead and use it
# test_basketball$avg_away_score <- ave(basketball_data$AwayScore, basketball_data$AwayTeam)
# okay now to the otehrs
# Things to do:
# Average Points Scored : Total Score, Home Score
# Average Points Allowed: Total Score, Home Score, Away Score
# Score difference: Total, Home, Away
# Winning Percentage: Total, Home, Away
# Conference
# Tournament Participation: boolean
# I see we got to make a brand new dataset
# Start with Average points
names(away_avg_score)[1] <- "TeamName"
names(home_avg_score)[1] <- "TeamName"

new_basketball <- merge(x=away_avg_score,y=home_avg_score,by="TeamName",all=TRUE)
names(new_basketball)[2] <- "avg_away_score"
names(new_basketball)[3] <- "avg_home_score"

new_basketball <- new_basketball %>%
  mutate(avg_total_score = (avg_away_score+avg_home_score)/2)

# Average Points Allowed: Total Score, Home Score, Away Score
home_avg_allowed <- kenpom22_conf %>%                                       
  group_by(home) %>%   
  # mutate(avg_away_score = mean(AwayScore))
  summarise_at(vars(score2),
               list(name = mean))

away_avg_allowed <- kenpom22_conf %>%                                       
  group_by(vis) %>%   
  # mutate(avg_away_score = mean(AwayScore))
  summarise_at(vars(score1),
               list(name = mean))

names(away_avg_allowed)[1] <- "TeamName"
names(home_avg_allowed)[1] <- "TeamName"
new_basketball <- merge(x=new_basketball,y=away_avg_allowed,by="TeamName",all=TRUE)
new_basketball <- merge(x=new_basketball,y=home_avg_allowed,by="TeamName",all=TRUE)
names(new_basketball)[5] <- "avg_home_allowed"
names(new_basketball)[6] <- "avg_away_allowed"

new_basketball <- new_basketball %>%
  mutate(avg_total_allowed = (avg_away_allowed+avg_home_allowed)/2)

# Score difference: Total, Home, Away
# Let's go with average score difference
# Away First
# how about we just use kenpom_conf22 as our dataset
# it's cleaner
away_avg_score_diff <- kenpom22_conf %>%                                       
  group_by(vis) %>%   
  summarise_at(vars(score_diff),
               list(name = mean))

home_avg_score_diff <- kenpom22_conf %>%                                       
  group_by(home) %>%   
  summarise_at(vars(score_diff),
               list(name = mean))

names(away_avg_score_diff)[1] <- "TeamName"
names(home_avg_score_diff)[1] <- "TeamName"
new_basketball <- merge(x=new_basketball,y=away_avg_score_diff,by="TeamName",all=TRUE)
new_basketball <- merge(x=new_basketball,y=home_avg_score_diff,by="TeamName",all=TRUE)
names(new_basketball)[8] <- "away_avg_score_diff"
names(new_basketball)[9] <- "home_avg_score_diff"
# now we need to change the sign of the away average difference
new_basketball$away_avg_score_diff <- new_basketball$away_avg_score_diff*-1

# now lets do win percentages
kenpom22_conf <- kenpom22_conf %>%
  group_by(home) %>%   
  mutate(win_home = ifelse(score_diff > 0,1,0))

#away wins
kenpom22_conf <- kenpom22_conf %>%
  group_by(vis) %>%   
  mutate(win_away = ifelse(score_diff < 0,1,0))

# now that we have the boolean
# we can do win percentage
home_win_percetage <- kenpom22_conf %>%
  group_by(home) %>%
  summarize(win_percentage = sum(win_home)/n()*100)
  
away_win_percetage <- kenpom22_conf %>%
  group_by(vis) %>%
  summarize(win_percentage = sum(win_away)/n()*100)

names(home_win_percetage)[1] <- "TeamName"
names(away_win_percetage)[1] <- "TeamName"
new_basketball <- merge(x=new_basketball,y=home_win_percetage,by="TeamName",all=TRUE)
new_basketball <- merge(x=new_basketball,y=away_win_percetage,by="TeamName",all=TRUE)
names(new_basketball)[10] <- "home_win_percetage"
names(new_basketball)[11] <- "away_win_percetage"

# dropped all nas
new_basketball <- new_basketball[complete.cases(new_basketball),]

new_basketball <- new_basketball %>%
  mutate(total_win_percentage = (home_win_percetage+away_win_percetage)/2)

# to get conference, we will have to left join new_basketball with kenpom22_conf
#this actually gets conference data
for(i in 1:nrow(new_basketball)) {
  for(j in 1:nrow(kenpom22_conf)) {
    if(new_basketball$TeamName[i] == kenpom22_conf$home[j]) {
      new_basketball$conference[i] <- kenpom22_conf$home_conference[j]
      break
    }
    if(new_basketball$TeamName[i] == kenpom22_conf$vis[j]) {
      new_basketball$conference[i] <- kenpom22_conf$vis_conference[j]
      break
    }
  }
}

# now get if they played in the tournament or not
for(i in 1:nrow(new_basketball)) {
  for(j in 1:nrow(tournament)) {
    if(new_basketball$TeamName[i] == tournament$ID[j]) {
      new_basketball$tournament[i] <- 1
      break;
    }
    else{
      new_basketball$tournament[i] <- 0
    }
  }
}

# we have finally got all the data we need!!!
new_basketball$TeamName <- as.factor(new_basketball$TeamName)
new_basketball$conference <- as.factor(new_basketball$conference)
new_basketball$tournament <- as.factor(new_basketball$tournament)

# now lasso modeling to predict total_win_percentage
library(glmnet)
log_win<- log(new_basketball[,12])
lm_fit <- lm(log_win~.-TeamName, data = new_basketball)

lasso_fit1 <- glmnet(as.matrix(new_basketball[,c(2,3,4,5,6,7,8,9)]), as.matrix(log_win), lambda = 1000, alpha = 1)
cv_lasso <- cv.glmnet(as.matrix(new_basketball[,c(2,3,4,5,6,7,8,9)]), as.matrix(new_basketball[,12]), alpha = 1)
best_lambda <- cv_lasso$lambda.min
plot(cv_lasso)

lasso_fit <- glmnet(as.matrix(new_basketball[,c(2,3,4,5,6,7,8,9)]), as.matrix(new_basketball[,12]), lambda = best_lambda, alpha = 1)
lasso_fit5
coef(lasso_fit5)

lm_fit_lasso <- lm(total_win_percentage~.-TeamName-avg_total_score-avg_away_score-avg_home_score-avg_home_allowed-conference, data = new_basketball)

# PCA
pca_basketball <- prcomp(new_basketball[2:11], scale = TRUE)
pca_basketball

plot(pca_basketball)

pca_data_basket <- as.data.frame(pca_basketball$x[, 1:2]) # extract first two columns and convert to data frame
pca_data_basket <- cbind(pca_data_basket, new_basketball$total_win_percentage) # bind by columns
colnames(pca_data_basket) <- c("PC1", "PC2", "Win Percentage") # change column names

ggplot(pca_data_basket) +
  aes(PC1, PC2, color = 1, shape = 1) + # define plot area
  geom_point(size = 2) +
  scale_shape_identity()# adding data points


umap_basketball <- umap(new_basketball[,2:11])
data.frame(umap_basketball$layout) %>%
  ggplot(aes(X1,X2, color = 1, shape = 1))+
  geom_point() +
  scale_shape_identity() +
  coord_fixed(ratio = 1)
