# setwd("~/Documents/FLAGCX/Projects/metricAssessment/metricAssessment")

# Capture Facebook posts
library(Rfacebook)
library(tidyverse) 
library(dplyr)
library(car)

token <- "EAACEdEose0cBAPlZBoqcPe2ZAOxtq1M5de5F9V1RXRyEdAZANxnVy7ROiKyPaOuEFtI4tsEAeMaCuVVbnQeQ9jGN4zY0nA36CgFeNsGQHW0v8jZBJFuD8OC49BcZCSAiZAZBn7NgYhOEvWCr3qR35FDBlsA5hpVj4808YGbZAYdAmLoFQvuZCQwgnP7whBnZBWoozDH5We3XvBfAZDZD"

pg <- getPage("103682003003732", token, n = 5000)
pg <- getPage("357274534427476", token, n = 5000)
pg <- getPage("104725456232742", token, n = 5000)
pg <- getPage("193944443965876", token, n = 5000)
pg <- getPage("125056230917537", token, n = 5000)
pg <- getPage("158219407534974", token, n = 5000)

# Visualize
glimpse(pg)

# Function to get Facebook reactions
getReactions <- function(post, token, verbose=TRUE, api=NULL){
  
  add_to_url <- paste0("?fields=reactions.type(LIKE).limit(0).summary(true).as(like),",
                       "reactions.type(LOVE).limit(0).summary(true).as(love),",
                       "reactions.type(HAHA).limit(0).summary(true).as(haha),",
                       "reactions.type(WOW).limit(0).summary(true).as(wow),",
                       "reactions.type(SAD).limit(0).summary(true).as(sad),",
                       "reactions.type(ANGRY).limit(0).summary(true).as(angry)")
  reactions <- data.frame()
  
  if (verbose==TRUE){ pb <- utils::txtProgressBar(min=0,max=length(post), style=3) }
  i = 0
  for (p in as.character(post)){
    url <- paste0('https://graph.facebook.com/', p, add_to_url)
    # making query
    content <- callAPI(url=url, token=token, api=api)
    # DF with results
    new.df <- data.frame(
      id = p,
      likes_count = ifelse(!is.null(content$like$summary$total_count),
                           content$like$summary$total_count, 0),
      love_count = ifelse(!is.null(content$love$summary$total_count),
                          content$love$summary$total_count, 0),
      haha_count = ifelse(!is.null(content$haha$summary$total_count),
                          content$haha$summary$total_count, 0),
      wow_count = ifelse(!is.null(content$wow$summary$total_count),
                         content$wow$summary$total_count, 0),
      sad_count = ifelse(!is.null(content$sad$summary$total_count),
                         content$sad$summary$total_count, 0),
      angry_count = ifelse(!is.null(content$angry$summary$total_count),
                           content$angry$summary$total_count, 0),
      stringsAsFactors=FALSE)
    reactions <- rbind(reactions, new.df)
    if (verbose==TRUE){ i <- i + 1; utils::setTxtProgressBar(pb, i) }
  }
  
  return(reactions)
  
}

# Create a vector of posts IDs
postsIDs <- data.matrix(pg$id)

reactions_postsAll <- getReactions(postsIDs, token, verbose = TRUE, api = "v2.11")

reactions_post1 <- getReactions("103682003003732_1578204332218151", token, verbose = TRUE, api = "v2.11")

sadV1 <- subset(reactions_postsAll, sad_count >=1)
angryV1 <- subset(reactions_postsAll, angry_count>=1)

innerSadAngry <- inner_join(sadV1, angryV1)

# Write reactions dataframes
write.csv(reactions_postsAll, "reactionsMK.csv")
write.csv(reactions_postsAll, "reactionsCoca.csv")
write.csv(reactions_postsAll, "reactionsCentauro.csv")
write.csv(reactions_postsAll, "reactionsCeA.csv")
write.csv(reactions_postsAll, "reactionsTim.csv")
write.csv(reactions_postsAll, "reactionsLoreal.csv")

reactionsMK <- read.csv("reactionsMK.csv", header = TRUE, sep = ",")
reactionsMK <- reactionsMK[ , -c(1)]

reactionsCoca <- read.csv("reactionsCoca.csv", header = TRUE, sep = ",")
reactionsCoca <- reactionsCoca[ , -c(1)]

reactionsCentauro <- read.csv("reactionsCentauro.csv", header = TRUE, sep = ",")
reactionsCentauro <- reactionsCentauro[ , -c(1)]

reactionsCeA <- read.csv("reactionsCeA.csv", header = TRUE, sep = ",")
reactionsCeA <- reactionsCeA[ , -c(1)]

reactionsTim <- read.csv("reactionsTim.csv", header = TRUE, sep = ",")
reactionsTim <- reactionsTim[ , -c(1)]

reactionsLoreal <- read.csv("reactionsLoreal.csv", header = TRUE, sep = ",")
reactionsLoreal <- reactionsLoreal[ , -c(1)]

# Put the datasets together
reactionsAll <- rbind(reactionsMK, reactionsCoca, reactionsCentauro, 
                      reactionsCeA, reactionsTim, reactionsLoreal)
write.csv(reactionsAll, "reactionsAll.csv")

reactionsAll <- read.csv("reactionsAll.csv", header = TRUE, sep = ",")
reactionsAll <- reactionsAll[ , -c(1)]

# probability Angry
# >= 1
angryDataframeOne <- subset(reactionsAll, angry_count >= 1)
Pa1 <- 2534/17677

## Scoring Methods
# Logistic Regression
# Boosted Decision Tree
# Averaged Perceptron
# Supported Vector Machine (SVM)

# Create dataset with 3 variables: Likes / Positive Reactions / Negative Reactions
dataSetReactions <- data.frame(reactionsAll$id, reactionsAll$likes_count,
                               paste(reactionsAll$love_count + reactionsAll$haha_count + reactionsAll$wow_count),
                               paste(reactionsAll$sad_count + reactionsAll$angry_count))

names(dataSetReactions) <- c("IDs", "Likes", "PositiveReactions", "NegativeReactions")

write.csv(dataSetReactions, "dataSetReactions.csv")

# Retrieve messages from posts
postsIDsMK <- data.frame(pg$message, pg$id)
postsIDsMK <- postsIDsMK[-c(1), ]
names(postsIDsMK) <- c("message", "id")
totalMK <- merge(reactionsMK, postsIDsMK, by="id")
totalMK <- totalMK[,c(1,8,2,3,4,5,6,7)]
write.csv(totalMK, "totalMK.csv")

postsIDsCoca <- data.frame(pg$message, pg$id)
postsIDsCoca <- postsIDsCoca[-c(1,2), ]
names(postsIDsCoca) <- c("message", "id")
totalCoca <- merge(reactionsCoca, postsIDsCoca, by="id")
totalCoca <- totalCoca[,c(1,8,2,3,4,5,6,7)]
write.csv(totalCoca, "totalCoca.csv")

PostsIDsCentauro <- data.frame(pg$message, pg$id)
names(PostsIDsCentauro) <- c("message", "id") 
totalCentauro <- merge(reactionsCentauro, PostsIDsCentauro, by="id")
totalCentauro <- totalCentauro[,c(1,8,2,3,4,5,6,7)]
write.csv(totalCentauro, "totalCentauro.csv")

# Scrap out outliers with few reactions (in our case with 0 NegativeReactions)
dataSetReactionsTwo <- subset(as.character(dataSetReactions, dataSetReactions$NegativeReactions >= 0))

# Scatterplots with fit lines
# Angry Vs. Sad
plot(innerSadAngry$angry_count, innerSadAngry$sad_count, main = "Dispersão",
     xlab = "Angry Count", ylab = "Sad Count", pch=20)
abline(lm(innerSadAngry$sad_count~innerSadAngry$angry_count), col="red") # regression line (y~x) 
lines(lowess(innerSadAngry$angry_count, innerSadAngry$sad_count), col="blue") # lowess line (x,y)

#  Sad Vs. Angry
plot(innerSadAngry$sad_count, innerSadAngry$angry_count, main = "Dispersão",
     xlab = "Sad Count", ylab = "Angry Count", pch=20)
abline(lm(innerSadAngry$angry_count~innerSadAngry$sad_count), col="red") # regression line (y~x) 
lines(lowess(innerSadAngry$sad_count, innerSadAngry$angry_count), col="blue") # lowess line (x,y)

# Posts with most angry reactions
innerSadAngryTwo <- subset(reactions_postsAll, angry_count>=10)

plot(innerSadAngryTwo$sad_count, innerSadAngryTwo$angry_count, main = "Dispersão",
     xlab = "Sad Count", ylab = "Angry Count", pch=20)
abline(lm(innerSadAngryTwo$angry_count~innerSadAngryTwo$sad_count), col="red") # regression line (y~x) 
lines(lowess(innerSadAngryTwo$sad_count, innerSadAngryTwo$angry_count), col="blue") # lowess line (x,y)

# x > 10 / 0.0033 (12/3646)
# sad / angry percentage
a1 <- subset(innerSadAngryTwo, id == "103682003003732_1758613910843858")
a1P <- sum(a1$sad_count, a1$angry_count)/sum(a1$likes_count, a1$love_count, a1$haha_count, a1$wow_count)

a2 <- subset(innerSadAngryTwo, id == "103682003003732_1751657491539500")
a2p <- sum(a2$sad_count, a2$angry_count)/sum(a2$likes_count, a2$love_count, a2$haha_count, a2$wow_count)

a3 <- subset(innerSadAngryTwo, id == "103682003003732_1742992505739332") 
a3p <- sum(a3$sad_count, a3$angry_count)/sum(a3$likes_count, a3$love_count, a3$haha_count, a3$wow_count)

a4 <- subset(innerSadAngryTwo, id == "103682003003732_1711266532245263")
a4p <- sum(a4$sad_count, a4$angry_count)/sum(a4$likes_count, a4$love_count, a4$haha_count, a4$wow_count)

a5 <- subset(innerSadAngryTwo, id == "103682003003732_1707129569325626")
a5p <- sum(a5$sad_count, a5$angry_count)/sum(a5$likes_count, a5$love_count, a5$haha_count, a5$wow_count)

a6 <- subset(innerSadAngryTwo, id == "103682003003732_1578204332218151")
a6p <- sum(a6$sad_count, a6$angry_count)/sum(a6$likes_count, a6$love_count, a6$haha_count, a6$wow_count)

a7 <- subset(innerSadAngryTwo, id == "103682003003732_1298161993555721")
a7p <- sum(a7$sad_count, a7$angry_count)/sum(a7$likes_count, a7$love_count, a7$haha_count, a7$wow_count)

a8 <- subset(innerSadAngryTwo, id == "103682003003732_1270544876317433")
a8p <- sum(a8$sad_count, a8$angry_count)/sum(a8$likes_count, a8$love_count, a8$haha_count, a8$wow_count)

a9 <- subset(innerSadAngryTwo, id == "103682003003732_1212159395489315")
a9p <- sum(a9$sad_count, a9$angry_count)/sum(a9$likes_count, a9$love_count, a9$haha_count, a9$wow_count)

a10 <- subset(innerSadAngryTwo, id == "103682003003732_1205447646160490")
a10p <- sum(a10$sad_count, a10$angry_count)/sum(a10$likes_count, a10$love_count, a10$haha_count, a10$wow_count)
 
a11 <- subset(innerSadAngryTwo, id == "103682003003732_1168694923169096")
a11p <- sum(a11$sad_count, a11$angry_count)/sum(a11$likes_count, a11$love_count, a11$haha_count, a11$wow_count)

a12 <- subset(innerSadAngryTwo, id == "103682003003732_1080832981955291")
a12p <- sum(a12$sad_count, a12$angry_count)/sum(a12$likes_count, a12$love_count, a12$haha_count, a12$wow_count)

dfAs <- data.frame(a1P, a2p, a3p, a4p, a5p, a6p, a7p, a8p, a9p, a10p, a11p, a12p)
write.csv(dfAs, "dfAs.csv")

m1 <- t(dfAs)
d2 <- data.frame(m1)

# mean = 0.03419386
mean(m1)
 

# Enhanced Scatterplot of Sad Vs. Angry by number of Likes 
s <- innerSadAngry$sad_count
a <- innerSadAngry$angry_count
l <- innerSadAngry$likes_count

scatterplot(sad_count ~ angry_count | id, data = innerSadAngry,
            xlab = "Sad Count", ylab = "Angry Count",
            main="Enhanced Scatter Plot",
            labels = row.names(innerSadAngry))

library(car) 
scatterplot(mpg ~ wt | cyl, data=mtcars, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(mtcars))



inner_join(df1, df2)
dfSessionA_4 <- dfSessionA_4[-c(6,7,8,9), ]
subset(MyDate, Ydat > 40)
