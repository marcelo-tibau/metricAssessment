# setwd("~/Documents/FLAGCX/Projects/metricAssessment/metricAssessment")

# Capture Facebook posts
library(Rfacebook)
library(tidyverse) 
library(dplyr)
library(car)
library(ggplot2)
library(corrplot)

token <- "EAACEdEose0cBAD2B7BPlCNxm04pQAs8eFSSzXd51qdNZCcDjkgtW1xdxjzo3NPZCX8T1V3e7ZChsN8ZAiotR4Vtb0ZCN6zOh78CIwJFLhISLisC7JjzEDz9PnZAdOzuCx8IuKxqoxZBkFOKNtNaUPrZBV0TaOlLHmtb0paSG5uPTDxADySX0nK13tA9hN1XmocN7k9AEEZCIEbQZDZD"

# Grab Facebook posts

pg1 <- getPage("103682003003732", token, n = 5000)
pg2 <- getPage("357274534427476", token, n = 5000)
pg3 <- getPage("104725456232742", token, n = 5000)
pg4 <- getPage("193944443965876", token, n = 5000)
pg5 <- getPage("125056230917537", token, n = 5000)
pg6 <- getPage("158219407534974", token, n = 5000)

# Create a vector of posts IDs
postsIDs <- data.matrix(pg1$id)
postsIDs <- data.matrix(pg2$id)
postsIDs <- data.matrix(pg3$id)
postsIDs <- data.matrix(pg4$id)
postsIDs <- data.matrix(pg5$id)
postsIDs <- data.matrix(pg6$id)

# Get reactions
reactionsMK <- getReactions(postsIDs, token, verbose = TRUE, api = "v3.0")

reactionsCoca <- getReactions(postsIDs, token, verbose = TRUE, api = "v3.0")

reactionsCentauro <- getReactions(postsIDs, token, verbose = TRUE, api = "v3.0")

reactionsCeA <- getReactions(postsIDs, token, verbose = TRUE, api = "v3.0")

reactionsTim <- getReactions(postsIDs, token, verbose = TRUE, api = "v3.0")

reactionsLoreal <- getReactions(postsIDs, token, verbose = TRUE, api = "v3.0")

# Retrieve messages from posts
postsIDsMK <- data.frame(pg1$message, pg1$id)
postsIDsMK <- postsIDsMK[-c(1), ]
names(postsIDsMK) <- c("message", "id")

postsIDsCoca <- data.frame(pg2$message, pg2$id)
names(postsIDsCoca) <- c("message", "id")

postsIDsCentauro <- data.frame(pg3$message, pg3$id)
names(postsIDsCentauro) <- c("message", "id")

postsIDsCeA <- data.frame(pg4$message, pg4$id)
names(postsIDsCeA) <- c("message", "id")

postsIDsTim <- data.frame(pg5$message, pg5$id)
names(postsIDsTim) <- c("message", "id")

postsIDsLoreal <- data.frame(pg6$message, pg6$id)
names(postsIDsLoreal) <- c("message", "id")

# Merge reactions and messages
totalMK <- merge(reactionsMK, postsIDsMK, by="id")
totalMK <- totalMK[,c(1,8,2,3,4,5,6,7)]
write.csv(totalMK, "totalMK.csv")

totalCoca <- merge(reactionsCoca, postsIDsCoca, by="id")
totalCoca <- totalCoca[,c(1,8,2,3,4,5,6,7)]
write.csv(totalCoca, "totalCoca.csv")

totalCentauro <- merge(reactionsCentauro, postsIDsCentauro, by="id")
totalCentauro <- totalCentauro[,c(1,8,2,3,4,5,6,7)]
write.csv(totalCentauro, "totalCentauro.csv")

totalCeA <- merge(reactionsCeA, postsIDsCeA, by="id")
totalCeA <- totalCeA[,c(1,8,2,3,4,5,6,7)]
write.csv(totalCeA, "totalCeA.csv")

totalTim <- merge(reactionsTim, postsIDsTim, by="id")
totalTim <- totalTim[,c(1,8,2,3,4,5,6,7)]
write.csv(totalTim, "totalTim.csv")

totalLoreal <- merge(reactionsLoreal, postsIDsLoreal, by="id")
totalLoreal <- totalLoreal[,c(1,8,2,3,4,5,6,7)]
write.csv(totalLoreal, "totalLoreal.csv")

# Log transformation to normally distributed
#MK - df$Assay <- gsub('-', '-0', df$Assay)
totalMK <- read.csv("totalMK.csv", header = TRUE, sep = ",")
totalMK <- totalMK[ , -c(1)]

totalMK$transf_Likes <- log(totalMK$likes_count)
#totalMK$transf_Likes <- gsub('-Inf', '0', totalMK$transf_Likes)
totalMK$transf_Love <- log(totalMK$love_count)
totalMK$transf_Love <- gsub('-Inf', '0', totalMK$transf_Love)
totalMK$transf_Love <- gsub('0.0000000', '0', totalMK$transf_Love)
totalMK$transf_Haha <- log(totalMK$haha_count)
totalMK$transf_Haha <- gsub('-Inf', '0', totalMK$transf_Haha)
totalMK$transf_Wow <- log(totalMK$wow_count)
totalMK$transf_Wow <- gsub('-Inf', '0', totalMK$transf_Wow)
totalMK$transf_Sad <- log(totalMK$sad_count)
totalMK$transf_Sad <- gsub('-Inf', '0', totalMK$transf_Sad)
totalMK$transf_Angry <- log(totalMK$angry_count)
totalMK$transf_Angry <- gsub('-Inf', '0', totalMK$transf_Angry)

write.csv(totalMK, "totalMKLog.csv")

totalMK <- read.csv("totalMKLog.csv", header = TRUE, sep = ",")
totalMK <- totalMK[ , -c(1)]

totalCoca <- read.csv("totalCoca.csv", header = TRUE, sep = ",")
totalCoca <- totalCoca[ , -c(1)]

totalCoca$transf_Like <- log(totalCoca$likes_count)
totalCoca$transf_Love <- log(totalCoca$love_count)
totalCoca$transf_Love <- gsub('-Inf', '0', totalCoca$transf_Love)
totalCoca$transf_Haha <- log(totalCoca$haha_count)
totalCoca$transf_Haha <- gsub('-Inf', '0', totalCoca$transf_Haha)
totalCoca$transf_Wow <- log(totalCoca$wow_count)
totalCoca$transf_Wow <- gsub('-Inf', '0', totalCoca$transf_Wow)
totalCoca$transf_Sad <- log(totalCoca$sad_count)
totalCoca$transf_Sad <- gsub('-Inf', '0', totalCoca$transf_Sad)
totalCoca$transf_Angry <- log(totalCoca$angry_count)
totalCoca$transf_Angry <- gsub('-Inf', '0', totalCoca$transf_Angry)

write.csv(totalCoca, "totalCocaLog.csv")
totalCocaLog <- read.csv("totalCocaLog.csv", header = TRUE, sep = ",")
totalCocaLog <- totalCocaLog[ , -c(1)]

# Histograms plots (reactons (x) Vs. count (y))
# visual representation of the distribution of a dataset
# MK
#hist(totalMK$likes_count, border = "darkgreen",
 #    col = "darkgreen", las=1, prob = TRUE)

hist(totalMK$transf_Likes, border = "darkgreen",
     col = "darkgreen", las=1)
# Like
ggplot(data = totalMK, aes(totalMK$transf_Likes)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
                 labs(title="Histograma Likes Mary Kay") +
                   labs(x="Likes (em log)", y="Frequência")

ggplot(data = totalCocaLog, aes(totalCocaLog$transf_Like)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Likes Coca-Cola") +
  labs(x="Likes (em log)", y="Frequência")

# Love
ggplot(data = corrMK2, aes(love_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Love Mary Kay") +
  labs(x="Love (em log)", y="Frequência")

ggplot(data = corrCoca2, aes(love_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Love Coca-Cola") +
  labs(x="Love (em log)", y="Frequência")

# Haha
ggplot(data = corrM3, aes(haha_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Haha Mary Kay") +
  labs(x="Haha (em log)", y="Frequência")

ggplot(data = corrCoca3, aes(haha_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Haha Coca-Cola") +
  labs(x="Haha (em log)", y="Frequência")

# Wow
ggplot(data = corrMK4, aes(wow_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Wow Mary Kay") +
  labs(x="Wow (em log)", y="Frequência")

ggplot(data = corrCoca4, aes(wow_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Wow Coca-Cola") +
  labs(x="Wow (em log)", y="Frequência")

# Sad
ggplot(data = corrMK5, aes(sad_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Sad Mary Kay") +
  labs(x="Sad (em log)", y="Frequência")

ggplot(data = corrCoca5, aes(sad_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Sad Coca-Cola") +
  labs(x="Sad (em log)", y="Frequência")

# Angry
ggplot(data = corrMK6, aes(angry_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Angry Mary Kay") +
  labs(x="Angry (em log)", y="Frequência")

ggplot(data = corrCoca6, aes(angry_log)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
  labs(title="Histograma Angry Coca-Cola") +
  labs(x="Angry (em log)", y="Frequência")

# Correlations
# corr_mat <- cor(mat,method="s")

# Like
corrMK1 <- data.frame(totalMK$likes_count, totalMK$transf_Likes)
names(corrMK1) <- c("likes_count", "likes_log")

qplot(likes_log, likes_count, data = corrMK1) + scale_y_continuous(limits = c(50000, 150000))

corrCoca1 <- data.frame(totalCocaLog$likes_count, totalCocaLog$transf_Like)
names(corrCoca1) <- c("likes_count", "likes_log")

qplot(likes_log, likes_count, data = corrCoca1) + scale_y_continuous(limits = c(500000, 1500000))


# Love
corrMK2 <- data.frame(as.numeric(totalMK$love_count), as.numeric(totalMK$transf_Love))
names(corrMK2) <- c("love_count", "love_log")

qplot(love_log, love_count, data = corrMK2) 

corrCoca2 <- data.frame(as.numeric(totalCocaLog$love_count), as.numeric(totalCocaLog$transf_Love))
names(corrCoca2) <- c("love_count", "love_log")

qplot(love_log, love_count, data = corrCoca2) + scale_y_continuous(limits = c(0, 150000))

# Haha
corrM3 <- data.frame(as.numeric(totalMK$haha_count), as.numeric(totalMK$transf_Haha))
names(corrM3) <- c("haha_count", "haha_log")

qplot(haha_log, haha_count, data = corrM3)

corrCoca3 <- data.frame(as.numeric(totalCocaLog$haha_count), as.numeric(totalCocaLog$transf_Haha))
names(corrCoca3) <- c("haha_count", "haha_log")

qplot(haha_log, haha_count, data = corrCoca3)

# Wow
corrMK4 <- data.frame(as.numeric(totalMK$wow_count), as.numeric(totalMK$transf_Wow))
names(corrMK4) <- c("wow_count", "wow_log")

qplot(wow_log, wow_count, data = corrMK4)

corrCoca4 <- data.frame(as.numeric(totalCocaLog$wow_count), as.numeric(totalCocaLog$transf_Wow))
names(corrCoca4) <- c("wow_count", "wow_log")

qplot(wow_log, wow_count, data = corrCoca4)

# Sad
corrMK5 <- data.frame(as.numeric(totalMK$sad_count), as.numeric(totalMK$transf_Sad))
names(corrMK5) <- c("sad_count", "sad_log")

qplot(sad_log, sad_count, data = corrMK5)

corrCoca5 <- data.frame(as.numeric(totalCocaLog$sad_count), as.numeric(totalCocaLog$transf_Sad))
names(corrCoca5) <- c("sad_count", "sad_log")

qplot(sad_log, sad_count, data = corrCoca5)

# Angry
corrMK6 <- data.frame(as.numeric(totalMK$angry_count), as.numeric(totalMK$transf_Angry))
names(corrMK6) <- c("angry_count", "angry_log")

qplot(angry_log, angry_count, data = corrMK6)

corrCoca6 <- data.frame(as.numeric(totalCocaLog$angry_count), as.numeric(totalCocaLog$transf_Angry))
names(corrCoca6) <- c("angry_count", "angry_log")

qplot(angry_log, angry_count, data = corrCoca6) + scale_y_continuous(limits = c(7500, 20000))

# Coca
ggplot(data = totalCoca, aes(totalCoca$likes_count)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey",
                 labs(title="Histograma Likes Mary Kay") +
                   labs(x="Likes (em log)", y="Frequência"))

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="Age", y="Count") + 
  xlim(c(18,52)) + 
  ylim(c(0,30))


qplot(totalCoca$likes_count,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for Likes", 
      xlab = "Age",  
      fill=I("blue"), 
      col=I("blue"), 
      alpha=I(.2))

#hist(AirPassengers, 
 #    main="Histogram for Air Passengers", 
  #   xlab="Passengers", 
  #   border="blue", 
   #  col="green", 
  #   xlim=c(100,700), 
   #  las=1, 
    # breaks=5, 
    # prob = TRUE)
