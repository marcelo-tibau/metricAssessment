# setwd("~/Documents/FLAGCX/Projects/metricAssessment/metricAssessment")

# Capture Facebook posts
library(Rfacebook)
library(tidyverse) 
library(dplyr)
library(car)
library(ggplot2)

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
totalMK$transf_Likes <- log(totalMK$likes_count)
#totalMK$transf_Likes <- gsub('-Inf', '0', totalMK$transf_Likes)
totalMK$transf_Love <- log(totalMK$love_count)
totalMK$transf_Love <- gsub('-Inf', '0.0000000', totalMK$transf_Love)
totalMK$transf_Love <- gsub('0.0000000', '0', totalMK$transf_Love)
totalMK$transf_Haha <- log(totalMK$haha_count)
totalMK$transf_Haha <- gsub('-Inf', '0', totalMK$transf_Haha)
totalMK$transf_Wow <- log(totalMK$wow_count)
totalMK$transf_Wow <- gsub('-Inf', '0', totalMK$transf_Wow)
totalMK$transf_Sad <- log(totalMK$sad_count)
totalMK$transf_Sad <- gsub('-Inf', '0', totalMK$transf_Sad)
totalMK$transf_Angry <- log(totalMK$angry_count)
totalMK$transf_Angry <- gsub('-Inf', '0', totalMK$transf_Angry)

# Histograms plots (reactons (x) Vs. count (y))
# visual representation of the distribution of a dataset
# MK
#hist(totalMK$likes_count, border = "darkgreen",
 #    col = "darkgreen", las=1, prob = TRUE)

hist(totalMK$transf_Likes, border = "darkgreen",
     col = "darkgreen", las=1)

ggplot(data = totalMK, aes(totalMK$transf_Likes)) +
  geom_histogram(binwidth = 0.5, col="white", fill="darkgrey") +
                 labs(title="Histograma Likes Mary Kay") +
                   labs(x="Likes (em log)", y="Frequência")

# Correlations
library(corrplot)



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
