# load packages needed
library(dplyr)
library(GGally)
library(stats)
library(ggplot2)

# read data
brexit <- read.csv("master-copy.csv")

# see if any missing values
anyNA(brexit)

# have a look
glimpse(brexit)

# scale remain precentage
brexit$Remain <- (brexit$Remain - mean(brexit$Remain))/max(brexit$Remain)

# filter out numerical data
brexit_numeric <- brexit %>% 
  select_if(is.numeric)

# Plot correlation coefficients
ggcorr(brexit_numeric, label = TRUE)

# Give only the correlation coefficients between Remain and others
cor(brexit_numeric)[ , 1]

# perform Principal Components Analysis
pca <- princomp(brexit_numeric, scores = T, cor = T)
summary(pca, loadings = T)

# Kaiser's criterion
eigen(cor(brexit_numeric))$values # first two

# screeplot
screeplot(pca, type = "l", main = "Scree Plot of PCA Analysis")

# Scores and loadings
pscores <- pca$scores
pload <- pca$loadings

# find the scores
scores1 <- pca$scores[ , 1]
scores2 <- pca$scores[ , 2]
scores3 <- pca$scores[ , 3]
scores4 <- pca$scores[ , 4]

# make a new dataframe contain scores1, scores2, parties and remain precentage
attach(brexit)
pcadata <- data.frame(scores1, scores2, scores3, scores4, Party, Remain)
detach(brexit)
glimpse(pcadata)

# plot first two PC and corlor by parites
ggplot(pcadata,aes(x = scores1, y = scores2, colour = Party)) + 
  geom_point() +
  ggtitle("Color by Parties") +
  xlab("PC1") +
  ylab("PC2")

# color by remain precentage
# assign new values to remain variable
for (i in 1:nrow(pcadata)){
  if(pcadata[i, 4] <= 0 ){
    pcadata[i, 4] <- "low"
  }else{
    pcadata[i, 4] <- "high"
  }
}

pcadata$Remain <- as.factor(pcadata$Remain)

ggplot(pcadata,aes(x = scores1, y = scores2, colour = Remain)) + 
  geom_point() +
  ggtitle("Color by Remain Precentage") +
  xlab("PC1") +
  ylab("PC2")


# did same thing to PC3 and PC4
ggplot(pcadata,aes(x = scores3, y = scores4, colour = Party)) + 
  geom_point() +
  ggtitle("Color by Parties") +
  xlab("PC3") +
  ylab("PC4")

ggplot(pcadata,aes(x = scores3, y = scores4, colour = Remain)) + 
  geom_point() +
  ggtitle("Color by Remain Precentage") +
  xlab("PC3") +
  ylab("PC4")



