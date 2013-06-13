setwd("~/Desktop/Analise de Dados I")
data = read.csv("Lab 2/tweets_by_hour.csv")

timestamp = data$timestamp
num_tweets = data$tweets

#length(num_tweets[num_tweets == 0])

timestampFormated = strptime(timestamp, "%Y%m%d%H")

plot(x=timestampFormated, y=num_tweets, xlab="Times", ylab="#Tweets", main="Times x #Tweets")

# FDP num_tweets
mu = mean(num_tweets)
stdev = sd(num_tweets)
png(filename="Lab 2/FDP de NumTweets.png")
#plot(pnorm(num_tweets, mu, var), type="l", col="red"); 
plot(norm(num_tweets, mean=mu, sd=stdev))
hist(num_tweets, probability=TRUE, col=gray(.9), main="Hist Num Tweets") 
dev.off()

# standard deviation for FDP
sd(num_tweets)

# FDA num_tweets
FDA_num_tweets = ecdf(num_tweets)
png(filename="Lab 2/FDA de NumTweets.png")
plot(FDA_num_tweets, xlab="tweets[i]", ylab="FDA(tweets[i])", main="FDA de Num Tweets")
dev.off()

summary(FDA_num_tweets)

median(num_tweets)
mode(num_tweets)
mean(num_tweets)
quantile(num_tweets, 0.25)
quantile(num_tweets, 0.5)
quantile(num_tweets, 0.75)
quantile(num_tweets, 1)

# questao 2 -----------------------------------------------------------------------------

timestampFormated[1]$mon

numTweetsPerHour <- aggregate(num_tweets, list("Hora" = timestampFormated$hour), FUN=mean)

numTweetsPerHour

png("Lab 2/Quantidade media de tweets por hora")
plot(numTweetsPerHour, ylab="Quantidade Media de Tweets", type="o", main="Media de Tweets por Hora do Dia")
dev.off()

# FDA numTweetsPerHour
FDA_numTweetsPerHour = ecdf(numTweetsPerHour$x)
png(filename="Lab 2/FDA de NumTweetsPerHour.png")
plot(FDA_numTweetsPerHour, xlab="tweets[i]", ylab="FDA(tweets[i]) per Hour", main="FDA de Num Tweets per Hour")
dev.off()

png("Lab 2/Hist of numTweetsPerHour$x")
hist(numTweetsPerHour$x)
dev.off()

summary(FDA_numTweetsPerHour)

median(numTweetsPerHour$x)
mode(numTweetsPerHour$x)
mean(numTweetsPerHour$x)
quantile(numTweetsPerHour$x, 0.25)
quantile(numTweetsPerHour$x, 0.5)
quantile(numTweetsPerHour$x, 0.75)
quantile(numTweetsPerHour$x, 1)

sd(numTweetsPerHour$x)

# questao 3 ------------------------------------------------------------------------------

norm <- rnorm(31793)

# histogram norm
png("Lab 2/Hist_norm")
hist(norm)
dev.off()

# FDA norm
FDA_norm = ecdf(norm)
png("Lab 2/FDA_norm")
plot(FDA_norm)
dev.off()