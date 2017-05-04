#---------------------------II.04 feladat---------------------------
#a függvény

get_tweets <- function(who = "", num = 10)
{
  if (who != "HillaryClinton" & who != "realDonaldTrump") return ("Wrong name")
  tweets2 <- tweets[tweets$handle == who,]
  x <- sort(tweets2$retweet_count + tweets2$favorite_count, decreasing = T)[1:num]
  return(tweets2$text[(tweets2$retweet_count + tweets2$favorite_count) %in% x])
}

