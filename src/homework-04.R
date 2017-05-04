#########################
# Házi feladat 4        #
# Programozás I.        #
# 2016/17. II. félév    #
# Mogyorósi Pálma       #
# 2017.05.01.           #
#########################

#---------------------------II.01 feladat---------------------------
# A data mappában találsz egy clinton_trump_tweets.csv nevű fájlt. 
# Ezt hívd be tweets néven. Az eredeti adatfájl innen származik, és 
# Hillary Clinton, valamint Donald Trump 6444 tweetjét tartalmazza 2016. 
# januártól szeptemberig. Ez az adatsor bővítve lett pár új oszloppal. 
# Az új oszlopok a tweetek szövegeinek különbözőképp feldolgozott verzióit 
# tartalmazza, valamint a szövegek legjellemzőbb szentimentjét és emócióját.

tweets <- read.csv2("data/clinton_trump_tweets.csv", stringsAsFactors = F)

View(tweets)


#---------------------------II.02 feladat---------------------------
# Kérd le, hány tweet származik Hillary Clintontól és hány Donald Trumptól! 
# Ezt ábrázold is a fig/sample/tweet1.png alapján, majd mentsd ki a plotot 
# a fig mappába tweet1.png néven!

library(reshape2)

library(ggplot2)

ggplot(tweets, aes(x = handle, fill = handle)) + 
  geom_bar() +
  ggtitle("Candidate Tweets") +
  xlab("") + ylab("Tweet frequency") +
  scale_fill_manual(values = c("blue", "red"),
                    labels = c("Hillary Clinton", "Donald Trump"),
                    name = "Candidate") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("fig/tweet1.png")

#---------------------------II.03 feladat---------------------------
# Kérd le, milyen nyelveken írta a tweeteket a két jelölt külön-külön! 
# Nézd meg, hogy a nem angol szövegek valóban nem angolok-e. Ha úgy gondolod, 
# hogy rosszul azonosították a nyelvet, akkor írd át az általad helyesnek váltre. 
# A spanyol nyelvű tweeteket nem kell végigböngészned, mert sok van, de a 
# többit mindenképp nézd meg. Ábrázold a két jelölt által használt nyelvek 
# gyakoriságát a fig/sample/tweet2.png szerint, majd mentsd ki a plotot a fig 
# mappába tweet2.png néven! Az oszlopszínek legyenek "darkgrey" és "cornflowerblue". 
# (listA nyelvek gyakorisága eltérhet az előző lépés miatt.)

tweets$lang <- as.factor(tweets$lang)

# megnézem külön, hogy Clinton és Trump milyen nyelveket használt

lang_clinton <- subset.data.frame(tweets, tweets$handle == "HillaryClinton", 
                                  select = c(handle, text, lang))

lang_trump <- subset.data.frame(tweets, tweets$handle == "realDonaldTrump", 
                                select = c(handle, text, lang))

# megnézem, milyen nyelveken tweeteltek

sort(summary(lang_clinton$lang), decreasing = T)

sort(summary(lang_trump$lang), decreasing = T)

# leellenőrzöm a nyelveket és megnézem a sorszámukat

text_fr <- tweets$text[which(tweets$lang == "fr")]
text_fr

text_da <- tweets$text[which(tweets$lang == "da")]
text_da

text_tl <- tweets$text[which(tweets$lang == "tl")]
text_tl

text_et <- tweets$text[which(tweets$lang == "et")]
text_et

text_fi <- tweets$text[which(tweets$lang == "fi")]
text_fi

# mind angol, megnézem a sorszámukat, átírom angolra

which(tweets$lang == "fr")
which(tweets$lang == "da")
which(tweets$lang == "tl")
which(tweets$lang == "et")
which(tweets$lang == "fi")

tweets[237,11] <- "en"
tweets[6206,11] <- "en"
tweets[6059,11] <- "en"
tweets[6064,11] <- "en"
tweets[6087,11] <- "en"
tweets[5378,11] <- "en"
tweets[5439,11] <- "en"
tweets[5296,11] <- "en"
tweets[5448,11] <- "en"

# ggplot

ggplot(tweets, aes(x = handle, fill = lang)) + 
  geom_bar(position="dodge") +
  ggtitle("Language of Tweets") +
  xlab("") + ylab("Tweet frequency") +
  scale_fill_manual(values = c("darkgray", "cornflowerblue"),
                    labels = c("English", "Spain"),
                    name = "Language") +
  scale_x_discrete(labels = c("Hillary Clinton", "Donald Trump")) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.ticks.x = element_blank())

# kimentés

ggsave("fig/tweet2.png")



#---------------------------II.04 feladat---------------------------
# Írj egy függvényt, ami a "Hillary Clinton" bemenet esetében Hillary Clinton, 
# "Donald Trump" bemenet esetében pedig Donald Trump tweetjeit adja ki a 
# retweet_count és a favorite_count összesített értéke alapján csökkenő sorrendben. 
# Legyen egy olyan argumentum is, amivel a kiadott tweetek számát lehet meghatározni.
# Az argumentumoknak legyen default értéke. A függvény írjon ki hibaüzenetet, 
# ha rossz nevet kap. Ha a kiadott tweetek száma meghaladja a lehetséges tweetek 
# számát, írja ki a tweetek max számát. Mikor a függvény kiadja a tweeteket, 
# írja ki, hogy kinek a tweetjeit írja ki és mennyit. Kérd le a függvénnyel 
# Hillary Clinton első 10, valamint Donald Trump első 15 legtöbbet retweetelt 
# és kedvelt tweetjét.

# a retweet és a favorite összegének csökkenő sorrendje, az első 10 elem kiíratása

sort(tweets$retweet_count + tweets$favorite_count, decreasing = T)[1:10]

# ugyenez Clintonra

tweets2 <- tweets[tweets$handle == "HillaryClinton",]
x <- sort(tweets2$retweet_count + tweets2$favorite_count, decreasing = T)[1:10]
tweets2$text[(tweets2$retweet_count + tweets2$favorite_count) %in% x]

# a függvények

get_tweets("HillaryClinton")
get_tweets("realDonaldTrump", 15)


#---------------------------III.01 feladat---------------------------
# A fivethirtyeight package-ben van egy hiphop_cand_lyrics dataset. 
# A fivethirtyeight-en ezt az elemzést készítették az adatokhoz.

library("fivethirtyeight")

data(hiphop_cand_lyrics)
hiphop_cand_lyrics

# Készítsd el ggplot2-vel az itt található első 2 ábrát úgy, hogy minél jobban 
# megegyezzen a plotok kinézete. Természetesen nem kell mindennek ugyanolyannak 
# lennie (pl. az oszlopoknak nem kell pontokból állnia), de törekedj a minél 
# nagyobb fokú egyezőségre. Mentsd ki az ábrákat a fig mappába hiphop1.png és 
# hiphop2.png néven!

# a szükséges oszlopok kiszedése

hiphopcand <- table(hiphop_cand_lyrics$candidate, 
                    hiphop_cand_lyrics$album_release_date)

# dataframe-mé alakítom

hiphopcand <- as.data.frame(hiphopcand)

# ggplot - nem jó az order

# más próbálkozások
# 1. hiphopcand <- hiphopcand[order(-hiphopcand$Freq),] 
# 2. positions <- c("TRUMP", "CLINTON", "BUSH", "CHRISTIE", "HUCKABEE", "SANDERS", "CARSON", "CRUZ")
# 3. install.packages("dplyr")
# require("dplyr")
# library("dplyr")


ggplot(hiphopcand, aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_dotplot(binaxis = "y", stackgroups = TRUE, dotsize = 0.5,
               binwidth = 3, binpositions = "all") +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs") +
  xlab("") + ylab("") +
  scale_fill_manual(values = c("#fec075", "#94d7fa", "#ff8974", "#ffd92f", "#fccde5",
                               "#66c2a5", "#a6d854", "#e78ac3"),
                    labels = c("TRUMP", "CLINTON", "BUSH", "CHRISTIE",
                               "HUCKABEE", "SANDERS", "CARSON", "CRUZ")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(),
        legend.position = "top") +
  scale_x_discrete(labels = c("1990" = "1990", "1995" = "'95","2000" = "2000",
                              "2005" = "'05","2010" = "'10","2015" = "'15"),
                   breaks = c(1990, 1995, 2000, 2005, 2010, 2015))

ggsave("fig/hiphop1.png")
