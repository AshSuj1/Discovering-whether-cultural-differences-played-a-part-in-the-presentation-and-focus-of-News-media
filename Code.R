library(tidyverse)
library(dplyr)
library(tokenizers)
library(tidytext)
library(magrittr)
library(splitstackshape)
library(ggplot2)





# Read data nto R
Abc_headlines <- read.csv("abcnews-date-text.csv")

# convert list into a dataframe
Abcheadlines <- data.frame(Abc_headlines)
head(Abcheadlines1, 25)
Abcheadlines <- transform(Abcheadlines, publish_date = as.Date(as.character(publish_date), "%Y%m%d"))
Abcheadlines$publish_date <- format(Abcheadlines$publish_date, "%Y")

#word_count <-cSplit(Abcheadlines, "headline_text", sep = " ", direction = "long") %>%
  #group_by(tolower(headline_text)) %>%
  #summarise(Count = n())

word_count <-cSplit(Abcheadlines, "headline_text", sep = " ", direction = "long") %>%
  group_by(tolower(headline_text), publish_date) %>%
  summarise(Count = n())

word_count <- data.frame(word_count)
colnames(word_count)[1] <- "word"

word_count$word <- removeWords(word_count$word, stopwords("english"))

nrc_fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

nrc_anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

?inner_join
fear <- word_count %>%
  inner_join(nrc_fear)

anger <- word_count %>%
  inner_join(nrc_anger)

joy <- word_count %>%
  inner_join(nrc_joy)


sentiment_analysis <- word_count %>%
  inner_join(get_sentiments("nrc"))

ggplot(subset(sentiment_analysis, sentiment %in% "anger")) + geom_bar(aes(publish_date, Count),stat = "identity", fill = "red")+ theme_bw() + theme( strip.background  = element_blank(),
                                                                                                                                                  panel.grid.major = element_line(colour = "grey80"),
                                                                                                                                                  panel.border = element_blank(),
                                                                                                                                                  axis.ticks = element_blank(),
                                                                                                                                                  panel.grid.minor.x=element_blank(),
                                                                                                                                                  panel.grid.major.x=element_blank() ) +theme(legend.position="bottom") #+  scale_fill_manual(values = farb)

ggplot(subset(sentiment_analysis, sentiment %in% "fear")) + geom_bar(aes(publish_date, Count),stat = "identity", fill = "green")+ theme_bw() + theme( strip.background  = element_blank(),
                                                                                                                                                      panel.grid.major = element_line(colour = "grey80"),
                                                                                                                                                      panel.border = element_blank(),
                                                                                                                                                      axis.ticks = element_blank(),
                                                                                                                                                      panel.grid.minor.x=element_blank(),
                                                                                                                                                      panel.grid.major.x=element_blank() ) +theme(legend.position="bottom") #+  scale_fill_manual(values = farb)


farb <- c("#428953", "#CE2929", "#A3DD57", "#77E599", "#5675D6", "#65ECEF", "#FF8B07", "#D0B100", "#636363")

ggplot(sentiment_analysis) + geom_bar(aes(publish_date, Count, fill= sentiment),stat = "identity") + facet_wrap(~ sentiment, nrow = 5)+ theme_bw() + theme( strip.background  = element_blank(),
                                                                                                                                                         panel.grid.major = element_line(colour = "grey80"),
                                                                                                                                                         panel.border = element_blank(),
                                                                                                                                                         axis.ticks = element_blank(),
                                                                                                                                                         panel.grid.minor.x=element_blank(),
                                                                                                                                                         panel.grid.major.x=element_blank() ) +theme(legend.position="bottom") #+  scale_fill_manual(values = farb)


ggplot(fear, aes(publish_date, word)) +
  geom_col(show.legend = FALSE)

fear_1 <- word_count %>%
  inner_join(nrc_joy) %>%
  count(Count, sentiment) %>%
  spread(sentiment, n, fill = 0)
fear_1 <- inner_join(fear_1, word_count)

word_count %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

countries <- c("afghanistan", "Albania", "Algeria", "andorra",  "angola", "antigua", "barbuda", "argentina", "armenia",  "aruba", "australia", "austria", "azerbaijan", "bahamas", "bahrain", "bangladesh", "barbados", "belarus", "belgium", "belize", "benin", "bermuda", "bhutan", "bolivia", "bosnia", "herzegovina", "botswana", "brazil", "brunei",
               "bulgaria", "burkina Faso", "burundi", "cambodia", "cameroon", "canada", "cape Verde",  "central African Republic", "chad", 
                "chile",  "china", "taiwan", "colombia", "comoros", "congo", "costa Rica", "côte d'Ivoire", "croatia", "cuba", "cyprus", "czech Republic",
               "denmark", "djibouti", "timor", "ecuador", "egypt", "salvador", "guinea", "eritrea", "estonia", "ethiopia", "fiji", "finland", "france",
               "gabon", "gambia", "georgia", "germany", "ghana", "greece", "grenada", "guatemala", "guinea", "guyana", "haiti", "honduras", "hungary",
               "iceland", "india", "indonesia", "iran", "iraq", "ireland", "israel", "italy", "jamaica", "japan", "jordan", "kazakhstan", "kenya", "kiribati", "kuwait", "kyrgyzstan",
                "laos", "latvia", "lebanon", "lesotho", "liberia", "libya", "liechtenstein", "lithuania", "luxembourg", "macedonia", "madagascar", "malawi", "malaysia", "maldives", "mali", "malta", "mauritania", "mauritius", "mexico", "micronesia",
                 "moldova", "monaco", "mongolia", "montenegro", "morocco", "mozambique", "myanmar", "namibia", "nauru", "nepal", "netherlands", "zealand", "nicaragua", "niger", "nigeria", "niue", "Korea", "norway",
                "oman", "pakistan", "palau", "panama", "paraguay", "peru", "philippines", "poland", "portugal", "qatar", "romania", "russia", "rwanda", "samoa",
                  "saudi", "senegal", "serbia", "seychelles", "sierra", "singapore", "slovakia", "slovenia", "somalia", "africa", "sudan", "spain", "lanka", "suriname", "swaziland", "sweden", "switzerland", "syria",
               "tajikistan", "tanzania", "thailand", "togo", "tonga", "trinidad", "tobago", "tunisia", "turkey", "turkmenistan", "tuvalu", "uganda", "uk", "Ukraine", "uae", "britain", "scotland", "america", "us", "uruguay", "uzbekistan",
               "vanuatu", "venezuela", "vietnam", "yemen", "zambia" ,"zimbabwe")

countries_a <- c("afghanistan", "albania", "algeria", "andorra",  "angola", "antigua", "barbuda", "argentina", "armenia",  "aruba", "australia", "america", "austria", "azerbaijan")
countries_b <- c("bahamas", "bahrain", "bangladesh", "barbados", "belarus", "belgium", "belize", "benin", "bermuda", "bhutan", "bolivia", "bosnia", "herzegovina", "botswana", "brazil", "brunei",
"bulgaria", "burkina Faso", "burundi") 
countries_c <- c("cambodia", "cameroon", "canada",  "chad", "chile", "taiwan", "colombia", "comoros", "congo", "croatia", "cuba", "cyprus", "czech")
countries_def <- c("denmark", "djibouti", "timor", "ecuador", "egypt", "salvador", "guinea", "eritrea", "estonia", "ethiopia", "fiji", "finland", "france")
countries_ik <- c("gabon", "gambia", "georgia", "germany", "ghana", "greece", "grenada", "guatemala", "guinea", "guyana", "haiti", "honduras", "hungary")
countries_lm <- c("iceland", "india", "indonesia", "iran", "iraq", "ireland", "israel", "italy", "jamaica", "japan", "jordan", "kazakhstan", "kenya", "kiribati", "kuwait", "kyrgyzstan")
countries_mm <- c("laos", "latvia", "lebanon", "lesotho", "liberia", "libya", "liechtenstein", "lithuania", "luxembourg", "macedonia", "madagascar", "malawi", "malaysia", "maldives", "mali", "malta", "mauritania", "mauritius", "mexico", "micronesia")
countries_mo <- c("moldova", "monaco", "mongolia", "montenegro", "morocco", "mozambique", "myanmar", "namibia", "nauru", "nepal", "netherlands", "zealand", "nicaragua", "niger", "nigeria", "niue", "Korea", "norway")
countries_os <- c("oman", "pakistan", "palau", "panama", "paraguay", "peru", "philippines", "poland", "portugal", "qatar", "romania", "russia", "rwanda", "samoa")
countries_st <- c("saudi", "senegal", "serbia", "seychelles", "sierra", "singapore", "slovakia", "slovenia", "somalia", "africa", "sudan", "spain", "lanka", "suriname", "swaziland", "sweden", "switzerland", "syria")
countries_tv <- c("tajikistan", "tanzania", "thailand", "togo", "tonga", "trinidad", "tobago", "tunisia", "turkey", "turkmenistan", "tuvalu", "uganda", "uk", "Ukraine", "uae", "britain", "scotland", "america", "us", "uruguay", "uzbekistan")
countries_vz <- c("vanuatu", "venezuela", "vietnam", "yemen", "zambia" ,"zimbabwe")


ggplot(subset(word_count,Word %in% countries_a)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_b)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_c)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_def)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_ik)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_lm)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_mm)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_mm)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_mm)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_os)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_st)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_tv)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)

ggplot(subset(word_count,Word %in% countries_vz)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 2500)


relevant_countries1 <- c("us", "china", "iraq")

relevant_countries1.1 <- c("bangladesh", "afghanistan", "salvador", "finland", "france", "india", "iran", "japan", "uk")
relevant_countries2 <- c("ireland", "lebanon", "libya", "zealand", "russia", "peru", "pakistan", "syria", "lanka",
                         "zimbabwe")


ggplot(subset(word_count,Word %in% relevant_countries1)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(300, 2500)

ggplot(subset(word_count,Word %in% relevant_countries2)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 400)

ggplot(subset(word_count,Word %in% relevant_countries1.1)) + geom_point(mapping = aes(x = publish_date, y = Count, color=Word)) + ylim(100, 400) +geom_smooth(mapping = aes(x = publish_date, y = Count))




ggplot(subset(Abcheadlines,headline_text %in% "fake-news")) + geom_bar(mapping = aes(x = publish_date))

ggplot(subset(word_count,word %in% c("nationalism", "islamophobia", "islam", "terrorism", "terrorist", "racism", "feminism", "trump", "russia", "nationalist"))) + geom_bar(mapping = aes(x = word, y = Count, fill= word), stat = "identity")

ggplot(subset(word_count,Word %in% "trump")) + geom_point(mapping = aes(x = publish_date, y = Count, color=publish_date))

ggplot(subset(word_count,Word %in% "obama")) + geom_point(mapping = aes(x = publish_date, y = Count, color=publish_date))

ggplot(subset(word_count,Word %in% "terrorism")) + geom_point(mapping = aes(x = publish_date, y = Count, color=publish_date))

ggplot(subset(word_count,Word %in% "feminism")) + geom_point(mapping = aes(x = publish_date, y = Count, color=publish_date))

ggplot(subset(word_count,word %in% "russia")) + geom_point(mapping = aes(x = publish_date, y = Count, color=publish_date))

# Read data nto R
I_headlines <- read.csv("india-news-headlines.csv")

# convert list into a dataframe
Iheadlines <- data.frame(I_headlines)
head(Iheadlines, 25)
Iheadlines <- transform(Iheadlines, publish_date = as.Date(as.character(publish_date), "%Y%m%d"))
Iheadlines$publish_date <- format(Iheadlines$publish_date, "%Y")


word_count1 <-cSplit(Iheadlines, "headline_text", sep = " ", direction = "long") %>%
  group_by(tolower(headline_text), publish_date) %>%
  summarise(Count = n())

word_count1 <- data.frame(word_count1)
colnames(word_count1)[1] <- "word"

word_count1$word <- removeWords(word_count1$word, stopwords("english"))
sentiment_analysis_I <- word_count1 %>%
  inner_join(get_sentiments("nrc"))

ggplot(sentiment_analysis_I) + geom_bar(aes(publish_date, Count, fill= sentiment),stat = "identity") + facet_wrap(~ sentiment, nrow = 5)+ theme_bw() + theme( strip.background  = element_blank(),
                                                                                                                                                            panel.grid.major = element_line(colour = "grey80"),
                                                                                                                                                            panel.border = element_blank(),
                                                                                                                                                            axis.ticks = element_blank(),
                                                                                                                                                            panel.grid.minor.x=element_blank(),
                                                                                                                                                            panel.grid.major.x=element_blank() ) +theme(legend.position="bottom") 



#data_list1 <- tidytext::unnest_tokens(read.csv("india-news-headlines.csv", stringsAsFactors = FALSE), word, headline_text)
#Iheadlines <- data.frame(data_list1)
#Iheadlines <- transform(Iheadlines, publish_date = as.Date(as.character(publish_date), "%Y%m%d"))
#Iheadlines$publish_date <- format(Iheadlines$publish_date, "%Y")
#tab <- table(Iheadlines$word)
#tab <- data.frame(word = names(tab), count = as.numeric(tab))
#tab <- inner_join(tab, Iheadlines)




ggplot(subset(word)) + geom_point(mapping = aes(x = Word, y = Count))

ggplot(tab$publish_date == '2001') + geom_point(mapping = aes(x = Word, y = Count > 1000))
ggplot(subset(tab,publish_date %in% "2001")) +
  geom_point(aes(word, count), position = "jitter")  + ylim(100, 1000)

ggplot(subset(tab,publish_date %in% "2001")) +
  geom_point(aes(word, count))  + ylim(100, 150)

tab <- inner_join(tab, wf) 

# take out all the stop words 

tab$word = removeWords(tab$word, stopwords("english"))
# frequencies words with the highest count aka filter data
# filter by year

install.packages("tm")
library(tm)

?stop_words
tab <- tab[duplicated(tab), ]

ggplot(subset(tab,word %in% "islam")) + geom_point(mapping = aes(x = publish_date, y = count, color=publish_date))

#write.csv(sentiment_analysis, file = "ABC_sentiment.csv")
#write.csv(sentiment_analysis_I, file = "Itimes_sentiment.csv")

word_count = word_count[ word_count$`tolower(headline_text)` %in% c('[A-Za-z]'),]

