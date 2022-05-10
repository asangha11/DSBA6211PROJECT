# packages
install.packages('quanteda')
install.packages('quanteda.textplots')
install.packages('quanteda.textstats')
install.packages('quanteda.textmodels')
install.packages('tidytext')
install.packages('DT')
install.packages('tm')
install.packages('wordcloud')
install.packages('tidyverse')
install.packages('stringr')
install.packages('leaflet')
install.packages('ggmap')
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(quanteda.textmodels)
library(data.table)
library(tidytext)
library(DT)
library(tm)
library(wordcloud)
library(tidyverse)
library(stringr)
library(leaflet)
library(ggmap)

#load data
reviews <- read.csv("reviews-2.csv")

#exploration
dim(reviews)

head(reviews,10)
#cleaning data
#removing stop words
reviews_text <- reviews %>%
  unnest_tokens(word, comments) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))
#visualize top 20 words
top_reviews <- reviews_text %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  top_n(n = 20, wt = count) %>%
  ggplot() +
  geom_bar(mapping = aes(x=reorder(word, count), y=count),
           stat="identity", fill = "dark green") +
  coord_flip() +
  labs(title="Top 20 words in Reviews",
       y="Word count", x="Words") +
  theme_minimal()

print(top_reviews)

#making a data frame of words and its frequency
clouddf <- as.data.frame(reviews_text %>% 
                         group_by(word) %>%
                         summarise(no_rows = length(word)))

#building the word cloud
wordcloud(words = clouddf$word, freq = clouddf$no_rows, min.freq = 5,
          max.words=150, random.order=FALSE, random.color=FALSE, rot.per=0.33, 
          colors=brewer.pal(1, "Dark2"))


