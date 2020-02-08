# Importing Packages

library(textdata)
library(tidytext)
library(tidyverse)
library(dplyr)
library(janeaustenr)
library(wordcloud)
library(textdata)
library(gutenbergr)
library(reshape2)
library(textreadr)
library(scales)
library(plotly)
library(igraph)
library(ggraph)
library(tm)
library(RColorBrewer)

# Reading Speech 1
file_1 <- read_document(file="Trump 2017.docx")
trump_2017 <- c(file_1)
trump_2017 <- data_frame(line=1, text=trump_2017)

# Tokenizing Speech 1
trump_2017_token <- trump_2017 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

trump_2017_token

# Reading Speech 2
file_2 <- read_document(file="Trump 2020.docx")
trump_2020 <- c(file_2)
trump_2020 <- data_frame(line=1, text=trump_2020)

# Tokenizing Speech 2
trump_2020_token <- trump_2020 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

trump_2020_token

# Reading Speech 3
file_3 <- read_document(file="Obama 2010.docx")
obama_2010 <- c(file_3)
obama_2010 <- data_frame(line=1, text=obama_2010)

# Tokenizing Speech 3
obama_2010_token <- obama_2010 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

obama_2010_token

# Combineing into single data frame
speech <- bind_rows(mutate(trump_2020_token, author="Trump2020"),
                    mutate(trump_2017_token, author= "Trump2017"),
                    mutate(obama_2010_token, author="Obama2010"))%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author)

speech

# Creating IDF
speech_idf <- speech %>%
  bind_tf_idf(word, author, n)

speech_idf

# Plotting IDF using Plotly
t <- plot_ly(data = speech_idf, x=~tf_idf, y=~word, color =~author, opacity = 0.6)
t

# Bing Sentiment Analysis using Plotly
# Trump 2020
trump2020_senti <- trump_2020 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

trump2020_bing <- trump2020_senti %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

trump2020_bing <- ggplotly(trump2020_bing)
trump2020_bing

# Trump 2017
trump2017_senti <- trump_2017 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

trump2017_bing <- trump2017_senti %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

trump2017_bing <- ggplotly(trump2017_bing)
trump2017_bing

# Obama 2010
obama_20210_senti <- obama_2010 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

obama_2010_bing <- obama_20210_senti %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

obama_2010_bing <- ggplotly(obama_2010_bing)
obama_2010_bing


# NRC Semtiment Analysis - Word Cloud
# Trump 2020
trump2020_senti_nrc <- trump_2020 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

trump2020_senti_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.6,0.6), title.size=1, rot.per=0.25)

# Trump 2017
trump_2017_senti_nrc <- trump_2017 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

trump_2017_senti_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.6,0.6), title.size=1, rot.per=0.25)

# Obama 2010
obama_2010_senti_nrc <- obama_2010 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

obama_2010_senti_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.6,0.6), title.size=1, rot.per=0.25)


# Creating Bigrams and ploting the networks
# Trump 2020
trump2020_bigrams <- trump_2020 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

trump2020_bigrams

trump2020_bigram_graph <- trump2020_bigrams %>%
  filter(n>2) %>%
  graph_from_data_frame()

ggraph(trump2020_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),  vjust =1, hjust=1)

# Trump 2017
trump2017_bigrams <- trump_2017 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

trump2017_bigrams

trump2017_bigram_graph <- trump2017_bigrams %>%
  filter(n>2) %>%
  graph_from_data_frame()

ggraph(trump2017_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),  vjust =1, hjust=1)

# Obama 2010
obama2010_bigrams <- obama_2010 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

obama2010_bigrams

obama2010_bigram_graph <- obama2010_bigrams %>%
  filter(n>2) %>%
  graph_from_data_frame()

ggraph(obama2010_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name),  vjust =1, hjust=1, scale=c(0.6,0.6))


# Creating single data frame with all tokens along with frequency proportions
frequency <- bind_rows(mutate(trump_2020_token, author="Trump2020"),
                       mutate(trump_2017_token, author= "Trump2017"),
                       mutate(obama_2010_token, author="Obama2010")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  #select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Trump2020`, `Trump2017`)

frequency

# Plotting a correlogram using Plotly
p <- ggplot(frequency, aes(x=proportion, y=Obama2010,
                           color = abs(Obama2010- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(aes(text=paste("word: ", word)), alpha=.1, size=2.5, width=0.3, height=0.3)+
  #geom_text(aes(label=word), colour="gray20", alpha=1) +
  #scale_x_log10(labels = percent_format())+
  #scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Obama2010", x=NULL)

p <- ggplotly(p)
p


# Creating a Document Term Matrix (DTM)
speech_dtm <- speech %>%
  group_by(author) %>%
  cast_dtm(author, word, n)

speech_dtm

# Creating a vector for negeation tokens
negation_tokens <- c('wall', 'student', 'job', 'american')

# Negeated Token for Trump 2020
trump2020_negated <- trump_2020 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

trump2020_negated

# Negeated Token for Trump 2017
trump2017_negated <- trump_2017 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

trump2017_negated

# Negeated Token for Obama 2010
obama2010_negated <- obama_2010 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

obama2010_negated

# Function to plot negeted tokens
negated_plot <- function(x){
  obama2010_negated %>%
    filter(word1 == x) %>%
    mutate(contribution = n* value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}

# Negeation plot using the plot fuction created above
negated_plot(x="job")
negated_plot(x="american")

# Sentiment Analysis using AFINN
# Trump 2020
trump2020_afinn <- trump_2020_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

trump2020_afinn

# Trump 2017
trump2017_afinn <- trump_2017_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

trump2017_afinn

# Obama 2010
obama_2010_afinn <- obama_2010_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))

obama_2010_afinn