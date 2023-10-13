################################################################################
# Analyzing NEPA Comments using natural language processing & chatGPT
################################################################################

# libraries: 
library(stringr)
library(data.table)
library(tm)
library(textstem)
library(dplyr)
library(topicmodels)
library(quanteda)
library(tidytext)
library(httr)
library(ggplot2)

# logging chatGPT api key: 
my_API <- "<insert chatGPT api key here>"

# sourcing in chatGPT api function: 
source("./R_Source/hey_chatGPT.R")

# reading in NEPA comments: 
nepa <- read.csv("./Data/NEPA_Comments_Oct2023.csv")
# grabbing only comments: 
nepa_comment <- nepa$Comment
# note here that EPIC has an attachment, rather than a full comment 
epic <- nepa[23272,]


################################################################################
# Quick tidying: 
################################################################################
# removing some \n as that could be annoying: 
nepa_comment_tidy <- gsub("[\n]", " ", nepa_comment)
nepa_comment_tidy <- gsub("\\\\", "", nepa_comment_tidy)
nepa_comment_tidy <- gsub("\t", "", nepa_comment_tidy)

# all lowercase: 
nepa_comment_tidy <- tolower(nepa_comment_tidy)

# okay, keep only one comment from those that were copied and pasted: 
nepa_comment_tidy_nodups <- unique(nepa_comment_tidy) 
# wow! removed 19% of comments

# trimming random white spaces: 
nepa_comment_tidy_nodups_ws <- gsub("   ", " ", nepa_comment_tidy_nodups)
nepa_comment_tidy_nodups_ws <- gsub("  ", " ", nepa_comment_tidy_nodups_ws)

# now let's focus on comments that were copied and pasted, but not identical. 
# we can do this by taking the first 500 characters of every single comment 
# and only grabbing items that are unique: 
begin_comment <- substr(nepa_comment_tidy_nodups_ws, 1, 500)
unique <- unique(begin_comment)
# grabbing all of the unique comments: 
comments <- data.frame(index = "", comment = "")
for(i in 1:length(unique)){
  uniq_i <- unique[i]
  comment_i <- nepa_comment_tidy_nodups_ws[startsWith(nepa_comment_tidy_nodups_ws, uniq_i)]
  comment_i <- comment_i[1] # grabbing just one if there are multiple 
  comment_i_df <- data.frame(index = i, comment = comment_i)
  comments <- rbind(comments, comment_i_df)
}

# okay, now we need to focus on comments that don't have attachments, since 
# we can't analyze them for content: 
comments_noatt <- comments[!grepl("attach", comments),]
comments_noatt <- comments[!grepl("attached", comments),]

################################################################################
# Random sample analysis with chatGPT:
################################################################################
# let's grab a random sample - anything more than 13 will crash chatGPT 
df <- data.frame(loop = "", response = "")
for(i in 1:40){ # you might have to loop through this in parts 
  rand <- sample(1:nrow(comments_noatt), 10, replace = FALSE)
  rand_samp <- comments_noatt[rand,]
  
  # tidying specific request: 
  request <- c("Provide a numbered list of the five main points from these comments, especially how the comment relates to technology: ", rand_samp$comment)
  request <- paste(request, collapse = ". ")
  
  # pulling chatGPT request: 
  response_i <- hey_chatGPT(request)
  # this takes about ~10 seconds
  df_i <- data.frame(loop = i, response = response_i)
  df <- rbind(df, df_i)
}

# For reference, to cycle through 10 times (grabbing 10 random comments and 
# using chatGPT API to summarize 5 main points), it takes approximately 3 mins

cat(df$response)
df_tidy <- df[2:nrow(df),]
# saving this this for later! 
# write.csv(df_tidy, "./Data/rand_comment_GPT_summary.csv")

# okay, now let's do another summary using chatGPT. 
# needed to split the records into two parts, or the request would be too large: 
df_tidy_p1 <- df_tidy[1:20,]
resp_all <- c("Provide a numbered list of the five main points from all of these comments, especially how they relate to technology: ", df_tidy_p1$response)
resp_all <- paste(resp_all, collapse = ". ")
# summarizing chatGPT's earlier responses into five main points: 
response_all <- hey_chatGPT(resp_all)
response_p1 <- response_all

# let's grab the second part: 
df_tidy_p2 <- df_tidy[21:40,]
resp_all <- c("Provide a numbered list of the five main points from all of these comments, especially how they relate to technology: ", df_tidy_p2$response)
resp_all <- paste(resp_all, collapse = ". ")
# summarizing chatGPT's earlier responses into five main points: 
response_p2 <- hey_chatGPT(resp_all)

# let's combine them: (doing some manual word-smithing from here, but the two 
# appear very similar)
all_resp <- c(response_p1, response_p2)
# write.csv(all_resp, "./Data/FINAL_GPT_summary.csv")

################################################################################
# Natural Language Processing 
################################################################################
# NOTE -- since semi-duplicated comments were removed, some of the frequencies 
# may actually be different... think on this one

# removing excess stop words for natural language processing: 
comments <- tm::Corpus(VectorSource(comments_noatt$comment))
comments <- tm::tm_map(comments, removePunctuation)
comments <- tm::tm_map(comments, removeWords, stopwords("english"))

# lemmatization to reduce words to root forms: 
# comm_lemm <- textstem::lemmatize_words(comments) # i didn't find this to 
# make that large of a difference 

# developing a document-term matrix: 
dtm <- DocumentTermMatrix(comments)
# char_comm <- as.character(comments)
# dtm <- quanteda::dfm(comments, verbose = FALSE)

# topic modeling using Latent Dirichlet Allocation (LDA):
comm_lda <- LDA(dtm, k = 5, control = list(seed = 1234))
# terms(comm_lda, 20)
lda_tidy <- tidytext::tidy(comm_lda)
# lda_tidy %>%
#   mutate(term = reorder_within(term, beta, topic)) %>%
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   scale_y_reordered()

# what's the sentiment for these words?
lda_sentiment <- lda_tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
lda_sentiment <- lda_sentiment %>%
  group_by(term) %>%
  summarize(sent = unique(sentiment))
ggplot(lda_sentiment) + 
  geom_bar(aes(x = sent, fill = sent)) + 
  theme_bw()

# frequency analysis: 
lda_tidy_summ <- lda_tidy %>%
  group_by(term) %>%
  summarize(mean_beta = mean(beta))
# removing random symbols and numbers: 
lda_tidy_summ_tidy <- lda_tidy_summ[nchar(lda_tidy_summ$term) > 1,]
lda_tidy_summ_tidy <- lda_tidy_summ_tidy[!grepl("[1-9]", lda_tidy_summ_tidy$term),]
# grabbing sentiments: 
# lda_tidy_summ_tidy <- lda_tidy_summ_tidy %>%
#   inner_join(get_sentiments("bing"), by = c(term = "word"))
lda_tidy_summ_tidy <- lda_tidy_summ_tidy[order(-lda_tidy_summ_tidy$mean_beta),]
# grabbing only the top 15: 
lda_tidy_summ_tiny <- lda_tidy_summ_tidy[1:40,]

ggplot(lda_tidy_summ_tiny, aes(x = reorder(term, -mean_beta), 
                               y = mean_beta)) + 
  ggchicklet::geom_chicklet(stat = "identity") + 
  theme_bw() + 
  coord_flip() + 
  labs(y = "Topic-Word Density", x = "Term")

# where do certain words of interest fall?
which(lda_tidy_summ_tidy$term == "technology") # 1357
which(lda_tidy_summ_tidy$term == "technologies") # 3297
which(lda_tidy_summ_tidy$term == "data") # 860
which(lda_tidy_summ_tidy$term == "AI") # none 
which(lda_tidy_summ_tidy$term == "IT") # none 
