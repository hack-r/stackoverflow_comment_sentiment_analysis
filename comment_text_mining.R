
# Libraries and Options ---------------------------------------------------
set.seed(2018)
pacman::p_load(aod, AER, arules, bit64, caret, circlize,data.table, 
            devtools, dplyr, e1071, fakeR, forecast, 
            ggplot2, ggraph, ggrepel, gridExtra, 
            gtools, igraph, imputeTS, lpSolveAPI, 
            lubridate, magick, Matrix, mclust, mco, Metrics, memery,
            ModelMetrics, nnet, odbc, pacman, radarchart, recipes, readr, 
            ROCR, RODBC, RRF, rstan, SnowballC, speedglm, sqldf, textclean,
            tidyverse, tidytext, tm, VGAM, widyr, xgboost, 
            xts, zoo) 
  

# Load Raw Data -----------------------------------------------------------
sample <- fread("..//Downloads//QueryResults.csv") 
data("stop_words")

# Save in R format
saveRDS(sample, "sample_raw.RDS")

# Exploratory Data Analysis + Feature Engineering -------------------------

# Have a quick look
glimpse(sample)
table(sample$flag_treatment) # roughly equal, slightly more YoY

ggplot(sample, aes(x=as.factor(flag_treatment), fill=as.factor(flag_treatment) )) + geom_bar( ) +
  scale_fill_brewer(palette = "Set1")

# Rename comment upvotes
sample$Number_Upvotes <- sample$Score
sample$Score          <- NULL

# Fix date 
sample$CreationDate <- as.Date(sample$CreationDate)

# UserDisplayName field = junk
sum(sample$UserDisplayName=="") / nrow(sample) # 0.99838 ??
sample$UserDisplayName <- NULL

# Exclude certain words for obvious reasons
# custom_stop_words <- c("Stack Overflow", "Stack Exchange", "")

# Basic text mining (TM) feature enginerring (FE) steps
sample <- sample %>% 
  mutate(text_clean = replace_grade(replace_internet_slang(replace_html(tolower(Text)))))  

glimpse(sample)

saveRDS(sample, "sample.RDS")

# Sentiment Analysis ------------------------------------------------------

# Unnest text data (1-gram only)
t1 <- as.data.table(sample) %>% 
  select(PostId, Id, text_clean, flag_treatment, Number_Upvotes) %>% 
  unnest_tokens(word, text_clean)
head(t1)

# Remove stop words
stop_words
my_stopwords <- data_frame(word = c(as.character(1:100),
                                    "exchange",
                                    "overflow",
                                    "stack",
                                    "stackoverflow",
                                    ".com",
                                    "com",
                                    "org",
                                    "net",
                                    "chat",
                                    "code",
                                    "https",
                                    "question",
                                    "data",
                                    "error",
                                    "stackoverflow.com",
                                    "file",
                                    "questions",
                                    "answer",
                                    "function",
                                    "add",
                                    "subtract",
                                    "minus",
                                    "plus",
                                    'positive',
                                    "negative",
                                    "warning",
                                    "http",
                                    "post",
                                    "string",
                                    "trouble",
                                    "problem",
                                    "issue",
                                    "check",
                                    "set",
                                    "read",
                                    "class",
                                    "run",
                                    "method",
                                    "server",
                                    "create",
                                    "array",
                                    "line",
                                    "change",
                                    "type",
                                    "object",
                                    "version",
                                    "table",
                                    "call",
                                    "list",
                                    "user",
                                    "output",
                                    "query",
                                    "time",
                                    "app",
                                    "page",
                                    "return",
                                    "files",
                                    "html",
                                    "image",
                                    "library",
                                    "write",
                                    "key",
                                    "python",
                                    "loop",
                                    "id",
                                    "java",
                                    "url",
                                    "project"))

t2 <- t1 %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stopwords, by = "word") %>%
  filter(str_detect(word, "[a-z]"))
head(t2)

t2 %>%
  count(word) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Join word scores
sample_sa <- t2 %>%
  inner_join(get_sentiments("afinn")) # score -5 (talking smack) to 5 (most positive)
head(sample_sa)

# Compare Sentiment YoY
yoy <- sample_sa %>% 
  group_by(flag_treatment) %>% 
  summarize(avg_sentiment_AFINN_scale = mean(score))
yoy

p <- ggplot(yoy, aes(x=as.factor(flag_treatment), y=avg_sentiment_AFINN_scale, color=as.factor(flag_treatment))) +
  geom_bar(stat="identity", fill="white")
p

plot(density(sample_sa$score[sample_sa$flag_treatment==0]), main= "Density of Comment Sentiment on AFINN Scale")
lines(density(sample_sa$score[sample_sa$flag_treatment==1]), col="blue")

t.test(sample_sa$score[sample_sa$flag_treatment==1],
       sample_sa$score[sample_sa$flag_treatment==0])

# Correlation between comment upvotes + sentiment
cor(sample_sa$Number_Upvotes, sample_sa$score)
cor(sample_sa$Number_Upvotes[sample$flag_treatment==1], sample_sa$score[sample$flag_treatment==1])
cor(sample_sa$Number_Upvotes[sample$flag_treatment==0], sample_sa$score[sample$flag_treatment==0])

