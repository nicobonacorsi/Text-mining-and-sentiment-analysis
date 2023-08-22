# In the file dfRecHoEoFw.Rdata, there is the data.frame dfRecHoEoFw which contains 900 reviews of three
# telecommunications and internet service companies: Ho-mobile, Eolo, Fastweb. For each review, the complete text
# (text field), date (date), the company it refers to (company), and a rating from 1 to 5 (vote) are provided.

dfRecHoEoFw

# 1. Perform text cleaning operations and correction of compound forms

# 1a Text cleaning
dfRecHoEoFw$txtp = cleanText(xtxt = dfRecHoEoFw$text, hashtag = TRUE, mention = TRUE, numbers = TRUE, punctuation = TRUE, lowercase = TRUE)

# 1b Identify any posts to be eliminated
visNGram(x = dfRecHoEoFw$txtp, ngrI = 2, ngrF = 4, nn = 50)

correz <- c("rapporto qualità prezzo", NA,
            "servizio clienti", "servizio_clienti",
            "call center", NA)

dfRecHoEoFw$txtp <- corFrmComp(vText = dfRecHoEoFw$txtp, correzioni = correz)
dfRecHoEoFw$txtp <- corNGram(x = dfRecHoEoFw$txtp, verbose = TRUE)

# 2. Lemmatize using the Italian-vit dictionary and use the stopword set stopwIT from IT_stopwords.Rdata
dfRecHoEoFw <- dfRecHoEoFw %>% mutate(doc_id = 1:nrow(dfRecHoEoFw), .before = 1)

un_model_I <- udpipe_load_model(file = "italian-isdt-ud-2.5-191206.udpipe")

outL <- lemmaUDP(x = dfRecHoEoFw$txtp,
                 model = un_model_I,
                 doc_id = dfRecHoEoFw$doc_id,
                 stopw = stopwIT,
                 userstopw = c("ho mobile", "eolo", "fastweb"),
                 verbose = TRUE)

# 3. Reconstruct lemmatized sentences preserving: adjectives, adverbs, nouns, proper nouns, and verbs
textL = outL %>% filter(!is.na(lemma) & STOP == FALSE & upos %in% c("ADJ", "ADV", "NOUN", "PROPN", "VERB")) %>%
  group_by(doc_id = as.numeric(doc_id)) %>%
  summarise(txtL = paste(lemma, collapse = " "))

# Match them to doc_id
dfRecHoEoFw = left_join(dfRecHoEoFw, textL, by = "doc_id")

# 4. Create a new data.frame selecting only texts with at least 5 lemmas
# Count lemmas
nparoleL = sapply(dfRecHoEoFw$txtL, FUN = wordcount)
# Select posts with at least 5 lemmas
dfRecHoEoFw2 = dfRecHoEoFw[nparoleL >= 4,]

# 5. Using this new data.frame, excluding terms in stopwITwc, report:
corp = Corpus(VectorSource(dfRecHoEoFw2$txtL))
tdml = TermDocumentMatrix(corp)
mtdml = as.matrix(tdml)
dfterm = data.frame(word = rownames(mtdml), freq = rowSums(mtdml)) %>%
  arrange(-freq)
# a. Frequency table of the top 10 most used lemmas,
# excluding stopwords present in stopwITwc
dfterm = dfterm %>% filter(!word %in% stopwITwc)
rownames(dfterm) <- NULL
# Table with the top 10 most frequent lemmas
head(dfterm, 10)

# b. Lemmatized wordcloud with TF weighting,
par(mar = c(0, 0, 0, 0))
wordcloud(words = dfterm$word,
          freq = dfterm$freq,
          scale = c(2, 0.2),
          max.words = 100, colors = brewer.pal(n = 5, name = "Set1"))
text(0.5, 1, "lemmatized TF wordcloud", font = 2)

# c. Lemmatized wordcloud with TF-IDF weighting
# Weighting
tdml_idf = tm::weightTfIdf(tdml)
mtdml_idf = as.matrix(tdml_idf)
dfterm_idf = data.frame(word = rownames(mtdml_idf), freq = rowSums(mtdml_idf)) %>%
  arrange(-freq)
rownames(dfterm_idf) <- NULL
# Exclude stopwords present in stopwITwc again
dfterm_idf <- dfterm_idf %>% filter(!word %in% stopwITwc)
par(mar = c(0, 0, 0, 0))
wordcloud(word = dfterm_idf$word,
          freq = dfterm_idf$freq,
          scale = c(2, 0.2),
          max.words = 100, colors = brewer.pal(n = 5, name = "Set1"))
text(0.5, 1, "lemmatized TF-IDF wordcloud", font = 2)

# 6. Report the TF comparison cloud for comparing lemmas in the three companies' texts
dftxt2 = dfRecHoEoFw2 %>% group_by(company) %>% summarise(testoL = paste(txtL, collapse = " "))

corp2 = Corpus(VectorSource(dftxt2$testoL))
tdml2 = TermDocumentMatrix(corp2)
tdml2_idf = tm::weightTfIdf(tdml2)
mtdml2_idf = as.matrix(tdml2_idf)
colnames(mtdml2_idf) <- dftxt2$company

par(mar = c(0, 0, 0, 0))
comparison.cloud(term.matrix = mtdml2_idf,
                 scale = c(2, 0.5, 2),
                 max.words = 100,
                 random.order = FALSE,
                 colors = c("blue", "red", "darkgreen"),
                 match.colors = TRUE,
                 title.size = 1.5)

# 7. Calculate the average sentiment score using sentiment dictionaries: Syuzhet, OpenR, NCR
lDiz <- list(Syuzhet = dSyuzB, OpenR = dOpenR, NCR = dNcr)

oout <- sentiMediaDiz(x = outL,  # lemmatization output
                      dict = lDiz,  # list of dictionaries to use
                      negators = polarityShifter,
                      amplifiers = intensifier,
                      deamplifiers = weakener)

oout$DizioPol
oout$MediaPol

# 8. Report:
# Add the average sentiment score to the dataframe
dfRSent <- left_join(dfRecHoEoFw2, oout$MediaPol, by = "doc_id")
# Calculate polarity classes
dfRSent$cl_mediaPol <- ifelse(dfRSent$mediaSent < 0, "Negative", ifelse(dfRSent$mediaSent > 0, "Positive", "Neutral"))

# a. Table with overall average sentiment score and per company,
# overall average sentiment score
mean(dfRSent$mediaSent)
# average sentiment score per company
dfRSent %>% group_by(company) %>% summarise(mean(mediaSent))

# b. Bar plot with distribution of polarity class per company
dfRSent %>% group_by(company, cl_mediaPol) %>% summarise(n = n()) %>% mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = company, y = perc, fill = cl_mediaPol)) + geom_col() + theme_light() +
  ggtitle("Distribution of polarity class per company")

# 9. Report a comparison cloud of lemmas in texts for Positive and Negative polarity classes
polRec <- myClassPolarity(textColumns = dfRecHoEoFw2$txtL,
                          algorithm = "bayes",
                          lexicon = "subjectivity_it_lem.csv")

# Emotion using naive bayes method
emoRec <- myClassEmotion(textColumns = dfRecHoEoFw2$txtL,
                         algorithm = "bayes",
                         lexicon = "emotions_it_lem.csv")

# Group texts by polarity
sottoinsieme = subset(dfRSent, cl_mediaPol == "Positive" | cl_mediaPol == "Negative")
plr.docs <- sottoinsieme %>% group_by(cl_mediaPol) %>% summarise(text = paste(txtL, collapse = " "))

corpP <- Corpus(VectorSource(plr.docs$text))
tdmP <- TermDocumentMatrix(corpP, control = list(wordLengths = c(0, Inf)))
tdmP2 = tm::weightTfIdf(tdmP)
mtdmP <- as.matrix(tdmP2)

colnames(mtdmP) <- plr.docs$cl_mediaPol
head(mtdmP)

par(mar = c(0, 0, 0, 0))
comparison.cloud(term.matrix = mtdmP, scale = c(3, 0.3), max.words = 100, title.size = 1.5, match.colors = TRUE)
text(0.5, 1, "comparison of polarity terms")

# 10. Calculate the vote classes 1:2=Negative, 3=Neutral, 4:5=Positive and compare with polarity classes:
dfRSent$clVoto <- cut(dfRSent$vote, breaks = c(1, 2, 3, 5), include.lowest = TRUE)
# a. Report a two-way comparison table of vote class – polarity class
tabS <- table(dfRSent$clVoto, dfRSent$cl_mediaPol)
addmargins(tabS)
# b. Calculate the accuracy between vote class and polarity class
sum(diag(tabS)) / sum(tabS)

# 11. Using the emotions lexicon with the Naive Bayes method, report the plot with emotion distribution
emoRec$documenti %>% group_by(best_fit) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n / sum(n) * 100)

emoRec$documenti %>% group_by(best_fit) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(best_fit, perc), y = perc, fill = best_fit)) +
  geom_col() +
  theme_light() +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(title = "Sentiment Emotions", subtitle = "Emotions Lexicon - NaiveBayes")
