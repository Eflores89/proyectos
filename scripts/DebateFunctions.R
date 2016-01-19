# script with functions for debate parsing... 
# make df
MakeDebateDF<-function(df){
  newdf <- data.frame(
    person = apply(df, 
                   MARGIN = 1, 
                   function(x){
                     stri_extract_first_regex(x, 
                                              "[A-Z'-]+(?=(:s))")
                   }),
    message = apply(df, 
                    MARGIN = 1, 
                    function(x){
                      stri_replace_first_regex(x,
                                               "[A-Z'-]+(?=(:s))", 
                                               "")
                    }),
    stringsAsFactors=FALSE
  )
  for (j in 2:nrow(newdf)) { 
    if (is.na(newdf[j,'person'])) 
    {newdf[j,'person'] <-  newdf[(j-1),'person'] }
  }
  
  return(newdf)
}

# ------ 
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer") 
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}

#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don’t want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}
# ---------------------------------
# ---------------------------------
UnlistAndExtractInfo <- function(candidate){
  # this function is not general - it only applies to these particular debates...
  # all the debates must be named the same in the parent env.
  # for example: debate_h ...
  
  allwords_1 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_h, person == candidate)['message'],
        1,
        paste))))
  allwords_2 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_c, person == candidate)['message'],
        1,
        paste))))
  allwords_3 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_b, person == candidate)['message'],
        1,
        paste))))
  allwords_4 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_w, person == candidate)['message'],
        1,
        paste))))
  allwords_5 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_v, person == candidate)['message'],
        1,
        paste))))
  allwords_6 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_s, person == candidate)['message'],
        1,
        paste))))
  df_insights <- data.frame(
    debate = c("Ohio", "California", "Colorado", "Wisconsin","Vegas","SCarolina"),
    average_intervention = c(mean(stri_count_words(
      apply(
        subset(debate_h, person == candidate)['message'],
        1,
        paste))),
      mean(stri_count_words(
        apply(
          subset(debate_c, person == candidate)['message'],
          1,
          paste))),
      mean(stri_count_words(
        apply(
          subset(debate_b, person == candidate)['message'],
          1,
          paste))),
      mean(stri_count_words(
        apply(
          subset(debate_w, person == candidate)['message'],
          1,
          paste))),
      mean(stri_count_words(
        apply(
          subset(debate_c, person == candidate)['message'],
          1,
          paste))),
      mean(stri_count_words(
        apply(
          subset(debate_s, person == candidate)['message'],
          1,
          paste)))
    ),
    words_total = c(length(allwords_1),
                    length(allwords_2),
                    length(allwords_3),
                    length(allwords_4),
                    length(allwords_5),
                    length(allwords_6)),
    words_unique = c(length(unique(allwords_1)),
                     length(unique(allwords_2)),
                     length(unique(allwords_3)),
                     length(unique(allwords_4)),
                     length(unique(allwords_5)),
                     length(unique(allwords_6))
    ),
    words_repeated_fromfirst = c(0, sum(allwords_2 %in% allwords_1), 
                                 sum(allwords_3 %in% allwords_1),
                                 sum(allwords_4 %in% allwords_1),
                                 sum(allwords_5 %in% allwords_1),
                                 sum(allwords_6 %in% allwords_1)),
    unique_words_repeated_fromfirst = c(0,
                                        length(unique(allwords_2[allwords_2 %in% allwords_1])),
                                        length(unique(allwords_3[allwords_3 %in% allwords_1])),
                                        length(unique(allwords_4[allwords_4 %in% allwords_1])),
                                        length(unique(allwords_5[allwords_5 %in% allwords_1])),
                                        length(unique(allwords_6[allwords_6 %in% allwords_1]))
    ),
    words_repeated_fromsecond = c(0, 0, 
                                  sum(allwords_3 %in% allwords_2),
                                  sum(allwords_4 %in% allwords_2),
                                  sum(allwords_5 %in% allwords_2),
                                  sum(allwords_6 %in% allwords_2)),
    unique_words_repeated_fromsecond = c(0, 0,
                                         length(unique(allwords_3[allwords_3 %in% allwords_2])),
                                         length(unique(allwords_4[allwords_4 %in% allwords_2])),
                                         length(unique(allwords_5[allwords_5 %in% allwords_2])),
                                         length(unique(allwords_6[allwords_6 %in% allwords_2]))
    ),
    words_repeated_fromthird = c(0, 0, 0,
                                 sum(allwords_4 %in% allwords_3),
                                 sum(allwords_5 %in% allwords_3),
                                 sum(allwords_6 %in% allwords_3)
                                 ),
    unique_words_repeated_fromthird = c(0, 0, 0,
                                        length(unique(allwords_4[allwords_4 %in% allwords_3])),
                                        length(unique(allwords_5[allwords_5 %in% allwords_3])),
                                        length(unique(allwords_6[allwords_6 %in% allwords_3]))
    )
    , stringsAsFactors = FALSE)
  return(df_insights)
}
