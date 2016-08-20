library(rvest)
library(stringr)
library(tm)
install.packages('SnowballC')
library(tm)
library(SnowballC)
library(wordcloud)
phds <- read_html('http://www.asc41.com/graduates1.htm')

d <- phds %>% 
  html_nodes('p , em , strong') %>% 
  html_text() %>%  
  #as.data.frame() %>% 
  gsub('Â','',.) %>% 
 # gsub('\r\n','',.) %>% 
  gsub('    ',' ',.) %>% 
  str_trim() 

  

  
  getSources()
  
  
  
  
  ds <- VectorSource(d)
  corp <- Corpus(ds)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, function(x){removeWords(x,c(stopwords(),
                                                   'university','chaired','criminal','august','state',
                                                   'may','crime','justice'))})
  #corp <- tm_map(corp, function(x){removeWords(x,c('????~','????o','?','???','T','????~'))})
  #corp <- tm_map(corp, function(x){removeWords(x,c('lyrics','greg','brett','just','dont','like','can','cant','theres','know'))})
  corp <- tm_map(corp,PlainTextDocument)
  term.matrix <- TermDocumentMatrix(corp)
  term.matrix <- as.matrix(term.matrix)
  wordcloud(corp,max.words=100)




d<- d %>% data.frame(matrix(unlist(d),nrow=1582,byrow=T))
View(d)
as.data.frame()
?str_split



demo(package = "rvest")
demo(zillow)

twi