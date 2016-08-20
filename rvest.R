
devtools::install_github('talgalili/installr')
library("installr")
updateR()
install.packages(c('highcharter'),dependencies = T)
devtools::install_github('cran/slam')

library(rvest)
library(stringr)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(DescTools)
library(ggmap)
library(Amelia)
library(leaflet)
library(tm)
library(highcharter)
source("https://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite('Rgraphviz')
devtools::install_github("cpsievert/LDAvis")
library(LDAvis)
library('graph')
library('Rgraphviz')
options(scipen=1000)
getwd()

#get data
phds <- read_html('http://www.asc41.com/graduates1.htm')

d <- phds %>% 
  html_nodes('p , em , strong') %>% 
  html_text() %>% 
  #gsub('â€œ','"',.) %>% 
  gsub('\r\n','',.) %>% 
  gsub("[\\]",'',.)

  #tail(d,50)  


##Parse
#Years
yr <- "[1-9][0-9]{3}"



years <- str_locate_all(d,yr)
yearloc <- lapply(years,'[',1)
yearloc <- data.frame(yearLoc=unlist(yearloc))
years <- data.frame(Year=as.integer(str_extract(d,yr)))

#find first period
period <- str_locate_all(d,"\\.")
period <- lapply(period,'[',1)
period <- data.frame(PeriodLoc=unlist(period))


#find special characters
char <- str_locate_all(d,"\\?")
char <- lapply(char,'[',1)
char <- data.frame(CharLoc=unlist(char))
str_sub(d,1,char$CharLoc)

#find first comma
comma <- str_locate_all(d,",")
comma <- lapply(comma,'[',1)
comma <- data.frame(commaLoc=unlist(comma))
str_sub(d,1,comma$commaLoc)


#find second comma
comma2 <- str_locate_all(d,",")
comma2 <- lapply(comma2,'[',2)
comma2 <- data.frame(comma2Loc=unlist(comma2))
str_sub(d,1,comma2$comma2Loc)


#extract name
matches <- cbind(period,char,comma,comma2)
matches$min <- apply(matches[,c(1,4)],1,min,na.rm=T)
names <- data.frame(Name=str_sub(d,1,matches$min))


#extract chair
chair <- str_locate_all(d,"Chaired")
chair <- lapply(chair,'[',1)
chair <- data.frame(chairLoc=unlist(chair))
chairLoc <- chair
chair <- str_sub(d,chair$chairLoc,yearloc$yearLoc-1)
chair <- str_trim(chair)
chair <- str_replace(chair,"Chaired by","")
chair <- str_replace(chair,"Chaired","")
chair <- str_replace(chair,"by","")
chair <- str_trim(chair)
chair <- gsub('January|February|March|April|May|June|July|August|September|October|November|December',
              '',chair)
chair <- str_trim(chair)
chair <- str_replace_all(chair,"[[:punct:]]","")
chair <- str_replace_all(chair,"\\t","")
chair <- str_replace_all(chair,"Dr","")
chair <- str_replace_all(chair,"PhD","")
chair <- str_replace_all(chair,"    ","")
chair <- str_trim(chair)


##Institution
inst <- str_sub(d,yearloc$yearLoc+4,str_length(d))
inst <- str_replace_all(inst,"[[:punct:]]","")
inst <- gsub("[[:digit:]]","",inst)
#inst <- gsub("[^[:alnum:]]","",inst)
inst <- str_trim(inst)
inst <- gsub("chaired by","",inst)
inst <- gsub("chaired","",inst)
inst <- gsub("by","",inst)
inst <- data.frame(inst)


##Geocode them
univ <- data.frame(inst=unique(inst))
univ$inst <- as.character(univ$inst)
univ <- na.omit(univ)
names(univ)
)

geo <- geocode(univ$inst)  
univ <- cbind(univ,geo)
names(univ) <- c('inst','lon','lat')
head(univ)
inst <- left_join(inst,univ,by='inst')
head(inst,30)


##title
title <- str_sub(d,matches$min,chairLoc$chairLoc)
title <- str_replace_all(title,"[[:punct:]]","")
#title <- str_trim(title)
#title <- gsub("[^[:alnum:]]","",title)
title <- data.frame(Title=title)



data <- cbind(years,names,chair,inst,title)
missmap(data)
Desc(data)
saveRDS(data,'Clean Grads2.rds')

  
list.files()
g <- readRDS("Clean Grads2.rds")
g <- g %>% filter(Year>2002) %>%   filter(Year<2016)

options(scipen=1000)
Desc(g,plotit=T)

table(g$Year)







  
  
 ##Word Cloud 
  ds <- VectorSource(g$Title)
  corp <- Corpus(ds)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, stemDocument)
  corp <- tm_map(corp, function(x){removeWords(x,c(stopwords(),
                                                   'univers','graduate','social','florida','houston',
                                                   'michael','john','college','jay',
                                                   'university','chaired','criminal','august','state',
                                                   'may','december','crime','justice','the',
                                                   'study','analysis','examination','and','the',
                                                   '\\?','\\???','o','othe','use','among'))})
  corp <- tm_map(corp,PlainTextDocument)
  term.matrix <- TermDocumentMatrix(corp)
  #term.matrix <- as.matrix(term.matrix)
  
  
  termfreq <- rowSums(as.matrix(term.matrix))
  terms <- data.frame(term=names(termfreq),freq=termfreq)
  
  
  pal <- brewer.pal(9,"Dark2")
  wordcloud(corp,max.words=200,colors=pal)


  

  findFreqTerms(term.matrix,10)
  
  str(terms)
 
top50 <- terms %>% 
  arrange(-freq) %>%
  top_n(50)

highchart() %>% 
  hc_title(text='Top 50 Terms in CJ Dissertations',align='left') %>% 
hc_add_series_labels_values(top50$term,top50$freq,
                            name='term',type='bar',dataLabels=list(enabled=F)) %>% 
  hc_xAxis(categories=top50$term)




  
  
#Graphs
findAssocs(term.matrix,"police",.2)
freq.terms <- findFreqTerms(term.matrix,lowfreq=15)
plot(term.matrix,term=freq.terms,corThreshold = .05,weighting=T)

  
  
#Cluster
tdm2 <- removeSparseTerms(term.matrix,sparse=.994)
  tdm2
  m2 <- as.matrix(tdm2)
  distMatrix <- dist(scale(m2))
  fit <- hclust(distMatrix,method='ward')
  plot(fit)
  rect.hclust(fit,k=6)
  ?plot
  
  
  
  m3 <- t(m2)
  k <-6
  kmeansResult <- kmeans(m3,k)
  round(kmeansResult$centers,digits=3)
  for (i in 1:k) {
    cat(paste("cluster ", i, ": ", sep = ""))
    s <- sort(kmeansResult$centers[i, ], decreasing = T)
    cat(names(s)[1:10], "\n")
  }
  ###Topics
  
  dtm <- as.DocumentTermMatrix(term.matrix)
  rowTotals <- apply(dtm,1,sum)
  dtm <- dtm[rowTotals>0,]
  install.packages('topicmodels')
library(topicmodels)
  
  lda <- LDA(dtm,k=10)
  term <- terms(lda,10)
  term
  
  
  
  
  
  
  
    ####Geo
  options(dplyr.print_max=50)

  
  gjs <- g %>% 
    select(inst,lat,lon) %>% 
    filter(!lon=='NA') %>%
    filter(lon < -5) %>% 
    group_by(inst,lon=round(lon,1),lat=round(lat,1)) %>% 
    summarise(num=n()) %>% 
    ungroup() %>% 
    arrange(inst)
  gjs
  
  inst <- g %>% 
    select(inst,lat,lon) %>% 
    group_by(inst,lon=round(lon,1),lat=round(lat,1)) %>% 
    
  head(inst)
gjs <- left_join(gjs,inst)
head(gjs)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(lng=~lon,lat=~lat,radius=~num/2,data=gjs,
                   popup=~inst)
  


  
  
  


