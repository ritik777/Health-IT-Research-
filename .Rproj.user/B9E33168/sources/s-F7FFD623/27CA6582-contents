# Collecting the Data 

#Admnistrative - Health Deficiences
#search for a deficiency number in Deficiency_ dataset
#MDS Quality Measures


#number, name, date, Deficiency number, Deficiency Description, corrected, severity code

library(dplyr)
library(ggplot2)
attach(Health_Deficiencies)
Deficiency_1 <- Health_Deficiencies %>% 
  select(Federal.Provider.Number,Provider.Name,Survey.Date,Deficiency.Tag.Number,Deficiency.Category,Deficiency.Description,Deficiency.Corrected,Scope.Severity.Code,Correction.Date)

# grouping deficiency category
p <- Deficiency_1 %>% group_by(Deficiency.Category) %>% summarise(number = n())
p <- data.frame(p)
p
#charting deficiency categories
charting_categories<-ggplot(p, aes(x=Deficiency.Category, y=number, fill = Deficiency.Category)) +
  geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 15))
charting_categories

# Key Opportunities in Health IT : Source Wikipedia
#health monitoring and diagnosis
#medical treatment and patient care
#pharmaceutical research and development
#clinic performance optimization[29]

#Types of Technology
#electronic medical records (EMR) 
#clinical decision support (CDS)
#computerized physician order entry (CPOE).
#They further defined applications for dispensing to include bar-coding at medication dispensing (BarD) 
#robot for medication dispensing (ROBOT)
#automated dispensing machines (ADM)
#electronic medication administration records (eMAR) (Administrative)
#bar-coding at medication administration (BarA or BCMA).

#Text Analysis of Administrative services
# EMR, CDS, CPOE, eMAR, BarA can be linked to administrative services 


text_quality <- Deficiency_1 %>% filter(Deficiency.Category == 'Administration Deficiencies') %>%  select(Deficiency.Description)
head(text_quality)


# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(text_quality$Deficiency.Description))
docs

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Text Analysis of Residents Rights DEficiencies



text_quality <- Deficiency_1 %>% filter(Deficiency.Category == 'Quality of Life and Care Deficiencies') %>%  select(Deficiency.Description)
head(text_quality)

#Takeaway comments: Implement a program that monitors antibiotic use


# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(text_quality$Deficiency.Description))
docs

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

text_quality <- Deficiency_1 %>% filter(Deficiency.Category == 'Environmental Deficiencies') %>%  select(Deficiency.Description)
head(text_quality)

#Takeaway comments: Implement a program that monitors antibiotic use


# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

docs <- Corpus(VectorSource(text_quality$Deficiency.Description))
docs

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Nursing Homes with Administrative Deficiencies. 
Administrative_deficient_Nursing_homes <-Health_Deficiencies %>%   filter(Deficiency.Category=="Administration Deficiencies")

Administrative_deficient_Nursing_homes <- Administrative_deficient_Nursing_homes %>% 
  select(Federal.Provider.Number,Provider.Name,Survey.Date,Deficiency.Tag.Number,Deficiency.Category,Deficiency.Description,Deficiency.Corrected,Scope.Severity.Code,Correction.Date)

#filtering out only deaths from death_20
deaths <- death_20 %>% select(`Federal Provider Number`,total_deaths,State,Geolocation)
names(deaths)[names(deaths) == "Federal Provider Number"] <- "Federal.Provider.Number"
deaths_where_administratative_deficient <-  merge(x=Administrative_deficient_Nursing_homes,y=deaths,by="Federal.Provider.Number")

# removing duplicates


deaths_where_administratative_deficient <- distinct(deaths_where_administratative_deficient)

write.csv(deaths_where_administratative_deficient, "deaths_where_administrative_deficient_complaints.csv")


