##Script final DDO 

###Instação de pacotes
install.packages("ROAuth")        ##Provides an interface to the OAuth 1.0 specification allowing users to authenticate via OAuth to the server of their choice.
install.packages("httr")          ##Useful tools for working with HTTP organised by HTTP verbs (GET(), POST(), etc). Configuration functions make it easy to control additional request components (authenticate(), add_headers() and so on).
install.packages("base64enc")     ##This package provides tools for handling base64 encoding. It is more flexible than the orphaned base64 package.
install.packages("tm")            ##A framework for text mining applications within R.
install.packages("SnowballC")     ##An R interface to the C libstemmer library that implements Porter's word stemming algorithm for collapsing words to a common root to aid comparison of vocabulary.
install.packages("twitteR")       ##Provides an interface to the Twitter web API.
install.packages("wordcloud")     ##Pretty word clouds.
install.packages("Rstem")         ##An R interface to the C code that implements Porter's word stemming algorithm for collapsing words to a common root to aid comparison of texts.
install.packages("sentiment")     ## sentiment is an R package with tools for sentiment analysis including bayesian classifiers for positivity/negativity and emotion classification.
install.packages("XML")           ##Many approaches for both reading and creating XML (and HTML) documents (including DTDs), both local and accessible via HTTP or FTP.
install.packages("RColorBrewer")  ##Provides color schemes for maps (and other graphics)
install.packages("ggmap")         ##A collection of functions to visualize spatial data and models on top of static maps from various online sources (e.g Google Maps and Stamen Maps).
install.packages("rworldmap")     ##Enables mapping of country level and gridded user datasets.
install.packages("data.table")    ##Fast aggregation of large data (e.g. 100GB in RAM), fast ordered joins, fast add/modify/delete of columns by group using no copies at all, list columns and a fast file reader (fread). Offers a natural and flexible syntax, for faster development.

library(ROAuth)
library(httr)
library(base64enc)
library(tm)
library(SnowballC)
library(twitteR)
library(wordcloud)
library(Rstem)
library(sentiment)
library(XML)
library(RColorBrewer)
library(ggmap)
library(rworldmap)
library(data.table)
require(twitteR)
##############################################################################################################################
######################################################################
##############Aquisição e pré-processamento de Dados#################
######################################################################

###############Autenticação da Api do TWitter########################
#1. Localizar configuraÃ§Ãµes de OAuth para twitter (Cache) 
#2. Registrando a aplicaÃ§Ã£o do twitter -> api.twitter.com
#3. Get Credenciais OAuth (usuário testKYC)
#4. Aplicando a API 
consumer_key <- "ZY8k4k2wfCpWtfcfh8NVBTieR"
consumer_secret <- "Z1LzewzaICCQEaMYI2Qy0SuLmydGoGIvEK3bEVDPauHp5xCiNn"
access_token <- "703023194542874624-jeEovmyqHy1Uo2e3jx2f2Wwef0vhFYD"
access_secret <- "pvVL60ChJLm16YbEbwhYhGzwZg5OUCJeXagkcdYbrGCr9"

## Autenticando no Twitter
setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)

#df2 = cancer_mama, 2426
#df3 = cancer_diagnostico, 68
#df4 = cancer_cirurgia, 184
#df5 = leucemia, 377
#df6 = ulcera, 132
#df7 = calculo_renal, 41
#df8 = transplante+rim, 17
#df9 = colesterol+exame, 15
#df10 = cornea+cirurgia, 5
#df11 = quimioterapia, 259
#df12 = hemodialise, 207


## Busca Twitter
tweets = searchTwitter("cancer+mama", n=3000, lang="pt",geocode ="-8.328880,-51.459632,3300km")

##Conversion to data.frames
df2 <- twListToDF(tweets)

View(df2)
length(df12$text)

#unindo Datasets
arquivo <- rbind(df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
View(arquivo)
length(arquivo$text)


write.table (arquivo,"C:/Users/manoe/Documents/MANOEL/R/Script_Final/Datasets/Dataset_unido_final/dataset_final.csv", sep=";", row.names = FALSE)



#Recarregar dados do Arquivo csv
arquivo <- read.csv("C:/Users/manoe/Documents/MANOEL/R/Script_Final/Datasets/Dataset_unido_final/Tweets.csv", header=TRUE, sep=";")
getwd()

View(arquivo)


##Selecionando apenas os campos que iremos utilizar 


arquivo$favorited     <- NULL
arquivo$favoriteCount <- NULL
arquivo$replyToSN     <- NULL
arquivo$truncated     <- NULL
arquivo$replyToSID    <- NULL
arquivo$id            <- NULL
arquivo$replyToUID    <- NULL
arquivo$statusSource  <- NULL
arquivo$retweetCount  <- NULL
arquivo$isRetweet     <- NULL
arquivo$ retweeted    <- NULL
arquivo$X <- NULL

View(arquivo)

arquivo1 <- arquivo

write.table (arquivo,"C:/Users/manoe/Documents/MANOEL/R/Script_Final/Datasets/Dataset_unido_final/dataset_final_limpo.csv", sep=";")

# Verificar os atributos do tweet e o tipo
sapply(arquivo,class)


# Verificar os atributos dos Campos (Alterar o tipo de Campo)
arquivo$text = as.character(arquivo$text)
arquivo$screenName = as.character(arquivo$screenName)
arquivo$created = as.Date(arquivo$created)
arquivo$longitude = as.numeric(as.character(arquivo$longitude))
arquivo$latitude = as.numeric(as.character(arquivo$latitude))
sapply(arquivo ,class) 
View(arquivo) 

##############################################################################################################################
######################################################################
###############Pre-processamento dos dados############################
######################################################################



View(arquivo) 

#########################################
####removendo palavras desnecessárias####

##substitui caracteres por nada (para limpeza do texto)
for(j in seq(arquivo$text))   
{ 
  arquivo$text[[j]] <- gsub("RT", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("RT @[a-z,A-Z]*:", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("/", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("/", " ", arquivo$text[[j]])   
  arquivo$text[[j]] <- gsub("@", " ", arquivo$text[[j]])   
  arquivo$text[[j]] <- gsub("\\|", " ", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("eduaubdedubu", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("achoo", "acho", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aaaaah", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("zzzz", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("kkkkk", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("whoiscarlos", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("whotramontini", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("yasmiimmendys", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("youtube", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("tuts", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("transe", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aff", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("affff", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("ahuahua", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("areduaubdedubueduaubdedubueduaubdedubuc", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("atacandoeduaubdedububeduaubdedubu", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("atacandoeduaubdedububeduaubdedubu", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("bbgirafa", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("bdbabygirl", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("beat", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("bugada", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("byatherolly", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("crl", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("crlloira", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("crushney", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub(" deixaeduaubdedubauabeduaubdedubu", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("deussss", "deus", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("dormi[rrr]", "dormir", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("dormirrrrrrr", "dormir", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("dormirto", "dormir", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eaequel", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubaedubcuua", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubaedubeu", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubedubcua", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubedubcub", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubedubcubeduaeubedubcubeduaeubedubcub", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubedubdueduaeubedubdueduaeubedubcua", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubedubu", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeubedubuae", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaeube[a-z,A-Z]*", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduau[a-z,A-Z]*", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eduaubd[a-z,A-Z]*", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("ed", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("zzzz", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("https[a-z,A-Z]*", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("happybdaypelanza", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("sdds", "saudades", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("eeeh[a-z,A-Z]*", "", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("uau", "", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("happybdaypelanza", "", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("?[?*]", "?", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("<ed><U+00A0>[a-z,A-Z]*", "", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("eduaubdedubu", "", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("<", "", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("eduaubdedubuaeduaubdedubu", "", arquivo$text[[j]]) 
  arquivo$text[[j]] <- gsub("eduaubdedubub", "", arquivo$text[[j]])
} 



View(arquivo)

##(Conversao+Tolower+removePontuaÃ§Ã£o+RemoveNumeros+RemoveStopWords)
arquivo$text <- iconv(arquivo$text,to="ASCII//TRANSLIT")

#Manipula Texto (Letra Minúscula)
arquivo$text <- tolower(arquivo$text )

#Transforma em um Corpus
myCorpus <- Corpus(VectorSource(arquivo$text))


##inspect(myCorpus[1:3])
writeLines(as.character(myCorpus))


#Manipula Texto (RemovePontuação)
arquivo$text  <- removePunctuation(arquivo$text )

#Manipula Texto (RemoveNúmeros)
arquivo$text  <- removeNumbers(arquivo$text )


myCorpus <- Corpus(VectorSource(arquivo$text))

# Construindo uma matriz de documentos versus termos:
docs_term <- DocumentTermMatrix(myCorpus)
docs_term
inspect(docs_term)
findFreqTerms(docs_term,lowfreq=20)

# Construindo uma matriz de termos e cada tweet que aparece
term_doc <- TermDocumentMatrix(myCorpus)
term_doc
inspect(term_doc)
findFreqTerms(term_doc,lowfreq=20)

#Stopwords - Remover StopWords
arquivo$text  <- removeWords(arquivo$text , stopwords('portuguese'))

#Steaming - reduz ao Radical
arquivo$text  <- stemDocument(arquivo$text , language = "portuguese")

View(arquivo)


####removendo palavras desnecessárias####
for(j in seq(arquivo$text))   
{ 
  arquivo$text[[j]] <- gsub("vxkeskhzx", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("yzrle", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("vxkeskhzx", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aaaaaaaaaaa", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aaaaaaaaaaaya", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aaaaaaaaaaaa", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aaaaaaaaaaaaaaaaaaaaaaaa", " ", arquivo$text[[j]])
  arquivo$text[[j]] <- gsub("aaaocar", " ", arquivo$text[[j]])
}

View(arquivo)


#Gravando Dataset Tratado
write.table (arquivo,"C:/Users/manoe/Documents/MANOEL/R/Script_Final/Datasets/Dataset_unido_final/teste.csv", sep=";")

##############################################################################################################################
######################################################################
###############API do GOOGLE MAPS#####################################
######################################################################

###################Tratando as informações do usuario para###########
###################conseguir sua latitude e longitude################
###################na API DO GOOGLE MAPS#############################

#Recarregar dados do Arquivo csv
arquivo_tratado <- read.csv("C:/Users/manoe/Documents/MANOEL/R/Script_Final/Datasets_Tratado/teste.csv", header=TRUE, sep=";")
getwd()

sapply(arquivo_tratado ,class)

# Verificar os atributos dos Campos (Alterar o tipo de Campo)
arquivo_tratado$text = as.character(arquivo_tratado$text)
arquivo_tratado$screenName = as.character(arquivo_tratado$screenName)
arquivo_tratado$created = as.Date(arquivo_tratado$created)
sapply(arquivo_tratado ,class)
View(arquivo_tratado)

##LookupUser pega as informações do perfil de cada pessoa, dos twittes
arquivo <- lookupUsers(arquivo_tratado$screenName)
df1 <- twListToDF(arquivo)
View(df1)


df1$location <- gsub("%", " ",df1$location)
df1$location

df1$description <- NULL
df1$statusesCount <- NULL
df1$followersCount <- NULL
df1$favoritesCount <- NULL
df1$friendsCount <- NULL
df1$url <- NULL
df1$name <- NULL
df1$created <- NULL
df1$protected <- NULL
df1$verified <- NULL
df1$lang <- NULL
df1$id <- NULL
df1$listedCount <- NULL
df1$followRequestSent <- NULL
df1$profileImageUrl <- NULL

View(df1)
View(arquivo_tratado)

#Unificando datasets

df <- merge(arquivo_tratado, df1, by="screenName")
View(df)



###API DO GOOGLE MAPS

idclient_key <- "898000346432-uohb0rjrrc69tj40oapkbh2tkslhb3dt.apps.googleusercontent.com" 
consumer_secret <- "KTvluGzCdFXFamFwlluVYk9s"
access_token <- "AIzaSyAH854zfvnAuYgYa1aPJyL3gvK7sGnVgVU" 
access_secret <- "afdae08feeb4d75c70f2a4dc63fbc162679b1164" 

api_key<- "AIzaSyDZ_EechmOQXEo-6L4-jAIEZFnMsvc-Tmc" 





#Install key package helpers:
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
#Install modified version of the geocode function
#(that now includes the api_key parameter):
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")

geocode_apply<-function(x){
  geocode(x, source = "google", output = "all", api_key="AIzaSyAH854zfvnAuYgYa1aPJyL3gvK7sGnVgVU")}

#realiza as pesquisas da localização no google maps
geocode_results <- sapply(df$location, geocode_apply, simplify = F)

##mostra total de resultados retornados

length(geocode_results)

geocode_results

##limpando os resultados do geocoding

##API do google maps analisa se o geocode foi retornado com éxito

condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results <- geocode_results[condition_a]

##elimina locais ambiguos ou que não existem

condition_b <- lapply(geocode_results, lapply, length)
condition_b2<-sapply(condition_b, function(x) x["results"]=="1")
geocode_results<-geocode_results[condition_b2]
length(geocode_results)

geocode_results

###http://www.r-bloggers.com/mapping-twitter-followers-in-r/
###One other check you'll have to do is to look for misformatted entries (a potential issue when simultaneously 
##dealing with addresses from across the world) and fix them when found. To help keep things clean, I posted a 
##cleaning script on Github that you can just run yourself.

source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")

###transformando os resultados misturados em um dataframe
results_b<-lapply(geocode_results, as.data.frame)

results_c<-lapply(results_b,function(x) subset(x, select=c("results.formatted_address",
                                                           "results.geometry.location")))

results_c

##Formata corretamente as colunas latitude e longitude
results_d<-lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                   lat=x[1,"results.geometry.location"],
                                                   lng=x[2,"results.geometry.location"]))
##para unir as colunas e um dataset
results_e<-rbindlist(results_d)

results_e
View(results_e)

##Tratando as informaçãoes do google Maps

View(df)
View(results_e)

#Manipula Texto (Letra Minúscula)
df$location <- tolower(df$location)
results_e$Location <- tolower(results_e$Location) 

View(df)
View(results_e)

for(j in seq(results_e$Location))   
{
  results_e$Location[[j]] <- gsub("mossorã³, mossorã³ - state of rio grande do norte, brazil", "mossoro", results_e$Location[[j]])
  results_e$Location[[j]] <- gsub("rio de janeiro, rio de janeiro - state of rio de janeiro, brazil", "rio de janeiro", results_e$Location[[j]])
  results_e$Location[[j]] <- gsub("pres. epitã¡cio, pres. epitã¡cio - sp, brazil", "são paulo", results_e$Location[[j]])
  results_e$Location[[j]] <- gsub("brazil", "brasil", results_e$Location[[j]])
}

View(results_e)

#Alterando nomes das Colunas
names(results_e)[1] <- paste("location")
names(results_e)[2] <- paste("lati")
names(results_e)[3] <- paste("long")


####Unindo o Geocode do google maps, com o dataset principal

Dataset_limpo <- merge(df, results_e, by = "location")
View(Dataset_limpo)

sapply(Dataset_limpo ,class)

Dataset_limpo$longitude <- as.numeric(Dataset_limpo$longitude)
Dataset_limpo$latitude <- as.numeric(Dataset_limpo$latitude)

sapply(Dataset_limpo ,class)

for(j in seq(Dataset_limpo$longitude))   
{
  Dataset_limpo$longitude[[j]] <- gsub(is.na, Dataset_limpo$long, Dataset_limpo$longitude[[j]])
}





#Gravando Dataset com o Geocoding do google
write.table (results_e,"C:/Users/manoe/Documents/MANOEL/R/Script_Final/Dados_GoogleMaps/matriz_geocoding.csv", sep=";")


##############################################################################################################################
######################################################################
###############Análise Exploratoria###################################
######################################################################

#Recarregar dados do Arquivo csv
arquivo_tratado <- read.csv("C:/Users/manoe/Documents/MANOEL/R/Script_Final/Datasets_Tratado/calculo_renal.csv", header=TRUE, sep=";")
getwd()

sapply(arquivo_tratado ,class)

# Verificar os atributos dos Campos (Alterar o tipo de Campo)
arquivo_tratado$text = as.character(arquivo_tratado$text)
arquivo_tratado$screenName = as.character(arquivo_tratado$screenName)
arquivo_tratado$created = as.Date(arquivo_tratado$created)
sapply(arquivo_tratado ,class)
View(arquivo_tratado)


wordcloud(arquivo_tratado$text, scale = c(8,0.5), max.words = 3000, random.order = FALSE,
          rot.per = 0.55, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
