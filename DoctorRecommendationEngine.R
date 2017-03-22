#!/usr/bin/env Rscript

# args <- commandArgs(TRUE)
# specality <- ''
# location <- ''
# if (length(args)==0) {
#   specality ='gynecologist'
#   location = 'mumbai'
# } else if (length(args)>1) {
#   specality = paste0(args[1],'')
#   location = paste0(args[2],'')
# }

require(rvest) || install.packages("rvest")
library("rvest")

require(stringr) || install.packages("stringr")
library(stringr)

require(RSelenium) || install.packages("RSelenium")
library(RSelenium)
require(qdap) || install.packages("qdap") # ensure java is up to date!
library(qdap)
require(dplyr) || install.packages("dplyr")
library(dplyr)
require(RCurl) || install.packages("RCurl")
library(RCurl)
require(text2vec) || install.packages("text2vec")
library(text2vec)
require(data.table) || install.packages("data.table")
library(data.table)
require(stringr) || install.packages("stringr")
library(stringr)
require(tm) || install.packages("tm")
library(tm)
require(RWeka) || install.packages("RWeka")
library(RWeka)
require(tokenizers) || install.packages("tokenizers")
library(tokenizers)
require(slam) || install.packages("slam")
library(slam)
require(plyr) || install.packages("plyr")
library(plyr)

text.clean = function(x)                    # text data
{ require("tm")
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  # x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}
#Check for Selenium server 
checkForServer() 

sel <- startServer(javaargs = c("-Dwebdriver.gecko.driver=\"C:/geckodriver.exe\""),
                   invisible = F)

remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4444,
                      browserName = "firefox",
                      extraCapabilities = list(marionette = TRUE))
#Open server
remDr$open()

i=0
l=10
docsdf <- data.frame()
reviewshelpful <- c()
reviewsrecent <- c()
filename <- paste0(paste0(location,'_'),paste0(specality,'.csv'))
urlbuild = paste0('https://www.practo.com/',paste0(paste0(location,'/'),paste0(specality,'?page=')))


urlmain = paste0(urlbuild,1)
#paste0('https://www.practo.com/mumbai/General%20Physician?page=1')
pagemain = read_html(urlmain)
#https://www.practo.com/mumbai/gynecologist

pagenext = html_text(html_nodes(pagemain,'h4'))
pagenext

Pagenumbers <- as.numeric((strsplit(pagenext[2]," ")[[1]][1]))
Pagenumbers = as.integer(Pagenumbers/10)
Pagenumbers = Pagenumbers+1
docexpyearscap <- list()
j=1
#start navigating pages in Practo
for (j in 1:Pagenumbers){
  #url1 = paste0("https://www.practo.com/mumbai/gynecologist?page=",j)
  url1 = paste0(urlbuild,j)
  if (j == l ){ 
    Sys.sleep(10)
    l=l+10
  }
  
  page1 = read_html(url1)
  docrecommendScore <- c()
  docnames = gsub("\n",'',html_text(html_nodes(page1,'h2')))
  detailnode = html_nodes(page1,'span')
  detailurl = html_attr(detailnode,"href")
  detailids = grep('http',detailurl,perl=TRUE)
  docexpyearscap = text.clean(html_text(html_nodes(page1,'.doc-exp-years')))
  
  docrecommendScore = text.clean(html_text(html_nodes(page1,'.recommend')))
  
  splitforRecmd <- strsplit(docrecommendScore, ' ')
  
  # takes the first/left part of matrix, after the comma
  RealRecommendScore <- trimws(sapply(splitforRecmd, function(x) x[1]))
  
  
  for (p in 1:length(detailids)){
    i=i+1
    urldet = (detailurl[detailids[p]])
    #print(urldet)
    page2 = read_html(paste0(urldet))
    docnames = paste(text.clean(gsub("\n",'',html_text(html_nodes(page2,'h1')))),collapse=" : ")
    docexpYears = text.clean(gsub("\n",'',docexpyearscap[p]))
    speclities = paste(text.clean(html_text(html_nodes(page2,'#infoTab .responsive-hide a'))),collapse=" : ")
    docclinic = paste(text.clean(html_text(html_nodes(page2,'.clinic-address h2'))),collapse=" : ")
    services = paste(text.clean(html_text(html_nodes(page2,'.service-cell'))),collapse=" : ")
    awards = paste(text.clean(html_text(html_nodes(page2,'.awards-block')) ),collapse=" : ")
    specializations  = paste(text.clean(html_text(html_nodes(page2,'.specialties-block')) ),collapse=" : ")
    education = paste(text.clean(html_text(html_nodes(page2,'.qualification-row'))),collapse=" : ")
    registration = paste(text.clean(html_text(html_nodes(page2,'.exp-details'))),collapse=" : ")
    regnumber = paste(text.clean(html_text(html_nodes(page2,'.exp-tenure'))),collapse=" : ")
    #print(urldet)
    membership = paste(text.clean(html_text(html_nodes(page2,'.membership-row'))),collapse=" : ")
    Exp = paste(text.clean(html_text(html_nodes(page2,'.organization-row .exp-details'))),collapse=" : ")
    page3 = read_html(gsub(" ", "",paste(str_trim(paste0(urldet)),'#recommended')))
    page4 = read_html(gsub(" ", "",paste(str_trim(paste0(urldet)),'#recent')))
    remDr$navigate(paste0(gsub(" ", "",paste(str_trim(paste0(urldet)),'#recommended'))))
    l=1
    check = 0
    #Get user reviews 
    if(length(html_text(html_nodes(page3,'.recommended-next-page'))) > 0 ) {
      check = 1
      
      for (var1 in check:l) {
        webElem <-  remDr$findElement(using = 'css selector', '.recommended-next-page')
        Sys.sleep(5)
        if (webElem$elementId !=0 & nchar(webElem$getElementText())>0){
          webElem$clickElement()
          l=l+1
        }
        else{
          check=0
          l=0
        }
      } 
    }
    
    if(length(html_text(html_nodes(page3,'.review-text'))) > 0 ) {
      reviewshelpfulnode <- remDr$findElements(using = 'css selector', ".review-text")
      reviewshelpful <- paste(text.clean(lapply(reviewshelpfulnode, function(x){x$getElementText()})),collapse=" : ")
    }
    else{
      reviewshelpful =""
    }
    
    remDr$navigate(paste0(gsub(" ", "",paste(str_trim(paste0(urldet)),'#recent'))))
    
    q=1
    check=0
    if(length(html_text(html_nodes(page4,'.recent-next-page'))) > 0 ) {
      check =1
      for (var2 in check:q) {
        
        webElem1 <- remDr$findElement(using = 'css selector', '.recent-next-page')
        if (webElem1$elementId!=0 & nchar(webElem1$getElementText()) > 0){
          webElem1$clickElement()
          q=q+1
        } else{
          check=0
          q=0
        }
      }
    }
    
    if(length(html_text(html_nodes(page4,'.less-review'))) > 0 ) {
      reviewsrecentnode <- remDr$findElements(using = 'css selector', ".less-review")
      reviewsrecent<- paste(text.clean(lapply(reviewsrecentnode, function(x){x$getElementText()})),collapse=" : ")
    }  else {
      reviewsrecent = ""
    }
    PractoRecommendScore = RealRecommendScore[p]
    
    docsmoddf = data.frame(docnames,docexpYears,speclities,PractoRecommendScore,Exp,docclinic,services,awards,specializations,regnumber,education,registration,membership,reviewshelpful,reviewsrecent)
    if (i==1) {
      docsdf <- data.frame(docnames,docexpYears,speclities,PractoRecommendScore,Exp,docclinic,services,awards,specializations,regnumber,education,registration,membership,reviewshelpful,reviewsrecent)
    }
    else{
      docsdf = rbind(docsdf,docsmoddf)
    }
    
  }
  
  
  j=j+1
}


docsdf = cbind(docsdf,location)

yearofreg <-  c()
newRegNum <- c()
newRegNum = ifelse(grepl(':',docsdf$regnumber),str_sub(docsdf$regnumber, (sapply(gregexpr("\\:", docsdf$regnumber), tail, 1)+1)),paste(docsdf$regnumber,""))
yearofreg = ifelse(nchar(paste(docsdf$registration))>4 ,paste(str_sub(docsdf$registration,-4,-1)),"")
docsdf = cbind(docsdf,newRegNum,yearofreg)
docsdf[9,15]
#docsdf[9,c("yearofreg")]
docsdf[,c("reviewsrecent")] =  gsub(": :","", gsub("list","",docsdf[,c("reviewsrecent")]))
docsdf[,c("reviewshelpful")] =   gsub(": :","",gsub("list","",docsdf[,c("reviewshelpful")]))
#docsdf = read.csv("cardiologist_chennai_Data.csv")


#Verify the registration number of doctors in Indian Medical Register
z=1
for (z in 1:nrow(docsdf)){
  #nrow(docsdf)){
  #print (z)
  if (length(paste(docsdf[z,c("newRegNum")]))>0 & nchar(paste(docsdf[z,c("newRegNum")]))>1) {
    session <- 'http://www.mciindia.org/InformationDesk/IndianMedicalRegister.aspx'
    
    remDr$navigate(paste0(session))
    #remDr$navigate(paste0('https://www.practo.com/chennai/doctor/m-kathiresan-cardiologist#recent'))
    
    webElem <-  remDr$findElement(using = 'css selector', '#dnn_ctr588_IMRIndex_Link_Advance')
    Sys.sleep(2)
    webElem$clickElement()
    Sys.sleep(5)  
    regnumElem = remDr$findElement(using = 'css selector', '#dnn_ctr588_IMRIndex_Txt_Registration')
    Sys.sleep(3)
    # print (str_trim(docsdf[z,c("newRegNum")]))
    regnumElem$sendKeysToElement(list(str_trim(docsdf[z,c("newRegNum")])))
    Sys.sleep(3)
    medcouncil = remDr$findElement(using = 'css selector', '#dnn_ctr588_IMRIndex_Txt_Year')
    Sys.sleep(3)
    #print (docsdf[z,c("yearofreg")])
    medcouncil$sendKeysToElement(list(str_trim(docsdf[z,c("yearofreg")])))
    regnumElem = remDr$findElement(using = 'css selector', '#dnn_ctr588_IMRIndex_Txt_Registration')
    Sys.sleep(5)
    submitbutn =   remDr$findElement(using = 'css selector', '.Button')
    Sys.sleep(3)
    submitbutn$clickElement()
    #tabelforregnum <- data.frame(col.names=c("Sno","year","RegnumMain","StateCouncil","Name","Father","Action"))
    Sys.sleep(5)
    regnumbertable = lapply(remDr$findElements(using = 'css selector', ".row td"), function(x){x$getElementText()})
    Sys.sleep(3)
    tabelforregnum <- as.data.frame(regnumbertable)
    Sys.sleep(5)
    if(ncol(tabelforregnum)>2){
      listval = paste(tabelforregnum[,3],collapse = ':')
      regnumfordoc = docsdf[z,c("newRegNum")]
      #str_trim(regnumfordoc) %in% c(listval)
      
      ScoreOnValidRegNum = ifelse(str_trim(regnumfordoc) %in% c(listval),1,0) 
      docsdf[z,c("ValidReg")] = ScoreOnValidRegNum
      #print(docsdf[z,c("ValidReg")])
      
    } else { 
      docsdf[z,c("ValidReg")] = 0
    }
  }
  else {
    
    docsdf[z,c("ValidReg")] = 0
    
    
  }
  z=z+1
  
}

#Write the doctor details in File Location_specalization(e.g mumbai_gynecologist.csv)
write.csv(docsdf,file = filename)

Hospital <- c()
#Get top 10 hospitals data of that city from Just Dail website
urljusdail = paste0(paste0("https://www.justdial.com/", location),"/Hospitals/ct-303527")
#urljusdail = paste0('https://www.justdial.com/Mumbai/Dental-Hospitals/ct-1000179114')
pgJD = read_html(urljusdail)

#remDr$setWindowSize(width = 800, height = 300)
Sys.sleep(2)

remDr$navigate(paste0(urljusdail))
Sys.sleep(3)

webElemJD <-  remDr$findElement(using = 'css selector', 'li[id=rating]')
Sys.sleep(4)
webElemJD$clickElement()
Sys.sleep(4)
# webElemJD$sendKeysToElement(list(key = "end"))
# Sys.sleep(2)
# webElemJD$clickElement()
# Sys.sleep(3)
HospitalElem = remDr$findElement(using = 'css selector', '.jcn a')
Hospital = tolower(gsub('list','',text.clean(lapply(remDr$findElements(using = 'css selector', ".jcn a"), function(x){x$getElementText()}))))
HospitalNamesDF <- as.data.frame(Hospital)
hospitalFile <- paste0(paste0('HospitalNames',location),'.csv')
#Save the data in HospitalNamesLocation(e,g HospitalNamesMumbai.csv)

write.csv(HospitalNamesDF,hospitalFile)
#read the data in HospitalNamesLocation(e,g HospitalNamesMumbai.csv)

HospitalNamesDF <- read.csv(hospitalFile)
#'HospitalNamesMumbai.csv')
docsdf <- read.csv(filename)
##polarity score on recent and old reviews

ReviewsHelpAll <- c()
ReviewsRecentAll <- c()
ReviewsHelpAll = text.clean(gsub(":", " ",docsdf$reviewshelpful))
ReviewsRecentAll = text.clean(gsub(":", " ",docsdf$reviewsrecent))
poliarityScoreHelpful <- c()
poliarityScoreHelpful = ""
poliarityScoreRecent <- c()
poliarityScoreRecent = ""
docsdf <- cbind(docsdf,ReviewsHelpAll,ReviewsRecentAll,poliarityScoreHelpful,poliarityScoreRecent)
docsdf[,c('poliarityScoreHelpful')] = sapply(docsdf[,c('poliarityScoreHelpful')],as.character)
docsdf[,c('poliarityScoreRecent')] = sapply(docsdf[,c('poliarityScoreRecent')],as.character)

##clean text

reviewscat <- c('ReviewsHelpAll','ReviewsRecentAll')
polarityscorecat <- c('poliarityScoreHelpful','poliarityScoreRecent')
var2 = 1
for (var2 in 1:2){
  reviewstring <-   reviewscat[var2]
  f=1
  for (f in 1:nrow(docsdf)){
    polaritystring = polarityscorecat[var2]
    data = data.frame(id = 1:length(docsdf[f,c(reviewstring)]), text = docsdf[f,c(reviewstring)], stringsAsFactors = F)
    x = paste(docsdf[f,c(reviewstring)],"")
    if(nchar(x)>1 & !is.na(x)){
      tok_fun = word_tokenizer
      it_m = itoken(x,
                    # preprocessor = text.clean,
                    tokenizer = tok_fun,
                    ids = data$ids,
                    progressbar = F)
      
      vocab = create_vocabulary(it_m)
      #,
      #ngram = c(2L, 2L))
      #stopwords = stopwords
      #)
      pruned_vocab = prune_vocabulary(vocab,
                                      term_count_min = 1)
      # doc_proportion_max = 0.5,
      # doc_proportion_min = 0.001)
      
      vectorizer = vocab_vectorizer(pruned_vocab)
      
      dtm_m  = create_dtm(it_m, vectorizer)
      
      dtm = as.DocumentTermMatrix(dtm_m, weighting = weightTf)
      a0 = (apply(dtm, 1, sum) > 0)   # build vector to identify non-empty docs
      dtm = dtm[a0,]                  # drop empty docs
      
      
      
      #--------------------------------------------------------#
      #             Sentiment Analysis                         #
      #--------------------------------------------------------#
      
      
      x1 = x[a0]    # remove empty docs from corpus
      
      
      pol = polarity(x1)         # Calculate the polarity from qdap dictionary
      wc = pol$all[,2]                  # Word Count in each doc
      val = pol$all[,3]                 # average polarity score
      p  = pol$all[,4]                  # Positive words info
      n  = pol$all[,5]                  # Negative Words info 
      docsdf[f,c(polaritystring)] = paste(val,"")
      #print(val)
    }
    
    f = f+1
  }
  var2 = var2+1
}

##Education weightage
#in case of mbbs score: 0.25 ; MD/ms : 0.5 ; DM/mch/dnb: 0.75 ;fellowship/fnb/dnb/frcp 0.95
scoreEducation <- c()
match1 <- c()
match1 <- sapply(gregexpr(paste(c('fellow','fnb ','frcp '),collapse = "|"),tolower(docsdf$education),perl=TRUE), function(m) m[length(m)])
match2 <- sapply(gregexpr(paste(c('dm ','mch ','dnb '),collapse = "|"),tolower(docsdf$education),perl=TRUE), function(m) m[length(m)]) 
match3 <- sapply(gregexpr(paste(c('md ','ms ','mds '),collapse = "|"),tolower(docsdf$education),perl=TRUE), function(m) m[length(m)]) 
match4 <- sapply(gregexpr(paste(c('mbbs ','bds '),collapse = "|"),tolower(docsdf$education),perl=TRUE), function(m) m[length(m)]) 

scoreEducation = ifelse(match1>0 ,0.95,ifelse(match2>0,0.75,ifelse(match3>0,0.5,ifelse(match4>0,0.25,0))))
docsdf$scoreEducation = scoreEducation
#docsdf[,c('scoreEducation')] = scoreEducation
##Number of Years weightage
#experience
expInYears = gsub("[^0-9]", "",docsdf$docexpYears)
expInYears = ifelse(expInYears>50 & scoreEducation > 0.5,1,ifelse(expInYears>50 & scoreEducation > 0.25,0.75,ifelse(expInYears>40 & scoreEducation >0.5,0.95,
                                                                                                                    ifelse(expInYears>40 & scoreEducation>0.25,0.5,ifelse(expInYears>30 & scoreEducation>0.5,0.95,ifelse(expInYears>30 & scoreEducation>0.25,0.75,ifelse(expInYears>20 & scoreEducation> 0.5,0.95,ifelse(expInYears>20 & scoreEducation>0.25,0.75,
                                                                                                                                                                                                                                                                                                                         ifelse(expInYears>15 & scoreEducation>0.5,0.75,ifelse(expInYears>15 & scoreEducation>0.25,0.5,
                                                                                                                                                                                                                                                                                                                                                                               ifelse(expInYears>10 & scoreEducation>0.5,0.75,ifelse(expInYears>10 & scoreEducation>0.25,0.5,
                                                                                                                                                                                                                                                                                                                                                                                                                                     ifelse(expInYears>5 & scoreEducation>0.5,0.5,0.25)))))))))))))
docsdf$expInYears = expInYears

##Doctors working in Famous hospitals considered for high rating
Tophospmatch <- c()
hospitalDF <- read.csv(hospitalFile)
hospitalNames = paste(hospitalDF$Hospital,collapse='|')

#strsplit(paste(hospitalDF$Hospital,collapse = ' ')," +")
match4 <- sapply(gregexpr(paste(c('award','medal','record holder'),collapse = "|"),tolower(gsub('awards and recognitions','',docsdf$awards)),perl=TRUE), function(m) m[length(m)]) 
match5 <- sapply(gregexpr(paste(c('paper','thesis'),collapse = "|"),tolower(gsub('awards and recognitions','',docsdf$awards)),perl=TRUE), function(m) m[length(m)]) 
match6 <- sapply(gregexpr(paste(c('president '),collapse = "|"),tolower(gsub('awards and recognitions','',docsdf$awards)),perl=TRUE), function(m) m[length(m)]) 


hospitalNames =  gsub(' ','',gsub('clinic|solutions|dental|speciality|multispeciality|hospital|hos','',hospitalNames))
Tophospmatch <- sapply(gregexpr(c(hospitalNames),tolower(docsdf$docclinic),perl=TRUE), function(m) m[length(m)]) 
Tophospmatch = ifelse(Tophospmatch>0,1,ifelse(Tophospmatch<=0 & match4>0,0.75,ifelse(Tophospmatch<=0 & match5>0,0.5,ifelse(match6>0,0.25,0)))) 
docsdf[,c('Tophospmatch')] = Tophospmatch

finalPolScore = ifelse(docsdf$poliarityScoreRecent != 0,docsdf$poliarityScoreRecent,docsdf$poliarityScoreHelpful)
docsdf$finalPolScore = finalPolScore
#docsdf[,c('finalPolScore')] =finalPolScore
write.csv(docsdf,file = filename)
#Final ranking dataframe
FinalDFForRanking <- data.frame()
FinalDFForRanking = as.data.frame(cbind(paste(docsdf$location,'') ,paste(docsdf$docnames,''),docsdf$scoreEducation,paste(docsdf$expInYears,''),docsdf$Tophospmatch,paste0(docsdf$finalPolScore,''),paste(docsdf$PractoRecommendScore,'')))
colnames(FinalDFForRanking) <- c("location","DocName","EducationScore","ExpInYears","TopHospitalWork","PolScore","PractoScore")

##Rscore calculation for Education 
meanEducScore = mean(ifelse(is.na(str_trim(FinalDFForRanking$EducationScore)),0,FinalDFForRanking$EducationScore))

#ISGEducScore = (meanEducScore-0.5)/0.14
SDEducScore = sd(ifelse(is.na(str_trim(FinalDFForRanking$EducationScore)),0,FinalDFForRanking$EducationScore))
NumberofDocs = nrow(FinalDFForRanking)
zscoreEdu = ((ifelse(is.na(str_trim(FinalDFForRanking$EducationScore)),0,FinalDFForRanking$EducationScore))-meanEducScore)/SDEducScore
RScoreEdu = (zscoreEdu+5)*5

##Rscore calculation for Experience 

meanExpScore = mean(ifelse(is.na(str_trim(FinalDFForRanking$ExpInYears)),0,FinalDFForRanking$ExpInYears))

#ISGExpScore = (meanExpScore-1.5)/1.4
SDExpScore = sd(ifelse(is.na(str_trim(FinalDFForRanking$ExpInYears)),0,FinalDFForRanking$ExpInYears))
zscoreExp = ((ifelse(is.na(str_trim(FinalDFForRanking$ExpInYears)),0,FinalDFForRanking$ExpInYears))-meanExpScore)/SDExpScore
RScoreExp = (zscoreExp+5)*5


##Rscore calculation for TopHospitalWork 

meanHosScore = mean(ifelse(is.na(str_trim(FinalDFForRanking$TopHospitalWork)),0,FinalDFForRanking$TopHospitalWork))

#ISGHosScore = (meanHosScore-1.5)/1.4
SDHosScore = sd(ifelse(is.na(str_trim(FinalDFForRanking$TopHospitalWork)),0,FinalDFForRanking$TopHospitalWork))
zscoreHos = ((ifelse(is.na(str_trim(FinalDFForRanking$TopHospitalWork)),0,FinalDFForRanking$TopHospitalWork))-meanHosScore)/SDHosScore
RScoreHos = (zscoreHos+5)*5

##Rscore calculation for Polarity Score 

meanPolScore = mean(ifelse(is.na(str_trim(FinalDFForRanking$PolScore)),0,FinalDFForRanking$PolScore))

#ISGHosScore = (meanHosScore-1.5)/1.4
SDPolScore = sd(ifelse(is.na(str_trim(FinalDFForRanking$PolScore)),0,FinalDFForRanking$PolScore))
zscorePol = ((ifelse(is.na(str_trim(FinalDFForRanking$PolScore)),0,FinalDFForRanking$PolScore))-meanPolScore)/SDPolScore
RScorePol = (zscorePol+5)*5

ScoreDataFrame <- data.frame(cbind(paste(docsdf$location,''),paste(docsdf$docnames,''),RScorePol,RScoreHos,RScoreExp,RScoreEdu,paste(docsdf$PractoRecommendScore,'')) )
colnames(ScoreDataFrame) <- c("location","DocName","RScorePol","RScoreHos","RScoreExp","RScoreEdu","PractoRank")
ScoreDataFrame[,3:6] <- lapply(ScoreDataFrame[,3:6], function(x) as.numeric(as.character(x)))

ScoreDataFrame$RScoremean <- rowMeans(ScoreDataFrame[,3:6], na.rm=TRUE)

FinalRankingDF <- 
  ScoreDataFrame %>%
  #group_by(location, DocName) %>% 
  mutate(PercentRank=rank(RScoremean)/length(RScoremean))

FinalRankingDF$PercentRank <- (FinalRankingDF$PercentRank)*100
FinalRankingDF$PractoRank <- ScoreDataFrame$PractoRank

Ranking <- FinalRankingDF

Ranking = Ranking[,c('location','DocName','PercentRank')]
Ranking$RegistrationVerfied <- gsub('0','N',gsub('1','Y',ifelse(is.na(docsdf$ValidReg),0,docsdf$ValidReg)))
Ranking$Specalist_In <- docsdf$speclities
Ranking$Hospital_Associated <- docsdf$docclinic

showtop15 <- head(arrange(Ranking,desc(PercentRank)), n = 15)
showtop15
resultfilename = paste0('Top15_',filename)
#Write the final top 15 result in file Top10_FileName(e.g Top10_mumbai_gynecologist.csv)
write.csv(showtop15,resultfilename)