


#parse game ids from bgg database source code to generate a list of all game ids



#https://boardgamegeek.com/browse/boardgame/page/1, 928 pages total


library(RCurl)
library(XML)
library(xml2)
library(dplyr)
library(rvest)
library(reshape)


#
# get all boardgame ids
#


ids <- data.frame(matrix(0,1,1))
names(ids) <- c("unique.unlist.id.list..")
for(i in 1:928){
  
  #i=3
  url <- sprintf("https://boardgamegeek.com/browse/boardgame/page/%i",i)
  html <- getURL(url, followlocation = TRUE)
  id.list <- list()
  while (regexpr("boardgame/[[:digit:]]{3,10}/[a-z]", html) > 0) {
    id.pos <- regexpr("boardgame/[[:digit:]]{3,10}/[a-z]", html)
    my.id <- substr(html, id.pos, id.pos + attributes(id.pos)$match.length)
    id.list[[(length(id.list) + 1)]] <- gsub("(^[[:alpha:]]*/)|(/[[:alpha:]]*$)", "", my.id)
    html <- substr(html, (id.pos + attributes(id.pos)$match.length), nchar(html))
    listdf <- data.frame(unique(unlist(id.list)))
    
    
  }
  
  ids <- rbind(ids,listdf)
  print(i)
  
}




uniquedf <- unique(ids)
uniquedf$unique.unlist.id.list.. <- as.numeric(uniquedf$unique.unlist.id.list..)

df = transform(df, FOO = colsplit(FOO, split = "\\|", names = c('a', 'b')))
uniquedf <- transform(ids, unique.unlist.id.list.. = colsplit(unique.unlist.id.list.., split="/", names=c('a','b')))
uniquedf <- unique(uniquedf$unique.unlist.id.list..$a)
uniquedf <- sort(uniquedf)

#name	ID	year	minplayers	maxplayers	numratings	rating	bayesaverage	playingtime	minplaytime	maxplaytime	numowners	recommendedage	weight	numawards	numexpansions	balance	balance_sd	total_recplayer_votes	expansion_flag


fulldata <- data.frame(matrix(data=0, ncol=29, nrow=1))
names(fulldata) <- c(
  "matrix.data...0..nrow...1..ncol...1."
  ,"name"                                
  ,"ID"                                  
  ,"year"                                
  ,"minplayers"                          
  ,"maxplayers"                          
  ,"numratings"                          
  ,"rating"                              
  ,"bayesaverage"                        
  ,"playingtime"                         
  ,"minplaytime"                         
  ,"maxplaytime"                         
  ,"numowners"                           
  ,"recommendedage"                      
  ,"weight"                              
  ,"numawards"                           
  ,"numexpansions"                       
  ,"balance"
  ,"balance_sd"
  ,"total_recplayer_votes"
  ,"expansion_flag"
  ,"type"
  ,"categories"
  ,"mechanics"
  ,"families"
  ,"designer"
  ,"artist"
  ,"publisher"
  ,"num_plays"
)

tempdf <- data.frame(matrix(data = 0, nrow = 1, ncol = 1))


#PCDATA invalid Char value 8 at 14940
#Error in if (ncol(best) < max_players) { :missing value where TRUE/FALSE needed




















for(i in uniquedf[uniquedf > 191938]){
  #uniquedf[uniquedf > 1
  
  i=68448
  #i=c("C:\\Users\\scott\\Desktop\\gamestest\\new\\1\\13.xml")
  j=sprintf("https://www.boardgamegeek.com/xmlapi/boardgame/%i?&stats=1",i)
  
  
  
  
  pg <- read_xml(j)
  
  if(length(xml_nodes(pg, xpath="//name") %>% xml_text()) == 0){
    next 
  }else if(length(xml_nodes(pg, xpath="//name") %>% xml_text())>1){
    tempdf$name <- xml_nodes(pg, xpath="//name[@primary='true']") %>% xml_text()
  }else{
    tempdf$name <- xml_nodes(pg, xpath="//name") %>% xml_text()
  }
  
  tempdf$ID <- i
  tempdf$year <- xml_nodes(pg, xpath="//yearpublished") %>% xml_text()
  tempdf$minplayers <- xml_nodes(pg, xpath="//minplayers") %>% xml_text()
  tempdf$maxplayers <- xml_nodes(pg, xpath="//maxplayers") %>% xml_text()
  tempdf$numratings <- xml_nodes(pg, xpath="//usersrated") %>% xml_text()
  tempdf$rating <- xml_nodes(pg, xpath="//average") %>% xml_text()
  tempdf$bayesaverage <- xml_nodes(pg, xpath="//bayesaverage") %>% xml_text()
  tempdf$playingtime <- xml_nodes(pg, xpath="//playingtime") %>% xml_text()
  tempdf$minplaytime <- xml_nodes(pg, xpath="//minplaytime") %>% xml_text()
  tempdf$maxplaytime <- xml_nodes(pg, xpath="//maxplaytime") %>% xml_text()
  tempdf$numowners <- xml_nodes(pg, xpath="//owned") %>% xml_text()
  tempdf$recommendedage <- xml_nodes(pg, xpath="//age") %>% xml_text()
  tempdf$weight <- xml_nodes(pg, xpath="//averageweight") %>% xml_text()
  tempdf$numawards <- length(xml_nodes(pg, xpath="//boardgamehonor") %>% xml_text())
  tempdf$numexpansions <- length(xml_nodes(pg, xpath="//boardgameexpansion") %>% xml_text())
  
  tempdf$type <- list(xml_nodes(pg, xpath="//boardgamesubdomain") %>% xml_text())
  tempdf$categories <- list(xml_nodes(pg, xpath="//boardgamecategory") %>% xml_text())
  tempdf$mechanics <- list(xml_nodes(pg, xpath="//boardgamemechanic") %>% xml_text())
  tempdf$families <- list(xml_nodes(pg, xpath="//boardgamefamily") %>% xml_text())
  
  tempdf$designer <- list(xml_nodes(pg, xpath="//boardgamedesigner") %>% xml_text())
  tempdf$artist <- list(xml_nodes(pg, xpath="//boardgameartist") %>% xml_text())
  tempdf$publisher <- list(xml_nodes(pg, xpath="//boardgamepublisher") %>% xml_text())
  
  #will this auto-flag non boardgames, since they wont have a //boardgamecategory?
  expansion_check <- paste(xml_nodes(pg, xpath="//boardgamecategory") %>% xml_text(), sep="", collapse="")
  
  if(length(grep("Expansion", expansion_check)) > 0){
    tempdf$expansion_flag = 1
  } else {
    tempdf$expansion_flag = 0
  }
  
  
  
  

  
  
  html <- getURL(j, followlocation = TRUE)
  xml_parse <- xmlParse(html)
  
  best <- xpathSApply(
    doc=xml_parse, 
    path="//poll[@name='suggested_numplayers']//results//result[@value='Best']"
    ,xmlAttrs)
  
  rec <- xpathSApply(
    doc=xml_parse, 
    path="//poll[@name='suggested_numplayers']//results//result[@value='Recommended']"
    ,xmlAttrs)
  
  not_rec <- xpathSApply(
    doc=xml_parse, 
    path="//poll[@name='suggested_numplayers']//results//result[@value='Not Recommended']"
    ,xmlAttrs)
  
  
  
  min_players = as.numeric(tempdf$minplayers)
  max_players = as.numeric(tempdf$maxplayers)
  
  
  vec <- numeric() 
  
  
  
  
  
  if(length(best) > 0){
    if(ncol(best) < max_players){
      index <- ncol(best)
    } else {
      index <- max_players
    }
    
    
    
    if(max_players > min_players){
      if(tempdf$expansion_flag == 0){
        if(!is.null(ncol(best))){
          
          
          for(j in min_players:index){
            
            reccomends = sum(
              as.numeric(best[2,j])
              ,as.numeric(rec[2,j])
            )
            
            total = sum(
              as.numeric(best[2,j])
              ,as.numeric(rec[2,j])
              ,as.numeric(not_rec[2,j])
            )
            
            x <- round(reccomends / total, digits=2)
            #print(x)
            vec <- c(vec, x)
            
          }
          
          
          tempdf$balance <- paste(vec,collapse="|")
          tempdf$balance_sd <- sd(vec)
          
          tempdf$total_recplayer_votes <- sum(
            sum(as.numeric(best[2,]))
            , sum(as.numeric(rec[2,]))
            , sum(as.numeric(not_rec[2,]))
          )
          
          
          
        }
        
      } else{
        
        tempdf$balance = "NA"
        tempdf$balance_sd = "NA"
        tempdf$total_recplayer_votes = "NA"
        
      }  
      
    } else {
      
      tempdf$balance = "NA"
      tempdf$balance_sd = "NA"
      tempdf$total_recplayer_votes = "NA"
      
    } 
  }else{
    tempdf$balance = "NA"
    tempdf$balance_sd = "NA"
    tempdf$total_recplayer_votes = "NA"
  }
  
  
  

  
#total plays
  
  #https://www.boardgamegeek.com/xmlapi2/plays&id=1
  
  plays_url <- sprintf("https://www.boardgamegeek.com/xmlapi2/plays&id=%i",i)
  plays_html <- getURL(plays_url, followlocation = TRUE)
  xml_plays <- xmlParse(plays_html)
  
  
  
  plays <- xpathSApply(
    doc=xml_plays, 
    path="//plays"
    ,xmlAttrs)
  
  tempdf$num_plays <- plays[1]
  
  
  
  
  
  fulldata <- rbind(fulldata, tempdf)
  
  #speeds that are too fast: 0.5
  #speeds that are too slow: 1
  
  print(i)
  Sys.sleep(0.7)
  
}

#save.image("C:/Users/v-scbur/Desktop/bgg7k.RData")



##########################################################################################
#cleanup


fulldata$matrix.data...0..nrow...1..ncol...1. <- NULL
fulldata$publisher <- NULL

fulldata$year	<- as.numeric(fulldata$year)
fulldata$minplayers	<- as.numeric(fulldata$minplayers)
fulldata$maxplayers	<- as.numeric(fulldata$maxplayers)
fulldata$numratings	<- as.numeric(fulldata$numratings)
fulldata$rating	<- as.numeric(fulldata$rating)
fulldata$bayesaverage	<- as.numeric(fulldata$bayesaverage)
fulldata$playingtime	<- as.numeric(fulldata$playingtime)
fulldata$minplaytime	<- as.numeric(fulldata$minplaytime)
fulldata$maxplaytime	<- as.numeric(fulldata$maxplaytime)
fulldata$numowners	<- as.numeric(fulldata$numowners)
fulldata$recommendedage	<- as.numeric(fulldata$recommendedage)
fulldata$weight	<- as.numeric(fulldata$weight)
fulldata$balance_sd	<- as.numeric(fulldata$balance_sd)
fulldata$total_recplayer_votes	<- as.numeric(fulldata$total_recplayer_votes)
fulldata$num_plays <- as.numeric(as.character(fulldata$num_plays))

fulldata$type <- as.character(fulldata$type)
fulldata$categories <- as.character(fulldata$categories)
fulldata$mechanics <- as.character(fulldata$mechanics)
fulldata$families <- as.character(fulldata$families)
fulldata$designer <- as.character(fulldata$designer)
fulldata$artist <- as.character(fulldata$artist)




find.list <- list( 'c\\(', '"', '\\)', ' ')
find.string <- paste(unlist(find.list), collapse = "|")
#
fulldata$type <- (gsub(find.string, replacement = "", fulldata$type))
fulldata$categories <- (gsub(find.string, replacement = "", fulldata$categories))
fulldata$mechanics <- (gsub(find.string, replacement = "", fulldata$mechanics))
fulldata$families <- (gsub(find.string, replacement = "", fulldata$families))
fulldata$designer <- (gsub(find.string, replacement = "", fulldata$designer))
fulldata$artist <- (gsub(find.string, replacement = "", fulldata$artist))







#########################################################################################

#gameplay hours, per player
#re-scaled ratings based on percentile from distribution. whats the percentile rating for a 7.5 bayes rated game?


 fulldata$playtimehours <- fulldata$playingtime/60 * fulldata$num_plays
 
 fulldata$playtime_per_owner <- fulldata$playtimehours / fulldata$numowners
 

 write.table(fulldata, "C:\\Users\\v-scbur\\Desktop\\fulldata.txt", sep="\t", quote = F, row.names = F)
 
 
 
 
 ###########################################################################################
 
 pubtable <- (table(stack(setNames(strsplit(as.character(fulldata$mechanics), ","), pubs$year))[2:1]))
 write.table(pubtable, "C:\\Users\\V-SCBUR\\mechanics.txt", sep="\t", row.names = T, quote=F)
 
 
 #############################################################################################
 
 bayes_subset <- subset(fulldata, subset = (fulldata$bayesaverage > 0))
 hist(bayes_subset$bayesaverage)
 
 #############################################################################################
 
 balance_sub <- subset(fulldata, subset=(fulldata$total_recplayer_votes > 3000), select = c(name, balance, balance_sd, total_recplayer_votes))
 
 
 
 
