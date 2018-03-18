#### 1. SETUP ####

library(twitteR)
library(data.table)
library(httr)
library(SchedulerR) 
library(RCurl)
library(ggplot2)
library(scales)
library(stringr)

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

source('ttAccount.R')

c25 <- c("dodgerblue2","#E31A1C", # red
         "green4",
         "#6A3D9A", # purple
         "#FF7F00", # orange
         "black","gold1",
         "skyblue2","#FB9A99", # lt pink
         "palegreen2",
         "#CAB2D6", # lt purple
         "#FDBF6F", # lt orange
         "gray70", "khaki2",
         "maroon","orchid1","deeppink1","blue1","steelblue4",
         "darkturquoise","green1","yellow4","yellow3",
         "darkorange4","brown")


#### 2. Data Input ####

#### 2.1 Working data ####

Dict.Account <- fread('./data/in/Dict.Accounts.txt') 

#### 3. Data Extract ####

#### 3.1 Twitter ID ####


for (i in 1:nrow(Dict.Account)){
    
    profTT <- getUser(Dict.Account[i, nazwaKonta])
    Dict.Account[i, idAccountNew := as.character(profTT$id)]
    
}


#### 3.2 Twitter Stats ####

variables = c('favoritesCount','followersCount', 'friendsCount', 'listedCount', 'statusesCount')

userInfo <- function(user){
    logUser <- getUser(user)
    dataUser <- c(logUser$favoritesCount, 
                  logUser$followersCount, 
                  logUser$friendsCount, 
                  logUser$listedCount, logUser$statusesCount)
    return(dataUser)
}

Out.All <- data.table()
Now <- Sys.time()
i = 1
for (i in 1:nrow(Dict.Account)){
    
    tmpData <- data.table(ExecDate = Now, 
                          idSocial = Dict.Account[i, idAccountNew], 
                          nameSocial = Dict.Account[i, nazwaKonta],
                          nameVariable = variables, 
                          Value = userInfo(Dict.Account[i, nazwaKonta]))
    Out.All <- rbind(Out.All, tmpData)
    
}

Out.Followers <- data.table()

for (i in 1:nrow(Dict.Account)){
    user <- lookup_users(Dict.Account[i, nazwaKonta])
    
    tmpData <- data.table(idSocial = Dict.Account[i, idAccountNew], 
                          nameSocial = Dict.Account[i, nazwaKonta],
                          Level = Dict.Account[i, Poziom],
                          idFollowers =  get_followers(Dict.Account[i, nazwaKonta], 
                                                       n = user$followers_count, 
                                                       retryonratelimit = TRUE))
    
    Out.Followers <- rbind(Out.Followers, tmpData)
}

Out.Followers[Level == 2, .N, nameSocial][order(-N)]
Out.Followers[Level == 1, .N, nameSocial][order(-N)]

Out.Followers[, ct := .N, idFollowers.user_id]

esa <- merge(Out.Followers[Level ==1], Out.Followers[ct == 2 & Level ==1, .(secAccount = nameSocial, idFollowers.user_id)], 
              by = 'idFollowers.user_id', all.x = T)

esa <- esa[nameSocial != secAccount | is.na(secAccount)]


tmp <- merge(esa[ , .(total = .N),.(nameSocial, Level)], esa[ct == 1 ,.(ct = .N), nameSocial], 
             by = 'nameSocial')
tmp[, secAccount := 'unikalny']


tmp3 <- merge(esa[ , .(total = .N),.(nameSocial, Level)], 
              esa[ct == 2 ,.(ct = .N), .(nameSocial, secAccount)], by = 'nameSocial', all.x = T)
tmp <- rbind(tmp, tmp3[, .(nameSocial, Level, total, ct, secAccount)])


tmp[, perc:= ct/total]
esa <- tmp[Level == 1]

accCol <- c25[1:19]

names(accCol) <- tmp[, unique(secAccount)]
tmp[nameSocial == 'LegiaWarszawa' & secAccount == 'lechpoznan']
tmp[nameSocial == 'lechpoznan' & secAccount == 'LegiaWarszawa']
ggplot(tmp) + 
    geom_bar(aes(x = reorder(nameSocial, -perc), y = perc, fill = reorder(secAccount, perc)), stat = 'identity') + 
    scale_y_continuous(labels = percent, limits = c(0,1))+
    scale_fill_manual(values = accCol)+
 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, size = 8), legend.title = element_blank(), 
          legend.position = 'bottom', axis.title = element_blank())
# esa[nameSocial == 'Jagiellonia1920'][order(-perc)][, .(secAccount, percent(perc))]


Uni.Followers <- unique(Out.Followers[, .(Level, idFollowers.user_id)])
Uni.Followers[, .N, Level]

write.csv(Out.Followers, './data/out/Followers.2018-03-17.csv', quote = F, row.names = F)
