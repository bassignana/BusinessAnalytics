#import the dataset ####
library(corrr)
library(rcompanion)
library(caret)
library(readr)
library(dplyr)
library(ggplot2)                  
library(GGally)
library(tidyverse)
library(sna) 
library(gtools)
library(tidyr)
library(gdata)
library(mgcv)
library(rpart) 
library(rpart.plot)
library(MLmetrics)
library(maptools)
library(sp)
library(rgdal)
setwd('/Users/tommasobassignana/')
dataBA1 <- read_csv("Desktop/business_analytics/dataset lavoro/databa.csv")
dataBA2 <- read_csv("Desktop/business_analytics/dataset lavoro/databa2.csv")
dataBA3 <- read_csv("Desktop/business_analytics/dataset lavoro/databa3.csv")
#changing character to factors manually ####
#this is for checking errors of this tipe
#X1          datai       orai    
#FALSE       TRUE      FALSE   <- character?

char_to_fac <- function(data){
  #boolean vec, T if variable is character
  character_vars <- lapply(data, class) == "character"
  data[, character_vars] <- lapply(data[, character_vars], as.factor)
  return(data)
  #additional check if needed
  #lapply(data, class) == "character" 
  #NOTE if uncommented, the trasformation will not be executed! for checks only!
}

# check if the transformation is desirable, change what is necessary than apply transformation
char_to_fac(dataBA1)
dataBA1$cap <- as.integer(dataBA1$cap)
dataBA1$importo <- as.factor(dataBA1$importo)
char_to_fac(dataBA2)
char_to_fac(dataBA3)

# apply
dataBA1 <- char_to_fac(dataBA1)
dataBA2 <- char_to_fac(dataBA2)
dataBA3 <- char_to_fac(dataBA3)

#from character to date variables ####
dataBA1$data_inizio <- as.Date(dataBA1$data_inizio, format = "%d /%m /%y")
dataBA2$datai <- as.Date(dataBA2$datai, format = "%d /%m /%y")
#%Y in capital letter for 4 digits years
dataBA3$ultimo_ing.x <- as.Date(dataBA3$ultimo_ing.x, format = "%Y -%m -%d")
dataBA3$abb13 <- as.Date(dataBA3$abb13, format = "%Y -%m -%d")
dataBA3$abb14 <- as.Date(dataBA3$abb14, format = "%Y -%m -%d")


#defining functions for cheking NA & duplicated semantic levels & basic plot ####
info_cat <- function(feature){
  #usefull dascriptive
  n <- sum(is.na(feature))
  l <- levels(feature)
  #printing
  if(length(l)<30){
    q <- cat("There're",n,"NAs and the levesl of the variable are")}else{
      q <- cat("There're",n,"NAs, there're too many levels to display")}
  
  p <- plot(feature)
  lst <- list(l)
  return(lst)
}


#implementing dimension control####
#Usefull for tracking loss of information 
#the idea is that i want to keep track of all the manipulation occured to a dataset so i can have a 
#clear picture if (when) a problem arise; i can also keep track of the total information in the 
#dataset by looking at his dimension
#i usually do this when a project, like this one, requires heavy data manipulation

DimHistDataBA1 <- list()
DimHistDataBA1[[1]] <- list(c(dim(dataBA1),"full"))
DimHistDataBA2 <- list()
DimHistDataBA2[[1]] <- list(c(dim(dataBA2),"full"))
DimHistDataBA3 <- list()
DimHistDataBA3[[1]] <- list(c(dim(dataBA3),"full"))


#factor's info v2####

info_cat(dataBA1$importo) #è ordinal?
info_cat(dataBA1$sesso)
# i would like to impute missing values by sampling from the empirical distribution of the variable sesso
# but before doing so i want to check if the distribution of sex is very unbalanced in some factors
 
ggplot(dataBA1, 
       aes(x = riduzione, 
           fill = sesso)) + 
  geom_bar(position = "stack")

ggplot(dataBA1, 
       aes(x = sconto, 
           fill = sesso)) + 
  geom_bar(position = "stack")

#NA's imputation by empyrical distribution
probM <- sum((dataBA1$sesso=="M"), na.rm = TRUE)/sum((dataBA1$sesso=="M"|dataBA1$sesso=="F"),na.rm = TRUE)
probF <- 1-probM
dataBA1$sesso[is.na(dataBA1$sesso)] <- sample(x = c("M","F"), size = 1, replace = TRUE, prob = c(probM, probF))
rm(probF, probM)

info_cat(dataBA1$sconto)
info_cat(dataBA1$riduzione)
info_cat(dataBA1$tipo_pag)
info_cat(dataBA1$agenzia)
info_cat(dataBA1$agenzia_tipo)
# i'll treat "dato mancante" as an NA and since there're very few of them, i'll drop the observations
sum(dataBA1$agenzia_tipo=="DATO MANCANTE")
# i'll drop this 276 obs because i think thai this is an important variable for predicting churn
dataBA1 <- dataBA1[!dataBA1$agenzia_tipo=="DATO MANCANTE",]
DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"removing dato mancante"))

levels(dataBA1$professione)
#rimuovere professione
dataBA1 <- dataBA1[,-12]
DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"removing variable professione"))
info_cat(dataBA1$nuovo_abb)
info_cat(dataBA2$museo)
#i can remove MOSTRA...but at the end of the day i can use these levels to predict who the customers go to the event with
info_cat(dataBA2$prov_museo)
info_cat(dataBA2$com_museo)

#range, outliers and missing values on numerical variables####
info_cnt <- function(feature){
  n <- sum(is.na(feature))
  r <- range(na.omit(feature))
  p <- plot(feature)
  cat("There're",n,"NAs and the range WITHOUT NA's is from",r[1], "to", r[2])
}
info_cnt(dataBA1$cap)
#for now, i don't do anything addressig NA's or erroneus values

#importo vs riduzione e sconto
ggplot(dataBA1, 
       aes(x = riduzione, 
           fill = as.factor(importo))) + 
  geom_bar(position = "stack")

ggplot(dataBA1, 
       aes(x = sconto, 
           fill = as.factor(importo))) + 
  geom_bar(position = "stack")
plot(as.factor(dataBA1$importo))
table(as.factor(dataBA1$importo))
table(dataBA1$sconto)
table(dataBA1$riduzione)

info_cnt(dataBA1$data_inizio)#good plot
qplot(x = data_inizio, data=dataBA1, stat="summary", geom="bar")

info_cnt(dataBA1$data_nascita)#outliers 
# i want to have age between 3 and 101 adn i want to impute the mean age if the distribution is not bimodal
# i called the museums and they said that there are no age restriction for the membership
età <- 2013 - dataBA1$data_nascita
dataBA1$età <- età
DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"add età to dataBA1"))
rm(età)
table(dataBA1$età)

#[which(dataBA1$età>0),] removes both NA's and negative data
p <- ggplot(data=dataBA1[which(dataBA1$età>0),], aes(x=età)) + geom_density(color="darkblue", fill="lightblue")
p
# Add mean line
p + geom_vline(aes(xintercept = mean(età)),
               linetype="dashed", color="orange", size=1)
rm(p)
#it seems resonable to me to impute the mean and not the mode to not accentuate the pike 
dataBA1provv <- filter(dataBA1, età > 3)
dataBA1provv <- filter(dataBA1provv, età < 102)
# DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"now 3<età<101"))
dataBA1$età[which(dataBA1$età<3)] <- mean(dataBA1provv$età, na.rm = TRUE)
dataBA1$età[which(dataBA1$età>101)] <- mean(dataBA1provv$età, na.rm = TRUE)
dataBA1$età[is.na(dataBA1$età)] <- mean(dataBA1provv$età, na.rm = TRUE)
rm(dataBA1provv)
info_cnt(dataBA2$datai)

info_cnt(dataBA2$orai)#outliers!
#now i need to distinguish from working days (roughly) and weekend days
dataBA2$WE <- rep(c(1,1,0,0,0,0,0), length.out = length(dataBA2$datai))
DimHistDataBA2[[length(DimHistDataBA2)+1]] <- list(c(dim(dataBA2),"add dummy for WeekEnd"))

#plot to find outliers
ore <- as.POSIXct(dataBA2$orai, format="%H:%M:%S")
range(ore)
?trunc()
trunc(range(ore), "hours")#rounding!!
range_rounded <- trunc(range(ore), "hours")
hist(ore, breaks=seq(range_rounded[1], range_rounded[2]+3600, by="30 min") )
#ci sono degli outliers in ore = 00:00

# c'è una differenza di distribuzione degli ingressi tra giorni lavorativi e del weekend?
data.provv1 <- filter(dataBA2, as.character(orai) != "00:00:00", WE == 1)
data.provv2 <- filter(dataBA2, as.character(orai) != "00:00:00", WE == 0)
ggplot(data=data.provv1, aes(x=orai)) + geom_density()
ggplot(data=data.provv2, aes(x=orai)) + geom_density()

ggplot() +
  geom_bar(data=data.provv1, aes(x=orai), color = "blue") + 
  geom_bar(data=data.provv2, aes(x=orai))

ggplot(data=data.provv1, aes(x=orai)) + geom_bar()
ggplot(data=data.provv2, aes(x=orai)) + geom_bar()

rm(data.provv1,data.provv2)
#the distribution is almast the same in both working and weekend dates
#there are twice as many entries in the working days compared to the entries in the weekend dates
#here we notice that the distribution is strongly bimodal, hence the imputation of outliers is somewhat more problematic
#how many outliers are there?
sum(dataBA2$orai==00:00)

 data.provv <- filter(dataBA2, as.character(orai) != "00:00:00")

# there are A LOT of possible ways to deal with this situation
# the one i choose is a trade off between what is theoretically sound and the effort needed to 
# deal with such a relative small persentage of observations 

set.seed(123)
hist(sample(as.numeric(data.provv$orai), length(data.provv$orai), replace = T))
dataBA2$orai[which(dataBA2$orai==00:00:00)] <- sample(as.numeric(data.provv$orai), 1, replace = T)
#check
ore <- as.POSIXct(dataBA2$orai, format="%H:%M:%S")
range_rounded <- trunc(range(ore), "hours")
hist(ore, breaks=seq(range_rounded[1], range_rounded[2]+3600, by="30 min") )# BUON PLOT
DimHistDataBA2[[length(DimHistDataBA2)+1]] <- list(c(dim(dataBA2),"removing outliers in orai"))
rm(data.provv,range_rounded,ore)


info_cnt(dataBA2$importo)
info_cnt(dataBA3$ultimo_ing.x)
info_cnt(na.omit(dataBA3$ultimo_ing.x))#occhio al range
range(as.Date(na.omit(dataBA3$ultimo_ing.x)))
range(dataBA2$datai)
range(dataBA1$data_inizio)
View(dataBA3)

#correlazioni####
# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
mixed_assoc <-  function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal <- function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      #assoc and type are variable names
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}
#plot
na.omit(dataBA1) %>% #i don't know why, but sometimes if there're NA's in the data it gives an error
  select(importo,sconto,riduzione,agenzia_tipo,sesso,età) %>%
  mixed_assoc() %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot(min_cor = 0, colors = c("orange", "blue"), repel = T)
#problematic cleaning of dataBA2####
#i'm assumung that duplicated etries ie. two entries with the same id in the same minute or separated by 1 or 2 minutes
#are not duplicated, but they simply rappresent a pair of people, one with the membership and the other 
#allowed to use the membership to get a free entry
#this assumption can not be verified but it is heavily corroborated by the fact that a lot of time the price 
#would have been charged by the museum is different between entries, denoting two types of different customers ie man vs woman ecc
to_remove <- duplicated(dataBA2[,c(2,3,8)])#la colonna 8 va agginta per discriminare di casi in cui due utenti diversi entrino nello stesso minuto
furbetti1 <- dataBA2[to_remove,]
to_keep <- !to_remove
dataBA2 <- dataBA2[to_keep,]
#removing entries in different minutes
#i'm assuming that a person enters one time per day in a museum
to_remove <- duplicated(dataBA2[,c(2,5,8)])
furbetti2 <- dataBA2[to_remove,]
to_keep <- !to_remove
dataBA2 <- dataBA2[to_keep,]
rm(to_remove, to_keep)
DimHistDataBA2[[length(DimHistDataBA2)+1]] <- list(c(dim(dataBA2),"removing (all?) -duplicated- entries"))

#furbetti is the dataset of all the people that have shared their membership BUT the price reflect the
#true unknown person that has used the membership the second time 
furbetti <- rbind(furbetti1, furbetti2)
rm(furbetti1, furbetti2)

#some feature engineering####

#almeno un ingresso V2####
dataBA3$almeno_un_ingresso <- 1
dataBA3$almeno_un_ingresso[is.na(dataBA3$ultimo_ing.x)] <- 0
DimHistDataBA3[[length(DimHistDataBA3)+1]] <- list(c(dim(dataBA3),"dummy per almeno un ingresso"))
names(dataBA3)[names(dataBA3) == 'almeno_un_ingresso'] <- 'almeno_un_ingressoBA3'

#how many people visited a museum at least one time according to dataBA3
sum(dataBA3$almeno_un_ingresso)
#how many people visited a museum at least one time according to dataBA2
length(unique(dataBA2$CodCliente))

#the data refers to the same period, it's tricky because in dataBA2 the last entry recorded
#is in date "2013-11-30" while in dataBA3 is in date "2014-03-30"
#hence i have to combine the two information to list all the people that have entered the museums

#almeno un ingresso V3####
u1 <- unique(dataBA1$codcliente)
u2 <- unique(dataBA2$CodCliente)
length(u1)
length(u2)
'%nin%' <-  Negate('%in%')
u3<- u1[u1 %nin% u2]
dataBA1$almeno_un_ingressoBA1 <- 1
dataBA1$almeno_un_ingressoBA1[u1 %in% u3] <- 0
sum(dataBA1$almeno_un_ingressoBA1)
rm(u1,u2,u3)
DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"dummy per almeno un ingresso"))

#now i'll merge the 2 variable almeno_un_ingressoBA1 and almeno_un_ingressoBA3 to check
databaprovv <- dataBA3[,c(2,7)]
#why do i keep only complete obs in datBA1? because if i do otherwise, i'll have to eliminate the incoplete obs, sooner or later
dataBA1provv <- merge(x = dataBA1, y = databaprovv, by.x = "codcliente", by.y = "codcliente", all.x = T)
dataBA1provv$almeno_un_ing <- 0
dataBA1provv$almeno_un_ingressoBA3[is.na(dataBA1provv$almeno_un_ingressoBA3)] <- 2#this is done to make the if else work
try(for (i in 1:length(dataBA1provv$almeno_un_ing)) {
  print(i)
  if (dataBA1provv$almeno_un_ingressoBA1[i] == 1 | dataBA1provv$almeno_un_ingressoBA3[i] == 1) {
    dataBA1provv$almeno_un_ing[i] <- 1
  }
  
})#there is obiusly a better way
dataBA1provv <- dataBA1provv[,-c(17,16)]
dataBA1 <- dataBA1provv
rm(databaprovv, dataBA1provv,i)
DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"dummy finale per almeno un ingresso, rm"))



#number of total visits####
frq <- table(dataBA2$CodCliente)
frq_data <- as.data.frame(frq)
frq_data <- rename(frq_data, ingressi_tot = Freq)
dataBA1 <- merge(dataBA1, frq_data, by.x = c("codcliente"), by.y = c("Var1"), all.x = TRUE)
DimHistDataBA1[[length(DimHistDataBA1)+1]] <- list(c(dim(dataBA1),"add ingressi_tot variable"))
rm(frq)
rm(frq_data)


#cumulative number of visits####
#ordino per valori crescenti di codice_cliente
dataBA2 <- dataBA2[order(dataBA2$CodCliente), ]
DimHistDataBA2[[length(DimHistDataBA2)+1]] <- list(c(dim(dataBA2),"ordering, cresc, bu CodCliente"))
#inizializzo la nuova variabile
n_visita <- rep(1,length(dataBA2$CodCliente))
#aggiornamento del numero delle visite
i <- 1
try(while (i<length(dataBA2$CodCliente)) {
  while(dataBA2$CodCliente[i+1]==dataBA2$CodCliente[i]) {
    n_visita[i+1] <- n_visita[i]+1
    i <- i+1
  }
  i <- i+1
}, silent = T)

#merge della nuova variabile nel dataset
dataBA2$n_visita <- n_visita  
DimHistDataBA2[[length(DimHistDataBA2)+1]] <- list(c(dim(dataBA2),"add n_visita variable"))
rm(n_visita,i)

#data primo ingresso versione semplice####
df <- select(filter(dataBA2, n_visita < 2), CodCliente, datai)
df <- rename(df,  data_prima_visita = datai)
#uso il merge invece di aggiungere una nuova variabile se ho dei casi mancanti
dataBA3 <- merge(dataBA3, df, by.x = c("codcliente"), by.y = c("CodCliente"), all.x = TRUE)
DimHistDataBA3[[length(DimHistDataBA3)+1]] <- list(c(dim(dataBA3),"add data_prima_visita variable"))
rm(df)
#periodo di attività ovvero dell'uso dellabbonamento####
dataBA3$periodo_attività  <- dataBA3$ultimo_ing.x-as.Date(dataBA3$data_prima_visita)
DimHistDataBA3[[length(DimHistDataBA3)+1]] <- list(c(dim(dataBA3),"add periodo attività variable"))

#lasso di tempo tra l'acquisto dell'abbonamento e il primo utilizzo####
dataBA3$inter <- as.Date(dataBA3$data_prima_visita) - dataBA3$abb13
DimHistDataBA3[[length(DimHistDataBA3)+1]] <- list(c(dim(dataBA3),"add inter variable"))

#churn vaiable####
churn <- dataBA3$si2014
churn[churn==0] <- 2#churn
churn[churn==1] <- 0#non churn
churn[churn==2] <- 1#churn
#merge
dataBA3$churn <- churn
dataBA3 <- dataBA3[,-3]
rm(churn,almeno_un_ing)

#check on the dataBA3 dataset####
#this should be zero
sum(is.na(dataBA3$churn))
#grouping by entry####
group_maker <- function(col_ore, col_id, secondi_p, secondi_g){
  list_id <- list()
  q <- 1 
  while(q<=length(col_ore)){
    i <- 0
    f <- 0
    try(while((col_ore[q+1]-col_ore[q])<secondi_p & (col_ore[q+1]-col_ore[q-i])<(secondi_g)){
      q <- q+1
      
      i <- i+1
      
      f <- 1
    }, silent = T)#the error is only formal...
    
    try(if (f==0) {
      vec_id_list <- list()
      vec_id_list <- col_id[q+1]
      list_id[[q]] <- vec_id_list                                     
      q <- q+1                                            
    }else{#f==1
      vec_id_list <- list()
      s <- q-i
      h <- q
      vec_id_list <- col_id[s:h]
      list_id[[q-1]] <- vec_id_list
    })
    
  }
  return(list_id)
}

#
# here i use the group_maker function 365 times, one time per every day in the dataset
length(unique(dataBA2$datai)) #365 
lbig <- list() 
for (i in 1:length(unique(dataBA2$datai))) {
  compagni_f <- filter(dataBA2, datai==unique(datai)[i])
  #distingue tra musei attraverso il fatto che tra l'ultima visita di un museo e la prima del museo successivo passano delle ore
  #potrebbe essere migliorato con un riferimento esplicito al museo per evitare qualsiasi errore
  lbig[[i]] <- group_maker(col_ore = compagni_f$orai, col_id = compagni_f$CodCliente, secondi_p = 240, secondi_g = 600)
}
View(lbig)#one sublist for every day
rm(compagni_f)
#
#removing NA's and NULL's in all sublists
try(for (i in 1:length(unique(dataBA2$datai))) {
  to_rm <- c()
  try(for (j in 1:length(lbig[[i]])) {
    if (length(lbig[[i]][[j]])<=1) {
      to_rm <- append(to_rm, j, length(to_rm)+1)
    }else{}
  })
  print(i)
  lbig[[i]] <- lbig[[i]][-to_rm]
})
View(lbig)
#
# now i create a dataset that contain all possible combination of people in a single group that (could) have been toghether
lpai <- list()
try(for (i in 1:length(lbig)) {#i is the day
  l <- list() # l contains all possible PAIRS of people that COULD, MAYBE(?) have visited the museum toghther
  try(for (j in 1:length(lbig[[i]])) {
    print(j)  # j is every group within a single day
    u <- lbig[[i]][[j]]
    u <- u[!duplicated(u)]
    if(length(u)>1){
      c <- as.data.frame(combinations(length(u),2,as.vector(u) ) )
      c$pair <- (paste(c$V1, c$V2, sep = " "))
      as.vector(c$pair)
      l[[j]] <- as.vector(c$pair)}else{}
  })
  lpai[[i]] <- l
})
View(lpai)
rm(lbig, i, j, to_rm, u,l,c)
# 
lpai <- lapply(lpai, FUN = unlist, recursive=FALSE)

na.pad <- function(x,len){
  x[1:len]#pad with na
}

makePaddedDataFrame <- function(l,...){
  maxlen <- max(sapply(l,length))
  data.frame(sapply(l,na.pad,len=maxlen),...)
}
#this two functions are needed to create a dataframe where the columns have the same length
#making the actual dataframe 
#MAYBE R WILL CRUSH IN ONE OF THE NEXT LINES SO SAVE ENVIROMENT AND DATA
buddies <- makePaddedDataFrame(lpai)
#
#analysing duplicates in the dataset of pairs to find who ACTUALLY went to a museum with the same person more than once
DUP <- as.vector(unlist(buddies))
DUP <- DUP[duplicated2(DUP, bothWays=TRUE)]#here i make a vector containing only all the duplicated(i.e more than once) pairs of id's that went togheather to the museums
tab <- table(DUP)
tab #here you can see how many times the pair went toghether
table(furbetti$CodCliente)#here you can see how many times a person shared his membership with someone else 

rm(lpai,DUP)
#visualizing and making actual groups from pairs####
df_freq <- as.data.frame(tab)
rm(tab)
df_freq <- separate(data = df_freq, col = c("DUP"), into =c("p1","p2"), sep = " ", remove = TRUE)
df_freq_small <- df_freq[df_freq$Freq>4,]#i have to take a subset because for the next step i don't have enough RAM in my pc to store the matrix
View(df_freq_small)
#HERE R MIGHT CRASH!!!!!!
el <- as.matrix(df_freq_small)
labels      <- unique( c(el[,1], el[,2]) )
A           <- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels# it is, unfortunatly, needet to make this simple step work
rm(labels)
#Don't try to View() A in it's entirety

A[ el[,1:2] ] <- as.numeric( el[,3] )
gplot(A)
gplot(A, jitter = F, gmode = "ggraph" )
rm(el)
#how many unique people does a single id go to a museum with####
#note: i have to use the for loop because af a strange behaviour in the R implementation of merge (see side 6)
frq <- table(df_freq$p1)
frq_data1 <- as.data.frame(frq)

frq <- table(df_freq$p2)
frq_data2 <- as.data.frame(frq)
df_amici <- merge(frq_data1, frq_data2, by.x = c("Var1"), by.y = c("Var1"), all = TRUE)
df_amici[is.na(df_amici)] <- 0
df_amici$tot <- df_amici$Freq.x + df_amici$Freq.y 
#to adjust for the repeated count of frequencies
for (i in 1:length(df_amici$Freq.y)) {
  if(df_amici$Freq.y[i]>0 & df_amici$Freq.x[i]>0){
    df_amici$tot[i] <- df_amici$tot[i] - 1
  }
}
rm(frq, frq_data1, frq_data2, df_amici)
dataBA1 <- merge(dataBA1, df_amici[,c(1,4)], by.x = c("codcliente"), by.y = c("Var1"), all.x = T)
dataBA3 <- merge(dataBA3, df_amici[,c(1,4)], by.x = c("codcliente"), by.y = c("Var1"), all.x = T)
colnames(dataBA1)[which(names(dataBA1) == "tot")] <- "amici_tot"
colnames(dataBA3)[which(names(dataBA3) == "tot")] <- "amici_tot"
dataBA1$amici_tot[is.na(dataBA1$amici_tot)] <- 0
dataBA3$amici_tot[is.na(dataBA3$amici_tot)] <- 0


#creazione dataBA4####
#dataset utilizzato dai modelli
#
dataBA4 <- merge(dataBA1, dataBA3, by.x =  c("codcliente"), by.y = c("codcliente"),all.y = T)
DimHistdataBA4 <- list()
DimHistdataBA4[[1]] <- list(c(dim(dataBA4),"full"))
#collapsing the variables amici_tot in one variable
sum(is.na(dataBA4$amici_tot.x))
dataBA4$amici_tot.x[is.na(dataBA4$amici_tot.x)] <- dataBA4$amici_tot.y[is.na(dataBA4$amici_tot.x)] 
sum(is.na(dataBA4$amici_tot.x))

sum(is.na(dataBA4$amici_tot.y))
dataBA4$amici_tot.y[is.na(dataBA4$amici_tot.y)] <- dataBA4$amici_tot.x[is.na(dataBA4$amici_tot.y)] 
sum(is.na(dataBA4$amici_tot.y))

sum(dataBA4$amici_tot.y!=dataBA4$amici_tot.x)
#the two variables don't need to be altered because they are identical in this dataset
colnames(dataBA3)[which(names(dataBA3) == "amici_tot.x")] <- "amici_tot"


#considering that this is a dataframe for models, i can delete some variables
nearZeroVar(dataBA4)
#i delete abb 14 because it will be 0 variance, like nuovo_abb
dataBA4 <- within(dataBA4, rm( abb14, X1.x, X1.y, nuovo_abb, cap, amici_tot.y, almeno_un_ingressoBA3,data_nascita))
DimHistdataBA4[[length(DimHistdataBA4)+1]] <- list(c(dim(dataBA4),"rm abb14, X1.x, X1.y, nuovo_abb,amici_tot.y, cap, almeno_un_ingressoBA3,data_nascita variables"))


#converting dates to seasons####
getSeason <- function(dates) {
  fFE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # first Fall Equinox
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2013-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2013-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2013-9-15",  format = "%Y-%m-%d") # second Fall Equinox
  sWS <- as.Date("2013-12-15", format = "%Y-%m-%d") # second Winter Solstice
  sSE <- as.Date("2014-3-15",  format = "%Y-%m-%d") # Spring Equinox
  # Convert dates from any year to 2012 AND 2013 dates
  d <- as.Date(strftime(dates, format="%Y-%m-%d"))
  ifelse( d >= fFE & d < WS, "Fall",
          ifelse (d >= WS & d < SE, "Winter",
                  ifelse (d >= SE & d < SS, "Spring",
                          ifelse (d >= SS & d < FE, "Summer", 
                                  ifelse(d >= FE & d < sWS, "Fall",
                                         ifelse(d >= sWS & d < sSE, "Winter", "Spring" ))))))
}

range(na.omit(dataBA4$data_inizio))
range(na.omit(dataBA4$ultimo_ing.x))
range(na.omit(dataBA4$abb13))
range(na.omit(dataBA4$data_prima_visita))

dataBA4$data_inizio <- getSeason(dataBA4$data_inizio)
dataBA4$ultimo_ing.x <- getSeason(dataBA4$ultimo_ing.x)
dataBA4$abb13 <- getSeason(dataBA4$abb13)
dataBA4$data_prima_visita <- getSeason(dataBA4$data_prima_visita)

dataBA4$data_inizio <- as.factor(dataBA4$data_inizio)
dataBA4$ultimo_ing.x <- as.factor(dataBA4$ultimo_ing.x)
dataBA4$abb13 <- as.factor(dataBA4$abb13)
dataBA4$data_prima_visita <- as.factor(dataBA4$data_prima_visita)

#importo totale ipotetico speso dagli abbonati####
#slow
u <- unique(dataBA2$CodCliente)
hyp_spesa_abb <- rep(0, length(u))
j <- 1

for (i in u) {
  f <- filter(dataBA2, CodCliente == i)
  f$importo <- as.integer(f$importo)
  tot <- sum(f$importo)
  hyp_spesa_abb[j] <- tot
  j <- j + 1
}

#dataframe di passaggio
df <- data.frame(u,hyp_spesa_abb)
#merge
dataBA4 <- merge(df, dataBA4, by.x = c("u"), by.y = c("codcliente"), all.y = TRUE)
DimHistdataBA4[[length(DimHistdataBA4)+1]] <- list(c(dim(dataBA4),"add hyp_spesa_abb variable"))
rm(df,u,hyp_spesa_abb,j,i,tot,f)


#ricavo società abbonamento musei####
to_pay <- dataBA4$hyp_spesa_abb/2
ricavo_soc <- as.numeric(as.integer(as.character(dataBA4$importo))-to_pay)
qplot(ricavo_soc)
dataBA4$ricavo_soc <- ricavo_soc
DimHistdataBA4[[length(DimHistdataBA4)+1]] <- list(c(dim(dataBA4),"add ricavo_soc variable"))
dataBA4 <- rename(dataBA4, codcliente = u)
rm(ricavo_soc, to_pay)

#cleaning dataBA4####
dataBA4$hyp_spesa_abb[dataBA4$almeno_un_ing == 0] <- 0
dataBA4$ingressi_tot[dataBA4$almeno_un_ing == 0] <- 0
dataBA4$periodo_attività[dataBA4$almeno_un_ing == 0] <- 0
dataBA4$inter[dataBA4$almeno_un_ing == 0] <- 0
dataBA4$ricavo_soc[dataBA4$almeno_un_ing == 0] <- 0

#in these i leave NA
dataBA4$data_prima_visita
dataBA4$ultimo_ing.x

#trick to eliminate the difftime attribure
dataBA4$periodo_attività <- as.integer(as.character(dataBA4$periodo_attività))
dataBA4$inter <- as.integer(dataBA4$inter)



######################## plots and descriptive ############################
#maps####
shp <- readOGR(dsn = "/Users/tommasobassignana/Desktop/business_analytics/Business-Analytics-2019_20/CAP_NordOvest_Shp", layer="cap_NO")
range(na.omit(dataBA1$cap))
tab <- table(na.omit(dataBA1$cap))
tab <- as.data.frame(tab)
tab <- tab[tab$Var1 %in% 10010:46100,]
tab$Var1 <- as.integer(as.character(tab$Var1))
shp@data$freq <- 0
shp@data$freq[shp@data[[1]] %in% tab$Var1] <- tab$Freq
try(for (j in 1:length(shp@data$freq)) {
  print(j)
  for (i in tab$Var1 ) {
    if(shp@data[[1]][j]==i){shp@data$freq[j] <- tab$Freq[tab$Var1==i]}
  }
})
#plots
spplot(shp, zcol = "freq")
shp1 <- shp
shp1 <- shp1[shp@data$IT_CAP >= 10121, ]
shp1 <- shp1[shp@data$IT_CAP < 10156, ]
shp1 <- subset(shp1, IT_CAP >= 10121)
shp1 <- subset(shp1, IT_CAP <= 10156)
spplot(shp1, zcol = "freq")
shp <- shp[shp@data$freq > 501, ]
spplot(shp, zcol = "freq")
rm(shp,shp1,tab)
#plots####
# Setup
options(scipen=999)  # turn off scientific notation like 1e+06

#Histogram on a Continuous (Numeric) Variable
q <- ggplot(dataBA4, aes(età)) +
  geom_histogram(aes(fill=sesso), 
                 bins=20, 
                 col="black", 
                 size=.1) + 
  scale_fill_brewer(palette = "Spectral") +
  xlim(c(0, 90)) 
plot(q)
rm(q)
######################## probability of churn ############################
#cleaning for logistic regression####
#let's see if the class is balanced
table(dataBA4$churn)
table(dataBA4$churn)[[2]][1]/(table(dataBA4$churn)[[2]][1]+table(dataBA4$churn)[[1]][1])

#si può anche provare a fare un ensamble model 
#prendendo subsample dei due casi e combinandoli in più modelli
dataBA4.log <- dataBA4

# collapsing some levels based on they frequency####


collapse_lvls <- function(factor_var,limit) {
  #factor_var è la variabile di cui voglio far collassare i livelli
  #di solito del tipo df$nome_della_var_fattoriale
  
  #limit è il valore entro il quale i livelli vengono aggregati
  
  #
  tb <- table(factor_var)
  to_collapse <- c()
  to_mantain <- c()
  for (i in 1:length(factor_var)) {
    ifelse(as.integer(tb[i])<=limit,to_collapse[i] <- levels(factor_var)[i],to_mantain[i] <- levels(factor_var)[i])
  }
  #
  to_collapse <- na.omit(to_collapse)
  
  # Get levels and add "Altro"
  levels <- levels(factor_var)
  levels[length(levels) + 1] <- "Altro"
  
  for (i in 1:length(to_collapse)) {
    tona <- factor_var == to_collapse[i]
    factor_var[tona] <- NA
  }
  
  # refactor Species to include "Altro" as a factor level
  # and replace NA with "Altro"
  factor_var <- factor(factor_var, levels = levels)
  factor_var[is.na(factor_var)] <- "Altro"
  
  #
  factor_var <- factor(factor_var)
}
is.fact <- sapply(dataBA4.log, is.factor)
is.fact
dataBA4.log$sconto <- collapse_lvls(dataBA4.log$sconto, 2000)
dataBA4.log$riduzione <- collapse_lvls(dataBA4.log$riduzione, 2000)
dataBA4.log$tipo_pag <- collapse_lvls(dataBA4.log$tipo_pag, 2000)
dataBA4.log$comune <- collapse_lvls(dataBA4.log$comune, 2000)
dataBA4.log$agenzia_tipo <- collapse_lvls(dataBA4.log$agenzia_tipo, 2000)
rm(is.fact)

#deleting obs with level "DATO MANCANTE" in comune
dataBA4.log <- dataBA4.log[dataBA4.log$comune != "DATO MANCANTE",]
table(dataBA4.log$churn)[[2]][1]/(table(dataBA4.log$churn)[[2]][1]+table(dataBA4.log$churn)[[1]][1])
#the balance in the dataset is mantained

sum(is.na.data.frame(dataBA4.log))
sapply(dataBA4.log, function(x) sum(is.na(x)))

#an option to dealing with NA's
dataBA4.log <- subset(dataBA4.log, select = -c(data_prima_visita, ultimo_ing.x))
dataBA4.log <- subset(dataBA4.log, select = -c(agenzia))
dataBA4.log <- dataBA4.log[complete.cases(dataBA4.log),]

str(dataBA4.log)
summary(dataBA4.log)
#logistic reg####

#train and test split
#a simple control
nearZeroVar(dataBA4.log) #why?
sum(is.na.data.frame(dataBA4.log))

set.seed(123)
trainIndex.log <- createDataPartition(dataBA4.log$churn, p = 0.75, list = F)

str(dataBA4.log)

#as.ordinal
lev <- levels(dataBA4.log$importo)
lev
dataBA4.log$importo <- factor(dataBA4.log$importo, levels = lev, labels = lev, ordered = T)
is.ordered(dataBA4.log$importo)
levels(dataBA4.log$importo)
rm(lev)
#based on this regression we can assume that we can remove hyp_spesa_abb, wich is logical because
#a customer dosen't probably care about the hypotetical price of the tickets
#logit.prob <- predict(logit.fit, newdata = dataBA4, type = "response")
dataBA4.log$churn <- as.factor(dataBA4.log$churn)
levels(dataBA4.log$churn) <- c("No", "Yes")
levels(dataBA4.log$churn) 
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  classProbs = TRUE,
  summaryFunction = multiClassSummary)


logit.fit1 <- train(churn ~., dataBA4.log[complete.cases(dataBA4.log),-c(1,2,4,15,19)],
                    method = "glm",
                    family = "binomial",
                    trControl = fitControl,
                    metric = "accuracy")
options(scipen=999)
summary(logit.fit1)
variable_imp <- varImp(logit.fit1, scale = T)

plot(variable_imp, top = dim(variable_imp$importance)[1])

# GAM per classification####
trainIndex.gam <- trainIndex.log
dataBA4.gam <- dataBA4
is.fact <- sapply(dataBA4.gam, is.factor)
is.fact
dataBA4.gam$importo <- as.integer(as.character(dataBA4.gam$importo))
dataBA4.gam <- dataBA4.gam[,!is.fact]
sum(is.na.data.frame(dataBA4.gam))
str(dataBA4.gam)
# I() per indicare che la variabile è binaria, anche I(wage< 149) diventa binaria
g <- gam(as.factor(churn) ~ s(età), family = "binomial", data = dataBA4.gam[trainIndex.gam,])
plot(g, pages = 1, seWithMean=TRUE)

g <- gam(as.factor(churn) ~ s(età, by = almeno_un_ing) + almeno_un_ing, family = "binomial", data = dataBA4.gam[trainIndex.gam,])
plot(g, pages = 1, seWithMean=TRUE)

g <- gam(as.factor(churn) ~ s(età)+s(inter), family = "binomial", data = dataBA4.gam[trainIndex.gam,])
plot(g, pages = 1, seWithMean=TRUE)

g <- gam(as.factor(churn) ~ s(età) + s(inter) + s(amici_tot.x) +s(periodo_attività),
         family = "binomial", data = dataBA4.gam[trainIndex.gam,])
plot(g, seWithMean=TRUE)

#altri algo che possono spiegare in modo intuitivo la variable importance####
#single TREE
dataBA4.tree <- dataBA4
dataBA4.tree$churn <- ifelse(dataBA4.tree$churn < 1,"No","Yes")
dataBA4.tree$churn <- as.factor(dataBA4.tree$churn)

dataBA4.tree$sconto <- collapse_lvls(dataBA4.tree$sconto, 2000)
dataBA4.tree$riduzione <- collapse_lvls(dataBA4.tree$riduzione, 2000)
dataBA4.tree$tipo_pag <- collapse_lvls(dataBA4.tree$tipo_pag, 2000)
dataBA4.tree$comune <- collapse_lvls(dataBA4.tree$comune, 2000)
dataBA4.tree$agenzia_tipo <- collapse_lvls(dataBA4.tree$agenzia_tipo, 2000)

sum(is.na.data.frame(dataBA4.tree))
sapply(dataBA4.tree, function(x) sum(is.na(x)))

#an option to dealing with NA's
dataBA4.tree <- subset(dataBA4.tree, select = -c(data_prima_visita, ultimo_ing.x))
dataBA4.tree <- subset(dataBA4.tree, select = -c(agenzia))
dataBA4.tree <- dataBA4.tree[complete.cases(dataBA4.tree),]

trainIndex.tree <- createDataPartition(dataBA4.tree$churn, p = 0.75, list = F)

rpart.fit <- rpart(churn ~ ., dataBA4.tree[trainIndex.tree,-1], method = "class")
summary(rpart.fit)




#punto 3####
#training####
data4training <- dataBA4
data4training$churn[data4training$churn==1] <- "Yes"
data4training$churn[data4training$churn==0] <- "No"
data4training$churn <- as.factor(data4training$churn)
data4training$importo <- as.factor(data4training$importo)
data4training$importo <- collapse_lvls(data4training$importo, 2000)
data4training$sconto <- collapse_lvls(data4training$sconto, 2000)
data4training$riduzione <- collapse_lvls(data4training$riduzione, 2000)
data4training$tipo_pag <- collapse_lvls(data4training$tipo_pag, 2000)
data4training$agenzia_tipo <- collapse_lvls(data4training$agenzia_tipo, 2000)
data4training$comune <- collapse_lvls(data4training$comune, 2000)
data4training$almeno_un_ing <- as.factor(data4training$almeno_un_ing)

sum(is.na.data.frame(data4training))
sapply(data4training, function(x) sum(is.na(x)))

#an option to dealing with NA's
data4training <- subset(data4training, select = -c(data_prima_visita, ultimo_ing.x))
data4training <- subset(data4training, select = -c(agenzia))
data4training <- data4training[complete.cases(data4training),]



set.seed(123)
index <- createDataPartition(y = data4training$churn, p= 0.7, list = FALSE)
data4training.training <- data4training[index,]
data4training.testing <- data4training[-index,]
str(data4training.training)

control <- trainControl(method = "repeatedcv", # use N-fold cross validation
                        number = 5, # the number of folds
                        repeats = 1,
                        classProbs = TRUE, 
                        summaryFunction = twoClassSummary,
                        savePredictions = "final")

#knn 
library(kknn)

knn.fit <- train(churn~.,
                 data = data4training.training[,-1],
                 method = "kknn",
                 preProcess = c("center", "scale"),
                 trControl = control,
                 na.action = na.omit)
knn.fit
save.image("~/Desktop/BA_tr_1.RData")
#pls 
library(pls)
pls.fit <- train(churn~.,
                 data = data4training.training[,-1],
                 method="pls",
                 #metric = "accuracy" ,
                 na.action=na.omit,
                 trControl = control
)
summary(pls.fit)
pls.fit
save.image("~/Desktop/BA_tr_2.RData")


install.packages("import")
install.packages("mboost")
library("import")
library("mboost")
bGAM.fit <- train(churn~hyp_spesa_abb+età+ingressi_tot+periodo_attività+
                    inter+ricavo_soc+amici_tot.x,
                  data = data4training.training[,-1],
                  method="gamboost",
                  #metric = "accuracy" ,
                  na.action=na.omit,
                  trControl = control
)
bGAM.fit
save.image("~/Desktop/BA_tr_3.RData")


#qda 
install.packages("klaR")
install.packages("MASS")
library("MASS")
library("klaR")
qda.fit <- train(churn~.,
                 data = data4training.training[,-1],
                 method="stepQDA",
                 #metric = "accuracy" ,
                 na.action=na.omit,
                 trControl = control
)
qda.fit
save.image("~/Desktop/BA_tr_4.RData")


#random forest 
install.packages("e1071")
install.packages("ranger")
install.packages("dplyr")
library("e1071")
library("ranger")
library("dplyr")
ranger.fit <- train(churn~.,
                    data = data4training.training[,-1],
                    method="ranger",
                    #metric = "accuracy" ,
                    na.action=na.omit,
                    trControl = control
)
ranger.fit
save.image("~/Desktop/BA_tr_5.RData")


#extreme gradient boosting 
install.packages("xgboost")
library("plyr")
library("xgboost")
extreme.fit <- train(churn~.,
                     data = data4training.training[,-1],
                     method="xgbDART",
                     #metric = "accuracy" ,
                     na.action=na.omit,
                     trControl = control
)
extreme.fit
save.image("~/Desktop/BA_tr_6.RData")

#prediction####
knn.fit.probs <- predict(knn.fit, newdata=data4training.testing, type="prob")
pls.fit.probs <- predict(pls.fit, newdata=data4training.testing, type="prob")
bGAM.fit.probs <- predict(bGAM.fit, newdata=data4training.testing, type="prob")
qda.fit.probs <- predict(qda.fit, newdata=data4training.testing, type="prob")
ranger.fit.probs <- predict(ranger.fit, newdata=data4training.testing, type="prob")
extreme.fit.probs<- predict(extreme.fit, newdata=data4training.testing, type="prob")
#definire la matrice di costi e benefici individuale ####

#
#Y|p beneficio uguale alla stima del ricavo per l'associazione o il ricavo dell'anno prima - il costo del contatto
#Y|n -1 spreco un contatto 
#N|p 0 non ho costi derivanti dall'abbandono 
#N|n 0 non contatto perchè non abbandona
ben_Yp <- data4training.testing$ricavo_soc
cst_Yn <- rep(-1, length(data4training.testing$ricavo_soc))
cst_Np <- rep(0, length(data4training.testing$ricavo_soc))
ben_Nn <- rep(0, length(data4training.testing$ricavo_soc))

#optimizing the treshold####
threshold_opt <- function(algo.fit.probs) {
  cutoffs <- seq(0.1,0.9,0.1)
  accuracy <- NULL
  false_positive <- NULL
  for (i in seq(along = cutoffs)){
    prediction <- ifelse(algo.fit.probs[[2]] >= cutoffs[i], "Yes", "No") #Predicting for cut-off
    accuracy <- c(accuracy, length(which(data4training.testing$churn == prediction))/length(prediction)*100)
    conf.mat <- confusionMatrix(as.factor(prediction), data4training.testing$churn, positive = "Yes")
    false_positive <- c(false_positive, conf.mat$table[2]/length(data4training.testing$codcliente))
  }
  par(mfrow=c(2,1))
  p1 <- plot(cutoffs, accuracy, pch =19,type='b',col= "steelblue",
        xlab="Cutoff Level", ylab = "Accuracy %")
  p2 <- plot(cutoffs, false_positive, pch =19,type='b',col= "orange",
       xlab="Cutoff Level", ylab = "false_positive %")
  par(mfrow=c(1,1))
}

threshold_opt(knn.fit.probs)
threshold_opt(pls.fit.probs)
threshold_opt(bGAM.fit.probs)
threshold_opt(qda.fit.probs)
threshold_opt(ranger.fit.probs)
threshold_opt(extreme.fit.probs)

#from numerical probs to class probs(y, n) by treshold####
contrasts(data4training$churn)  # Yes = 1, No = 0
knn.fit.probs.class <- rep("No", length(knn.fit.probs[[2]]))
knn.fit.probs.class[knn.fit.probs[[2]] > 0.5] <- "Yes" #treshold 0.5
knn.fit.probs.class <- as.factor(knn.fit.probs.class)
pls.fit.probs.class <- rep("No", length(pls.fit.probs[[2]]))
pls.fit.probs.class[pls.fit.probs[[2]] > 0.5] <- "Yes"
pls.fit.probs.class <- as.factor(pls.fit.probs.class)
bGAM.fit.probs.class <- rep("No", length(bGAM.fit.probs[[2]]))
bGAM.fit.probs.class[bGAM.fit.probs[[2]] > 0.5] <- "Yes"
bGAM.fit.probs.class <- as.factor(bGAM.fit.probs.class)
qda.fit.probs.class <- rep("No", length(qda.fit.probs[[2]]))
qda.fit.probs.class[qda.fit.probs[[2]] > 0.5] <- "Yes"
qda.fit.probs.class <- as.factor(qda.fit.probs.class)
ranger.fit.probs.class <- rep("No", length(ranger.fit.probs[[2]]))
ranger.fit.probs.class[ranger.fit.probs[[2]] > 0.5] <- "Yes"
ranger.fit.probs.class <- as.factor(ranger.fit.probs.class)
extreme.fit.probs.class <- rep("No", length(extreme.fit.probs[[2]]))
extreme.fit.probs.class[extreme.fit.probs[[2]] > 0.5] <- "Yes"
extreme.fit.probs.class <- as.factor(extreme.fit.probs.class)
#confusion matrix####
cnf_mat.knn <- confusionMatrix(knn.fit.probs.class, data4training.testing$churn, positive = "Yes")
cnf_mat.knn$table
cnf_mat.pls <- confusionMatrix(pls.fit.probs.class, data4training.testing$churn, positive = "Yes")
cnf_mat.bGAM <- confusionMatrix(bGAM.fit.probs.class, data4training.testing$churn, positive = "Yes")
cnf_mat.qda <- confusionMatrix(qda.fit.probs.class, data4training.testing$churn, positive = "Yes")
cnf_mat.ranger <- confusionMatrix(ranger.fit.probs.class, data4training.testing$churn, positive = "Yes")
cnf_mat.extreme <- confusionMatrix(extreme.fit.probs.class, data4training.testing$churn, positive = "Yes")

#creare la matrice di confusione sotto forma di tassi e trovare le P(A) e le P(A|B)####
act_p <- (cnf_mat.knn$table[3] + cnf_mat.knn$table[4])/length(data4training.testing$codcliente) #prob
act_n <- (cnf_mat.knn$table[1] + cnf_mat.knn$table[2])/length(data4training.testing$codcliente) #prob
PYgiven_p <- cnf_mat.knn$table[4]/(cnf_mat.knn$table[3] + cnf_mat.knn$table[4]) #probability
PNgiven_p <- cnf_mat.knn$table[3]/(cnf_mat.knn$table[3] + cnf_mat.knn$table[4]) #probability
PYgiven_n <- cnf_mat.knn$table[2]/(cnf_mat.knn$table[1] + cnf_mat.knn$table[2]) #probability
PNgiven_n <- cnf_mat.knn$table[1]/(cnf_mat.knn$table[1] + cnf_mat.knn$table[2]) #probability
#individual expected profit#### 
#il beneficio è uguale alla variabile ricavo del dataBA4 che indica il ricavo per l'associazione musei derivato da quell'abbonamento 
#Expected Value equazione completa
EV <- act_p*(PYgiven_p*ben_Yp + PNgiven_p*cst_Np) + act_n*(PNgiven_n*ben_Nn + PYgiven_n*cst_Yn)
#fare la curva del profitto atteso####
data4plot <- data4training.testing
data4plot$EV <- EV
data4plot <- data4plot[order(-data4plot$EV),]
data4plot$cumEV <- cumsum(data4plot$EV)
plot(data4plot$cumEV, ylab="cum Profit", xlab="nr instances", col=6, type="l")
abline(v=5000,par(lty = "dashed"))
#
data4plot <- data4training.testing
data4plot$prob.churn <- knn.fit.probs[[2]]
data4plot$EV <- EV
data4plot <- data4plot[order(-data4plot$prob.churn),]
data4plot$cumEV <- cumsum(data4plot$EV)
plot(data4plot$cumEV, ylab="cum Profit", xlab="nr instances", col=6, type="l")
abline(v=5000,par(lty = "dashed"))

#creating plot function####
profit_plot <- function(cnf_mat) {
  #creare la matrice di confusione sotto forma di tassi e trovare le P(A) e le P(A|B)###
  act_p <- (cnf_mat$table[3] + cnf_mat$table[4])/length(data4training.testing$codcliente) #prob
  act_n <- (cnf_mat$table[1] + cnf_mat$table[2])/length(data4training.testing$codcliente) #prob
  PYgiven_p <- cnf_mat$table[4]/(cnf_mat$table[3] + cnf_mat$table[4]) #probability
  PNgiven_p <- cnf_mat$table[3]/(cnf_mat$table[3] + cnf_mat$table[4]) #probability
  PYgiven_n <- cnf_mat$table[2]/(cnf_mat$table[1] + cnf_mat$table[2]) #probability
  PNgiven_n <- cnf_mat$table[1]/(cnf_mat$table[1] + cnf_mat$table[2]) #probability
  #individual expected profit###
  
  #Expected Value equazione completa
  EV <- act_p*(PYgiven_p*ben_Yp + PNgiven_p*cst_Np) + act_n*(PNgiven_n*ben_Nn + PYgiven_n*cst_Yn)
  #fare la curva del profitto atteso####
  data4plot <- data4training.testing
  data4plot$EV <- EV
  data4plot <- data4plot[order(-data4plot$EV),]
  data4plot$cumEV <- cumsum(data4plot$EV)
  plot(data4plot$cumEV, ylab="cum Profit", xlab="nr instances", col=6, type="l")
  p <- abline(v=5000,par(lty = "dashed"))
  p
  return(data4plot$cumEV[5000])
}
#
profit_plot(cnf_mat.knn)
profit_plot(cnf_mat.pls)
profit_plot(cnf_mat.qda)
profit_plot(cnf_mat.bGAM)
profit_plot(cnf_mat.ranger)
profit_plot(cnf_mat.extreme)
#the algorithm which maximize profits is the random forest.
ranger.fit.OptimalProbs <- predict(ranger.fit, data4training, type = "prob")
ranger.fit.probs.class <- rep("No", length(ranger.fit.OptimalProbs[[2]]))
ranger.fit.probs.class[ranger.fit.OptimalProbs[[2]] > 0.5] <- "Yes"
ranger.fit.probs.class <- as.factor(ranger.fit.probs.class)
cnf_mat.ranger.Full <- confusionMatrix(ranger.fit.probs.class, data4training$churn, positive = "Yes")
#defining the components for the expected value 
ben_Yp <- data4training$ricavo_soc
cst_Yn <- rep(-1, length(data4training$ricavo_soc))
cst_Np <- rep(0, length(data4training$ricavo_soc))
ben_Nn <- rep(0, length(data4training$ricavo_soc))
#
profit_plot_basic <- function(cnf_mat) {
  #creare la matrice di confusione sotto forma di tassi e trovare le P(A) e le P(A|B)###
  act_p <- (cnf_mat$table[3] + cnf_mat$table[4])/length(data4training$codcliente) #prob
  act_n <- (cnf_mat$table[1] + cnf_mat$table[2])/length(data4training$codcliente) #prob
  PYgiven_p <- cnf_mat$table[4]/(cnf_mat$table[3] + cnf_mat$table[4]) #probability
  PNgiven_p <- cnf_mat$table[3]/(cnf_mat$table[3] + cnf_mat$table[4]) #probability
  PYgiven_n <- cnf_mat$table[2]/(cnf_mat$table[1] + cnf_mat$table[2]) #probability
  PNgiven_n <- cnf_mat$table[1]/(cnf_mat$table[1] + cnf_mat$table[2]) #probability
  #individual expected profit###
  
  #Expected Value equazione completa
  EV <- act_p*(PYgiven_p*ben_Yp + PNgiven_p*cst_Np) + act_n*(PNgiven_n*ben_Nn + PYgiven_n*cst_Yn)
  #fare la curva del profitto atteso####
  data4plot <- data4training
  data4plot$EV <- EV
  data4plot <- data4plot[order(-data4plot$EV),]
  data4plot$cumEV <- cumsum(data4plot$EV)
  plot(data4plot$cumEV, ylab="cum EV", xlab="nr instances", col=6, type="l")
  p <- abline(v=5000,par(lty = "dashed"))
  p
  View(data4plot)
  return(data4plot$cumEV[5000])
}
profit_plot_basic(cnf_mat.ranger.Full)
#Questo approccio ha una serie di problemi, come il non prendere in considerazione la probabilità del churn e non tenere conto dell'interdipendenza tra le persone che vanno insieme al museo

profit_plot_Upgrade1 <- function(cnf_mat) {
  ben_Yp <- (data4training$ricavo_soc+(data4training$amici_tot.x*0.5*mean(data4training$ricavo_soc)))*(1-data4training$churn.prob)
  cst_Yn <- rep(-1, length(data4training$ricavo_soc))
  cst_Np <- rep(0, length(data4training$ricavo_soc))
  ben_Nn <- rep(0, length(data4training$ricavo_soc))
  #creare la matrice di confusione sotto forma di tassi e trovare le P(A) e le P(A|B)###
  act_p <- (cnf_mat$table[3] + cnf_mat$table[4])/length(data4training$codcliente) #prob
  act_n <- (cnf_mat$table[1] + cnf_mat$table[2])/length(data4training$codcliente) #prob
  PYgiven_p <- cnf_mat$table[4]/(cnf_mat$table[3] + cnf_mat$table[4]) #probability
  PNgiven_p <- cnf_mat$table[3]/(cnf_mat$table[3] + cnf_mat$table[4]) #probability
  PYgiven_n <- cnf_mat$table[2]/(cnf_mat$table[1] + cnf_mat$table[2]) #probability
  PNgiven_n <- cnf_mat$table[1]/(cnf_mat$table[1] + cnf_mat$table[2]) #probability
  #individual expected profit###
  
  EV <- act_p*(PYgiven_p*ben_Yp + PNgiven_p*cst_Np) + act_n*(PNgiven_n*ben_Nn + PYgiven_n*cst_Yn)
  #fare la curva del profitto atteso####
  data4plot <- data4training
  data4plot$EV <- EV
  data4plot <- data4plot[order(-data4plot$EV),]
  data4plot$cumEV <- cumsum(data4plot$EV)
  plot(data4plot$cumEV, ylab="cum Expected Profit", xlab="nr instances", col=6, type="l")
  abline(v=5000,par(lty = "dashed"))
  View(data4plot)
  return(data4plot$cumEV[5000])
}
profit_plot_Upgrade1(cnf_mat.ranger.Full)

#to contact
ben_Yp <- (data4training$ricavo_soc+(data4training$amici_tot.x*0.5*mean(data4training$ricavo_soc)))*(1-data4training$churn.prob)
cst_Yn <- rep(-1, length(data4training$ricavo_soc))
cst_Np <- rep(0, length(data4training$ricavo_soc))
ben_Nn <- rep(0, length(data4training$ricavo_soc))
#creare la matrice di confusione sotto forma di tassi e trovare le P(A) e le P(A|B)###
act_p <- (cnf_mat.ranger.Full$table[3] + cnf_mat.ranger.Full$table[4])/length(data4training$codcliente) #prob
act_n <- (cnf_mat.ranger.Full$table[1] + cnf_mat.ranger.Full$table[2])/length(data4training$codcliente) #prob
PYgiven_p <- cnf_mat.ranger.Full$table[4]/(cnf_mat.ranger.Full$table[3] + cnf_mat.ranger.Full$table[4]) #probability
PNgiven_p <- cnf_mat.ranger.Full$table[3]/(cnf_mat.ranger.Full$table[3] + cnf_mat.ranger.Full$table[4]) #probability
PYgiven_n <- cnf_mat.ranger.Full$table[2]/(cnf_mat.ranger.Full$table[1] + cnf_mat.ranger.Full$table[2]) #probability
PNgiven_n <- cnf_mat.ranger.Full$table[1]/(cnf_mat.ranger.Full$table[1] + cnf_mat.ranger.Full$table[2]) #probability
#individual expected profit###

#Expected Value equazione completa
EV <- act_p*(PYgiven_p*ben_Yp + PNgiven_p*cst_Np) + act_n*(PNgiven_n*ben_Nn + PYgiven_n*cst_Yn)
#fare la curva del profitto atteso####
to_contact <- data4training
to_contact$EV <- EV
to_contact <- to_contact[order(-to_contact$EV),]
to_contact$cumEV <- cumsum(to_contact$EV)
to_contact <- to_contact[1:5000,]

summary(to_contact)
#correlatins and association plot####
churn.prob <- predict(object = ranger.fit, newdata = data4training, type = "prob")
data4training$churn.prob <- churn.prob$Yes
na.omit(data4training) %>% #i don't know why, but sometimes if there're NA's in the data it gives an error
  select(età, churn.prob, periodo_attività, ingressi_tot, sconto, amici_tot.x) %>%
  mixed_assoc() %>%
  select(x, y, assoc) %>%
  spread(y, assoc) %>%
  column_to_rownames("x") %>%
  as.matrix %>%
  as_cordf %>%
  network_plot(min_cor = 0.3, colors = c("orange", "blue"), repel = T, curved = T)