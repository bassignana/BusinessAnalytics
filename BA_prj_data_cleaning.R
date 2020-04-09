#Depending on how you imported the csv file, you might have to trasform
#all the character variables into factors

#DataBA1 DataBA2 DataBA3 sono i nomi che ho dato ai 3 dataset.

#changing character to factors
character_vars <- lapply(dataBA1, class) == "character"
dataBA1[, character_vars] <- lapply(dataBA1[, character_vars], as.factor)
character_vars2 <- lapply(dataBA2, class) == "character"
dataBA2[, character_vars2] <- lapply(dataBA2[, character_vars2], as.factor)
character_vars3 <- lapply(dataBA3, class) == "character"
dataBA3[, character_vars3] <- lapply(dataBA3[, character_vars3], as.factor)

#from character to date variables
dataBA1$data_inizio <- as.Date(dataBA1$data_inizio, format = "%d /%m /%y")
dataBA2$datai <- as.Date(dataBA2$datai, format = "%d /%m /%y")
#%Y con Y maiuscola per gli anni a 4 cifre
dataBA3$ultimo_ing.x <- as.Date(dataBA3$ultimo_ing.x, format = "%Y -%m -%d")
dataBA3$abb13 <- as.Date(dataBA3$abb13, format = "%Y -%m -%d")
dataBA3$abb14 <- as.Date(dataBA3$abb14, format = "%Y -%m -%d")

#controllare che nelle variabilii qualitative non ci siano NA o due categorie che dovrebbero essere una sola
levels(dataBA1$sconto)
levels(dataBA1$riduzione)
#no NA
levels(dataBA1$tipo_pag)
#no na
levels(dataBA1$agenzia)
#no na
levels(dataBA1$agenzia_tipo)
sum(dataBA1$agenzia_tipo=="DATO MANCANTE")
#rimuovere i 50 dati mancanti
dataBA1 <- dataBA1[!dataBA1$agenzia_tipo=="DATO MANCANTE",]
levels(dataBA1$sesso)
#no na
levels(dataBA1$professione)
#rimuovere professione
dataBA1 <- dataBA1[,-12]
levels(dataBA1$nuovo_abb)
sum(dataBA1$nuovo_abb=="VECCHIO ABBONATO")
#solo un blank ""
#rimozione blank
dataBA1 <- dataBA1[!dataBA1$nuovo_abb=="",]
levels(dataBA2$museo)
#NESSUN NA
levels(dataBA2$prov_museo)
levels(dataBA2$com_museo)
sum(dataBA2$com_museo=="")
#un solo blank
#rimozione
dataBA2 <- dataBA2[!dataBA2$com_museo=="",]
#gli NA non sono da rimuovere qui poichè collegate al churn