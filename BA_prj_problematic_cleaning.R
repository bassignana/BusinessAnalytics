#####
#problematic cleaning
#campione del dataset contenente solo il cliente con id = 13
dataBA2$CodCliente==13
sum(dataBA2$CodCliente==13)
df <- dataBA2[dataBA2$CodCliente==13,]
df
#      X1 datai   orai   importo museo                      prov_museo com_museo   CodCliente
#1 150542 12/04/… 15:00      5   REGGIA DI VENARIA REALE    TO         VENARIA RE…         13
#2 150543 12/04/… 15:00      5   REGGIA DI VENARIA REALE    TO         VENARIA RE…         13
#3 151255 12/04/… 18:57      2.5 FONDAZIONE SANDRETTO RE R… TO         TORINO              13
#4 161239 14/04/… 15:39      4   "MOSTRA \\ROBERT CAPA - R… TO         TORINO              13
#5 332519 13/07/… 17:07      3   FORTE DI BARD              AO         BARD                13
#6 332521 13/07/… 17:07      2.5 FORTE DI BARD              AO         BARD                13
#7 407348 03/09/… 17:42      4.5 MUSEO NAZIONALE DEL CINEMA TO         TORINO              13
#8 519516 16/11/… 15:50      5   MAO                        TO         TORINO              13
#9 532136 24/11/… 14:45      2   PAV                        TO         TORINO              13

#come si vede, le prime due righe mostrano due ingressi fatti con lo stesso abbonamento
#nello stesso minuto

#la riga 5 e 6 mostrano come vi siano due ingressi nello stesso minuto ma con importi 
#differenti

#la mia ipotesi è che la prima variabile, qui X1, contrassegna in ordine cronologico 
#tutti gli utilizzi dell'abbonamento, distinguendo anche quelli fatti all'interno dello
#stesso minuto ma in momenti differenti, da cui dedurrei:
#--l'abbonamento è stato utilizzato due volte come si vede in riga 1 e 2, dove la prima è
#contrassegnata dal valore di X1 150542 e la seconda dal valore 150543
#--l'abbonamento è stato utilizzato due volte come si vede in riga 5 e 6, MA è stato
#utilizzato un altro abbonamento nel frattempo: si vede dai valori di X1 332519 e 332521
#(infatti nel database completo si vede che un'altro abb è stato utilizzato a torino)

#Se si assume che siano tutti gli ingressi siano stati fatti da persone diverse
#e non siano dei semplici record duplicati, con l'ipotesi che il primo ad utilizzare
#l'abbonamento sia il possessore effettivo
#si ha un totale di 125945 ingressi fatti dal vero possessore sui 545085 ingressi registrati

#per rimuovere i "duplicati" ho usato la seguente funzione
duplicated(df[,1]) #righe in cui il valore della prima colonna è duplicato
duplicated(df[,2]) #righe in cui il valore della seconda colonna è duplicato
duplicated(df[,c(2,3)]) #righe in cui il valore della seconda colonna E della terza(congiuntamene) è duplicato
duplicated(df[,c(2,3,4)]) #...

#se volessimo tenere solo in considerazione gli ingressi del vero proprietario, 
#assumendo che sia il primo in ordine temporale ad aver utilizzato l'abbonameto

#righe da rimuovere
df[duplicated(df[,c(2,3)]),]
#X1 datai    orai   importo museo                   prov_museo com_museo     CodCliente
#1 150543 12/04/13 15:00      5   REGGIA DI VENARIA REALE TO         VENARIA REALE         13
#2 332521 13/07/13 17:07      2.5 FORTE DI BARD           AO         BARD                  13



#sullintero dataset avrei dimensioni di partenza
dim(dataBA2)
#   545085      8

#righe da rimuovere dall'intero dataset
dataBA2 <- dataBA2[order(dataBA2$CodCliente),] #gli ID dei clieni devono essere crescenti per far funzionare il metodo di pulizia
dim(dataBA2[duplicated(dataBA2[,c(2,3)]),])
# 419140 righe da rimuovere, e non sono neanche tutte, perchè non sono state eliminate
#tutte quelle righe dove gli abbonamenti sono stati timbrati ad una distanza in minuti
#maggiore di 0. Comunque, anche tralasciando questi ultimi casi, avremmo effettivamente
#solo 545085 - 419140 = 125945 ingressi "validi" e mi corregga se sbaglio, si potranno 
#usare solo questi ultimi per fare rispondere ai 3 punti dato che sarebbe erroneo
#attribuire le altre entrate ai proprietari dell'abbonamento

