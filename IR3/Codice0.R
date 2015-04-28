#Lista di tutti gli stem
datahere = "C:/Users/Marco/Desktop/IR3"
setwd(datahere)
path="output.txt"
pesi=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(pesi)=c("stem","doc","peso")
#Vettore con tutti gli stems (unici e mai ripetuti)
listastems=unique(pesi[, "stem"])

#Matrice con tutti i pesi degli stem dei vari documenti (3204 righe e 5222 colonne)
C=matrix(0, max(pesi[,2]), length(listastems))
for (i in 1:max(pesi[,2]))
{
    usedstems=pesi[pesi[, "doc"]==i,1]
    usedweights=pesi[pesi[, "doc"]==i,3]
    for (j in 1:length(usedstems))
    {    
        pos=match(usedstems[j], listastems)
        C[i,pos]= usedweights[j]
	}
} 

#Normalizzazione matrice dei pesi
for (i in 1:max(pesi[,2]))
{
    maxw=max(C[i,]);
    C[i,]=C[i,]/maxw;
} 

#Matrice con tutti gli stem delle varie query (5222 righe e 64 colonne)
path="query-stem.txt"
query=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(query)=c("queryid","stem")
#Vettore con tutti le query (uniche e mai ripetute)
listaquery=unique(query[, "queryid"])
listaquery=sort(listaquery)
Y=matrix(0, length(listastems), length(listaquery))
for (i in 1:length(listaquery))
{
    usedstems=query[query[, "queryid"]==i,2]
    for (j in 1:length(usedstems))
    {    
        Y[match(usedstems[j], listastems),i]=1
	}
}

#Normalizzazione
for (i in 1:length(listastems))
{
    maxw=max(Y[i,]);
	#Evito di dividere per 0 nel caso sia tutto nullo
	if(maxw!=0)
	{
		Y[i,]=Y[i,]/maxw;
	}
}

#Rank per le varie query (al momento senza correlazione)
#MANCA CORRELAZIONE!
ranks=C%*%Y

#Stampa
for (i in 1:length(listaquery))
{
	#Creo un Data-Frame per sfruttare "order", che mi ordina le colonne in automatico
	dt=data.frame(ncol=2, col.names = c("doc", "rank"));
	dt=data.frame(1:max(pesi[,2]), ranks[,i]);
	dt=dt[order(-dt[,2]),];
    for (j in 1:length(dt[,2]))
	{
		#Non stampo i giudizi nulli, altrimenti avrei troppe righe in output
		if(dt[j,2]!=0)
		{
			prova = paste(i,"Q0",dt[j,1],j,dt[j,2],"G7R1");
			cat(prova, file="output2.txt", sep="\t", append=TRUE)
			cat("\n", file="output2.txt", append=TRUE)
		}
	}
}