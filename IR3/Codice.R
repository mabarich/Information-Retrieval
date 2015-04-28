#Lista di tutti gli stem
datahere = "C:/Users/Marco/Desktop/IR3"
setwd(datahere)
path="output.txt"
pesi=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(pesi)=c("stem","doc","peso")
#Vettore con tutti gli stems (unici e mai ripetuti)
listastems=unique(pesi[, "stem"])

#Matrice con tutti I pesi degli stem dei vari documenti (3204 righe e 5222 colonne)
C=matrix(0, max(pesi[,2]), length(listastems))
T=matrix(0, max(pesi[,2]), length(listastems))
for (i in 1:max(pesi[,2]))
{
    usedstems=pesi[pesi[, "doc"]==i,1]
    usedweights=pesi[pesi[, "doc"]==i,3]
    for (j in 1:length(usedstems))
    {    
        pos=match(usedstems[j], listastems)
        C[i,pos]= usedweights[j]
		T[i,pos]= usedweights[j]
	}
} 

#Normalizzazione matrice dei pesi della matrice C
for (i in 1:max(pesi[,2]))
{
    maxw=max(C[i,]);
    C[i,]=C[i,]/maxw;
} 

#Uso la matrice C non normalizzata per il calcolo di R
R=(t(T)%*%T)
#Normalizzazione matrice di correlazione R (normalizzazione attraverso la norma del vettore #della base)
for (i in 1:length(listastems))
{
    norm=sqrt(sum(R[i,]^2));
    R[i,]=R[i,]/norm;
} 








C1=matrix(0, max(pesi[,2]), length(listastems))
for (i in 1:max(pesi[,2]))
{
    usedstems=pesi[pesi[, "doc"]==i,1]
    usedweights=pesi[pesi[, "doc"]==i,3]
    for (j in 1:length(usedstems))
    {    
        pos=match(usedstems[j], listastems)
        C1[i,pos]= usedweights[j]
	}
} 
for (i in 1:3204)
{
    norm=sqrt(sum(C1[i,]^2));
    C1[i,]=C1[i,]/norm;
}
R=(t(C1)%*%C1)
for (i in 1:length(listastems))
{
    norm=sqrt(sum(R[i,]^2));
    R[i,]=R[i,]/norm;
} 
ranks=C1%*%R%*%Y

#Stampa
for (i in 1:length(listaquery))
{
    #Creo un Data-Frame per sfruttare "order", che mi ordina le colonne in automatico
    dt=data.frame(ncol=2, col.names = c("doc", "rank"));
    dt=data.frame(1:max(pesi[,2]), ranks[,i]);
    dt=dt[order(-dt[,2]),];
    for (j in 1:1000)
    {
        {
            prova = paste(i,"Q0",dt[j,1],j,dt[j,2],"G7R3");
            cat(prova, file="output_lab3.txt", sep="\t", append=TRUE)
            cat("\n", file="output_lab3.txt", append=TRUE)
        }
    }
}








media=mean(R)
for(i in 1:5222)
{
	for(j in 1:5222)
	{
		if(R[i,j]<media)
		{
			R[i,j]=0;
		}
	}
}









#Matrice contenente tutti gli stem delle varie query (5222 righe e 64 colonne), ogni colonna #rappresenta una query
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

#Rank per le varie query (al momento senza correlazione)
ranks=C%*%R%*%Y

#Stampa
for (i in 1:length(listaquery))
{
    #Creo un Data-Frame per sfruttare "order", che mi ordina le colonne in automatico
    dt=data.frame(ncol=2, col.names = c("doc", "rank"));
    dt=data.frame(1:max(pesi[,2]), ranks[,i]);
    dt=dt[order(-dt[,2]),];
    for (j in 1:1000)
    {
        {
            prova = paste(i,"Q0",dt[j,1],j,dt[j,2],"G7R3");
            cat(prova, file="output_lab3.txt", sep="\t", append=TRUE)
            cat("\n", file="output_lab3.txt", append=TRUE)
        }
    }
}
