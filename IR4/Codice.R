#Lista di tutti i documenti
datahere = "C:/Users/Marco/Desktop/IR4"
setwd(datahere)
path="output.txt"
pesi=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(pesi)=c("stem","doc","peso")
#Vettore con tutti gli stems (unici e mai ripetuti)
listastems=unique(pesi[, "stem"])

#Matrice con tutti i pesi degli stem dei vari documenti (3204 righe e 5222 colonne)
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
		T[i,pos]= 1
	}
} 

#Normalizzazione matrice dei pesi della matrice C
for (i in 1:max(pesi[,2]))
{
    maxw=max(C[i,]);
    C[i,]=C[i,]/maxw;
} 

#Matrice contenente tutti gli stem delle varie query (5222 righe e 64 colonne), ogni colonna #rappresenta una query
path="query-stem.txt"
query=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(query)=c("queryid","stem")
#Vettore con tutti le query (uniche e mai ripetute)
listaquery=unique(query[, "queryid"])
listaquery=sort(listaquery)
#Matrice per RF pseudo
Y=matrix(0, length(listastems), length(listaquery))
for (i in 1:length(listaquery))
{
    usedstems=query[query[, "queryid"]==i,2]
    for (j in 1:length(usedstems))
    {    
        Y[match(usedstems[j], listastems),i]=1
	}	
}
#Matrice per RF esplicito
Y1=Y

#Lista di tutti i rank prodotti nel terzo laboratorio
path="output_lab3.txt"
ranks=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(ranks)=c("query","Q0","doc","rank","giudizio","run")

#Lista dei "giudizi dell'utente"
path="qrels-treceval.txt"
user=read.table(text=readLines(path))
#Assegnazione del nome delle variabili
names(user)=c("query","Q0","doc","1")

#RF pseudo: aggiungo i primi 50 e sottraggo i 150 successivi
#r e n fissati a priori
r=50
n=200
a=1/r
b=-(1/(n-r))
for (i in 1:length(listaquery))
{
	useddocs=ranks[ranks[, "query"]==i,3]
	useddocs=useddocs[1:n]
    for (j in 1:r)
    {    
        Y[,i]=Y[,i]+(a*C[useddocs[j],])
	}
	for (j in (r+1):n)
    {    
        Y[,i]=Y[,i]+(b*C[useddocs[j],])
	}
}

#RF esplicita: aggiungo i primi 50 e sottraggo i 150 successivi
#n fissato a priori, r è la cardinalità dell'intersezione
n=100
for (i in 1:length(listaquery))
{
	useddocs=ranks[ranks[, "query"]==i,3];
	useddocs=useddocs[1:n];
	userdocs=user[user[, "query"]==i,3];
	intersection=intersect(useddocs, userdocs);
	notintersection=setdiff(useddocs,intersection);
	r=length(intersection);
	if (r!=0)
	{
		a=1/r;
		b=-(1/(n-r));
		for (j in 1:r)
		{    
			Y1[,i]=Y1[,i]+(a*C[intersection[j],]);
		}
		for (k in 1:(n-r))
		{    
			Y1[,i]=Y1[,i]+(b*C[notintersection[k],]); 
		}
	}
}

#Uso la matrice C non normalizzata per il calcolo di R
R=(t(C)%*%C)
#Normalizzazione matrice di correlazione R (normalizzazione attraverso la norma del vettore #della base)
for (i in 1:length(listastems))
{
    norm=sqrt(sum(R[i,]^2));
    R[i,]=R[i,]/norm;
} 

#Ricalcolo per RF pseudo
ranks=C%*%R%*%Y
#Ricalcolo per RF esplicita
ranks1=C%*%R%*%Y1

#Stampa per RF pseudo
for (i in 1:length(listaquery))
{
    #Creo un Data-Frame per sfruttare "order", che mi ordina le colonne in automatico
    dt=data.frame(ncol=2, col.names = c("doc", "rank"));
    dt=data.frame(1:max(pesi[,2]), ranks[,i]);
    dt=dt[order(-dt[,2]),];
    for (j in 1:1000)
    {
        {
            prova = paste(i,"Q0",dt[j,1],j,dt[j,2],"G7R3P");
            cat(prova, file="output_lab3P.txt", sep="\t", append=TRUE)
            cat("\n", file="output_lab3P.txt", append=TRUE)
        }
    }
}

#Stampa per RF esplicita
for (i in 1:length(listaquery))
{
    #Creo un Data-Frame per sfruttare "order", che mi ordina le colonne in automatico
    dt=data.frame(ncol=2, col.names = c("doc", "rank"));
    dt=data.frame(1:max(pesi[,2]), ranks1[,i]);
    dt=dt[order(-dt[,2]),];
    for (j in 1:1000)
    {
        {
            prova = paste(i,"Q0",dt[j,1],j,dt[j,2],"G7R3E");
            cat(prova, file="output_lab3E.txt", sep="\t", append=TRUE)
            cat("\n", file="output_lab3E.txt", append=TRUE)
        }
    }
}
