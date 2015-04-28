#Leggo da file
datahere = "C:/Users/Marco/Desktop/IR2"
setwd(datahere)
path="freq.docid.stem.txt"
stems=read.table(text=readLines(path))
#Do un nome alle variabil
names(stems)=c("freq","id","stem")
#Vettore con tutti gli stems (unici e mai ripetuti)
listastems=unique(stems[, "stem"])
#Vettore con tutti i documenti
listadocumenti=unique(stems[, "id"])
#Calcolo N
N=3204 
#Calcolo nj

#DT è un data-frame con 2 colonne. Nella prima c’è lo stem, nella seconda nj (numero di #documenti in cui appare il descrittore)
dt=data.frame(ncol=2, col.names = c("stem", "cont"))
dt=data.frame(listastems, 1:5222)
for (i in 1:length(listastems)) 
{
	dt[i,2]=nrow(stems[stems[, "stem"]==listastems[i],])
}

#Calcolo il peso
for(i in 1:nrow(stems))
{
	frequenza=stems[i,1];
	peso_specifico_descrittore=log(N/(dt[dt[,"listastems"]==stems[i,3],2]));
	peso_generalita_documento=10/(nrow(stems[stems[, "id"]==stems[i,2],]));
	risultato=frequenza*peso_specifico_descrittore*peso_generalita_documento;
	prova = paste(stems[i,3],stems[i,2],risultato);
	cat(prova, file="output.txt", sep="\t", append=TRUE)
	cat("\n", file="output.txt", append=TRUE)
}
