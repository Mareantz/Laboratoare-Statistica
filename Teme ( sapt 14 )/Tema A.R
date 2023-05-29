#A1 (a)
A1a = function(lambda, p, n, k) {
  x = k:n
  poisson = dpois(x, lambda)
  geometric = dgeom(x, p)
  binomial = dbinom(x, n, p)
  barplot(poisson,col="red")
  barplot(geometric,add=T,col="blue")
  barplot(binomial,add=T,col="green")
}

A1a(2, 0.3, 10, 1)

#A1 (b)
p=0.3
x=seq(1,10000,by=2)
P_odd=sum(dgeom(x,p))
P_geq_4=sum(dgeom(4:50,p))
P_leq_20=sum(dgeom(1:20,p))
cat("P_odd = ", P_odd,"\n")
cat("P_>=4 = ", P_geq_4,"\n")
cat("P_<=20 = ", P_leq_20,"\n")

#A1 (c)
A1c = function(lambda) 
{
  k = 0
  p = 1
  while (p >= 1e-7) 
  {
    k = k + 1
    p = ppois(k , lambda, lower.tail = FALSE)
  }
  return(k)
}
A1c(2)

#A2 (a)
A2a=function(fisier) 
{
  note=read.csv(fisier, header = T, sep=',')
  noteP=note[['P']]
  noteS=note[['S']]
  medianP = median(noteP)
  medianS = median(noteS)
  meanP = mean(noteP)
  meanS = mean(noteS)
  sdP = sd(noteP)
  sdS = sd(noteS)
  quartilesP = c(as.vector(quantile((noteP)))[1+1], as.vector(quantile((noteP)))[3+1])
  quartilesS = c(as.vector(quantile((noteS)))[1+1], as.vector(quantile((noteS)))[3+1])
  
  cat("\nnote1 Statistics:\n")
  cat("Median: ", medianP, "\n")
  cat("Mean: ", meanP, "\n")
  cat("Standard Deviation: ", sdP, "\n")
  cat("Quartiles: ", quartilesP, "\n")
  
  cat("\nnote2 Statistics:\n")
  cat("Median: ", medianS, "\n")
  cat("Mean: ", meanS, "\n")
  cat("Standard Deviation: ", sdS, "\n")
  cat("Quartiles: ", quartilesS, "\n")
}

A2a("note.csv")

#A2 (b)
A2b=function(fisier, ensantion) 
{
  note=read.csv(fisier, header = T, sep=',')
  note_ensantion=note[[ensantion]]
  M=mean(note_ensantion)
  S=sd(note_ensantion)
  cat("\nMedia ensantion", ensantion, ":", M, " sd: ", S, "\n")
  note1=vector()
  j=0
  cat("Valorile aberante sunt: ")
  for(i in 1:length(note_ensantion)) {
    if((note_ensantion[i]>=(M-2*S))&(note_ensantion[i]<=(M+2*S))) {
      j=j+1
      note1[j]=note_ensantion[i]
    }
    else
      cat(note_ensantion[i], " ") #valorile aberante
  }
  cat("\n")
  return(note1) #vectorul curatat
}

A2b("note.csv",'P')
A2b("note.csv",'S')

#A2 (c)
A2c=function(fisier) 
{
  note=read.csv(fisier, header = T, sep=',')
  x=A2b("note.csv", 'P')
  y=A2b("note.csv", 'S')
  interval = seq(0, 10, 1)
  hist(x, breaks = interval, freq = T, right = T, col = "blue")
  hist(y, breaks = interval, freq = T, right = T, col = "red")
}

A2c("note.csv")