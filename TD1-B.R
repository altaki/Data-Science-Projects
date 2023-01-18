#TD1
#Part A #Introduction to R

#import library
#The pacman package is an R package management tool that combines the functionality
#of base library related functions into intuitively named functions.
install.packages("dplyr")
library("dplyr")
library(ggplot2)
library(pacman)
require(pacman)
library("readxl")

#used to gather information about the current working pathname or default working directory.
getwd()   #curent working directory

setwd("C:/Program Files/RStudio")  #change working directory


#appeal for the data
tableau = read_excel("data-c-mm.xlsx")


#The first 6 rows of the dataframe
head(tableau)
#Shows basic statistic informations about your dataset
summary(tableau)

#Check if your data is a dataframe
print(is.data.frame(tableau))
#Print nb of rows and columns
print(ncol(tableau))
print(nrow(tableau))
#print dimension of your dataset
dim(tableau)



#This command shows the columns Nombre.de.skieurs
tableau$Nombre_de_skieurs

#This command shows all the rows of 11 columns
tableau[,12]

#Calculate the mean of nb of skieurs 
mean(tableau$Nombre_de_skieurs)



#1st question #graphics
#boxplot() #hist() #pie()

plot(tableau$`Neige_Station₍m)`, tableau$Nombre_de_skieurs)


#2sd question #look at the correlation between these two variables
cor(tableau$`Neige_Station₍m)`, tableau$Nombre_de_skieurs)

#quantitative and qualitative variables
#diagramme en baton==boxplot      #diagramme à moustaches==barplot

#create a vector jours
jours=c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche") 

#The rep(x) function replicate the value 'x' 
#illustrer le nb moyenne de skieurs par jour sur un diagramme
moyennes=rep(0,7) 

k=0
for (i in jours)
{
  k=k+1 
  moyennes[k]=mean(as.numeric(unlist(tableau[tableau$`Jour de la semaine`==i,12])))
}

barplot(moyennes, 
        xlab = "jours",
        ylim = c(0, 20000),
        names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
        cex.sub = 0.5, cex.main=0.5, cex.axis=0.8, cex.lab=1)

#Factor() function is used to categorize data

tableau$`Jour de la semaine`=factor(tableau$`Jour de la semaine`,levels=jours)
boxplot(tableau$Nombre_de_skieurs~tableau$`Jour de la semaine`,
        xlabel="Jour de la semaine",
        ylabel="Nombre of Skieurs",
        cex.axis = 0.6)






#iflese command return 1 if 1 if nb. of skieurs>14000, 0 else.
tableau$ForteFreq=ifelse(tableau[,12]>14000,1,0)
#table()uses the cross-classifying factors to build a contingency table
#of the counts at each combination of factor levels.
table(ForteFreq)
table(tableau$`Vacances Zone A`,tableau$ForteFreq)
weekend=ifelse(tableau$`Jour de la semaine`=="Samedi"|tableau$`Jour de la semaine`=="Dimanche",1,0)
tableau=cbind(tableau,weekend)
table(tableau$weekend,tableau$ForteFreq)

tableau[tableau$Nombre_de_skieurs>20000,]
tableau2=tableau[tableau$Enneigment_moyen_des_pistes>1.5,]

mean(tableau$Nombre_de_skieurs)
mean(tableau2$Nombre_de_skieurs)




#PART B
tableau$`Vacances Zones B`

#Q1 faire une representation graphique de chaque variable


#allows u to group by vacances zone A 
tableau %>% group_by(`Vacances Zone A`) %>% summarise(count=n())
tbl <- with(tableau, table(`Vacances Zone A`)); barplot(tbl, legend = TRUE) 

#plot zone A and zone B on the same figure
tbl <- with(tableau, table(`Vacances Zone A`, `Vacances Zones B`));
     barplot(tbl, legend = TRUE)


tbl <- with(tableau, table(`Vacances Zone A`)); 
pie(tbl, legend = TRUE)


hist(tableau$Nombre_de_skieurs)
plot(tableau$Nombre_de_skieurs)
boxplot(tableau$Nombre_de_skieurs)

#Q2 calculer la moyenne, mediane, les quartiles de variables nb of skieurs
mean(tableau$Nombre_de_skieurs)
median(tableau$Nombre_de_skieurs)
quantile(tableau$Nombre_de_skieurs,0.75)-quantile(tableau$Nombre_de_skieurs,0.25)
max(tableau$Nombre_de_skieurs)-min(tableau$Nombre_de_skieurs)


#Q3 calculer la somme des 50 premiers entiers using a for loop
S=0
for (i in 1:50)
{
  S=S+i
}
S

#Q4 write the exp code
expn=function(x,n)
{
  S=0
  for (i in 0:n)
  {
    S=S+x^i/factorial(i)
  }
  return(S)
}

#Q5 calculate the mean of a vector

moyenne=function(X)
{
  n=length(X)
  S=0
  for (i in 1:n)
  {
    S=S+X[i]
  }
  return(S/n)
}

#Q6 calculer la variance
variance=function(X)
{
  n=length(X)
  m=moyenne(X)
  V=0
  for (i in 1:n)
  {
    V= V + (X[i]-m)^2
  }
  return(V/n)
}

#Q7 write a code to combute median
mediane=function(X)
{
  n=length(X)
  Y=sort(X)
  if (n%%2==1)
  {
    R=Y[(n+1)/2]
  }
  else
  {
    R=1/2*(Y[n/2]+Y[n/2+1])
  }
  return(R)
}

#Q8 add a columns which contains the snow depth in the resort in feet
neigestation.pieds=tableau$`Neige_Station₍m)`*3.28
tableau2=cbind(tableau, neigestation.pieds)

#Q9 add a columns to the table which contains nb. of Skieurs centré et réduit
skieurscentrereduit=((tableau$Nombre_de_skieurs-mean(tableau$Nombre_de_skieurs))/sd(tableau$Nombre_de_skieurs))
tableau3=cbind(tableau2,skieurscentrereduit)


#Part C

#Q1
Soleil=ifelse(tableau$`Heures de soleil prévue par la météo il y a 3 jours`<=2,"Faible",
              ifelse(tableau$`Heures de soleil prévue par la météo il y a 3 jours`<=5,"Moyen","Beaucoup"))
Soleil
table(Soleil)
Affluence=ifelse(tableau$Nombre_de_skieurs>=20000,"Forte",
                 ifelse(tableau$Nombre_de_skieurs>=10000,"Moyenne","Faible"))
table(Affluence)

#Q2
T=table(Soleil,Affluence)

#Q3
A=matrix(1:12,nrow=3,ncol=4,byrow=TRUE)
A


#Q4
apply(A,1,sum)
apply(A,2,sum)

#Q5
profil=function(X)
{
  return(X/sum(X))
}

t(apply(T,1,profil))
apply(T,2,profil)

#Q6
Khi2=function(X)
{
  n=sum(X)
  TotLigne=apply(X,1,sum)
  TotColonne=apply(X,2,sum)
  Th=TotLigne%*%t(TotColonne)/sum(X)
  Distance=(X-Th)^2/Th
  return(sum(Distance))
}


#Q7
Khi2(T)

chisq.test(T)


#Q9
tableau$`Jour de la semaine`=factor(tableau$`Jour de la semaine`,levels=jours)
boxplot(tableau$Nombre_de_skieurs~tableau$`Jour de la semaine`)

str(tableau$Mois)


#Q10
rapcorrel=function(X,Y)
{
  modalites=unique(X)
  k=length(modalites)
  n=length(Y)
  m=mean(Y)
  VTot=1/n*sum(Y^2)-m^2
  Vintra=0
  Vinter=0
  for (i in modalites)
  {
    mc=mean(Y[X==i])
    Vintra=Vintra+sum((Y[X==i]-mc)^2)
    Vinter=Vinter+sum(X==i)*(m-mc)^2
  }
  Vintra=Vintra/n
  Vinter=Vinter/n
  return(c(Vinter)/VTot)
}

R=rapcorrel(tableau$`Jour de la semaine`,tableau$Nombre_de_skieurs)

