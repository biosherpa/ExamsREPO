#Examen 19092019 sobre controles de drogas en tráfico
#No hago source porque van a tener todos el mismo.


rm(list=ls())
#set.seed(14949477) #Esta linea se comenta en el raw.
# if(!require("car")){install.packages('car')} else {require('car')}
# # if(!require("psych")){install.packages('psych')} else{require('psych')}
# if(!require("epiDisplay")){install.packages('epiDisplay')} else {require('epiDisplay')}


multinom<-function(n,prob){
  multi<-rmultinom(n,1,prob)
  multit<-t(multi)
  mat0<-matrix(rep(0,n),ncol=1)
  maxi<-length(prob)
  for (i in 1:maxi){
    mat0<-mat0+
      assign(paste("m",i,sep=""),matrix(c(multit[,i]*i),ncol=1))
    i+1	
    
  }
  return(mat0)
  #vec<<-mat0 # la doble fecha lo mete en Global.env, pero dicen que esto no es muy aconsejable
  #rm(list=ls(-1))
}

n=sample(200:400,1) 

id<-(1:n)
model<-rbinom(1,1,.5)

############

age<-rnorm(n,18,0.5)
sex<-rbinom(n,1,.35)

#Para los consumos utilizo tabla 5 del informa de la comunidad de Madrid 
#http://www.comunidad.madrid/sites/default/files/doc/sanidad/drog/encuesta_domiciliaria_sobre_alcohol_y_drogas_comunidad_de_madrid_2017.pdf
#Prevalencia de consumo últimos 12 meses en grupo 15-24 años, pero no estatifican por sexo.
#Al final lo enfoco como las determinaciones en sujetos en los que se detectó consumo en controles de tráfico.
# 2000 controles y consumos los que corresponda

smoke<-ifelse(sex==0,rbinom(length(sex==0),1,.35)
              ,rbinom(length(sex==1),1,.40)
)
alcohol<-ifelse(sex==0,rbinom(length(sex==0),1,.6)
              ,rbinom(length(sex==1),1,.75)
)
cannabis<-ifelse(smoke==0,rbinom(length(smoke==0),1,.07)
                ,rbinom(length(smoke==1),1,.14)
)

cannacons<-ifelse(cannabis==0,0,multinom(n,c(0.1,0.2,0.5,0.1,0.1)))


cocaine<-ifelse(sex==0,rbinom(length(sex==0),1,.02)
               ,rbinom(length(sex==1),1,.02)
)

metanfeta<-ifelse(sex==0,rbinom(length(sex==0),1,.05)
                ,rbinom(length(sex==1),1,.07)
)




##Creación de las variable thc en orina y después mediante modelo lineal.
# https://www.matillaplant.com/blog-marihuana/thc-que-es/#Cuanto_dura_el_THC_en_la_orina
  
set.seed(14949477)
eps<-rnorm(n,0,1.2)
sigma0<-2
mean0<-90

intercerpt<-rnorm(n,mean0,sigma0)
eps<-rnorm(n,0,sigma0*1.2)
bcannabis<-rnorm(n,1,2)
bcannacons<-rnorm(n,6,1.5)
bcocaine<-rnorm(n,4,0.5)
bsmoke<-rnorm(n,4.1,2)
bage<-rnorm(n,0.1,.003)


thc<-ifelse(cannabis==0,NA,intercerpt+bcannacons*cannacons+bsmoke*smoke+bage*age+bcocaine*cocaine+eps)


#Programa experimental reducción del consumo.

group<-ifelse(cannabis==1,rbinom(length(cannabis==1),1,.5),NA)


#bajo frecuencia de consumo en grupo intervención.
#niveles deberían bajar por bajar frecuencia de consumo. Baja frecuencia de consumo in intervenido
#bajarán niveles medios del grupo, pero no bajarán distancias entre frecuencias de consumo.

  cannacons12<-0
  cond<-group==0&!is.na(group)
  cannacons12[cond]<-multinom(length(group[cond]),c(0.1,0.3,0.4,0.1,0.1))
  cond<-group==1&!is.na(group)
  cannacons12[cond]<-multinom(length(group[cond]),c(0.7,0.2,0.1))
 

thc12<-ifelse(cannabis==0,NA,intercerpt+bcannacons*cannacons12+eps)


drugs<-data.frame(id,age,sex,cannabis,cannacons,cannacons12,thc,thc12,group,alcohol,smoke,cocaine,metanfeta)
# fit<-lm(thc~age+sex+alcohol+smoke+cocaine+as.factor(cannacons),data=drugs)
# summary(fit)
# boxplot(drugs$thc~drugs$cannacons)



##CONVERSIÓN EN FACTORES DE ALGUNAS VARIABLES.

drugs$sex<-factor(drugs$sex,labels=c('female','male'))
drugs$smoke<-factor(drugs$smoke,labels=c('non-smoker','smoker'))
drugs$group<-factor(drugs$group,labels=c('Control','Intervention'))
drugs$cannabis<-factor(drugs$cannabis,labels=c('No','Yes'))
drugs$alcohol<-factor(drugs$alcohol,labels=c('No','Yes'))
drugs$cocaine<-factor(drugs$cocaine,labels=c('No','Yes'))
drugs$metanfeta<-factor(drugs$metanfeta,labels=c('No','Yes'))





######################

# 
drugs<-drugs[,c('id', 'age', 'sex','smoke', 'group','cannabis','cannacons','cannacons12','thc','thc12','cocaine','alcohol','metanfeta')]
rm(list=setdiff(ls(), "drugs"))
#rm(list=ls(pattern=glob2rx(pattern = "brca$log*")))

# subset(drugs,!is.na(group))

# drugs$inc<-ifelse(is.na(drugs$group),0,1)

