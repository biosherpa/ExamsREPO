
#multinom####
multinom<-function(n,prob){
  multi<-rmultinom(n,1,prob)
  multit<-t(multi)
  mat0<-matrix(rep(0,n),n,1)
  maxi<-length(prob)
  for (i in 1:maxi){
    mat0<-mat0+
      assign(paste("m",i,sep=""),matrix(c(multit[,i]*i),n,1))
    i+1	
    
  }
  return(mat0)
}

{#Construcción de variables####
## sociodemográficas ####
# set.seed(1234)
n=sample(1750:1800,size = 1)
histclin<-(10000:(9999+n))
sexo<-rbinom(n,1,.52)
edad<-trunc(ifelse (sexo==1,rnorm(length(sexo==1),71.4,7.9), rnorm (length(sexo==0),71,7.8)))
# time_dx <- rt(length(histclin),ncp=1.4,df = 20)
# time_dx <- rnorm(length(histclin),1.4,1.5)
time_dx <- rchisq(length(histclin),30)/25
## Clinical subgroup. ####
grado<-rbinom(length(histclin),1,.385)
#Para construir factor.
# 1=Mild dementia due to Alzheimer’s disease 
# 0=Mild cognitive impairment due to Alzheimer’s disease


apoE_carrier <- factor(multinom(length(histclin),c(.31,.53,.16)),
                       labels = c('Non_carrier','Heterozygote','Homozygote'))

##Construcción del grupo de tratamiento.####
grupo <- rbinom(length(histclin),1,.5) #Grupo de tratamiento.

}

{#Creación de puntuaciones baseline en diferentes escalas. ####

# 
# * Plus–minus values are means ±SD. ApoE denotes apolipoprotein E.
# † Race and ethnic group were determined by the participants.
# ‡ Global Clinical Dementia Rating (CDR) scores range from 0 to 3, with higher scores indicating greater impairment. A
# score of 0.5 is considered to be the threshold for Alzheimer’s disease and was required for trial enrollment.
# § Scores on the CDR–Sum of Boxes (CDR-SB) range from 0 to 18, with higher scores indicating greater impairment.
# ¶ Values for amyloid burden on positron-emission tomography (PET) were for the PET substudy population.
# ‖ Scores on the 14-item cognitive subscale of the Alzheimer’s Disease Assessment Scale (ADAS-cog14) range from 0 to
# 90, with higher scores indicating greater impairment.
# ** Values for the Alzheimer’s Disease Composite Score (ADCOMS) range from 0 to 1.97, with higher scores indicating
# greater impairment.
# †† Scores on the Alzheimer’s Disease Cooperative Study–Activities of Daily Living Scale for Mild Cognitive Impairment
# (ADCS-MCI-ADL) range from 0 to 53, with lower scores indicating greater impairment.
# ‡‡ Scores on the Mini–Mental State Examination (MMSE) range from 0 to 30, with lower scores indicating greater impairment.


  cdr_sb_t0 <- round(rnorm(length(histclin),3.17,1.34),1) #cdr cada celda 0-3, sum of boxex (sb) rango de 0 a 18.
  cdr_sb_t0 <-ifelse(cdr_sb_t0<0,0,cdr_sb_t0)
  
  mmse_t0 <- round(rnorm(length(histclin),25.2,2.2),1)#0-30
  mmse_t0 <-ifelse(mmse_t0>30,median(mmse_t0)+rnorm(1,0.5,.25),mmse_t0)
  
  adas_cog14_t0 <- round(rnorm(length(histclin),24.5,7),1) #0-90
  adas_cog14_t0 <-ifelse(adas_cog14_t0<0,0,adas_cog14_t0)
  
  adcs_mci_t0 <-  round(rnorm(length(histclin),41,6.9),1) # 0-53
  # adcs_mci_t0 <-  40+round(rchisq(length(histclin),41),1)/35 # 0-53
  
  adcs_mci_t0 <-ifelse(adcs_mci_t0>=53,53,adcs_mci_t0)
           
  } 
    

{# Construcción del df ####

datos=data.frame(histclin,
                 sexo=factor(sexo,labels=c("Mujer","Hombre")),
                 edad=round(edad,0),
                 grado=factor(grado,labels=c("Mild Cognitive Impairment due to AD",
                                             "Mild Dementia due to AD")),
                 grupo=factor(grupo,labels=c("Placebo",
                                             "Lecanemab")),
                 apoE_carrier,
                 cdr_sb_t0,
                 adas_cog14_t0,
                 adcs_mci_t0,
                 mmse_t0)
}

#CDR-SB#############

##creación variable puntuación en el cdr_sb_t18####
{

#Estimación asumiendo que el cambio es una función lineal que solo depende del grupo
  
beta_g<- -0.45
sigma <- 0.15
eps <- rnorm(length(datos$histclin),0,sigma)
datos$cdr_sb_dif_18 <- 0+beta_g*as.numeric(datos$grupo)-1+eps
datos[datos$cdr_sb_t0+datos$cdr_sb_dif_18<0,'cdr_sb_dif_18'] <-rnorm(length(datos[datos$cdr_sb_t0+datos$cdr_sb_dif_18<0,'cdr_sb_dif_18']),
                                                                     0,0.2)


datos$cdr_sb_t18 <- datos$cdr_sb_t0+datos$cdr_sb_dif_18

}

#MMSE######
##creación variable puntuación en el mmse_t18####
{
  #Estimación asumiendo que el cambio es una función lineal que solo depende del grupo
  
  beta_g <- -0.55
  sigma <- 0.42
  eps <- rnorm(length(datos$histclin),0,sigma)
  datos$mmse_dif_18 <- 0+beta_g*as.numeric(datos$grupo)-1+eps
  datos[datos$mmse_t0+datos$mmse_dif_18<0,'mmse_dif_18'] <-rnorm(length(datos[datos$mmse_t0+datos$mmse_dif_18<0,'mmse_dif_18']),
                                                                       0,0.2)
  datos$mmse_t18 <- datos$mmse_t0+datos$mmse_dif_18
  
  
}

# ADAS-COG14 #############
##creación variable puntuación en el adas_cog14_t18####
{
  # intercept<-ifelse(datos$grupo=="Placebo", mean(datos[datos$grupo=="Placebo",'adas_cog14_t0']),
  #                   mean(datos[datos$grupo=="Lecanemab",'adas_cog14_t0'])
  # )
  #Estimación asumiendo que el cambio es una función lineal que solo depende del grupo
  
  beta_g<- -1.44
  sigma <- 0.9
  eps <- rnorm(length(datos$histclin),0,sigma)
  # datos$adas_cog14_t18 <- rnorm(n,intercept+beta_g*as.numeric(datos$grupo)+eps,sigma)
  datos$adas_cog14_dif_18 <- 0+beta_g*as.numeric(datos$grupo)-1+eps
  # datos$adas_cog14_t18 <- intercept+beta_g*as.numeric(datos$grupo)-1+eps
  # datos$adas_cog14_dif_18 <- datos$adas_cog14_t18-datos$adas_cog14_t0
  datos[datos$adas_cog14_t0+datos$adas_cog14_dif_18<0,'adas_cog14_dif_18'] <-rnorm(length(datos[datos$adas_cog14_t0+datos$adas_cog14_dif_18<0,'adas_cog14_dif_18']),
                                                                       0,0.2)
  
  datos$adas_cog14_t18 <- datos$adas_cog14_t0+datos$adas_cog14_dif_18
  }
##############

# ADCS-MCI #############
##creación variable puntuación en el adcs-mci_t18####
{
  #Estimación asumiendo que el cambio es una función lineal que solo depende del grupo
  
  beta_g<- 2
  sigma <- 1
  eps <- rnorm(length(datos$histclin),0,sigma)
  datos$adcs_mci_dif_18 <- 0+beta_g*as.numeric(datos$grupo)-1+eps
  datos[datos$adcs_mci_dif_t0+datos$adcs_mci_dif_18<0,'adcs_mci_dif_18'] <-rnorm(length(datos[datos$adcs_mci_dif_t0+datos$adcs_mci_dif_18<0,'adcs_mci_dif_18']),
                                                                       0,0.2)
  datos[datos$adcs_mci_dif_t0+datos$adcs_mci_dif_18>53,'adcs_mci_dif_18'] <-rnorm(length(datos[datos$adcs_mci_dif_t0+datos$adcs_mci_dif_18<0,'adcs_mci_dif_18']),
                                                                       0,0.2)
  
  
  
  datos$adcs_mci_t18 <- datos$adcs_mci_t0+datos$adcs_mci_dif_18
  }
#ADVERSE EVENTS#############

{
## ae_serious #############
# Serious adverse event 126 (14.0) 101 (11.3)
# set.seed(1234)
datos$logb0<-rnorm(length(datos$histclin),0,.1)
datos$logbgrupo<-rnorm(length(datos$histclin),0.01,.025)
datos$logbgrado<-rnorm(length(datos$histclin),0.05,.06)
sigma <- 0.2
eps <- rnorm(length(datos$histclin),0,sigma)

datos$logit<-(datos$logb0 +
                  datos$logbgrupo*as.numeric(datos$grupo)-1+
                  datos$logbgrado*as.numeric(datos$grado)-1+
                  eps)


datos$ae_serious<-datos$logit/(1+datos$logit)
#Corte del (1-cuantil)% superior de riesgo para establecer AE.
datos$ae_serious<-ifelse(datos$ae_serious>quantile(datos$ae_serious,c(.87)),1,0)
}

###########
{

## ae_headache #############
# Headache 100 (11.1) 73 (8.1)# set.seed(1234)
datos$logb0<-rnorm(length(datos$histclin),-.75,.1)
datos$logbgrupo<-rnorm(length(datos$histclin),0.0005,.025)
datos$logbgrado<-rnorm(length(datos$histclin),0.05,.06)
sigma <- 0.2
eps <- rnorm(length(datos$histclin),0,sigma)

datos$logit<-(datos$logb0 +
                datos$logbgrupo*as.numeric(datos$grupo)-1+
                datos$logbgrado*as.numeric(datos$grado)-1+
                eps)


datos$ae_headache<-datos$logit/(1+datos$logit)
#Corte del (1-cuantil)% superior de riesgo para establecer AE.
datos$ae_headache<-ifelse(datos$ae_headache>quantile(datos$ae_headache,c(.9)),1,0)


}

###########

datos[,grepl('^log*',x = colnames(datos))] <-NULL

datos <- datos[,c("histclin",'sexo',
                  'edad','grado','grupo','apoE_carrier',
                  "cdr_sb_t0","cdr_sb_t18",
                  "adas_cog14_t0","adas_cog14_t18",
                  "adcs_mci_t0","adcs_mci_t18",
                  "mmse_t0","mmse_t18",
                  "ae_serious","ae_headache"
)]
rm(list=setdiff(ls(), "datos"))

