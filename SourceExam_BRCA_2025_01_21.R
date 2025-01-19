{rm(list=ls())
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
# set.seed(1234)

n=sample(1600:1800,size = 1)
histclin<-(10000:(9999+n))

{#Construcción de variables####
  # Generando variables fisiológicas
  # Paso 1: Generar Datos Ficticios
  
  
  age<-rnorm(n,56,2.1)
  hb<-10+rchisq(n,2)
  smoke<-rbinom(n,1,.35)
  er<-rbinom(n,1,.68)
  
  Tn<-multinom(n,c(.23,.3,.27,.21))
  
  # Categorías de T:
  #   T0: No hay evidencia de tumor primario.
  # Tis: Carcinoma "in situ" (tumor no invasivo, confinado al lugar donde comenzó, sin diseminarse a tejidos cercanos).
  # T1-T4: Indica el tamaño y/o extensión del tumor. Los números más altos generalmente significan:
  #   Tumores más grandes.
  # Mayor invasión a tejidos cercanos.
  
  N<-ifelse(Tn==1,multinom(length(Tn==1),c(.6,.2,.15,.05)),
            ifelse(Tn==2,multinom(length(Tn==2),c(.5,.3,.15,.05)),
                   ifelse(Tn==3,multinom(length(Tn==3),c(.4,.25,.2,.15)),
                          multinom(length(Tn==4),c(.3,.3,.25,.15)))))
  M<-ifelse(Tn==1,multinom(length(Tn==1),c(0.8,.2)),
            ifelse(Tn==2,multinom(length(Tn==2),c(.7,.3)),
                   ifelse(Tn==3,multinom(length(Tn==3),c(.6,.4)),
                          multinom(length(Tn==4),c(.3,.7)))))
  
  # Estadiaje apoyado en TNM
  #iMPORTANTE MANTENER ORDEN, VAN ENCADENADAS.
  
  stageIV<-ifelse(M==2,4,0)
  stage0<-ifelse(Tn==1 & N==1 & M==1 & stageIV==0,1,0)
  stageI<-ifelse(Tn==2 & N==1 & stageIV==0,1,0)
  stageII<-ifelse(Tn<4 & N<=2 & stage0==0 & stageI==0 & stageIV==0,2,0)
  stageIII<-ifelse(stageIV!=4 & (Tn<=4 & N>2 | (Tn==4 & N==2) | Tn==5),3,0)

  stage<-factor(stageI+stageII+stageIII+stageIV,
                levels = 0:4,
                labels = c("0","I","II","III","IV")
  )
  
 
  
  
  
  brca <- data.frame(histclin,
                     age,
                     hb,
                     smoke,
                     stage ,
                     Tn = factor(Tn),
                     #así coge las que tenga, porque la última es poco probable.
                     N,
                     M,
                     er = as.character(as.numeric(factor(er))-1)
                     )
  
  # checkpoint
  # gtsummary::tbl_summary(brca %>% 
  #                          select(-histclin))
  
}
{ ## Modelo lineal para crear concentración de CA15.3 BASAL.
  
  #ca15_bas<-linmodel b1-t,b2-n,b3-tnm,b4-er,)
  #creaci?n variable ca15
  intercept<-rnorm(n,12,3)
  # brca$linb1<-ifelse(as.numeric(Tn)==4,rnorm(10,.004),rnorm(1,0.003))
  # brca$linb2<-ifelse(N>=3,rnorm(n,0.2,0.0025),rnorm(n,0.05,0.0025))
  # brca$linb3<-ifelse(as.numeric(stage)>=2,rnorm(n,0.4,0.0025),rnorm(n,0.1,0.0025))
  # brca$linb4<-ifelse(as.numeric(er)==1,rnorm(n,1.5,0.0025),rnorm(n,.25,0.0025))
  
  brca$linb1<-rnorm(n,1.5,.25)
  brca$linb2<-rnorm(n,1.5,.25)
  brca$linb3<-rnorm(n,1.5,.25)
  brca$linb4<-rnorm(n,1.5,.25)
  
  
  sigma <- 1
  eps <- rnorm(n,0,sigma)
  brca$ca15_bas<-intercept+
    brca$linb1*as.numeric(brca$Tn)+
    brca$linb2*brca$N+
    brca$linb3*as.numeric(brca$stage)+
    brca$linb4*as.numeric(brca$er)+
    eps
  
  #checkpoint
  # summary(brca$ca15_bas)
  # boxplot(ca15_bas~stage,data=brca)
  # by(brca$ca15_bas,brca$stage,summary)
  # par(mfrow=c(2,3))
  # by(brca$ca15_bas,brca$stage,hist)
  # par(mfrow=c(1,2))
  

}
 { 
  ####################################################
  #modelo logístico para muerte en función de diferentes variables
  
   # Definir los ORs y sus desviaciones estándar (log(OR) y log-std)
   ORs <- c(1.28, 2.3, 2.5, 2.11, 0.58, 0.15)  # Ejemplo de ORs
   # Generar log(ORs)
   log_ORs <- log(ORs)
   SE_ORs <- rep(0.1,length(log_ORs))    # Desviaciones estándar de los ORs
   
   # Generar errores aleatorios para los coeficientes (log(OR))

   logb <- sapply(1:length(log_ORs), function(i) rnorm(n, mean = log_ORs[i], sd = SE_ORs[i]))
   
   # Crear dummies para 'stage'
   stage_dummies <- model.matrix(~ stage - 1, data = brca)
   
   # Calcular logits
   brca$logit <- (
     logb[,1 ] +                     # Intercepto
       logb[,2 ] * scale(brca$ca15_bas) +     # ca15_bas
       rowSums(stage_dummies * logb[,3 ]) +  # stage
       logb[,4 ] * as.numeric(brca$Tn) +
       logb[,5 ] * brca$hb +
       logb[,6 ] * as.numeric(brca$er)
   )
   
   # exp(mean(c(logb[,3],
   #            logb[,3]-1.96 *logb[,3],
   #            logb[,3]+1.96*logb[,3]
   # ))
   # )
   
   
   # Añadir error aleatorio al logit
   sigma <- 0.05  # Desviación estándar del error
   eps <- rnorm(n, mean = 0, sd = sigma)
   
   # El logit final es la suma de los logits individuales más el error aleatorio
   brca$logit_final <- brca$logit + eps
   
   # Transformamos el logit en una probabilidad usando la función sigmoide
   brca$riskdeath <- exp(brca$logit_final) / (1 + exp(brca$logit_final))
   
   
   # Convertimos la probabilidad en una variable binaria (death = 1 si riesgo > 0.75, de lo contrario 0)
   brca$death <- brca$death <- rbinom(n, size = 1, prob = brca$riskdeath)
   
   # table(brca$death)
   # Ajustar un modelo de regresión logística con todas las variables
   # model1 <- glm(death ~ ca15_bas + stage + Tn + hb + er, family = binomial(link = "logit"), data = brca)
   # 
   # # Mostrar el resumen del modelo
   # summary(model1)
   # 
   # 
   # tbl_uni <- gtsummary::tbl_uvregression(
   #   data = brca %>% select(
   #     death, 
   #     ca15_bas, 
   #      stage,
   #     Tn,hb,er
   #   ) ,
   #   # mutate(across(.cols=c(stage,Tn),.fns= ~factor(.))),
   #   method = glm,
   #   y = death,  # Aquí 'factor(stage)' es clave
   #    exponentiate = TRUE,  # Exponentiar los coeficientes (para obtener ORs)
   #   conf.int = TRUE       # Incluir intervalos de confianza
   # )
   # 
   # tbl_uni
   # 
   # 
   # # Ajustar modelo logístico
   # model <- glm(death ~ ca15_bas + stage + Tn + hb + er, family = binomial(link = "logit"), data = brca)
   # 
   # # Resumen del modelo y OR estimados
   # library(gtsummary)
   # tbl_multi <- gtsummary::tbl_regression(
   #   model, exponentiate = TRUE, conf.int = TRUE
   # )
   # tbl_multi
   # 
   # gtsummary::tbl_merge(list(tbl_uni,tbl_multi))
      
 }
    
{
      #creaci?n variable ca15_6m
    intercept<-brca$ca15_bas
    brca$linb1_6m<--rnorm(n,.15,0.23)*brca$ca15_bas
    brca$linb2_6m<--rnorm(n,.55,0.23)*brca$ca15_bas
    #brca$linb2_6m<-ifelse(N>=3,-rnorm(n,2,0.0025),-rnorm(n,0.5,0.0025))
    #brca$linb3_6m<-ifelse(stage>=3,-rnorm(n,3,0.0025),-rnorm(n,0.1,0.0025))
     #brca$linb4_6m<-ifelse(er==1,-rnorm(n,3,0.025),-rnorm(n,.25,0.0025))
  
  sigma <- 1
  eps <- rnorm(n,0,sigma)
  brca$ca15_6m <- intercept+
    brca$linb1_6m +
    brca$linb2_6m * brca$death +  eps
  
  #chekpoint
    # hist(brca$ca15_bas,col="red",
    #      # breaks=10,
    #      xlim=c(0,1.1*max(c(brca$ca15_bas,brca$ca15_6m))))
    # hist(brca$ca15_6m,
    #      # breaks=10,
    #      col="blue",add=TRUE)
    # t.test(brca$ca15_bas,brca$ca15_6m,paired=TRUE)
    # summary(brca[,c("ca15_bas", "ca15_6m")])
    # brca$ca15_dif <- brca$ca15_bas-brca$ca15_6m
    # boxplot(brca$ca15_dif~brca$death)
    # boxplot(brca$ca15_dif)
    # 
    # t.test(brca[brca$death==1,'ca15_dif'])
    # t.test(brca[brca$death==0,'ca15_dif'])
  
  
  #checkpoint
  
  # tbl_uni <- gtsummary::tbl_uvregression(
  #   data = brca %>% select(
  #     death, 
  #     ca15_dif, 
  #     stage,
  #     Tn,hb,er
  #   ) ,
  #   # mutate(across(.cols=c(stage,Tn),.fns= ~factor(.))),
  #   method = glm,
  #   y = death,  # Aquí 'factor(stage)' es clave
  #   exponentiate = TRUE,  # Exponentiar los coeficientes (para obtener ORs)
  #   conf.int = TRUE       # Incluir intervalos de confianza
  # )
  # 
  # tbl_uni
  # 
  # 
  # # Ajustar modelo logístico
  # model2 <- glm(death ~ ca15_dif + stage + Tn + hb + er, family = binomial(link = "logit"), data = brca)
  # 
  # # Resumen del modelo y OR estimados
  # library(gtsummary)
  # tbl_multi <- gtsummary::tbl_regression(
  #   model2, exponentiate = TRUE, conf.int = TRUE
  # )
  # tbl_multi
  # 
  # gtsummary::tbl_merge(list(tbl_uni,tbl_multi))

    }


#Factores
  {
  brca$Tn<-factor(brca$Tn,labels=c("Tis", "T1","T2","T3"))     
  brca$N<-factor(brca$N,labels=c("N0","N1","N2","N3")) 
  brca$M<-factor(brca$M,labels=c("M0","M1"))
  # brca$stage<-factor(brca$stage,labels=c("stage 0", "stage I", "stage II", "stage III", "stage IV"))
  brca$smoke<-factor(brca$smoke,labels=c('No','Yes'))
  brca$death<-factor(brca$death,labels=c('No','Yes'))
  brca$er<-factor(brca$er,labels=c('ER-','ER+'))
  }
 
 
###########

# brca[,grepl('^log*',x = colnames(brca))] <-NULL
#   brca[,grepl('^lin*',x = colnames(brca))] <-NULL
  
{
datos <- brca[, c("histclin", "age", "hb", "smoke", "stage", "Tn", "N", "M", "er", "ca15_bas", "riskdeath", "death", "ca15_6m")]

rm(list=setdiff(ls(), "datos"))
print(getwd())
# write.csv(brca,paste0(getwd(),'/brca.csv')) #No me arriesgo.
write.csv(datos,'~/datos.csv',row.names = F) # Lo coloco en HOME
datos <- read.csv('~/datos.csv')

}
}
