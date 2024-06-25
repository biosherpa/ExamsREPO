rm(list=ls())
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

n=sample(600:1800,size = 1)
histclin<-(10000:(9999+n))

{#Construcción de variables####
  # Generando variables fisiológicas
  # Paso 1: Generar Datos Ficticios
  set.seed(123)
  edad <- round(runif(n, 18, 90))  # Edad entre 18 y 90 años
  peso <- round(rnorm(n, 70, 15), 1)  # Peso en kg con media 70 y desviación estándar 15
  altura <- round(rnorm(n, 1.7, 0.1), 2)  # Altura en metros con media 1.7 y desviación estándar 0.1
  bmi <- peso/altura^2


  pas <- round(rnorm(n, 120, 15))  # Presión sistólica en mm Hg con media 120 y desviación estándar 15
  pad <- round(rnorm(n, 80, 10))  # Presión diastólica en mm Hg con media 80 y desviación estándar 10
  col <- round(rnorm(n, 200, 30))  # Colesterol en mg/dL con media 200 y desviación estándar 30
  gluc_t0 <- round(rnorm(n, 115, 7.5),2)  # Glucosa en mg/dL con media 100 y desviación estándar 20
  ad_oral <- factor(ifelse(gluc_t0 >=126, rbinom(length(gluc_t0[gluc_t0 >=126]),1,.75),0),labels = c('No tratamiento AO','En tratamiento AO'))
  sexo <- sample(c("Hombre", "Mujer"),prob =c(.25,.75),size = n, replace = TRUE)  # Sexo
}
  #hba1c_pre gluc_t1  
  {

    beta_gluc <- rnorm(n, 0.5, 0.002)
    eps = rnorm(n, 0, 0.25)
    hba1c_pre <-   0.5 + beta_gluc * gluc_t0 / 10 + eps
    # hba1c_pre <- ifelse(hba1c_pre<5,sample(hba1c_pre,size = length(hba1c_pre<4),replace = T),hba1c_pre)
    # table(hba1c_pre>6.5)
    # plot(gluc_t0,hba1c_pre)
    
    diab <- gluc_t0>126 | hba1c_pre >7
    
    
    #Comprobación   
    # cor(gluc_t0, hba1c_pre)
    # data.frame(gluc_t0, hba1c_pre,ad_oral) %>%
    #   mutate(diab_hba = hba1c_pre>6.5) %>% 
    #   ggplot2::ggplot(aes(gluc_t0, hba1c_pre, color = ad_oral,shape = diab_hba)) +
    #   geom_point()
    
    # gluc_t1
    beta_ao <- rnorm(n,-12,0.001)
    gluc_t1 <- gluc_t0 +beta_ao*(as.numeric(ad_oral)-1)+eps
    
    #hba1c_pos
    beta_gluc <- rnorm(n, 0.30, 0.01)
    beta_ao <- rnorm(n, -0.15,0.002)
    eps = rnorm(n, 0, 1.25)
    
    hba1c_pos <- 4 + beta_gluc * gluc_t1 / 10 + beta_ao * as.numeric(ad_oral)-1
    #Corrijo valores no fisiológicos.
    # hba1c_pos <- ifelse(hba1c_pos<5,sample(hba1c_pos[hba1c_pos>5 & hba1c_pos<10],size = length(hba1c_pos[hba1c_pos>5 & hba1c_pos<7]),replace = T),hba1c_pos)
    # data.frame(gluc_t1, hba1c_pos) %>%
    #   mutate(diab_hba = hba1c_pos>6.5) %>%
    #   ggplot2::ggplot(aes(gluc_t1, hba1c_pos, color = ad_oral,shape = diab_hba)) +
    #   geom_point()
    # cor(gluc_t1, hba1c_pos)

    
  }
  
  
  datos <- data.frame(histclin,
    edad,peso,altura,bmi,
    pas,pad,col,gluc_t0,gluc_t1,hba1c_pre,hba1c_pos,
    ad_oral,diab ) 
  
  {#Modelo logístico para creación de complicaciones.
  
  set.seed(1234)
  datos$logb0<-rnorm(length(datos$histclin),0,.1)
  datos$logbad_oral<-rnorm(length(datos$histclin),-0.15,.025)
  datos$logbdiab<-rnorm(length(datos$histclin),0.05,.06)
  sigma <- 0.2
  eps <- rnorm(length(datos$histclin),0,sigma)
  
  datos$logit<-(datos$logb0 +
                  datos$logbad_oral*as.numeric(datos$ad_oral)-1+
                  datos$logbdiab*as.numeric(datos$diab)-1+
                  eps)
  
  
  datos$event_cv<-datos$logit/(1+datos$logit)
  #Corte del (1-cuantil)% superior de riesgo para establecer AE.
  datos$event_cv<-ifelse(datos$event_cv>quantile(datos$event_cv,c(.75)),1,0)
  
  prop.table(table(datos$ad_oral,datos$event_cv),1)
  fit.eve <- glm(data=datos,
                 event_cv~ad_oral,
                 family = 'binomial'
  )
  library(modelsummary)
  library(kableExtra)
  library(gt)
  modelsummary::modelsummary (fit.eve,exponentiate = T)
  }
    
  write.csv(datos,'datos.csv')
    
   
 
###########

datos[,grepl('^log*',x = colnames(datos))] <-NULL

  datos <- datos[, c(
    "histclin",
    "edad",
    "peso",
    "altura",
    "bmi",
    "pas",
    "pad",
    "col",
    "gluc_t0",
    "gluc_t1",
    "hba1c_pre",
    "hba1c_pos",
    "ad_oral",
    # "diab",
    "event_cv"
  )]
rm(list=setdiff(ls(), "datos"))
print(getwd())
write.csv(datos,'~/diab.csv')
