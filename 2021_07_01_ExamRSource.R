multinom<-function(n,prob){
  multi<-rmultinom(n,1,prob)
  multit<-t(multi)
  mat0<-matrix(rep(0,n),,1)
  maxi<-length(prob)
  for (i in 1:maxi){
    mat0<-mat0+
      assign(paste("m",i,sep=""),matrix(c(multit[,i]*i),,1))
    i+1	
    
  }
  return(mat0)
  #vec<<-mat0 # la doble fecha lo mete en Global.env, pero dicen que esto no es muy aconsejable
  #rm(list=ls(-1))
}

#variables base####
set.seed(1234)

n=sample(600:800,1) 

histclin<-(1:n)
age<-rnorm(n,49,2.1)
sex<-factor(rbinom(n,1,.65),labels=c('Female','Male'))
smoke<-factor(rbinom(n,1,.65),labels=c('No','Yes'))
who.stage<-multinom(n,prob=c(.5,.25,.15,0.10))
treat<-rbinom(length(histclin),1,.5)

#dataframe

hiv<-data.frame(histclin,age,sex,smoke,who.stage,treat)

#nuevas variables.####
#cd4.0####
  hiv$cd4.0<-as.numeric(round(ifelse(who.stage==1,rnorm(length(who.stage==1),800,100),
        ifelse(who.stage==2,rnorm(length(who.stage==2),600,100),
        ifelse(who.stage==3,rnorm(length(who.stage==3),450,140),
        rnorm(length(who.stage==4),250,80)
        ))),0)
  )
#viral load.0####
  #modelo con dos variables, stage y cd4.0
  
  hiv$who.stage.vl.linb<-ifelse(who.stage==1,rnorm(length(who.stage==1),+100,20),
                                  ifelse(who.stage==2,rnorm(length(who.stage==2),+200,20),
                                  ifelse(who.stage==3,rnorm(length(who.stage==3),+350,24),
                                  ifelse(who.stage==4,rnorm(length(who.stage==4),+400,22)
                                         ,NA)
                                  )
                                  )
    )
  hiv$cd4.vl.beta<-rnorm(length(histclin),-0.3,0.1)

    intercept<-rnorm(length(histclin),2500,.03)
    sigma <- 1
    eps <- rnorm(n,0,sigma)
    
    hiv$viral_load.0<-intercept +
      hiv$who.stage.vl.linb+
      hiv$cd4.vl.beta*hiv$cd4.0+ eps
  #Por si acaso, sustituyo 0 por límite de deteccion del aparato usando 10.
    hiv$viral_load.0<-as.numeric(ifelse(hiv$viral_load.0<=0,rnorm(1,10,0.25),hiv$viral_load.0)
    )#límite de deteccion del aparato
    

#variable muerte desde modelo logístico con tres variables#####
#modelo sencillo b1-who.stage+b2-cd4.count+ b3-treat

hiv$logb0<-rnorm(length(hiv$histclin),.005,.0001)
hiv$logb1<-rnorm(length(hiv$histclin),.25,.02)
hiv$logb2<-rnorm(length(hiv$histclin),-0.1,.05)
hiv$logb3<-rnorm(length(hiv$histclin),-25,0.4)



  sigma <- 1
  eps <- rnorm(n,0,sigma)
  hiv$logit<-( hiv$logb0 +
                    hiv$logb1*hiv$who.stage+#ser?a mejor construir dummy
                    hiv$logb2*hiv$cd4.0 +
                    hiv$logb3*hiv$treat +
                    eps
                    )
  
  hiv$riskdeath<-1/(1+exp(-hiv$logit))
  hiv$death<-ifelse(hiv$riskdeath>quantile(hiv$riskdeath)[4],1,0)

#Variable viral_load.6 con dos predictoras treat y death##########
intercept<-hiv$viral_load.0+rnorm(length(hiv$histclin),-100,300)
  #Da igual lo que ponga en segundo miembro porque multiplica por 0 en dicotómica.
hiv$treat.vl6.linb1<-ifelse(hiv$treat==1,rnorm(length(hiv[hiv$treat==1,'histclin']),-1200,10),
                            rnorm(length(hiv[hiv$treat==0,'histclin']),-145,30))
hiv$death.vl6.linb2<-ifelse(hiv$death==1,rnorm(length(hiv[hiv$death==1,'histclin']),+500,400),
                            rnorm(length(hiv[hiv$death==0,'histclin']),-45,30))
##asume que grupo en tratamiento estándar tambien disminuye carga.
  
  sigma <- 1
  eps <- rnorm(n,0,sigma)
  hiv$viral_load.6<-intercept+hiv$treat.vl6.linb1*hiv$treat+
    hiv$death.vl6.linb2*hiv$death+ eps
  hiv$viral_load.6<-as.numeric(ifelse(hiv$viral_load.6<=0,rnorm(1,10,0.25),hiv$viral_load.6)
  )
  
  # checkpoint
  # 
  # hist(hiv$viral_load.6,col=rgb(1,0,0,.25),xlim=c(200,3500))
  # hist(hiv$viral_load.0,col=rgb(0,1,0,.25),add=T)
  
  
 #lipodistrofia####

hiv$lipodys12<-ifelse(hiv$treat==1,rbinom(length(hiv$treat==1),1,.1),
                      ifelse(hiv$treat==0,rbinom(length(hiv$treat==0),1,.4),NA)
)

#Variable cd4.6 lm con tres var##########
intercept<-hiv$cd4.0+rnorm(length(hiv$histclin),500,100)
#utilzo lm con tres variabels treat, death y viralload6
hiv$treat.cd46.linb1<-rnorm(length(hiv$histclin),250,50)
hiv$death.cd46.linb2<-rnorm(length(hiv$histclin),-100,10)
hiv$vl.cd46.linb3<-rnorm(length(hiv$histclin),-0.01,0.2)


  
  sigma <- 1
  eps <- rnorm(n,0,sigma)
  hiv$cd4.6<-intercept+
    hiv$treat.cd46.linb1*hiv$treat+
    hiv$death.cd46.linb2*hiv$death+
    hiv$vl.cd46.linb3*hiv$viral_load.6+
    eps

namevar<-c("histclin", "age", "sex", "smoke", "who.stage", "treat", "cd4.0",  "cd4.6",
         "viral_load.0", "viral_load.6","lipodys12", "death" 
        )

hiv$who.stage<-factor(hiv$who.stage,labels=c('I','II','III','IV'))

hiv<-hiv[,namevar]
# rm(list=setdiff(ls(),'hiv'))
save.image()



