#16-01-2020
#Usa la función que carga los vectores de ponderación y uso.
#Esta función:
#Ejecuta los cálculos ponderados para todas las preguntas.
#Para ello genera 
#Genera xls con tres matrices:
####Notas de cada apartado y pregunta en base 10.
####Notas de cada apartado y pregunta tras la ponderación.
####Matriz de ponderación.
#Además genera xlsx en el que va incluyendo el nombre de cada alumno con su nota final. 
# (cuidado no controla duplicados, solo añade lineas)

#Ene2021_añado funcion y matrices aquí para pasar a Adri.
##################################################### ###
pond.cor<-function(vecpond,vecus,veccal,namecrit){
  # vecpond<-c(15,15,25,35) #pesos tal cual.
  vec.weight<-vecpond/sum(vecpond);vec.weight
  # veccal<-c(5,5,5,5)
  # vecus<-c(1,1,1,1)
  vecpond.cor<-vec.weight*vecus/sum(vec.weight*vecus,na.rm = T) #reparto de restos de forma proporcional a su peso en consjunto de pesos activos.
  punto.pond<-vecpond.cor*veccal
  tabres<-rbind(veccal,vecpond,vecus,round(vecpond.cor,5),round(punto.pond,5))
  rownames(tabres)<-c('Puntos','Peso original','Uso s/n','Peso corregido','punto*peso corregido')
  # colnames(tabres)<-paste0('criterio/preg',1:length(vecpond))
  colnames(tabres)<-namecrit#sustituyo para que aparezcan nombres de criterio
  tabres
  # p.mat<<-tabres
  # print(tabres)
  # print(paste('Puntuación total: ',row.names(vecus),sum(punto.pond)))
}

# Adaptación a ene2021.
#Para simplificar algo el código, meto las matrices de uso y ponderación
#directamente en la función que crea la nota.
#Dejo npreg porque lo usa en otras partes del código de Notafin.R

##################################################### ###
#La función lee los vectores cal que hay que introducir en la corrección
#Yo los suelo agrupar en el encabezado. Ejemplo. Cada apartado 
#se puntúa de 1 a 10.
# cal1<-c(0,0,8,0)
# cal2<-c(0,0,3,2)
# cal3<-c(0,0,7,10)
# cal4<-rep(0,4)
# cal5<-c(0,0,10,8)


###notafin.R necesita la función anterior, pond.cor.
##en la función notafin.R, la función pond.cor toma los argumentos
##de los que se entregan a la función notafin.R
## Yo lo tenía para que leyese source pond.cor de un directorio local
## así evitaba meterlo en el código de notafin.R

##También tenía fuera la generación de las matrices de las matrices de ponderación (peso de cada aspecto)
#y de uso (cuál/cuales de los ítem puntúa) 
#Para no complicarte más la vida, este año he metido (perdiendo plasticidad)
# todas dentro de la función notafin.R También he fijado el npreg.
#por lo que con tener los vectores de calificación y correr notafin.R()
#debería funcionar.

notafin.R<-function(matvecpond,matvecus,matcal,npreg,namecrit){
  library(xlsx)
  npreg=5
  pesopreg <- c(20,20,10,25,25)
  
  matvecpond<-matrix(rep(pesopreg,npreg),nrow=npreg,byrow = T)
  #Matriz 1jul22 con 5+1. P1, ejecución,
  # p2 too
  # p3 ejecución e interpretación
  # p4 todo.
  # p5 todo
  # p6 extra para compilación suma 0.25 en ejecución.
  # namecrit<-c('Hipótesis 20%','Justificación 20%','Comprobacion 10%','Código 25%','Interpretación 25%')
  # namecrit<-paste(c('Hipótesis','Justificación','Comprobacion','Código','Interpretación'),
  #                 paste0(pesopreg,"%"),sep = " ")
  namecrit<-c('Hipótesis','Justificación','Comprobacion','Código','Interpretación') #Elimino los % de los títulos porque cuando cambian confunden.
  #En enero 2024 introduje tres modelos, uno por cada grupo de prácticas y por tanto el vector de uso cambia de nos a
  # otros. Así que he de cargar un vector de uso diferente en función del grupo de prácticas.
  
   # tipo <- 3
  matvecus<-if(tipo==3){ matrix(c(NA,NA,NA,1,NA,
                     1,1,1,1,1,
                     NA,NA,NA,1,1,
                     1,1,1,1,1,
                     1,1,1,1,1#,
                     # NA,NA,NA,NA,1#, compilo rmd preg extra.
                     # 0,0,0,1
),nrow=npreg,byrow = T)


  } else if (tipo==2) { matrix(c(NA,NA,NA,1,NA,
                               1,1,1,1,1,
                               1,1,1,1,1,#Este es diferente respecto al grupo 3
                               1,1,1,1,1,
                               1,1,1,1,1#,
                               # NA,NA,NA,NA,1#, compilo rmd preg extra.
                               # 0,0,0,1
  ),nrow=npreg,byrow = T)
  } else if (tipo==1) {
    matrix(c(NA,NA,NA,1,1,#Aquí valoro interpretación porque la hay
             NA,NA,NA,1,1,
             1,1,1,1,1,#Este es diferente respecto al grupo 3. El G2 y el G1 tienen la misma matriz de vecus.
             1,1,1,1,1,
             1,1,1,1,1#,
             # NA,NA,NA,NA,1#, compilo rmd preg extra.
             # 0,0,0,1
    ),nrow=npreg,byrow = T)
    
    
  }
  
  dimnames(matvecus) <- list(paste0('Preg',1:npreg),
                             namecrit)
  

  #construyo matriz de calificaciones de ejercicio alumno
  #lo meto dentro de la función porque estos vectores sí cambian de uno a otro.
  matcal<-matrix(nrow=npreg,ncol=length(namecrit))
  for (i in 1:npreg){
    namevec<-parse(text=paste('cal',i,sep=''))
    matcal[i,]<-eval(namevec)
    
  }
 
  #matriz de vectores crudos
  mat.res.1<-matrix(nrow=npreg,ncol=length(namecrit))
  colnames(mat.res.1)<-namecrit
  rownames(mat.res.1)<-paste0('Preg',1:npreg)
  mat.res.2<-matrix(nrow=npreg,ncol=length(namecrit))
  colnames(mat.res.2)<-namecrit
  rownames(mat.res.2)<-paste0('Preg',1:npreg)
  mat.pond<-matrix(nrow=npreg,ncol=length(namecrit))
  colnames(mat.pond)<-namecrit
  rownames(mat.pond)<-paste0('Preg',1:npreg)
  
  for (i in 1:npreg){
    mat.res.1[i,]<-pond.cor(vecus=matvecus[i,],veccal=matcal[i,]
                            ,vecpond=matvecpond[i,],namecrit = namecrit)[1,]
    mat.pond[i,]<-pond.cor(vecus=matvecus[i,],veccal=matcal[i,]
                           ,vecpond=matvecpond[i,],namecrit = namecrit)[4,] #peso corregido
    mat.res.2[i,]<-pond.cor(vecus=matvecus[i,],veccal=matcal[i,]
                            ,vecpond=matvecpond[i,],namecrit = namecrit)[5,] #punto*pesocorregido
    # 
    # rm(list=ls()[! ls() %in% preserve])
    # notafin.R(matvecpond,matvecus,matcal,npreg,namecrit)
    # 
    
  }
  # mat.pond[mat.pond==0]<-NA
  # mat.pond <<- mat.pond
  # 
  mat.pond.print <<- mat.pond
  # mat.pond.print[mat.pond.print==0]<-NA
  # mat.pond.print<<-mat.pond.print

  Print<-'Puntuaciones por apartados sobre 10'
  
  
  mat.res.uno<-round(mat.res.1
          #2021_07_03 Elimino media de notas crudas porque creo que confunde
          ,2)
  # mat.res.dos<-round(cbind(mat.res.2,TotPreg_10=apply(mat.res.2,1,sum,na.rm=T)),2)
  mat.res.dos <- mat.res.2
  mat.res.tres<-round(rbind(mat.res.dos[1:(npreg),],
                            #2021_07_03 indexo mat.res.dos para evitar que incluya la P7 en promedio
                            #2024_01_02 cambio p7 a pextra paraflexibilizar archivo: Asumo que la extra siempre es la última.
                            MeanPregOb_10=apply(mat.res.dos[1:dim(mat.res.dos)[1],]/mat.pond,2,mean,na.rm=T)#,
                            
                            #2025 solo 5 preguntas sin extra por lo que comento y cambio dimensiones de matrices
                            # uso npreg en vez de npreg-1
                            # MeanPregOb_10=apply(mat.res.dos[1:dim(mat.res.dos)[1]-1,]/mat.pond[-6,],2,mean,na.rm=T),
                            # PExtra=mat.res.dos[npreg,]/40
                            # ,PExtra=sum(mat.res.2[npreg,]/10)
                            
                        )
                      ,2
                  )
  mat.res.tres <- round(cbind(mat.res.tres,`TotPreg_10`=apply(mat.res.tres,1,sum,na.rm=T)),2)
  # mat.res.tres[npreg,6]<-mat.res.tres[npreg,6]/5 #A falta de algo mejor corrijo manualmente. No funciona porque los pesos son diferentes en cada conjunto
  mat.res.tres[npreg,6]<-mean(mat.res.tres[1:5,6]) #A falta de algo mejor corrijo manualmente.
  
  # apply(mat.res.dos[1:dim(mat.res.dos)[1]-1,],2,mean,na.rm=T)
  sumaMeanExtra=sum(mean(mat.res.tres[1:5,6]),mat.res.tres[npreg+1,6],na.rm=T)
  #Divido la suma total de meanpregob10 por 5 preguntas y divido la Pextra (sober 10) entre 40 para que sume 0.25
  mat.res.tres<-rbind(mat.res.tres
                      ,NotaFinalR=if(sumaMeanExtra>=10) {c(rep(0,5),10)} 
                      else{
                        c(rep(0,5),sumaMeanExtra)
                        }
                      )#Si no divido por 2, sale el
    #las saco después de generarls porque de hacerlo directamente luego hace cosas extrañas.
  
  matvecus.print <<-matvecus
  
  matvecpond.print <<-matvecpond
  
  mat.res.uno.print <<-mat.res.uno #salen para poder ser usados en rmd de corrección
  
  mat.res.dos.print <<-mat.res.dos 

  mat.res.tres.print <<-mat.res.tres

  #echo de comprobación
  # print(matvecus.print)
  # print(matvecpond.print)
  # print(mat.res.uno.print)
  # print(mat.pond.print)
  # print(mat.res.tres.print)

  notatot<-as.numeric(mat.res.tres[dim(mat.res.tres)[1]
                        ,dim(mat.res.tres)[2]])
  notatot.print <<- notatot

  namealumno<-substr(paste0(basename(getwd())),start=0,stop=regexpr('_',paste0(basename(getwd())))[1]-1)
  filename=paste0('./',namealumno,'_',notatot,'.xlsx')
  write.xlsx2(mat.res.uno,file=filename,sheetName=paste('NotasCrudas',1))
  write.xlsx2(mat.res.tres,file=filename,sheetName=paste('NotasPonderadas',2),append=T)
  write.xlsx2(mat.pond,file=filename,sheetName='MatrixPonderación',append=T)
  

}



# # Vectores de prueba longitud 5
# cal1<-c(0,0,0,10,10)
# cal2<-c(0,0,0,10,10)
# cal3<-c(10,10,10,10,10)
# cal4<-c(10,10,10,10,10)
# cal5<-c(0,0,0,10,10)
# # cal6<-c(10,10,10,8)
# # cal7<-c(5,5,5,5)
# tipo <-1

 notafin.R()
 # rm(list=setdiff(ls(),c("notafin.R","pond.cor",'datos')))
