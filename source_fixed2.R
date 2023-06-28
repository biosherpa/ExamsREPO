pck.list <- list('janitor','dplyr','tidyr')
notinst.packages<-pck.list[!pck.list %in% installed.packages()]
lapply(notinst.packages,install.packages,dependencies=TRUE)
lapply(pck.list[!paste0('package:',pck.list)%in%search()],require,character.only=TRUE)


datos <- readr::read_csv(url("https://raw.githubusercontent.com/Adrimgedu/test/main/HDHI%20Admission%20data.csv"))

datos$GENDER <- factor(datos$GENDER)
datos$RURAL <- factor(datos$RURAL)
datos$`TYPE OF ADMISSION-EMERGENCY/OPD` = factor(datos$`TYPE OF ADMISSION-EMERGENCY/OPD`)
datos$OUTCOME <- factor(datos$OUTCOME)

datos <- dplyr::sample_n(datos,size = 600)
datos <- janitor::clean_names(datos)

datos <- dplyr::select(.data = datos,sno,mrd_no,d_o_a,d_o_d,age,gender,rural,type_of_admission_emergency_opd,duration_of_stay,outcome,smoking,alcohol,hb,tlc,urea,creatinine,anaemia,heart_failure,pulmonary_embolism,chest_infection)
datos <- tidyr::drop_na(datos) #Añado nombre.
rm(list=ls()[!ls()%in% c('datos')])

   