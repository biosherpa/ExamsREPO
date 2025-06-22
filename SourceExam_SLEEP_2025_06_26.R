# Función para generar multinomial categórica (simplificada y clara)
rm(list=ls())
multinom_sample <- function(n, prob) {
  apply(rmultinom(n, 1, prob), 2, function(x) which(x == 1))
}

# Función para generar variables clínicas y fisiológicas básicas
generate_clinical_data <- function(n) {
  age <- rnorm(n, 30, 2.1)
  height <- rnorm(n,178,5)
  bmi <- rnorm(n,30,1)
  weight <- bmi*(height/100)^2
  smoke <- factor(multinom_sample(n,c(.6,.1,.3)), labels = c("NoSomoking","Exfumador", "Fumador actual"))
  sleepdur_base <- rnorm(n,6,.25)
  group_ct <- factor(rbinom(n,1,.5))

  df <- data.frame(age,height,
             bmi,
             weight,
             smoke,
             sleepdur_base,
             group_ct
  )
  
  # Redondear solo las columnas numéricas
  is_num <- sapply(df, is.numeric)
  df[is_num] <- apply(df[is_num], 2, round, digits = 2)
  
  return(df)
}


generate_sleepdur_3yr <- function(data) {
  mean_sldurbas <- mean(data$sleepdur_base)
  n <- nrow(data)
  intercept <- rnorm(n, mean_sldurbas, 1)
  sigma <- 0.3
  eps <- rnorm(n, 0, sigma)
  min_sleep <- 4
  max_sleep <- 10
  
  linb1 <- rnorm(n, 1.25, 0.1)
  # linb2 <- rnorm(n, 1.5, .25)
  # linb3 <- rnorm(n, 1.5, .25)
  # linb4 <- rnorm(n, 1.5, .25)
  # 
  sleepdur_3yr <- intercept +
    linb1 * as.numeric(data$group_ct)-1 +
    # linb2 * as.numeric(data$N) +
    # linb3 * as.numeric(data$stage) +
    # linb4 * as.numeric(data$er) +
    eps
  
  # sleepdur_3yr <- round(pmax(min_sleep, pmin(sleepdur_3yr, max_sleep)), 2)
  # sleepdur_3yr <- logistic_rescale(sleepdur_3yr,min_sleep,max_sleep)
  
  sleepdur_3yr
}

####### COMPROBACIONES #####

  # datos$sleepdur_base
  # min_sleep <- 4
  # max_sleep <- 10
  # datos$sleepdur_3yr <- round(pmax(min_sleep, pmin(generate_sleepdur_3yr(datos),max_sleep)),2)
  # datos$sleepdur_3yr <- generate_sleepdur_3yr(datos)
  # 
  # par(mfrow=c(2,1))
  # 
  # lapply(datos[datos$group_ct == "0",c('sleepdur_base','sleepdur_3yr')],hist)
  # lapply(datos[datos$group_ct == "1",c('sleepdur_base','sleepdur_3yr')],hist)
  # 
  # psych::describeBy(datos[,c('sleepdur_base','sleepdur_3yr',"group_ct")],group = "group_ct")
  # t.test((datos$sleepdur_base-datos$sleepdur_3yr)~datos$group_ct)

#### FIN COMPROBACIONES ######       

generate_weight_3yr <- function(data) {
  mean_wgtbas <- mean(data$weight)
  n <- nrow(data)
  intercept <- rnorm(n, mean_wgtbas, 2)
  sigma <- 0.3
  eps <- rnorm(n, 0, sigma)
 
  
  linb1 <- rnorm(n, -2.25, 0.1)
  # linb2 <- rnorm(n, 1.5, .25)
  # linb3 <- rnorm(n, 1.5, .25)
  # linb4 <- rnorm(n, 1.5, .25)
  # 
  weight_3yr <- intercept +
    linb1 * as.numeric(data$group_ct)-1 +
    # linb2 * as.numeric(data$N) +
    # linb3 * as.numeric(data$stage) +
    # linb4 * as.numeric(data$er) +
    eps
  
  # weight_3yr <- round(pmax(min_sleep, pmin(weight_3yr, max_sleep)), 2)
  # weight_3yr <- logistic_rescale(weight_3yr,min_sleep,max_sleep)
  
  weight_3yr
}

####### COMPROBACIONES #####
# datos$weight_3yr <- round(generate_weight_3yr(datos),2)
# datos$weight_3yr


# Función para generar la variable EVENTO CV (logística)
# BMI e incremento de peso (más alto en 3yr, por tanto positivo) aumentan el riesgo,
# sleepdur_dif positiva, sueño más largo en 3yr, disminuye riesgo ecv
generate_ecv <- function(data) {
  
  var1 <- data$bmi
  var2 <- data$sleepdur_3yr - data$sleepdur_base
  var3 <- data$weight_3yr - data$weight
  var4 <- data$group_ct
  tabaco_dummies <- model.matrix(~ smoke, data = data)[, -1]
  
  n <- nrow(data)
  
  ORs <- c(1, #b0
           1.2, #bvar1
           0.2, #bvar2
           1.75, #bvar3
           0.75, #bvar4
           1.1,
           1.4
           )  # Intercepto = 1 para log(1)=0
  log_ORs <- log(ORs)
  SE_ORs <- rep(0.1, length(ORs))
  
  logb <- sapply(1:length(log_ORs), function(i) rnorm(n, mean = log_ORs[i], sd = SE_ORs[i]))
  
  scaled_var1 <- scale(var1)
  scaled_var2 <- scale(var2)
  scaled_var3 <- scale(var3)
  
  logit <- logb[,1] + 
    logb[,2] * scaled_var1 + 
    logb[,3] * scaled_var2 + 
    logb[,4] * scaled_var3 +
    logb[,5] * as.numeric(var4)+
    logb[,6] * tabaco_dummies[,1] +   # coef * dummy ex fumador
    logb[,7] * tabaco_dummies[,2]     # coef * dummy actual fumador
  
  eps <- rnorm(n, 0, 0.05)
  logit_final <- logit + eps
  
  risk_cv <- exp(logit_final) / (1 + exp(logit_final))
  
  # risk_cv1 <- factor(rbinom(n, 1, prob = risk_cv), labels = c("No_CVE", "Yes_CVE"))
  risk_cv <- factor(risk_cv > sample(x = c(.85,.88,.9),
                                     size =1,
                                     replace =T), 
                    labels = c("No_CVE", "Yes_CVE")
                    )
  
  # return(risk_cv1)
  return(risk_cv)
  
}

generate_ecv <- function(data) {
  var1 <- data$bmi
  var2 <- data$sleepdur_3yr - data$sleepdur_base
  var3 <- data$weight_3yr - data$weight
  var4 <- data$group_ct
  tabaco_dummies <- model.matrix(~ smoke, data = data)[, -1]
  
  n <- nrow(data)
  
  ORs <- c(0.5,     # intercepto bajo para reducir riesgo base
           1.2,     # var1
           0.2,     # var2
           1.75,    # var3
           0.75,    # var4
           1.1,     # tabaco ex
           1.4      # tabaco actual
  )
  
  log_ORs <- log(ORs)
  SE_ORs <- rep(0.1, length(ORs))  # Puedes reducir a 0 si no quieres variabilidad entre individuos
  SE_ORs[1] <- 6
  logb <- sapply(1:length(log_ORs), function(i) rnorm(n, mean = log_ORs[i], sd = SE_ORs[i]))
  
  scaled_var1 <- scale(var1)
  scaled_var2 <- scale(var2)
  scaled_var3 <- scale(var3)
  
  logit <- logb[,1] + 
    logb[,2] * scaled_var1 + 
    logb[,3] * scaled_var2 + 
    logb[,4] * scaled_var3 +
    logb[,5] * as.numeric(var4) +
    logb[,6] * tabaco_dummies[,1] +   # ex fumador
    logb[,7] * tabaco_dummies[,2]     # fumador actual
  
  eps <- rnorm(n, 0, 0.15)
  logit_final <- logit + eps
  
  # Probabilidad predicha
  risk_cv <- exp(logit_final) / (1 + exp(logit_final))
  
  # Establecer umbrales por grupo para lograr 10% (group_ct = 1) y 6% (group_ct = 0)
  umbral_g1 <- quantile(risk_cv[var4 == 1], probs = 0.95)  # deja 10% con evento
  umbral_g0 <- quantile(risk_cv[var4 == 0], probs = 0.92)  # deja 6% con evento
  
  risk_cv_bin <- ifelse(
    (var4 == 1 & risk_cv > umbral_g1) |
      (var4 == 0 & risk_cv > umbral_g0),
    1, 0
  )
  
  risk_cv <- factor(risk_cv_bin, labels = c("No_CVE", "Yes_CVE"))
  
  return(risk_cv)
}



######### COMPROBACIONES ####
# datos$ecv <- generate_ecv (datos)

    # prop.table(table(datos$group_ct,datos$ecv),1)
    # datos$dif_sleep <- datos$sleepdur_3yr-datos$sleepdur_base
    # datos$dif_weight <- datos$weight_3yr-datos$weight
    # 
    # 
    # fit_ecv <- glm(ecv~bmi+dif_sleep + dif_weight + group_ct, data=datos,
    #                family = "binomial")
    # summary(fit_ecv)
    # 
    # or_table <- data.frame(
    #   OR = exp(coef(fit_ecv)),
    #   CILOW = exp(confint(fit_ecv))[, 1],
    #   CIUPP = exp(confint(fit_ecv))[, 2],
    #   p.value = summary(fit_ecv)$coefficients[, "Pr(>|z|)"]
    # )
    # 
    # # Redondear y crear tabla bonita
    # or_table %>%
    #   round(3) %>%
    #   gt::gt(rownames_to_stub = TRUE) %>%
    #   gt::fmt_number(decimals = 2) %>%
    #   gt::cols_label(
    #     OR = "OR",
    #     CILOW = "IC 2.5%",
    #     CIUPP = "IC 97.5%"
    #   ) %>%
    #   gt::tab_header(
    #     title = "Odds Ratios y Intervalos de Confianza"
    #   )
    # 
    # library(gtsummary)
    # 
    # tbl_uni <- tbl_uvregression(
    #   data = datos,
    #   method = glm,
    #   y = ecv,
    #   method.args = list(family = binomial),
    #   exponentiate = TRUE,
    #   include = c(bmi, dif_sleep, dif_weight, group_ct)
    # )
    # 
    # modelo_multi <- glm(ecv ~ bmi + dif_sleep + dif_weight + group_ct, data = datos, family = binomial)
    # 
    # tbl_multi <- tbl_regression(
    #   modelo_multi,
    #   exponentiate = TRUE
    # )
    # 
    # tbl_merge(
    #   tbls = list(tbl_uni, tbl_multi),
    #   tab_spanner = c("**Univariable**", "**Multivariable**")
    # )
    # 
    
######### FIN COMPROBACIONES ####
    
# Función principal que genera todo y devuelve dataframe listo
  # set.seed(123)
  n <- sample(x = 1000:1500,1)


generate_sleep_dataset <- function(n) {
  data <- generate_clinical_data(n)
  data$sleepdur_3yr <- round(generate_sleepdur_3yr(data),2)
  data$weight_3yr <- round(generate_weight_3yr(data),2)
  data$ecv <- generate_ecv(data)
  data
}

datos <- generate_sleep_dataset(n)

write.csv(
  generate_sleep_dataset(n),
          "~/sleep.csv")


rm(list=c(ls(pattern="generate"),'multinom_sample','n'))


