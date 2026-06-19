# ========================================================================
# SCRIPT DE GENERACIÓN DE DATOS - EXAMEN 2026
# ========================================================================
rm(list=ls())

# Función auxiliar
multinom_sample <- function(n, prob) {
  apply(rmultinom(n, 1, prob), 2, function(x) which(x == 1))
}

# 1. IDENTIFICACIÓN DEL ALUMNO (Semilla única)
# -----------------------------------------------------------------------
# Intentamos obtener el usuario de Windows
usuario <- try(Sys.info()[["user"]], silent = TRUE)

if (inherits(usuario, "try-error") || is.null(usuario) || usuario == "") {
  # SI FALLA: Usamos el tiempo actual (segundos desde 1970) como semilla
  # Esto garantiza que cada examen sea diferente incluso si falla el nombre
  semilla_robusta <- as.integer(Sys.time()) 
} else {
  # SI FUNCIONA: Usamos tu lógica de posición de letras para consistencia
  letras_raw <- as.integer(charToRaw(usuario))
  posiciones <- 1:length(letras_raw)
  semilla_robusta <- sum(letras_raw * posiciones)
}


set.seed(semilla_robusta)
n <- sample(1500:2200, 1)

# 2. GENERACIÓN DE VARIABLES BASALES (Inspirado en STEP UP Trial)
# -----------------------------------------------------------------------
generate_clinical_data <- function(n) {
  # Forzamos rangos biológicos lógicos (18-85 años)
  age <- round(pmin(pmax(rnorm(n, 47, 12), 18), 85))
  sex <- factor(rbinom(n, 1, 0.75), labels = c("Female", "Male"))
  # height <- rnorm(n, 170, 7)
  # bmi <- pmin(pmax(rnorm(n, 39.9, 7.1), 20), 70)
  # weight <- bmi * (height/100)^2
  smoke <- rbinom(n,1,prob = 0.35)
  gravedad <- runif(n, 2, 8)
  prob_tx <- plogis(-3 + 0.8 * gravedad) 
  treat <- rbinom(n, 1, prob_tx)
  # NUEVA VARIABLE: Consumo de alcohol (sí/no), aproximadamente 30% de la muestra
  alcohol <- rbinom(n, 1, prob = 0.3)
  alcohol <- factor(alcohol, levels = c(0, 1), labels = c("No", "Sí"))
  
  # Dolor basal (EVA 0-10) correlacionado con gravedad
  eva_basal <- 2 + 0.7 * gravedad + rnorm(n, 0, 1)
  eva_basal <- pmin(pmax(eva_basal, 0), 10)
  
  # Dolor a los 6 meses (SIN interacción aún, todo igual)
  efecto_treat_6m <- -1.8 * treat
  evolucion_control_6m <- -0.5
  eva_6wk <- eva_basal + evolucion_control_6m + efecto_treat_6m + rnorm(n, 0, 0.8)
  eva_6wk <- pmin(pmax(eva_6wk, 0), 10)
  
  # Dolor a los 12 meses (SIN interacción aún, todo igual)
  efecto_treat_12m <- -1.5 * treat
  evolucion_control_12m <- -0.3
  eva_12wk <- eva_basal + evolucion_control_12m + efecto_treat_12m + rnorm(n, 0, 0.9)
  eva_12wk <- pmin(pmax(eva_12wk, 0), 10)
  
  # Dolor final (para los análisis simples, tomamos el de 12 meses como final)
  dolor_eva_final <- eva_12wk
  
  # --- INTERACCIÓN: efecto del treat reducido en consumidores de alcohol ---
  # Coeficiente de interacción negativo: el alcohol reduce el beneficio del treat
  interaccion_alcohol <- -1.2 * (treat == 1) * (alcohol == "Sí")
  
  # Dolor final CON interacción (solo para demostrar, manteniendo la original)
  dolor_eva_final_interaccion <- dolor_eva_final + interaccion_alcohol + rnorm(n, 0, 0.3)
  dolor_eva_final_interaccion <- pmin(pmax(dolor_eva_final_interaccion, 0), 10)
  
  
  df <- data.frame(
    id = 1:n,
    gravedad = round(gravedad, 1),
    treat = factor(treat, levels = c(0, 1), labels = c("Estándar", "Nuevo")),
    age = age,
    sex = factor(sex, levels = c("Male", "Female")),
    smoke = smoke,
    alcohol = alcohol,  # Nueva variable
    eva_basal = round(eva_basal, 1),
    eva_6wk = round(eva_6wk, 1),
    eva_12wk = round(eva_12wk, 1),
    dolor_eva_final = round(dolor_eva_final, 1),
    dolor_eva_final_interaccion = round(dolor_eva_final_interaccion, 1)  # Para demostrar interacción
  )
  

  # Redondeo de basales
  is_num <- sapply(df, is.numeric)
  df[is_num] <- round(df[is_num], 2)
  return(df)
}

# 3. GENERACIÓN DE RESULTADOS Y EVENTOS (Estructura de Diccionario Plano)
# -----------------------------------------------------------------------
generate_outcomes <- function(data) {
  # data <- datos_temp # para pruebas.
  n_size <- nrow(data)
  
  # ########################################################################
  # PARÁMETROS EDITABLES 
  # ########################################################################
  
  # --- EFICACIA EVA (cm) ---
  eff_A_t1 <- -1.5
  eff_A_t2 <- -3
  
  # --- EVENTO_ae (ORs) ---
  p0_ae     <- 0.05
  or_ae_A_t1   <- 1.2
  or_ae_A_t2   <- 1.5
  or_ae_smoke <- 2
  
  # # --- NÁUSEAS (ORs) ---
  # p0_na      <- 0.05
  # or_na_24   <- 3.80
  # or_na_72   <- 8.50
  # 
  # # --- OTROS EVENTOS ADVERSOS (SAE) (ORs) ---
  # p0_bi      <- 0.015
  # or_bi_24   <- 1.60
  # or_bi_72   <- 2.20
  # 
  # p0_pa      <- 0.002
  # or_pa_72   <- 3.50
  # 
  # p0_re      <- 0.010
  # or_re_72   <- 1.90
  # 
  # p0_rn      <- 0.005
  # or_rn_na   <- 6.00
  # 
  # ########################################################################
  # LÓGICA DE CÁLCULO
  # ########################################################################
  
  # --- PESO FINAL ---
  cambio_eva <- ifelse(data$treat == "Nuevo", eff_A_t1,
                     ifelse(data$treat == "Estándar", 0, 0))
  data$eva_basal<- data$eva_basal + cambio_eva + 0.08*(data$age-40) + rnorm(n_size, 0, 2)
  
  # --- EVENTO CV (Interacción por edad) ---
  log_or_ae <- ifelse(data$treat == "Nuevo", log(or_ae_A_t1),
                      ifelse(data$treat == "Estándar", log(1), 0)
                      )
  
  es_joven <- data$age < 60
  efecto_f <- log_or_ae * ifelse(es_joven, 1, -0.6)
  
  
  logit_ae <- log(p0_ae/(1-p0_ae)) + efecto_f + log(or_ae_smoke)*(data$sex=="Male")
  
  data$ae <- factor(rbinom(n, 1, exp(logit_ae)/(1+exp(logit_ae))), labels=c("No", "Yes"))
  
  # --- NÁUSEAS ---
  # log_or_na <- ifelse(data$treat == "Semaglutide 2.4 mg", log(or_na_24),
  #                     ifelse(data$treat == "Semaglutide 7.2 mg", log(or_na_72), 0))
  # logit_na <- log(p0_na/(1-p0_na)) + log_or_na + log(0.45)*(data$sex=="Male")
  # data$nauseas <- factor(rbinom(n, 1, exp(logit_na)/(1+exp(logit_na))), labels=c("No", "Yes"))
  
  # --- SAEs (Biliar, Pancreatitis, Retinopatía, Renal) ---
  # w_loss_pct <- (data$weight - data$weight_72wk) / data$weight
  
  # Biliar
  # log_or_bi <- ifelse(data$treat == "Semaglutide 2.4 mg", log(or_bi_24),
  #                     ifelse(data$treat == "Semaglutide 7.2 mg", log(or_bi_72), 0))
  # logit_bi <- log(p0_bi/(1-p0_bi)) + log_or_bi + log(5.0)*pmax(0, w_loss_pct)
  # data$sae_biliar <- factor(rbinom(n, 1, exp(logit_bi)/(1+exp(logit_bi))), labels=c("No", "Yes"))
  
  # Otros
  # data$sae_pancr  <- factor(rbinom(n, 1, p0_pa * ifelse(data$treat=="Semaglutide 7.2 mg", or_pa_72, 1)), labels=c("No", "Yes"))
  # data$sae_retino <- factor(rbinom(n, 1, p0_re * ifelse(data$treat=="Semaglutide 7.2 mg", or_re_72, 1)), labels=c("No", "Yes"))
  
  # Renal (Mediación)
  # logit_rn <- log(p0_rn/(1-p0_rn)) + log(or_rn_na)*(data$nauseas=="Yes") + log(1.3)*(data$age>65)
  # data$sae_renal <- factor(rbinom(n, 1, exp(logit_rn)/(1+exp(logit_rn))), labels=c("No", "Yes"))
  
  # Limpieza Pesos
  data$eva_basal <- round(pmin(pmax(data$eva_basal, 0), 10), 2)
  return(data)
}

# ========================================================================
# 4. EJECUCIÓN Y GUARDADO FINAL
# ========================================================================

# Ejecución en cadena
datos_temp <- generate_clinical_data(n)
datos <- generate_outcomes(datos_temp)

# --- REVERSIÓN DE FACTORES ---

# Convertimos treat a numérico (0, 1, 2) para que el alumno tenga que factorizarlo
datos$treat <- as.numeric(as.factor(datos$treat)) - 1

# Aseguramos que el resto sean caracteres o números, no factores
datos$smoke <- as.character(datos$smoke)
datos$sex   <- as.character(datos$sex)
datos$ae <- as.numeric(datos$ae)

# Guardado CSV
# Solo se ejecuta si el usuario eres TÚ (pon aquí tu usuario de PC)
if (Sys.info()["user"] == "jesus.esteban") {
  
  # try() evita que el script se pare si algo falla (ej. disco lleno o ruta mal escrita)
  try({
    write.csv(datos, "~/pain_ct.csv", row.names = FALSE)
    saveRDS(datos, "~/pain_ct_respaldo.rds")
  }, silent = TRUE)
  
}

library(labelled)
# Añadimos etiquetas descriptivas (opcional, por si tu df ya las tiene)
var_label(datos) <- list(
  id                      = "ID",
  gravedad                = "Índice de gravedad basal",
  treat                   = "Grupo de tratamiento asignado",
  age                     = "Edad al ingreso",
  sex                     = "Sexo del paciente",
  smoke                   = "Hábito tabáquico",
  alcohol                 = "Consumo de alcohol",
  eva_basal               = "Escala Visual Analógica (EVA) basal",
  eva_6wk                 = "Escala Visual Analógica (EVA) a las 6 semanas",
  eva_12wk                = "Escala Visual Analógica (EVA) a las 12 semanas",
  dolor_eva_final         = "Dolor EVA final (Modelo simple)",
  dolor_eva_final_interaccion = "Dolor EVA final (Modelo con interacción)",
  ae                      = "Presencia de Evento Adverso "
)

# Limpieza del entorno para el alumno
rm(list=c(ls(pattern="generate"), 'datos_temp', 'n', 'semilla_robusta', 
          'letras_raw', 'posiciones', 'usuario', 'multinom_sample'))

# Mensaje de confirmación
cat("\n========================================\n")
cat("  DATASET 'datos' CARGADO (N =", nrow(datos), ")\n")
cat("  Archivo 'pain_ct.csv' guardado en Home  \n")
cat("========================================\n")

