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
usuario <- Sys.info()[["user"]]
letras_raw <- as.integer(charToRaw(usuario))
posiciones <- 1:length(letras_raw)
semilla_robusta <- sum(letras_raw * posiciones)

set.seed(semilla_robusta)
n_size <- sample(1500:2200, 1)

# 2. GENERACIÓN DE VARIABLES BASALES (Inspirado en STEP UP Trial)
# -----------------------------------------------------------------------
generate_clinical_data <- function(n) {
  # Forzamos rangos biológicos lógicos (18-85 años)
  age <- round(pmin(pmax(rnorm(n, 47, 12), 18), 85))
  sex <- factor(rbinom(n, 1, 0.75), labels = c("Female", "Male"))
  height <- rnorm(n, 170, 7)
  bmi <- pmin(pmax(rnorm(n, 39.9, 7.1), 20), 70)
  weight <- bmi * (height/100)^2
  
  smoke <- factor(multinom_sample(n, c(.6, .1, .3)), 
                  labels = c("NoSmoking", "Exfumador", "Fumador actual"))
  treat <- factor(multinom_sample(n, c(.25, .25, .5)), 
                  labels = c("Placebo", "Semaglutide 2.4 mg", "Semaglutide 7.2 mg"))
  
  df <- data.frame(age, sex, height, bmi, weight, smoke, treat)
  
  # Redondeo de basales
  is_num <- sapply(df, is.numeric)
  df[is_num] <- round(df[is_num], 2)
  return(df)
}

# 3. GENERACIÓN DE RESULTADOS Y EVENTOS (Estructura de Diccionario Plano)
# -----------------------------------------------------------------------
generate_outcomes <- function(data) {
  n <- nrow(data)
  
  # ########################################################################
  # PARÁMETROS EDITABLES (Cambia solo los números aquí)
  # ########################################################################
  
  # --- EFICACIA PESO (kg) ---
  eff_24 <- -17.3
  eff_72 <- -20.9
  
  # --- EVENTO CARDIOVASCULAR (ORs) ---
  p0_cv      <- 0.05
  or_cv_24   <- 0.82
  or_cv_72   <- 0.72
  or_cv_male <- 1.65
  
  # --- NÁUSEAS (ORs) ---
  p0_na      <- 0.10
  or_na_24   <- 3.80
  or_na_72   <- 8.50
  
  # --- OTROS EVENTOS ADVERSOS (SAE) (ORs) ---
  p0_bi      <- 0.015
  or_bi_24   <- 1.60
  or_bi_72   <- 2.20
  
  p0_pa      <- 0.002
  or_pa_72   <- 3.50
  
  p0_re      <- 0.010
  or_re_72   <- 1.90
  
  p0_rn      <- 0.005
  or_rn_na   <- 6.00
  
  # ########################################################################
  # LÓGICA DE CÁLCULO
  # ########################################################################
  
  # --- PESO FINAL ---
  cambio_p <- ifelse(data$treat == "Semaglutide 2.4 mg", eff_24,
                     ifelse(data$treat == "Semaglutide 7.2 mg", eff_72, 0))
  data$weight_72wk <- data$weight + cambio_p + 0.08*(data$age-45) + rnorm(n, 0, 4)
  
  # --- EVENTO CV (Interacción por edad) ---
  log_or_cv <- ifelse(data$treat == "Semaglutide 2.4 mg", log(or_cv_24),
                      ifelse(data$treat == "Semaglutide 7.2 mg", log(or_cv_72), 0))
  es_joven <- data$age < 60
  efecto_f <- log_or_cv * ifelse(es_joven, 1, -0.6)
  
  logit_cv <- log(p0_cv/(1-p0_cv)) + efecto_f + log(or_cv_male)*(data$sex=="Male")
  data$ev_cv <- factor(rbinom(n, 1, exp(logit_cv)/(1+exp(logit_cv))), labels=c("No", "Yes"))
  
  # --- NÁUSEAS ---
  log_or_na <- ifelse(data$treat == "Semaglutide 2.4 mg", log(or_na_24),
                      ifelse(data$treat == "Semaglutide 7.2 mg", log(or_na_72), 0))
  logit_na <- log(p0_na/(1-p0_na)) + log_or_na + log(0.45)*(data$sex=="Male")
  data$nauseas <- factor(rbinom(n, 1, exp(logit_na)/(1+exp(logit_na))), labels=c("No", "Yes"))
  
  # --- SAEs (Biliar, Pancreatitis, Retinopatía, Renal) ---
  w_loss_pct <- (data$weight - data$weight_72wk) / data$weight
  
  # Biliar
  log_or_bi <- ifelse(data$treat == "Semaglutide 2.4 mg", log(or_bi_24),
                      ifelse(data$treat == "Semaglutide 7.2 mg", log(or_bi_72), 0))
  logit_bi <- log(p0_bi/(1-p0_bi)) + log_or_bi + log(5.0)*pmax(0, w_loss_pct)
  data$sae_biliar <- factor(rbinom(n, 1, exp(logit_bi)/(1+exp(logit_bi))), labels=c("No", "Yes"))
  
  # Otros
  data$sae_pancr  <- factor(rbinom(n, 1, p0_pa * ifelse(data$treat=="Semaglutide 7.2 mg", or_pa_72, 1)), labels=c("No", "Yes"))
  data$sae_retino <- factor(rbinom(n, 1, p0_re * ifelse(data$treat=="Semaglutide 7.2 mg", or_re_72, 1)), labels=c("No", "Yes"))
  
  # Renal (Mediación)
  logit_rn <- log(p0_rn/(1-p0_rn)) + log(or_rn_na)*(data$nauseas=="Yes") + log(1.3)*(data$age>65)
  data$sae_renal <- factor(rbinom(n, 1, exp(logit_rn)/(1+exp(logit_rn))), labels=c("No", "Yes"))
  
  # Limpieza Pesos
  data$weight_72wk <- round(pmin(pmax(data$weight_72wk, 38), 250), 2)
  return(data)
}

# ========================================================================
# 4. EJECUCIÓN Y GUARDADO FINAL
# ========================================================================

# Ejecución en cadena
datos_temp <- generate_clinical_data(n_size)
datos <- generate_outcomes(datos_temp)

# --- REVERSIÓN DE FACTORES ---

# Convertimos treat a numérico (0, 1, 2) para que el alumno tenga que factorizarlo
datos$treat <- as.numeric(as.factor(datos$treat)) - 1

# Aseguramos que el resto sean caracteres o números, no factores
datos$smoke <- as.character(datos$smoke)
datos$sex   <- as.character(datos$sex)
datos$ev_cv <- as.numeric(datos$ev_cv)

# Guardado CSV
# Solo se ejecuta si el usuario eres TÚ (pon aquí tu usuario de PC)
if (Sys.info()["user"] == "TU_USUARIO_REAL") {
  
  # try() evita que el script se pare si algo falla (ej. disco lleno o ruta mal escrita)
  try({
    write.csv(datos, "~/step_ct.csv", row.names = FALSE)
    saveRDS(datos, "~/step_ct_respaldo.rds")
  }, silent = TRUE)
  
}

# Limpieza del entorno para el alumno
rm(list=c(ls(pattern="generate"), 'datos_temp', 'n_size', 'semilla_robusta', 
          'letras_raw', 'posiciones', 'usuario', 'multinom_sample'))

# Mensaje de confirmación
cat("\n========================================\n")
cat("  DATASET 'datos' CARGADO (N =", nrow(datos), ")\n")
cat("  Archivo 'step_ct.csv' guardado en Home  \n")
cat("========================================\n")
