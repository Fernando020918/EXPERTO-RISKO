# Script R: Cálculo EL, UL y Extreme Loss (Riesgo Operativo) - Con Matriz de Percentiles
# Autor: Generado para Pierina (UL = VaR - EL)
# Propósito: leer base de eventos (Excel), ajustar modelos de frecuencia y severidad,
#            simular compound model por factor y global, y exportar resultados a Excel + gráficos.
# NUEVO: Incluye matriz de percentiles 0%-100% por factor, con frecuencias también

# ---------- Parámetros (ajustables) ----------
params <- list(
  ruta_archivo = "C:/Users/ue01003237/Desktop/DOC Daniel Jadàn/Apetito 2025 por Perdida Esperada/Apetito 2025 por Perdida Esperada/Base reales + simulados_PS.xlsx",
  hoja = 3,
  usar_perdida = "Perdida Neta",
  ajustar_inflacion = FALSE,
  VaR_levels = c(0.95, 0.99),
  semilla = 12345,
  n_sims = 20000,
  umbral_muestra_pequena = 50,
  jitter_ratio = 0.05,
  distribuir_severidad = c("lnorm","gamma"),
  threshold_gpd_q = 0.95,
  ruta_salida = "resultados_riesgo_operativo.xlsx",
  ruta_log_omitidos = "factores_omitidos_log.csv",
  percentiles_seq = seq(0, 1, by = 0.01),  # 0% a 100% en pasos de 1%
  advertencia_muestra_pequena = 10,
  ruta_archivo_relativa = FALSE
)

# ---------- Paquetes necesarios ----------
paquetes <- c("readxl","dplyr","lubridate","fitdistrplus","MASS","evir",
              "ggplot2","openxlsx","janitor","stringr","tibble","readr",
              "survival","actuar","rriskDistributions","purrr")
inst <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(inst)) install.packages(inst)

library(readxl); library(lubridate); library(fitdistrplus)
library(MASS); library(dplyr); library(evir); library(ggplot2)
library(openxlsx); library(janitor); library(stringr); library(tibble); library(readr)
library(survival); library(actuar); library(rriskDistributions); library(purrr)

set.seed(params$semilla)

# ---------- Utilidades de logging ----------
warnings_log <- tibble::tibble(Factor = character(), Warning = character())
parametros_modelo <- tibble::tibble()
exclusion_log <- tibble::tibble(Row = integer(), Factor = character(), Motivo = character())

add_warning <- function(factor, msg){
  warnings_log <<- dplyr::bind_rows(warnings_log, tibble::tibble(Factor = factor, Warning = msg))
}

add_parametros <- function(factor, clave, valor){
  parametros_modelo <<- dplyr::bind_rows(parametros_modelo, tibble::tibble(Factor = factor, Parametro = clave, Valor = valor))
}

registrar_exclusion <- function(row_id, factor, motivo){
  exclusion_log <<- dplyr::bind_rows(exclusion_log, tibble::tibble(Row = row_id, Factor = factor, Motivo = motivo))
}

# ---------- Funciones auxiliares ----------
rgpd_custom <- function(n, xi, beta, threshold = 0){
  if(missing(xi) || length(xi)==0 || is.na(xi)) stop("xi inválido en rgpd_custom")
  if(missing(beta) || length(beta)==0 || is.na(beta) || beta <= 0) stop("beta inválido en rgpd_custom")
  if(xi == 0){
    return(threshold + rexp(n, rate = 1/beta))
  } else {
    u <- runif(n)
    return(threshold + (beta/xi) * ((1 - u)^(-xi) - 1))
  }
}

ajustar_severidad <- function(x, dist_names = c("lnorm","gamma")){
  res <- list(); fits <- list()
  for(d in dist_names){
    f <- try(fitdist(x, d), silent = TRUE)
    if(!inherits(f, "try-error")){
      if(!is.null(f$estimate) && is.numeric(unlist(f$estimate))){
        fits[[d]] <- f
        res[[d]] <- f$aic
      }
    }
  }
  if(length(res)==0) stop("No se pudo ajustar ninguna distribución de severidad")
  mejor <- names(which.min(unlist(res)))
  return(list(mejor = mejor, fits = fits, aics = res))
}

.get_gamma_params <- function(est){
  if("shape" %in% names(est)){
    shape <- as.numeric(est["shape"])
  } else if("alpha" %in% names(est)){
    shape <- as.numeric(est["alpha"])
  } else shape <- NA_real_
  if("rate" %in% names(est)){
    rate <- as.numeric(est["rate"])
  } else if("scale" %in% names(est)){
    sc <- as.numeric(est["scale"])
    rate <- ifelse(!is.na(sc) && sc != 0, 1/sc, NA_real_)
  } else rate <- NA_real_
  return(list(shape = shape, rate = rate))
}

.get_lnorm_params <- function(est){
  if("meanlog" %in% names(est) && "sdlog" %in% names(est)){
    return(list(meanlog = as.numeric(est["meanlog"]), sdlog = as.numeric(est["sdlog"])))
  } else if("mean" %in% names(est) && "sd" %in% names(est)){
    meanlog <- as.numeric(est["mean"])
    sdlog <- as.numeric(est["sd"])
    return(list(meanlog = meanlog, sdlog = sdlog))
  } else return(list(meanlog = NA_real_, sdlog = NA_real_))
}

simular_severidad_param <- function(n, fitobj, dist_name, data_empirica = NULL){
  if(n <= 0) return(numeric(0))
  if(is.null(fitobj) || is.null(dist_name)){
    if(!is.null(data_empirica) && length(data_empirica)>0) return(sample(data_empirica, n, replace = TRUE))
    stop("fitobj o dist_name nulos y no hay datos empíricos para fallback")
  }
  est <- fitobj$estimate
  if(dist_name == "lnorm"){
    lp <- .get_lnorm_params(est)
    if(!is.na(lp$meanlog) && !is.na(lp$sdlog)){
      return(rlnorm(n, meanlog = lp$meanlog, sdlog = lp$sdlog))
    } else if(!is.null(data_empirica) && length(data_empirica)>0) return(sample(data_empirica, n, replace = TRUE))
    else stop("Parámetros lnorm no encontrados y no hay datos empíricos")
  } else if(dist_name == "gamma"){
    gp <- .get_gamma_params(est)
    if(!is.na(gp$shape) && !is.na(gp$rate) && gp$rate > 0){
      return(rgamma(n, shape = gp$shape, rate = gp$rate))
    } else if(!is.null(data_empirica) && length(data_empirica)>0) return(sample(data_empirica, n, replace = TRUE))
    else stop("Parámetros gamma no válidos y no hay datos empíricos")
  } else {
    if(!is.null(data_empirica) && length(data_empirica)>0) return(sample(data_empirica, n, replace = TRUE))
    stop("Distribución no soportada y no hay datos empíricos")
  }
}

smoothed_bootstrap_draw <- function(x, n, jitter_ratio = 0.05){
  if(length(x) == 0) return(numeric(0))
  draws <- sample(x, n, replace = TRUE)
  jitter <- rnorm(n, mean = 0, sd = jitter_ratio * sd(x))
  pmax(draws + jitter, 0)
}

# ---------- Diagnósticos adicionales (inspirados en script exploratorio) ----------
extraer_metricas_gof <- function(gof_obj, dist_name){
  chisq_p <- if(!is.null(gof_obj$chisqpvalue) && dist_name %in% names(gof_obj$chisqpvalue)) gof_obj$chisqpvalue[[dist_name]] else NA_real_
  ks_p <- if(!is.null(gof_obj$ks) && dist_name %in% names(gof_obj$ks)) gof_obj$ks[[dist_name]] else NA_real_
  ad <- if(!is.null(gof_obj$ad) && dist_name %in% names(gof_obj$ad)) gof_obj$ad[[dist_name]] else NA_real_
  cvm <- if(!is.null(gof_obj$cvm) && dist_name %in% names(gof_obj$cvm)) gof_obj$cvm[[dist_name]] else NA_real_
  return(list(chisq_pvalue = chisq_p, ks_pvalue = ks_p, ad = ad, cvm = cvm))
}

generar_diagnostico_frecuencia <- function(counts, percentiles_seq){
  res <- list(tabla = NULL, percentiles = NULL, mejor = NULL)
  counts <- counts[!is.na(counts)]
  if(length(counts) == 0) return(res)
  fits <- list()
  try({ fits$pois <- fitdist(counts, "pois") }, silent = TRUE)
  try({ fits$nbinom <- fitdist(counts, "nbinom", method = "mle") }, silent = TRUE)
  fits <- Filter(Negate(is.null), fits)
  if(length(fits) == 0) return(res)
  aics <- vapply(fits, AIC, numeric(1))
  bics <- vapply(fits, BIC, numeric(1))
  gof <- try(gofstat(fits), silent = TRUE)
  tabla <- tibble::tibble()
  for(d in names(fits)){
    chisq_p <- ks_p <- ad <- cvm <- NA_real_
    if(!inherits(gof, "try-error")){
      met <- extraer_metricas_gof(gof, d)
      chisq_p <- met$chisq_pvalue; ks_p <- met$ks_pvalue; ad <- met$ad; cvm <- met$cvm
    }
    tabla <- dplyr::bind_rows(tabla, tibble::tibble(
      Distribucion = d,
      AIC = aics[[d]],
      BIC = bics[[d]],
      ChiSq_pvalue = chisq_p,
      KS_pvalue = ks_p,
      AD = ad,
      CVM = cvm
    ))
  }
  mejor <- names(which.min(aics))
  percentiles <- rep(NA_real_, length(percentiles_seq))
  boot_try <- try(bootdist(fits[[mejor]], niter = 2000), silent = TRUE)
  if(!inherits(boot_try, "try-error")){
    percentiles <- try(quantile(boot_try, probs = percentiles_seq), silent = TRUE)
    if(inherits(percentiles, "try-error")) percentiles <- rep(NA_real_, length(percentiles_seq))
  }
  res$tabla <- tabla
  res$percentiles <- percentiles
  res$mejor <- mejor
  return(res)
}

generar_diagnostico_impacto <- function(severidades, percentiles_seq){
  res <- list(tabla = NULL, percentiles = NULL, mejor = NULL)
  severidades <- severidades[!is.na(severidades) & severidades > 0]
  if(length(severidades) == 0) return(res)
  fits <- list()
  try({ fits$lnorm <- fitdist(severidades, "lnorm") }, silent = TRUE)
  try({ fits$llogis <- fitdist(severidades, "llogis", start = list(shape = 1, scale = max(1, median(severidades)))) }, silent = TRUE)
  try({ fits$pareto <- fitdist(severidades, "pareto", start = list(shape = 1, scale = max(1, median(severidades)))) }, silent = TRUE)
  try({ fits$weibull <- fitdist(severidades, "weibull") }, silent = TRUE)
  fits <- Filter(Negate(is.null), fits)
  if(length(fits) == 0) return(res)
  aics <- vapply(fits, AIC, numeric(1))
  bics <- vapply(fits, BIC, numeric(1))
  gof <- try(gofstat(fits), silent = TRUE)
  tabla <- tibble::tibble()
  for(d in names(fits)){
    chisq_p <- ks_p <- ad <- cvm <- NA_real_
    if(!inherits(gof, "try-error")){
      met <- extraer_metricas_gof(gof, d)
      chisq_p <- met$chisq_pvalue; ks_p <- met$ks_pvalue; ad <- met$ad; cvm <- met$cvm
    }
    tabla <- dplyr::bind_rows(tabla, tibble::tibble(
      Distribucion = d,
      AIC = aics[[d]],
      BIC = bics[[d]],
      ChiSq_pvalue = chisq_p,
      KS_pvalue = ks_p,
      AD = ad,
      CVM = cvm
    ))
  }
  mejor <- names(which.min(aics))
  percentiles <- rep(NA_real_, length(percentiles_seq))
  boot_try <- try(bootdist(fits[[mejor]], niter = 2000), silent = TRUE)
  if(!inherits(boot_try, "try-error")){
    percentiles <- try(quantile(boot_try, probs = percentiles_seq), silent = TRUE)
    if(inherits(percentiles, "try-error")) percentiles <- rep(NA_real_, length(percentiles_seq))
  }
  res$tabla <- tabla
  res$percentiles <- percentiles
  res$mejor <- mejor
  return(res)
}

# ---------- Cargar datos ----------
input_path <- params$ruta_archivo
if(isTRUE(params$ruta_archivo_relativa)){
  input_path <- file.path(getwd(), params$ruta_archivo)
}

df_raw <- readxl::read_excel(input_path, sheet = params$hoja) %>% janitor::clean_names()
df_raw <- df_raw %>% mutate(.row_id = dplyr::row_number())
required_cols <- c("factor","fecha_inicio","perdida_bruta","perdida_neta")
missing_cols <- setdiff(required_cols, names(df_raw))
if(length(missing_cols) > 0) stop(paste("Faltan columnas:", paste(missing_cols, collapse=", ")))
df <- df_raw

# ---------- Fecha robusta ----------
if(inherits(df$fecha_inicio, "Date")) df$fecha_inicio <- as.Date(df$fecha_inicio) else if(is.numeric(df$fecha_inicio)) df$fecha_inicio <- as.Date(df$fecha_inicio, origin = "1899-12-30") else {
  df <- df %>%
    mutate(.fecha_chr = as.character(fecha_inicio),
           .dmy = suppressWarnings(lubridate::dmy(.fecha_chr)),
           .mdy = suppressWarnings(lubridate::mdy(.fecha_chr)),
           .ymd = suppressWarnings(lubridate::ymd(.fecha_chr)))
  n_dmy <- sum(!is.na(df$.dmy))
  n_mdy <- sum(!is.na(df$.mdy))
  n_ymd <- sum(!is.na(df$.ymd))
  if(n_dmy >= n_mdy & n_dmy >= n_ymd) df$fecha_inicio <- df$.dmy
  else if(n_mdy >= n_dmy & n_mdy >= n_ymd) df$fecha_inicio <- df$.mdy
  else df$fecha_inicio <- df$.ymd
  df <- dplyr::select(df, -(.fecha_chr), -(.dmy), -(.mdy), -(.ymd))
}
if(!inherits(df$fecha_inicio, "Date")) stop("No se pudo convertir 'fecha_inicio' a Date.")

# ---------- Normalizar y seleccionar pérdidas ----------
df <- df %>%
  mutate(Factor = str_to_title(str_squish(str_trim(as.character(factor)))),
         Anio = lubridate::year(fecha_inicio),
         PerdidaBruta = perdida_bruta,
         PerdidaNeta = perdida_neta) %>%
  filter(Anio >= 2007 & Anio <= 2024)

usar_perdida_norm <- tolower(params$usar_perdida)
if(usar_perdida_norm %in% c("perdida_bruta","perdida bruta","bruta")) df <- df %>% mutate(Severidad = PerdidaBruta) else df <- df %>% mutate(Severidad = PerdidaNeta)

df <- df %>% mutate(Severidad = as.numeric(Severidad))

invalid_rows <- df %>% filter(is.na(Severidad) | Severidad < 0 | is.na(Factor))
if(nrow(invalid_rows) > 0){
  apply(invalid_rows, 1, function(r) registrar_exclusion(as.integer(r[[".row_id"]]), as.character(r[["Factor"]]), "Severidad NA/negativa o factor faltante"))
  add_warning("GLOBAL", paste0("Se excluyeron ", nrow(invalid_rows), " filas por severidad inválida o factor faltante"))
}
df <- df %>% filter(!is.na(Severidad) & Severidad >= 0 & !is.na(Factor))

duplicados <- df %>% group_by(Factor, fecha_inicio, Severidad) %>% filter(dplyr::n() > 1)
if(nrow(duplicados) > 0){
  add_warning("GLOBAL", paste0("Se detectaron ", nrow(duplicados), " posibles duplicados (Factor/Fecha/Severidad)"))
}

factores <- unique(df$Factor)
omit_log <- tibble::tibble(Factor = character(), Motivo = character(), Registros = integer())

# ---------- Función principal ----------
procesar_factor <- function(df_factor, nombre_factor){
  out <- list()
  if(nrow(df_factor) == 0) return(NULL)
  df_factor <- df_factor %>% mutate(Severidad = as.numeric(Severidad))
  
  df_plot <- data.frame(Severidad = df_factor$Severidad)
  df_plot <- df_plot[df_plot$Severidad > 0, , drop=FALSE]
  
  if(nrow(df_plot) > 0){
    p <- ggplot(df_plot, aes(x = Severidad)) +
      geom_histogram(aes(y = ..density..), bins = 50, fill = "grey80", color = "black", alpha = 0.6) +
      geom_density(color = "blue", size = 1) +
      scale_x_log10(limits = c(min(df_plot$Severidad), max(df_plot$Severidad))) +
      labs(title = paste("Densidad y Histograma de Severidad para factor", nombre_factor),
           x = "Severidad (escala logarítmica)",
           y = "Densidad") +
      theme_minimal()
    print(p)
  } else {
    cat("No hay severidades positivas para graficar\n")
  }
  
  annual <- df_factor %>% group_by(Anio) %>% summarise(AnnualLoss = sum(Severidad, na.rm=TRUE),
                                                       Count = n()) %>% ungroup()
  yrs <- 2007:2024
  annual <- full_join(tibble(Anio = yrs), annual, by = "Anio") %>% arrange(Anio)
  annual$AnnualLoss[is.na(annual$AnnualLoss)] <- 0
  annual$Count[is.na(annual$Count)] <- 0
  
  EL_obs <- mean(annual$AnnualLoss)
  sigma_obs <- sd(annual$AnnualLoss)
  
  VaR_obs <- sapply(params$VaR_levels, function(p){
    try(quantile(annual$AnnualLoss, probs = p, na.rm = TRUE), silent = TRUE)
  })
  VaR_obs <- unlist(VaR_obs)
  names(VaR_obs) <- paste0("VaR_obs_p", params$VaR_levels*100)
  UL_obs <- VaR_obs - EL_obs
  names(UL_obs) <- paste0("UL_obs_p", params$VaR_levels*100)
  
  lambda <- mean(annual$Count)
  var_count <- var(annual$Count)
  use_nb <- FALSE
  nb_fit <- NULL
  if(!is.na(lambda) && lambda > 0 && !is.na(var_count) && var_count > lambda + 1e-6){
    nb_fit_try <- try(glm.nb(Count ~ 1, data = annual), silent = TRUE)
    if(!inherits(nb_fit_try, "try-error") && AIC(nb_fit_try) < AIC(glm(Count~1,data=annual,family=poisson))) {
      use_nb <- TRUE
      nb_fit <- nb_fit_try
    }
  }
  if(use_nb && !is.null(nb_fit)){
    add_parametros(nombre_factor, "nb_size", nb_fit$theta)
    add_parametros(nombre_factor, "nb_mu", lambda)
  } else {
    add_parametros(nombre_factor, "poisson_lambda", lambda)
  }

  sev_data <- df_factor %>% filter(Severidad > 0) %>% pull(Severidad)
  n_sev <- length(sev_data)
  usar_smoothed <- (n_sev < params$umbral_muestra_pequena)

  if(n_sev < params$advertencia_muestra_pequena){
    add_warning(nombre_factor, paste0("Solo ", n_sev, " eventos positivos; se considera muestra pequeña"))
  }

  add_parametros(nombre_factor, "lambda_promedio", lambda)
  add_parametros(nombre_factor, "varianza_frecuencia", var_count)
  add_parametros(nombre_factor, "modelo_frecuencia", ifelse(use_nb, "nbinom", "poisson"))

  # Diagnósticos adicionales de frecuencia y severidad
  out$diag_freq <- generar_diagnostico_frecuencia(annual$Count, params$percentiles_seq)
  out$diag_sev <- generar_diagnostico_impacto(sev_data, params$percentiles_seq)
  
  if(n_sev < 1){
    add_warning(nombre_factor, "No hay severidades positivas; métricas simuladas en NA")
    out$resumen <- tibble(Factor = nombre_factor, EL_obs = EL_obs, sigma_obs = sigma_obs,
                          EL_sim = NA_real_, sigma_sim = NA_real_,
                          UL_obs_p95 = UL_obs[1], UL_obs_p99 = UL_obs[2],
                          UL_sim_p95 = NA_real_, UL_sim_p99 = NA_real_,
                          VaR_p95 = NA_real_, VaR_p99 = NA_real_,
                          ES_p95 = NA_real_, ES_p99 = NA_real_,
                          n_events = n_sev)
    return(out)
  }
  
  fitsev <- try(ajustar_severidad(sev_data, dist_names = params$distribuir_severidad), silent = TRUE)
  if(inherits(fitsev, "try-error")){
    add_warning(nombre_factor, "No se pudo ajustar severidad paramétrica; simulación omitida")
    out$resumen <- tibble(Factor = nombre_factor, EL_obs = EL_obs, sigma_obs = sigma_obs,
                          EL_sim = NA_real_, sigma_sim = NA_real_,
                          UL_obs_p95 = UL_obs[1], UL_obs_p99 = UL_obs[2],
                          UL_sim_p95 = NA_real_, UL_sim_p99 = NA_real_,
                          VaR_p95 = NA_real_, VaR_p99 = NA_real_,
                          ES_p95 = NA_real_, ES_p99 = NA_real_,
                          n_events = n_sev)
    return(out)
  }
  
  mejor_dist <- fitsev$mejor
  mejor_fit <- fitsev$fits[[mejor_dist]]
  cat("Mejor distribución para factor", nombre_factor, "es:", mejor_dist, "\n")
  out$sev_fit <- list(mejor = mejor_dist, fit = mejor_fit, aics = fitsev$aics)

   try({
     params_est <- as.list(mejor_fit$estimate)
     purrr::iwalk(params_est, function(val, nm) add_parametros(nombre_factor, paste0("sev_", mejor_dist, "_", nm), val))
     add_parametros(nombre_factor, "sev_mejor_aic", fitsev$aics[[mejor_dist]])
   }, silent = TRUE)
  
  thr <- quantile(sev_data, params$threshold_gpd_q, na.rm = TRUE)
  exceds <- sev_data[sev_data > thr]
  gpd_fit <- NULL
  if(length(exceds) >= 5){
    gpd_try <- try(evir::gpd(sev_data, threshold = thr), silent = TRUE)
    if(!inherits(gpd_try,"try-error")) gpd_fit <- gpd_try
  } else {
    add_warning(nombre_factor, paste0("Solo ", length(exceds), " excedencias; no se ajusta GPD"))
  }
  out$gpd <- if(!is.null(gpd_fit)) list(threshold=thr, fit=gpd_fit) else NULL
  if(!is.null(gpd_fit) && !is.null(gpd_fit$mles)){
    add_parametros(nombre_factor, "gpd_threshold", thr)
    add_parametros(nombre_factor, "gpd_xi", as.numeric(gpd_fit$mles[1]))
    add_parametros(nombre_factor, "gpd_beta", as.numeric(gpd_fit$mles[2]))
  }
  
  # ---------- SIMULACIÓN ----------
  if(usar_smoothed){
    add_warning(nombre_factor, "Usando smoothed bootstrap por muestra pequeña de severidad")
  }
  sim_annual <- numeric(params$n_sims)
  sim_freq <- integer(params$n_sims)  # NUEVO: guardar frecuencias simuladas
  
  for(i in seq_len(params$n_sims)){
    if(is.na(lambda) || lambda == 0) freq <- 0
    else if(use_nb && !is.null(nb_fit)){
      mu <- lambda; size <- nb_fit$theta; prob <- size/(size+mu)
      freq <- rnbinom(1,size=size,prob=prob)
    } else freq <- rpois(1,lambda)
    
    sim_freq[i] <- freq  # NUEVO
    
    if(freq == 0) sim_annual[i] <- 0
    else {
      if(usar_smoothed){
        draws <- smoothed_bootstrap_draw(sev_data, freq, jitter_ratio=params$jitter_ratio)
        sim_annual[i] <- sum(draws)
      } else if(!is.null(gpd_fit)){
        p_exc <- mean(sev_data > thr)
        is_exc <- runif(freq) < p_exc
        n_exc <- sum(is_exc); n_nonexc <- freq-n_exc
        draw <- if(n_nonexc>0) try(simular_severidad_param(n_nonexc, mejor_fit, mejor_dist, sev_data), silent=TRUE) else numeric(0)
        if(inherits(draw,"try-error")) draw <- sample(sev_data, n_nonexc, replace=TRUE)
        draw <- pmin(draw, thr)
        exc_draws <- if(n_exc>0){
          if(!is.null(gpd_fit$mles) && length(gpd_fit$mles)>=2){
            xi <- as.numeric(gpd_fit$mles[1]); beta <- as.numeric(gpd_fit$mles[2])
            try(rgpd_custom(n_exc, xi, beta, thr), silent=TRUE)
          } else sample(exceds, n_exc, replace=TRUE)
        } else numeric(0)
        sim_annual[i] <- sum(c(draw, exc_draws))
      } else {
        severs <- try(simular_severidad_param(freq, mejor_fit, mejor_dist, sev_data), silent=TRUE)
        if(inherits(severs,"try-error")) severs <- sample(sev_data, freq, replace=TRUE)
        sim_annual[i] <- sum(as.numeric(severs))
      }
    }
  }
  
  EL_sim <- mean(sim_annual)
  sigma_sim <- sd(sim_annual)
  
  VaRs <- try(quantile(sim_annual, probs = params$VaR_levels, na.rm = TRUE), silent = TRUE)
  if(inherits(VaRs,"try-error")) VaRs <- rep(NA_real_,length(params$VaR_levels))
  
  UL_sim <- VaRs - EL_sim
  names(UL_sim) <- paste0("UL_sim_p", params$VaR_levels*100)

  if(any(!is.na(VaRs) & VaRs < EL_sim)){
    add_warning(nombre_factor, "VaR simulado menor que EL: revisar ajuste/cola")
  }
  
  ESs <- sapply(params$VaR_levels, function(p){
    q <- try(quantile(sim_annual, probs=p, na.rm=TRUE), silent=TRUE)
    if(inherits(q,"try-error") || length(sim_annual[sim_annual>=q])==0) return(NA_real_)
    mean(sim_annual[sim_annual>=q], na.rm=TRUE)
  })
  
  # ---------- NUEVO: CÁLCULO DE PERCENTILES ----------
  percentiles_valores <- try(quantile(sim_annual, probs = params$percentiles_seq, na.rm = TRUE), silent = TRUE)
  if(inherits(percentiles_valores, "try-error")){
    percentiles_valores <- rep(NA_real_, length(params$percentiles_seq))
  }
  
  percentiles_freq <- try(quantile(sim_freq, probs = params$percentiles_seq, na.rm = TRUE), silent = TRUE)
  if(inherits(percentiles_freq, "try-error")){
    percentiles_freq <- rep(NA_real_, length(params$percentiles_seq))
  }
  
  out$sim <- list(sim_annual=sim_annual,
                  sim_freq=sim_freq,              # NUEVO
                  EL_sim=EL_sim,
                  sigma_sim=sigma_sim, 
                  VaRs=VaRs,
                  ESs=ESs,
                  usar_smoothed=usar_smoothed,
                  n_sev=n_sev,
                  percentiles=percentiles_valores,
                  percentiles_freq=percentiles_freq)   # NUEVO
  
  out$resumen <- tibble(Factor = nombre_factor,
                        Mejor_Distribucion = mejor_dist,
                        EL_obs = EL_obs, sigma_obs = sigma_obs,
                        EL_sim = EL_sim, sigma_sim = sigma_sim,
                        UL_obs_p95 = UL_obs[1], UL_obs_p99 = UL_obs[2],
                        UL_sim_p95 = UL_sim[1], UL_sim_p99 = UL_sim[2],
                        VaR_p95 = as.numeric(VaRs[1]), VaR_p99 = as.numeric(VaRs[2]),
                        ES_p95 = ESs[1], ES_p99 = ESs[2],
                        n_events = n_sev,
                        usar_smoothed = usar_smoothed)
  
  return(out)
}

# ---------- Procesar factores ----------
all_results <- list()
for(f in factores){
  cat("\n--- Procesando factor:", f, "---\n")
  df_f <- df %>% filter(Factor==f)
  res_f <- try(procesar_factor(df_f, f), silent=TRUE)
  if(inherits(res_f,"try-error") || is.null(res_f)){
    omit_log <- bind_rows(omit_log, tibble(Factor=f, Motivo="error procesar_factor", Registros=nrow(df_f)))
    next
  }
  all_results[[f]] <- res_f
}

# ---------- Procesar GLOBAL ----------
res_global <- try(procesar_factor(df, "__GLOBAL__"), silent=TRUE)
if(!inherits(res_global,"try-error") && !is.null(res_global)) all_results[["__GLOBAL__"]] <- res_global else omit_log <- bind_rows(omit_log, tibble(Factor="__GLOBAL__", Motivo="error global", Registros=nrow(df)))

# ---------- NUEVO: CONSTRUIR MATRIZ DE PERCENTILES ----------
cat("\n--- Construyendo matriz de percentiles ---\n")

percentiles_labels <- paste0(params$percentiles_seq * 100, "%")
matriz_percentiles <- tibble(Percentil = percentiles_labels)

# Estadísticas para montos
stats_media <- c()
stats_min <- c()
stats_max <- c()
stats_std <- c()

# Estadísticas para frecuencias (NUEVO)
stats_media_freq <- c()
stats_min_freq <- c()
stats_max_freq <- c()
stats_std_freq <- c()

# Agregar columnas por factor para montos y frecuencias
for(nombre_factor in names(all_results)){
  res <- all_results[[nombre_factor]]
  
  if(!is.null(res$sim) && !is.null(res$sim$percentiles)){
    # Montos
    matriz_percentiles[[paste0(nombre_factor, "_Monto")]] <- as.numeric(res$sim$percentiles)
    # Frecuencias simuladas
    matriz_percentiles[[paste0(nombre_factor, "_Frecuencia")]] <- as.numeric(res$sim$percentiles_freq)
    
    # Estadísticas montos
    stats_media <- c(stats_media, mean(res$sim$sim_annual, na.rm = TRUE))
    stats_min <- c(stats_min, min(res$sim$sim_annual, na.rm = TRUE))
    stats_max <- c(stats_max, max(res$sim$sim_annual, na.rm = TRUE))
    stats_std <- c(stats_std, sd(res$sim$sim_annual, na.rm = TRUE))
    
    # Estadísticas frecuencias
    stats_media_freq <- c(stats_media_freq, mean(res$sim$sim_freq, na.rm = TRUE))
    stats_min_freq <- c(stats_min_freq, min(res$sim$sim_freq, na.rm = TRUE))
    stats_max_freq <- c(stats_max_freq, max(res$sim$sim_freq, na.rm = TRUE))
    stats_std_freq <- c(stats_std_freq, sd(res$sim$sim_freq, na.rm = TRUE))
  }
}

# Crear data frame de estadísticas
stats_df <- tibble(
  Estadistica = c("Media", "Min", "Max", "Std Dev"),
  stringsAsFactors = FALSE
)

# Agregar estadísticas de montos por factor
for(i in seq_along(names(all_results))){
  col_name <- names(all_results)[i]
  stats_df[[paste0(col_name, "_Monto")]] <- c(stats_media[i], stats_min[i], stats_max[i], stats_std[i])
  stats_df[[paste0(col_name, "_Frecuencia")]] <- c(stats_media_freq[i], stats_min_freq[i], stats_max_freq[i], stats_std_freq[i])
}

# ---------- Diagnósticos agregados ----------
diag_freq_df <- tibble()
diag_sev_df <- tibble()
freq_ajuste_percentiles <- tibble(Percentil = percentiles_labels)
sev_ajuste_percentiles <- tibble(Percentil = percentiles_labels)

for(nombre_factor in names(all_results)){
  res <- all_results[[nombre_factor]]
  if(!is.null(res$diag_freq$tabla)){
    diag_freq_df <- bind_rows(diag_freq_df, res$diag_freq$tabla %>% mutate(Factor = nombre_factor, Mejor = res$diag_freq$mejor))
  }
  if(!is.null(res$diag_freq$percentiles)){
    freq_ajuste_percentiles[[nombre_factor]] <- as.numeric(res$diag_freq$percentiles)
  }
  if(!is.null(res$diag_sev$tabla)){
    diag_sev_df <- bind_rows(diag_sev_df, res$diag_sev$tabla %>% mutate(Factor = nombre_factor, Mejor = res$diag_sev$mejor))
  }
  if(!is.null(res$diag_sev$percentiles)){
    sev_ajuste_percentiles[[nombre_factor]] <- as.numeric(res$diag_sev$percentiles)
  }
}

# ---------- Guardar resultados ----------
res_list <- lapply(all_results, function(x) if(!is.null(x$resumen)) x$resumen else NULL)
res_list <- res_list[!vapply(res_list, is.null, logical(1))]
if(length(res_list)==0) stop("No hay resultados válidos para exportar.")
resumen_df <- bind_rows(res_list)

wb <- createWorkbook()
addWorksheet(wb, "Resumen")
addWorksheet(wb, "Percentiles")
addWorksheet(wb, "Estadisticas")
addWorksheet(wb, "Omitidos")
addWorksheet(wb, "Diag_Frecuencia")
addWorksheet(wb, "Diag_Impacto")
addWorksheet(wb, "Percentiles_Frecuencia_Ajuste")
addWorksheet(wb, "Percentiles_Impacto_Ajuste")
addWorksheet(wb, "Warnings")
addWorksheet(wb, "Exclusiones")
addWorksheet(wb, "Setup")
addWorksheet(wb, "Parametros_Modelo")

writeData(wb, "Resumen", resumen_df)
writeData(wb, "Percentiles", matriz_percentiles)
writeData(wb, "Estadisticas", stats_df)
writeData(wb, "Omitidos", omit_log)
if(nrow(diag_freq_df) > 0) writeData(wb, "Diag_Frecuencia", diag_freq_df)
if(nrow(diag_sev_df) > 0) writeData(wb, "Diag_Impacto", diag_sev_df)
if(ncol(freq_ajuste_percentiles) > 1) writeData(wb, "Percentiles_Frecuencia_Ajuste", freq_ajuste_percentiles)
if(ncol(sev_ajuste_percentiles) > 1) writeData(wb, "Percentiles_Impacto_Ajuste", sev_ajuste_percentiles)
if(nrow(warnings_log) > 0) writeData(wb, "Warnings", warnings_log)
if(nrow(exclusion_log) > 0) writeData(wb, "Exclusiones", exclusion_log)

meta_info <- tibble(
  Clave = c("ruta_archivo", "hoja", "usar_perdida", "semilla", "n_sims", "fecha_ejecucion", "git_commit"),
  Valor = c(as.character(input_path), as.character(params$hoja), params$usar_perdida, params$semilla, params$n_sims, as.character(Sys.time()),
            tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "no_git"))
)
writeData(wb, "Setup", meta_info)

if(nrow(parametros_modelo) > 0){
  writeData(wb, "Parametros_Modelo", parametros_modelo)
}

headerStyle <- createStyle(fontSize = 11, fontColour = "#FFFFFF",
                           halign = "center", fgFill = "#4F81BD",
                           border = "TopBottomLeftRight", borderColour = "#000000",
                           textDecoration = "bold")
addStyle(wb, "Percentiles", headerStyle, rows = 1, cols = 1:ncol(matriz_percentiles), gridExpand = TRUE)
if(ncol(freq_ajuste_percentiles) > 1) addStyle(wb, "Percentiles_Frecuencia_Ajuste", headerStyle, rows = 1, cols = 1:ncol(freq_ajuste_percentiles), gridExpand = TRUE)
if(ncol(sev_ajuste_percentiles) > 1) addStyle(wb, "Percentiles_Impacto_Ajuste", headerStyle, rows = 1, cols = 1:ncol(sev_ajuste_percentiles), gridExpand = TRUE)
if(nrow(warnings_log) > 0) addStyle(wb, "Warnings", headerStyle, rows = 1, cols = 1:ncol(warnings_log), gridExpand = TRUE)
if(nrow(exclusion_log) > 0) addStyle(wb, "Exclusiones", headerStyle, rows = 1, cols = 1:ncol(exclusion_log), gridExpand = TRUE)
if(nrow(parametros_modelo) > 0) addStyle(wb, "Parametros_Modelo", headerStyle, rows = 1, cols = 1:ncol(parametros_modelo), gridExpand = TRUE)

percentiles_destacados <- c(51, 91, 96, 100)  # filas (50%, 90%, 95%, 99%)
highlightStyle <- createStyle(fgFill = "#FFF2CC", border = "TopBottomLeftRight")
for(row in percentiles_destacados){
  addStyle(wb, "Percentiles", highlightStyle, rows = row + 1, cols = 1:ncol(matriz_percentiles), gridExpand = TRUE)
  if(ncol(freq_ajuste_percentiles) > 1) addStyle(wb, "Percentiles_Frecuencia_Ajuste", highlightStyle, rows = row + 1, cols = 1:ncol(freq_ajuste_percentiles), gridExpand = TRUE)
  if(ncol(sev_ajuste_percentiles) > 1) addStyle(wb, "Percentiles_Impacto_Ajuste", highlightStyle, rows = row + 1, cols = 1:ncol(sev_ajuste_percentiles), gridExpand = TRUE)
}

saveWorkbook(wb, params$ruta_salida, overwrite = TRUE)
cat("\nArchivo guardado en:", params$ruta_salida, "\n")
cat("Hojas incluidas: Resumen, Percentiles, Estadisticas, Omitidos, Diag_Frecuencia, Diag_Impacto, Percentiles_Frecuencia_Ajuste, Percentiles_Impacto_Ajuste, Warnings, Exclusiones, Setup, Parametros_Modelo\n")
