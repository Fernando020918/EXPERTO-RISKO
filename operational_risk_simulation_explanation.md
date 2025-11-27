# Explicación de adiciones recientes

A continuación se describe, línea por línea, lo nuevo incorporado al script `operational_risk_simulation.R` respecto al ajuste y diagnóstico adicional que replica las pruebas del guion exploratorio.

## Paquetes adicionales
- **26-35**: Se agregan los paquetes `survival`, `actuar`, `rriskDistributions` y `purrr`, y se cargan sus librerías para permitir pruebas de ajuste ampliadas en frecuencia y severidad y facilitar el volcado estructurado de parámetros.

## Logging, trazabilidad y controles de calidad
- **37-54**: Se introducen utilidades globales (`warnings_log`, `parametros_modelo`, `exclusion_log`) y helpers (`add_warning`, `add_parametros`, `registrar_exclusion`) para centralizar advertencias, parámetros estimados y exclusiones de datos.
- **90-121**: El cargado de datos admite rutas relativas, agrega identificadores de fila y registra exclusiones por severidad negativa/NA o factor faltante; también señala posibles duplicados.

## Bloque de diagnósticos adicionales
- **124-172**: Se introducen utilidades para extraer métricas de bondad de ajuste (`extraer_metricas_gof`) y generar diagnósticos de frecuencia (`generar_diagnostico_frecuencia`). Este bloque prueba Poisson y binomial negativa, calcula AIC/BIC, p-valores (Chi-cuadrado, KS) y estadísticos AD/CVM, identifica la mejor distribución y produce percentiles bootstrap con `bootdist`.
- **175-217**: Se replica el esquema para severidad en `generar_diagnostico_impacto`, probando lognormal, log-logística, Pareto y Weibull, con los mismos indicadores y percentiles bootstrap.

## Integración en el procesamiento por factor
- **315-318**: En `procesar_factor`, se ejecutan los diagnósticos recién creados para cada factor y se guardan en `out$diag_freq` y `out$diag_sev`.
- **321-333**: Se registran parámetros de frecuencia (Poisson/NB) y advertencias cuando el número de eventos es bajo.
- **345-370**: Se guardan parámetros de severidad, AIC del mejor ajuste y parámetros GPD cuando aplica; si no hay excedencias suficientes, se registra advertencia.
- **411-431**: Se siguen calculando percentiles 0-100% de las pérdidas simuladas y ahora también de las frecuencias simuladas (`percentiles_freq`), que se almacenan junto con las métricas simuladas y se añaden advertencias si el VaR queda por debajo del EL.

## Matriz de percentiles y estadísticas extendidas
- **464-517**: La matriz de percentiles ahora agrega, para cada factor, columnas de montos y de frecuencias simuladas, y calcula estadísticas (media, mínimo, máximo, desviación estándar) para ambos.

## Agregación de diagnósticos y exportación a Excel
- **520-539**: Se consolidan las tablas de diagnóstico de frecuencia y severidad de todos los factores y los percentiles bootstrap de ajuste.
- **540-556**: Se añaden hojas nuevas `Warnings`, `Exclusiones`, `Setup` y `Parametros_Modelo`. `Setup` captura ruta, hoja, semilla, número de simulaciones, timestamp y hash de commit; `Parametros_Modelo` almacena parámetros estimados por factor.
- **547-565**: El libro de Excel sigue incluyendo las hojas de diagnóstico (`Diag_Frecuencia`, `Diag_Impacto`) y percentiles de ajuste (`Percentiles_Frecuencia_Ajuste`, `Percentiles_Impacto_Ajuste`).
- **566-580**: Se aplican estilos de encabezado y resaltado a las nuevas hojas, igual que a la hoja principal de percentiles.
- **582-584**: El mensaje final lista las nuevas hojas añadidas en el archivo de salida.
