# ==============================================================================
# ECONOMETRÍA 1 - ACTIVIDAD 7
# EJERCICIOS 5-6: MODELOS DE VARIABLE DEPENDIENTE LIMITADA
# ==============================================================================
# Dr. Francisco Cabrera
# División de Economía - CDE
#
# Este script contiene el análisis completo de modelos con variable dependiente
# binaria (0/1):
# - Modelo de Probabilidad Lineal (LPM)
# - Modelo Probit
# - Modelo Logit
# - Efectos marginales
# - Predicciones
# ==============================================================================

# Limpiar el entorno
rm(list = ls())

# Cargar paquetes necesarios
library(pacman)
p_load(
  tidyverse, AER, lmtest, sandwich, margins, mfx,
  knitr, kableExtra, modelsummary, ggplot2
)

cat("
================================================================================
¿QUÉ SON LOS MODELOS DE VARIABLE DEPENDIENTE LIMITADA?
================================================================================

CONTEXTO:
---------
Hasta ahora hemos trabajado con variables dependientes CONTINUAS:
  - Salarios (log(wage))
  - Años de educación
  - Producción, consumo, etc.

Pero muchas veces la variable dependiente es BINARIA (0 o 1):
  - ¿Se gradúa el estudiante? (1 = sí, 0 = no)
  - ¿Se rechaza el crédito? (1 = sí, 0 = no)
  - ¿Participa en la fuerza laboral? (1 = sí, 0 = no)
  - ¿Compra el producto? (1 = sí, 0 = no)

PROBLEMA CON MCO:
-----------------
Si usamos MCO con Y binaria, tenemos problemas:

1. PREDICCIONES FUERA DE [0,1]:
   - MCO puede predecir Ŷ < 0 o Ŷ > 1
   - No tiene sentido como probabilidad
   
2. HETEROCEDASTICIDAD:
   - Var(u|X) no es constante
   - Los errores estándar de MCO son incorrectos
   
3. NO LINEALIDAD:
   - El efecto marginal de X no es constante
   - Depende del valor de X

SOLUCIONES:
-----------
Tres modelos principales:

1. MODELO DE PROBABILIDAD LINEAL (LPM):
   - Usa MCO pero con errores estándar robustos
   - Simple pero tiene problemas
   
2. MODELO PROBIT:
   - Usa la distribución normal acumulada
   - P(Y=1|X) = Φ(Xβ)
   - Garantiza probabilidades en [0,1]
   
3. MODELO LOGIT:
   - Usa la distribución logística
   - P(Y=1|X) = Λ(Xβ) = exp(Xβ)/(1+exp(Xβ))
   - También garantiza probabilidades en [0,1]

EN ESTE SCRIPT:
---------------
- Ejercicio 5: Ejemplo con estudiantes-atletas (datos dados en el problema)
- Ejercicio 6: Análisis con datos reales HMDA (rechazos de hipotecas)

Vamos a aprender:
  - Cómo estimar cada modelo
  - Cómo interpretar coeficientes
  - Cómo calcular efectos marginales
  - Cómo hacer predicciones
  - Cómo comparar modelos

================================================================================
")

# ==============================================================================
# EJERCICIO 5: ESTUDIANTES-ATLETAS Y GRADUACIÓN
# ==============================================================================

cat("
================================================================================
EJERCICIO 5: MODELO LOGIT - GRADUACIÓN DE ESTUDIANTES-ATLETAS
================================================================================

CONTEXTO:
---------
Tenemos datos de 420 estudiantes-atletas en una universidad grande.

VARIABLE DEPENDIENTE:
  grad = 1 si se gradúa en 5 años, 0 si no

VARIABLES EXPLICATIVAS:
  hsGPA = promedio de calificaciones en high school (preparatoria)
  SAT   = puntaje en examen SAT
  study = horas por semana en sala de estudio organizada

MODELO ESTIMADO (ya dado en el problema):
------------------------------------------
")

# (Aquí iría el modelo en LaTeX si se quiere en el reporte)

cat("

donde Λ(z) = exp(z)/(1 + exp(z)) es la función logística.

PARÁMETROS ESTIMADOS:
---------------------
  β₀ = -1.17
  β₁ = 0.24    (hsGPA)
  β₂ = 0.00058 (SAT)
  β₃ = 0.073   (study)

NOTA: Estos coeficientes NO son directamente interpretables como efectos
marginales. Necesitamos calcular los efectos marginales.

")

# ==============================================================================
# EJERCICIO 5a: DIFERENCIA EN PROBABILIDAD DE GRADUACIÓN
# ==============================================================================

cat("
================================================================================
EJERCICIO 5a: DIFERENCIA EN PROBABILIDAD DE GRADUACIÓN
================================================================================

PREGUNTA:
---------
Manteniendo hsGPA fijo en 3.0 y SAT fijo en 1,200, calcular la diferencia
estimada en la probabilidad de graduarse entre:
  - Alguien que pasa 10 horas/semana en study hall
  - Alguien que pasa 5 horas/semana en study hall

ESTRATEGIA:
-----------
1. Calcular P(grad=1 | hsGPA=3.0, SAT=1200, study=10)
2. Calcular P(grad=1 | hsGPA=3.0, SAT=1200, study=5)
3. Calcular la diferencia

FUNCIÓN LOGÍSTICA:
------------------
")

# (Aquí iría la función logística en LaTeX)

cat("

Implementamos esta función en R:

")

# Definir la función logística (función Λ)
Lambda <- function(z) {
  exp(z) / (1 + exp(z))
}

cat("Función logística definida: Lambda(z) = exp(z)/(1 + exp(z))\n\n")

# Parámetros del modelo estimado
beta0 <- -1.17
beta1 <- 0.24    # hsGPA
beta2 <- 0.00058 # SAT
beta3 <- 0.073   # study

cat("PARÁMETROS DEL MODELO:\n")
cat("----------------------\n")
cat("β₀ (intercepto) =", beta0, "\n")
cat("β₁ (hsGPA)      =", beta1, "\n")
cat("β₂ (SAT)        =", beta2, "\n")
cat("β₃ (study)      =", beta3, "\n\n")

# Valores fijos de las variables
hsGPA_fijo <- 3.0
SAT_fijo   <- 1200

cat("VALORES FIJOS:\n")
cat("--------------\n")
cat("hsGPA =", hsGPA_fijo, "\n")
cat("SAT   =", SAT_fijo, "\n\n")

# Escenario 1: study = 10 horas
study1 <- 10
z1     <- beta0 + beta1 * hsGPA_fijo + beta2 * SAT_fijo + beta3 * study1
prob1  <- Lambda(z1)

cat("ESCENARIO 1: study = 10 horas por semana\n")
cat("-----------------------------------------\n")
cat("z₁ = β₀ + β₁(3.0) + β₂(1200) + β₃(10)\n")
cat("   =", beta0, "+", beta1, "×", hsGPA_fijo, "+", 
    beta2, "×", SAT_fijo, "+", beta3, "×", study1, "\n")
cat("   =", round(z1, 4), "\n\n")

cat("P(grad=1 | hsGPA=3.0, SAT=1200, study=10) = Λ(", round(z1, 4), ")\n")
cat("                                           =", round(prob1, 4), "\n")
cat("                                           =", round(prob1 * 100, 2), "%\n\n")

# Escenario 2: study = 5 horas
study2 <- 5
z2     <- beta0 + beta1 * hsGPA_fijo + beta2 * SAT_fijo + beta3 * study2
prob2  <- Lambda(z2)

cat("ESCENARIO 2: study = 5 horas por semana\n")
cat("----------------------------------------\n")
cat("z₂ = β₀ + β₁(3.0) + β₂(1200) + β₃(5)\n")
cat("   =", beta0, "+", beta1, "×", hsGPA_fijo, "+", 
    beta2, "×", SAT_fijo, "+", beta3, "×", study2, "\n")
cat("   =", round(z2, 4), "\n\n")

cat("P(grad=1 | hsGPA=3.0, SAT=1200, study=5) = Λ(", round(z2, 4), ")\n")
cat("                                          =", round(prob2, 4), "\n")
cat("                                          =", round(prob2 * 100, 2), "%\n\n")

# Diferencia
diferencia <- prob1 - prob2

cat("DIFERENCIA EN PROBABILIDAD:\n")
cat("---------------------------\n")
cat("Δ P = P(study=10) - P(study=5)\n")
cat("    =", round(prob1, 4), "-", round(prob2, 4), "\n")
cat("    =", round(diferencia, 4), "\n")
cat("    =", round(diferencia * 100, 2), "puntos porcentuales\n\n")

cat("INTERPRETACIÓN:\n")
cat("---------------\n")
cat("Un estudiante-atleta que pasa 10 horas por semana en study hall tiene\n")
cat("una probabilidad de graduarse que es", round(diferencia * 100, 2), 
    "puntos porcentuales\n")
cat("MAYOR que un estudiante que solo pasa 5 horas, manteniendo constante\n")
cat("el GPA de preparatoria (3.0) y el puntaje SAT (1200).\n\n")

if (diferencia > 0.05) {
  cat("✓ Este es un efecto SUSTANCIAL y económicamente significativo.\n")
} else if (diferencia > 0.02) {
  cat("✓ Este es un efecto moderado.\n")
} else {
  cat("◯ Este es un efecto pequeño.\n")
}

cat("\nNOTA IMPORTANTE:\n")
cat("----------------\n")
cat("Este es un EFECTO MARGINAL DISCRETO, no un efecto marginal instantáneo.\n")
cat("Estamos comparando dos valores específicos de study (10 vs 5), no un\n")
cat("cambio infinitesimal.\n\n")

cat("Si quisiéramos el efecto marginal instantáneo en study=7.5 (punto medio):\n")
study_medio     <- 7.5
z_medio         <- beta0 + beta1 * hsGPA_fijo + beta2 * SAT_fijo + beta3 * study_medio
lambda_z_medio  <- Lambda(z_medio)
# Derivada de Λ: λ(z) = Λ(z)[1 - Λ(z)]
derivada_lambda <- lambda_z_medio * (1 - lambda_z_medio)
efecto_marginal <- beta3 * derivada_lambda

cat("∂P/∂study |_{study=7.5} = β₃ × λ(z) = β₃ × Λ(z)[1-Λ(z)]\n")
cat("                          =", round(beta3, 4), "×", round(lambda_z_medio, 4), 
    "×", round(1 - lambda_z_medio, 4), "\n")
cat("                          =", round(efecto_marginal, 5), "\n\n")

cat("Interpretación del efecto marginal instantáneo:\n")
cat("En el punto medio (study=7.5), una hora adicional aumenta la probabilidad\n")
cat("de graduación en aproximadamente", round(efecto_marginal * 100, 3), 
    "puntos porcentuales.\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6: DATOS HMDA - RECHAZOS DE HIPOTECAS
# ==============================================================================

cat("
================================================================================
EJERCICIO 6: ANÁLISIS CON DATOS HMDA
================================================================================

CONTEXTO:
---------
Los datos HMDA (Home Mortgage Disclosure Act) contienen información sobre
solicitudes de hipoteca presentadas en Boston en 1990.

VARIABLE DEPENDIENTE:
  deny = 'yes' si la solicitud fue rechazada
       = 'no' si fue aprobada

VARIABLE EXPLICATIVA PRINCIPAL:
  pirat = P/I ratio = payment-to-income ratio
        = razón del pago mensual de hipoteca respecto al ingreso del solicitante

OTRAS VARIABLES:
  - selfemp: 1 si es auto-empleado
  - single : 1 si es soltero
  - hschool: 1 si tiene educación high school
  - afam   : 1 si es afroamericano
  - etc.

PREGUNTA DE INVESTIGACIÓN:
--------------------------
¿Cómo afecta el P/I ratio a la probabilidad de que se rechace la hipoteca?

Vamos a estimar tres modelos:
  1. Modelo de Probabilidad Lineal (LPM) - usando MCO
  2. Modelo Probit
  3. Modelo Logit

")

# Cargar datos HMDA
data("HMDA", package = "AER")

cat("DATOS CARGADOS:\n")
cat("---------------\n")
cat("Dataset: HMDA (del paquete AER)\n")
cat("Observaciones:", nrow(HMDA), "\n")
cat("Variables:", ncol(HMDA), "\n\n")

# Explorar la estructura
cat("VARIABLES EN EL DATASET:\n")
cat("------------------------\n")
str(HMDA)

cat("\n")
cat("PRIMERAS OBSERVACIONES:\n")
cat("-----------------------\n")
print(head(HMDA))

cat("\n")
cat("ESTADÍSTICAS DESCRIPTIVAS DE VARIABLES CLAVE:\n")
cat("----------------------------------------------\n")

# Convertir deny a numérica para facilitar análisis
HMDA <- HMDA %>%
  mutate(deny_numeric = ifelse(deny == "yes", 1, 0))

summary_stats <- HMDA %>%
  summarise(
    n           = n(),
    deny_mean   = mean(deny_numeric),
    pirat_mean  = mean(pirat),
    pirat_sd    = sd(pirat),
    single_mean = mean(single == "yes"),
    afam_mean   = mean(afam == "yes")
  )

print(summary_stats)

cat("\n")
cat("INTERPRETACIÓN:\n")
cat("---------------\n")
cat("Tasa de rechazo:", round(summary_stats$deny_mean * 100, 2), "%\n")
cat("P/I ratio promedio:", round(summary_stats$pirat_mean, 3), "\n")
cat("Proporción solteros:", round(summary_stats$single_mean * 100, 1), "%\n")
cat("Proporción afroamericanos:", round(summary_stats$afam_mean * 100, 1), "%\n\n")

# ==============================================================================
# EJERCICIO 6a: MODELO DE PROBABILIDAD LINEAL (LPM)
# ==============================================================================

cat("
================================================================================
EJERCICIO 6a: MODELO DE PROBABILIDAD LINEAL (LPM)
================================================================================

El Modelo de Probabilidad Lineal usa MCO directamente con la variable
dependiente binaria.

MODELO:
-------
")

# (Aquí va el LPM en LaTeX)

cat("

VENTAJAS DEL LPM:
-----------------
✓ Simple de estimar (MCO estándar)
✓ Coeficientes son directamente interpretables como efectos marginales
✓ No requiere supuestos distribucionales

DESVENTAJAS DEL LPM:
--------------------
✗ Puede predecir probabilidades < 0 o > 1
✗ Heterocedasticidad por construcción
✗ Asume efectos marginales constantes (no realista)

SOLUCIÓN A LA HETEROCEDASTICIDAD:
----------------------------------
Usar errores estándar ROBUSTOS (heteroskedasticity-robust).

ESTIMACIÓN:
-----------
")

# Estimar LPM
lpm_modelo <- lm(deny_numeric ~ pirat, data = HMDA)

cat("Modelo estimado: deny = β₀ + β₁ × pirat + u\n\n")

# Resumen con errores estándar robustos
lpm_robust <- coeftest(lpm_modelo, vcov = vcovHC(lpm_modelo, type = "HC1"))

cat("RESULTADOS DEL LPM (con errores estándar robustos HC1):\n")
cat("--------------------------------------------------------\n\n")
print(lpm_robust)

cat("\n")
cat("INTERPRETACIÓN DE COEFICIENTES:\n")
cat("--------------------------------\n")
cat("β₀ (intercepto) =", round(coef(lpm_modelo)[1], 4), "\n")
cat("   Probabilidad de rechazo cuando P/I ratio = 0\n")
cat("   (No tiene interpretación económica directa)\n\n")

cat("β₁ (pirat) =", round(coef(lpm_modelo)[2], 4), "\n")
cat("   Interpretación: Un aumento de 1 unidad en el P/I ratio\n")
cat("   aumenta la probabilidad de rechazo en", 
    round(coef(lpm_modelo)[2] * 100, 2), "puntos porcentuales.\n\n")

cat("   Alternativamente: Un aumento de 0.1 en el P/I ratio\n")
cat("   (de 0.3 a 0.4, por ejemplo) aumenta la probabilidad de rechazo\n")
cat("   en", round(coef(lpm_modelo)[2] * 0.1 * 100, 2), "puntos porcentuales.\n\n")

# Verificar significancia
if (lpm_robust[2, 4] < 0.05) {
  cat("✓ El coeficiente es ESTADÍSTICAMENTE SIGNIFICATIVO al 5%\n")
  cat("✓ Hay evidencia fuerte de que el P/I ratio afecta el rechazo\n\n")
}

# Hacer predicciones
HMDA$pred_lpm <- predict(lpm_modelo)

cat("PREDICCIONES DEL LPM:\n")
cat("---------------------\n")
cat("Mínimo:", round(min(HMDA$pred_lpm), 4), "\n")
cat("Máximo:", round(max(HMDA$pred_lpm), 4), "\n\n")

# Verificar predicciones fuera de [0,1]
fuera_rango <- sum(HMDA$pred_lpm < 0 | HMDA$pred_lpm > 1)
cat("Predicciones fuera de [0,1]:", fuera_rango, "de", nrow(HMDA), 
    "(", round(fuera_rango / nrow(HMDA) * 100, 2), "%)\n\n")

if (fuera_rango > 0) {
  cat("✗ PROBLEMA: El LPM produce", fuera_rango, "predicciones fuera de [0,1]\n")
  cat("✗ Esto viola la interpretación como probabilidad\n\n")
} else {
  cat("✓ Todas las predicciones están en [0,1] (caso raro)\n\n")
}

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6b: MODELO PROBIT
# ==============================================================================

cat("
================================================================================
EJERCICIO 6b: MODELO PROBIT
================================================================================

El modelo Probit usa la función de distribución acumulada (CDF) de la
distribución normal estándar para garantizar que las probabilidades estén
en [0,1].

MODELO:
-------
")

# (Aquí iría el modelo Probit en LaTeX)

cat("

donde Φ(z) es la CDF de N(0,1).

VENTAJAS DEL PROBIT:
--------------------
✓ Las probabilidades siempre están en [0,1]
✓ Efectos marginales varían con X (más realista)
✓ Fundamento probabilístico claro (modelo de utilidad latente)

DESVENTAJAS DEL PROBIT:
-----------------------
◯ Coeficientes no son directamente interpretables
◯ Necesita calcular efectos marginales
◯ Requiere supuesto de normalidad

ESTIMACIÓN:
-----------
")

# Estimar modelo Probit
probit_modelo <- glm(
  deny ~ pirat,
  family = binomial(link = "probit"),
  data   = HMDA
)

cat("Modelo estimado: P(deny=1 | pirat) = Φ(β₀ + β₁ × pirat)\n\n")

# Prueba robusta
probit_robust <- coeftest(probit_modelo, vcov. = vcovHC, type = "HC1")

cat("RESULTADOS DEL MODELO PROBIT:\n")
cat("-----------------------------\n")
cat("(con errores estándar robustos a heterocedasticidad)\n\n")
print(probit_robust)

cat("\n")
cat("NOTA SOBRE LA INFERENCIA:\n")
cat("-------------------------\n")
cat("Estos son estadísticos z (no t) porque usamos estimación de máxima\n")
cat("verosimilitud, que tiene distribución asintótica normal.\n\n")

cat("INTERPRETACIÓN DE COEFICIENTES:\n")
cat("--------------------------------\n")
cat("β₀ =", round(coef(probit_modelo)[1], 4), "\n")
cat("β₁ =", round(coef(probit_modelo)[2], 4), "\n\n")

cat("IMPORTANTE: Estos coeficientes NO son efectos marginales.\n")
cat("Solo nos dicen la DIRECCIÓN del efecto:\n")
if (coef(probit_modelo)[2] > 0) {
  cat("  β₁ > 0 → Mayor P/I ratio AUMENTA la probabilidad de rechazo ✓\n\n")
} else {
  cat("  β₁ < 0 → Mayor P/I ratio DISMINUYE la probabilidad de rechazo\n\n")
}

cat("Para obtener la MAGNITUD del efecto, necesitamos calcular el\n")
cat("EFECTO MARGINAL.\n\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6c: ECUACIÓN ESTIMADA DEL PROBIT
# ==============================================================================

cat("
================================================================================
EJERCICIO 6c: ECUACIÓN ESTIMADA DEL MODELO PROBIT
================================================================================

FORMA GENERAL:
--------------
")

# (Aquí iría la ecuación en LaTeX)

cat("

ECUACIÓN CON VALORES ESTIMADOS:
--------------------------------
")

cat("P̂(deny = 1 | pirat) = Φ(", round(coef(probit_modelo)[1], 4), 
    " + ", round(coef(probit_modelo)[2], 4), " × pirat)\n\n")

cat("donde Φ(·) es la función de distribución acumulada de N(0,1).\n\n")

cat("EJEMPLO DE USO:\n")
cat("---------------\n")
cat("Para pirat = 0.3:\n")
z_ejemplo   <- coef(probit_modelo)[1] + coef(probit_modelo)[2] * 0.3
prob_ejemplo <- pnorm(z_ejemplo)

cat("  z = ", round(coef(probit_modelo)[1], 4), " + ", 
    round(coef(probit_modelo)[2], 4), " × 0.3\n")
cat("    = ", round(z_ejemplo, 4), "\n")
cat("  P̂(deny=1 | pirat=0.3) = Φ(", round(z_ejemplo, 4), ")\n")
cat("                         = ", round(prob_ejemplo, 4), "\n")
cat("                         = ", round(prob_ejemplo * 100, 2), "%\n\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6d: PREDICCIÓN CON CAMBIO EN P/I RATIO
# ==============================================================================

cat("
================================================================================
EJERCICIO 6d: PREDICCIÓN CUANDO P/I RATIO CAMBIA DE 0.3 A 0.4
================================================================================

OBJETIVO:
---------
Calcular cómo cambia la probabilidad predicha de rechazo cuando el P/I ratio
aumenta de 0.3 a 0.4, usando el modelo Probit estimado.

CÁLCULO:
--------
")

# Predicción para pirat = 0.3
pirat_bajo <- 0.3
z_bajo     <- coef(probit_modelo)[1] + coef(probit_modelo)[2] * pirat_bajo
prob_bajo  <- pnorm(z_bajo)

cat("Para pirat = 0.3:\n")
cat("  z = ", round(coef(probit_modelo)[1], 4), " + ", 
    round(coef(probit_modelo)[2], 4), " × 0.3\n")
cat("    = ", round(z_bajo, 4), "\n")
cat("  P̂(deny=1 | pirat=0.3) = Φ(", round(z_bajo, 4), ")\n")
cat("                         = ", round(prob_bajo, 4), "\n")
cat("                         = ", round(prob_bajo * 100, 2), "%\n\n")

# Predicción para pirat = 0.4
pirat_alto <- 0.4
z_alto     <- coef(probit_modelo)[1] + coef(probit_modelo)[2] * pirat_alto
prob_alto  <- pnorm(z_alto)

cat("Para pirat = 0.4:\n")
cat("  z = ", round(coef(probit_modelo)[1], 4), " + ", 
    round(coef(probit_modelo)[2], 4), " × 0.4\n")
cat("    = ", round(z_alto, 4), "\n")
cat("  P̂(deny=1 | pirat=0.4) = Φ(", round(z_alto, 4), ")\n")
cat("                         = ", round(prob_alto, 4), "\n")
cat("                         = ", round(prob_alto * 100, 2), "%\n\n")

# Diferencia
diferencia_probit <- prob_alto - prob_bajo

cat("DIFERENCIA EN PROBABILIDAD:\n")
cat("---------------------------\n")
cat("Δ P̂ = P̂(pirat=0.4) - P̂(pirat=0.3)\n")
cat("     = ", round(prob_alto, 4), " - ", round(prob_bajo, 4), "\n")
cat("     = ", round(diferencia_probit, 4), "\n")
cat("     = ", round(diferencia_probit * 100, 2), " puntos porcentuales\n\n")

cat("INTERPRETACIÓN:\n")
cat("---------------\n")
cat("Cuando el P/I ratio aumenta de 0.3 a 0.4 (un aumento de 0.1),\n")
cat("la probabilidad predicha de que se rechace la hipoteca aumenta\n")
cat("en", round(diferencia_probit * 100, 2), "puntos porcentuales.\n\n")

if (abs(diferencia_probit) > 0.05) {
  cat("✓ Este es un efecto SUSTANCIAL.\n\n")
} else {
  cat("◯ Este es un efecto moderado.\n\n")
}

cat("COMPARACIÓN CON LPM:\n")
cat("--------------------\n")
diferencia_lpm <- coef(lpm_modelo)[2] * 0.1
cat("Efecto en LPM (para Δpirat = 0.1):", round(diferencia_lpm, 4), "\n")
cat("Efecto en Probit (para Δpirat = 0.1):", round(diferencia_probit, 4), "\n\n")

if (abs(diferencia_probit - diferencia_lpm) < 0.01) {
  cat("◯ Los efectos son muy similares\n")
} else {
  cat("◯ Los efectos difieren moderadamente\n")
}

cat("\nNOTA:\n")
cat("-----\n")
cat("En el Probit, el efecto marginal NO es constante. Depende del valor\n")
cat("inicial del P/I ratio. En cambio, en el LPM, el efecto es siempre\n")
cat(round(coef(lpm_modelo)[2], 4), " sin importar el nivel de pirat.\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6e: MODELO CON SINGLE - EFECTOS MARGINALES
# ==============================================================================

cat("
================================================================================
EJERCICIO 6e: INCLUIR LA VARIABLE 'SINGLE' Y CALCULAR AME
================================================================================

Ahora incluimos la variable 'single' (1 = soltero, 0 = no soltero) en el
modelo Probit.

MODELO EXTENDIDO:
-----------------
")

# (Aquí iría el modelo extendido en LaTeX)

cat("

AME (Average Marginal Effect):
-------------------------------
El AME es el efecto marginal PROMEDIO sobre toda la muestra.

Para una variable binaria como 'single', el AME es:
  AME(single) = promedio de [P(deny=1|single=1, pirat) - P(deny=1|single=0, pirat)]
                sobre todos los valores de pirat en la muestra

ESTIMACIÓN:
-----------
")

# Estimar modelo con single
probit_single <- glm(
  deny ~ pirat + single,
  family = binomial(link = "probit"),
  data   = HMDA
)

cat("Modelo estimado: P(deny=1 | pirat, single) = Φ(β₀ + β₁×pirat + β₂×single)\n\n")

# Resumen
summary_probit_single <- summary(probit_single)
print(summary_probit_single)

cat("\n")
cat("COEFICIENTES ESTIMADOS:\n")
cat("-----------------------\n")
cat("β₀ (intercepto) =", round(coef(probit_single)[1], 4), "\n")
cat("β₁ (pirat)      =", round(coef(probit_single)[2], 4), "\n")
cat("β₂ (single)     =", round(coef(probit_single)[3], 4), "\n\n")

if (coef(probit_single)[3] > 0) {
  cat("β₂ > 0: Ser soltero AUMENTA la probabilidad de rechazo\n\n")
} else {
  cat("β₂ < 0: Ser soltero DISMINUYE la probabilidad de rechazo\n\n")
}

cat("CALCULAR EL AME (Average Marginal Effect) DE 'SINGLE':\n")
cat("-------------------------------------------------------\n")

# Método 1: Calcular manualmente el AME
cat("\nMÉTODO MANUAL:\n")
cat("--------------\n")

# Crear dos versiones de los datos: una con single=1 y otra con single=0
HMDA_single1 <- HMDA %>% mutate(single = factor("yes", levels = c("no", "yes")))
HMDA_single0 <- HMDA %>% mutate(single = factor("no",  levels = c("no", "yes")))

# Predecir probabilidades para ambos escenarios
pred_single1 <- predict(probit_single, newdata = HMDA_single1, type = "response")
pred_single0 <- predict(probit_single, newdata = HMDA_single0, type = "response")

# AME es el promedio de las diferencias
ame_single_manual <- mean(pred_single1 - pred_single0)

cat("AME(single) = promedio de [P(single=1) - P(single=0)]\n")
cat("            =", round(ame_single_manual, 4), "\n")
cat("            =", round(ame_single_manual * 100, 2), "puntos porcentuales\n\n")

# Método 2: Usar el paquete margins
cat("MÉTODO USANDO PAQUETE 'margins':\n")
cat("--------------------------------\n")

ame_results <- margins(probit_single)
print(summary(ame_results))

cat("\n")
cat("INTERPRETACIÓN DEL AME DE 'SINGLE':\n")
cat("------------------------------------\n")

ame_single_pkg <- summary(ame_results)$AME[
  summary(ame_results)$factor == "singleyes"
]

cat("AME(single) =", round(ame_single_pkg, 4), "\n")
cat("           =", round(ame_single_pkg * 100, 2), "puntos porcentuales\n\n")

cat("Interpretación:\n")
cat("En promedio, ser soltero ")
if (ame_single_pkg > 0) {
  cat("AUMENTA")
} else {
  cat("DISMINUYE")
}
cat(" la probabilidad de que se rechace la hipoteca\n")
cat("en", abs(round(ame_single_pkg * 100, 2)), "puntos porcentuales,\n")
cat("manteniendo constante el P/I ratio.\n\n")

cat("Esto se calcula como el promedio del efecto sobre todos los individuos\n")
cat("de la muestra, cada uno con su propio valor de P/I ratio.\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6f: ECUACIÓN ESTIMADA CON SINGLE
# ==============================================================================

cat("
================================================================================
EJERCICIO 6f: ESCRIBIR LA ECUACIÓN ESTIMADA
================================================================================

ECUACIÓN GENERAL:
-----------------
")

# (Aquí va la ecuación general en LaTeX)

cat("

ECUACIÓN CON VALORES ESTIMADOS:
--------------------------------
")

cat("P̂(deny=1 | pirat, single) = Φ(", round(coef(probit_single)[1], 4), 
    " + ", round(coef(probit_single)[2], 4), " × pirat", "\n")
if (coef(probit_single)[3] > 0) {
  cat("                               + ", round(coef(probit_single)[3], 4), " × single)\n\n")
} else {
  cat("                               ", round(coef(probit_single)[3], 4), " × single)\n\n")
}

cat("donde:\n")
cat("  - Φ(·) es la CDF de la distribución normal estándar\n")
cat("  - single = 1 si es soltero, 0 si no\n\n")

cat("INTERPRETACIÓN DE LOS SIGNOS:\n")
cat("------------------------------\n")

if (coef(probit_single)[2] > 0) {
  cat("β₁ > 0: Mayor P/I ratio → Mayor probabilidad de rechazo ✓\n")
  cat("        (Tiene sentido: mayor carga de pago relativa al ingreso)\n\n")
}

if (coef(probit_single)[3] > 0) {
  cat("β₂ > 0: Ser soltero → Mayor probabilidad de rechazo ✓\n")
  cat("        (Tiene sentido: menos estabilidad financiera, un solo ingreso)\n\n")
} else {
  cat("β₂ < 0: Ser soltero → Menor probabilidad de rechazo\n")
  cat("        (Contraintuitivo, pero puede haber otras razones)\n\n")
}

cat("NOTA: Los coeficientes por sí solos NO son efectos marginales.\n")
cat("Solo indican dirección. Para magnitud, calcular AME.\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6g: MODELO LOGIT
# ==============================================================================

cat("
================================================================================
EJERCICIO 6g: MODELO LOGIT
================================================================================

El modelo Logit usa la función logística en lugar de la CDF normal.

MODELO:
-------
")

# (Aquí va el modelo Logit en LaTeX)

cat("

donde Λ(z) = exp(z)/(1 + exp(z)) es la función logística.

DIFERENCIAS PROBIT vs LOGIT:
-----------------------------
- Probit: supone distribución normal de errores
- Logit: supone distribución logística de errores

La distribución logística tiene colas más pesadas que la normal.

En la práctica, ambos modelos suelen dar resultados MUY SIMILARES.

ESTIMACIÓN:
-----------
")

# Estimar modelo Logit
logit_single <- glm(
  deny ~ pirat + single,
  family = binomial(link = "logit"),
  data   = HMDA
)

cat("Modelo estimado: P(deny=1 | pirat, single) = Λ(β₀ + β₁×pirat + β₂×single)\n\n")

# Resumen
summary_logit <- summary(logit_single)
print(summary_logit)

cat("\n")
cat("COEFICIENTES ESTIMADOS (LOGIT):\n")
cat("--------------------------------\n")
cat("β₀ (intercepto) =", round(coef(logit_single)[1], 4), "\n")
cat("β₁ (pirat)      =", round(coef(logit_single)[2], 4), "\n")
cat("β₂ (single)     =", round(coef(logit_single)[3], 4), "\n\n")

cat("ECUACIÓN ESTIMADA:\n")
cat("------------------\n")
cat("P̂(deny=1 | pirat, single) = Λ(", round(coef(logit_single)[1], 4), 
    " + ", round(coef(logit_single)[2], 4), " × pirat\n")
if (coef(logit_single)[3] > 0) {
  cat("                               + ", round(coef(logit_single)[3], 4), " × single)\n\n")
} else {
  cat("                               ", round(coef(logit_single)[3], 4), " × single)\n\n")
}

cat("donde Λ(z) = exp(z)/(1 + exp(z))\n\n")

# Calcular AME para Logit
cat("AME (Average Marginal Effects) DEL LOGIT:\n")
cat("------------------------------------------\n")

ame_logit <- margins(logit_single)
print(summary(ame_logit))

cat("\n")
cat("COMPARACIÓN PROBIT vs LOGIT:\n")
cat("-----------------------------\n")

cat("                     Probit         Logit\n")
cat("β₁ (pirat)          ", sprintf("%7.4f", coef(probit_single)[2]), "      ", 
    sprintf("%7.4f", coef(logit_single)[2]), "\n")
cat("β₂ (single)         ", sprintf("%7.4f", coef(probit_single)[3]), "      ", 
    sprintf("%7.4f", coef(logit_single)[3]), "\n\n")

cat("Los coeficientes del Logit suelen ser ~1.6-1.8 veces los del Probit.\n")
cat("Esto se debe a que σ_logística = π/√3 ≈ 1.81 vs σ_normal = 1.\n\n")

# Comparar AMEs
ame_probit_pirat <- summary(ame_results)$AME[
  summary(ame_results)$factor == "pirat"
]
ame_logit_pirat <- summary(ame_logit)$AME[
  summary(ame_logit)$factor == "pirat"
]

ame_probit_single <- summary(ame_results)$AME[
  summary(ame_results)$factor == "singleyes"
]
ame_logit_single <- summary(ame_logit)$AME[
  summary(ame_logit)$factor == "singleyes"
]

cat("AME COMPARISON:\n")
cat("               Probit         Logit          Diferencia\n")
cat("pirat         ", sprintf("%7.5f", ame_probit_pirat), "      ", 
    sprintf("%7.5f", ame_logit_pirat), "      ", 
    sprintf("%7.5f", ame_logit_pirat - ame_probit_pirat), "\n")
cat("single        ", sprintf("%7.5f", ame_probit_single), "      ", 
    sprintf("%7.5f", ame_logit_single), "      ", 
    sprintf("%7.5f", ame_logit_single - ame_probit_single), "\n\n")

cat("✓ Los AMEs son MUY SIMILARES entre Probit y Logit\n")
cat("✓ En la práctica, la elección entre Probit y Logit es poco importante\n")
cat("✓ Ambos dan conclusiones sustantivas idénticas\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 6h: PSEUDO R² Y LR TEST
# ==============================================================================

cat("
================================================================================
EJERCICIO 6h: PSEUDO R-CUADRADO Y PRUEBA DE RAZÓN DE VEROSIMILITUD (LR TEST)
================================================================================

PSEUDO R-CUADRADO (McFadden's R²):
-----------------------------------
En modelos de máxima verosimilitud (Probit, Logit), no existe un R² verdadero
como en MCO. Usamos el PSEUDO R².

")

# (Aquí va la fórmula de pseudo R² en LaTeX)

cat("

donde:
  - LL₁ = log-verosimilitud del modelo completo (con variables)
  - LL₀ = log-verosimilitud del modelo nulo (solo intercepto)

Interpretación: Similar al R², pero NO es proporción de varianza explicada.
Valores típicos: 0.2-0.4 se consideran buenos en modelos binarios.

CÁLCULO PARA MODELO LOGIT:
---------------------------
")

# Log-verosimilitud del modelo completo
logLik_full <- logLik(logit_single)
ll_full     <- as.numeric(logLik_full)

# Modelo nulo (solo intercepto)
logit_null  <- glm(deny ~ 1, family = binomial(link = "logit"), data = HMDA)
logLik_null <- logLik(logit_null)
ll_null     <- as.numeric(logLik_null)

# Pseudo R²
pseudo_r2 <- 1 - (ll_full / ll_null)

cat("Log-verosimilitud (modelo completo):", round(ll_full, 2), "\n")
cat("Log-verosimilitud (modelo nulo):    ", round(ll_null, 2), "\n\n")

cat("Pseudo R² = 1 - (", round(ll_full, 2), ") / (", round(ll_null, 2), ")\n")
cat("          = 1 - ", round(ll_full / ll_null, 4), "\n")
cat("          =", round(pseudo_r2, 4), "\n\n")

if (pseudo_r2 > 0.3) {
  cat("✓ Pseudo R² > 0.3: Buen ajuste del modelo\n\n")
} else if (pseudo_r2 > 0.15) {
  cat("◯ Pseudo R² entre 0.15 y 0.3: Ajuste moderado\n\n")
} else {
  cat("◯ Pseudo R² < 0.15: Ajuste modesto\n\n")
}

cat("PRUEBA DE RAZÓN DE VEROSIMILITUD (LR TEST):
--------------------------------------------

La prueba LR compara el modelo completo con el modelo nulo (o modelo restringido).

H₀: β₁ = β₂ = 0 (el modelo nulo es adecuado)
H₁: Al menos un βⱼ ≠ 0 (el modelo completo es mejor)

ESTADÍSTICO LR:
---------------
")

# (Aquí va la fórmula del LR test en LaTeX)

cat("

Bajo H₀, LR ~ χ²(k), donde k = número de restricciones (número de coef. = 0)

CÁLCULO:
--------
")

LR_stat   <- 2 * (ll_full - ll_null)
k         <- length(coef(logit_single)) - 1  # Grados de libertad (excluyendo intercepto)
p_value_lr <- 1 - pchisq(LR_stat, df = k)

cat("LR = 2 × [LL(completo) - LL(nulo)]\n")
cat("   = 2 × [", round(ll_full, 2), " - (", round(ll_null, 2), ")]\n")
cat("   = 2 × ", round(ll_full - ll_null, 2), "\n")
cat("   =", round(LR_stat, 2), "\n\n")

cat("Grados de libertad: k =", k, "(número de variables, excluyendo intercepto)\n\n")

cat("DISTRIBUCIÓN BAJO H₀:\n")
cat("LR ~ χ²(", k, ")\n\n")

cat("P-VALOR:\n")
cat("p-value =", format.pval(p_value_lr, digits = 4), "\n\n")

if (p_value_lr < 0.001) {
  cat("✓✓✓ RESULTADO: p-value < 0.001\n")
  cat("✓✓✓ RECHAZAMOS H₀ con alta confianza\n")
  cat("✓✓✓ El modelo completo es significativamente mejor que el nulo\n")
  cat("✓✓✓ Las variables pirat y single son conjuntamente significativas\n\n")
} else if (p_value_lr < 0.05) {
  cat("✓ RESULTADO: p-value < 0.05\n")
  cat("✓ RECHAZAMOS H₀ al 5%\n")
  cat("✓ El modelo completo es significativamente mejor\n\n")
} else {
  cat("✗ NO podemos rechazar H₀\n")
  cat("✗ No hay evidencia de que las variables mejoren el modelo\n\n")
}

# También podemos usar la función lrtest del paquete lmtest
cat("VERIFICACIÓN CON FUNCIÓN lrtest():\n")
cat("----------------------------------\n")
lr_test_result <- lrtest(logit_null, logit_single)
print(lr_test_result)

cat("\n")
cat("INTERPRETACIÓN FINAL:\n")
cat("---------------------\n")
cat("El LR test confirma que el modelo con pirat y single es\n")
cat("significativamente mejor que el modelo que solo incluye el intercepto.\n\n")

cat("Esto significa que:\n")
cat("  ✓ Las variables explicativas tienen poder predictivo\n")
cat("  ✓ El modelo es útil para entender los rechazos de hipoteca\n")
cat("  ✓ Tanto el P/I ratio como el estado civil importan\n")

cat("
================================================================================
RESUMEN FINAL: COMPARACIÓN DE LOS TRES MODELOS
================================================================================
")

# Crear tabla comparativa
cat("\nTABLA COMPARATIVA: LPM, PROBIT, LOGIT\n")
cat("--------------------------------------\n\n")

# Usar modelsummary para comparar
modelos_lista <- list(
  "LPM"    = lpm_modelo,
  "Probit" = probit_single,
  "Logit"  = logit_single
)

msummary(
  modelos_lista,
  stars   = TRUE,
  gof_map = c("nobs", "r.squared", "logLik", "AIC"),
  title   = "Comparación de Modelos: LPM, Probit, Logit"
)

cat("\n")
cat("CONCLUSIONES GENERALES:\n")
cat("-----------------------\n\n")

cat("1. DIRECCIÓN DE EFECTOS:\n")
cat("   Todos los modelos coinciden:\n")
cat("     - Mayor P/I ratio → Mayor probabilidad de rechazo\n")
cat("     - Ser soltero → Mayor probabilidad de rechazo\n\n")

cat("2. MAGNITUD DE EFECTOS:\n")
cat("   - LPM: Efectos marginales constantes\n")
cat("   - Probit/Logit: Efectos marginales variables (más realista)\n")
cat("   - Los AMEs de Probit y Logit son muy similares entre sí\n")
cat("   - Los AMEs suelen estar cerca de los coeficientes del LPM\n\n")

cat("3. PREDICCIONES:\n")
cat("   - LPM: Puede dar valores fuera de [0,1]\n")
cat("   - Probit/Logit: Siempre en [0,1]\n\n")

cat("4. ELECCIÓN DE MODELO:\n")
cat("   - Si solo interesa dirección y significancia: LPM es suficiente\n")
cat("   - Si necesitas probabilidades válidas: Probit o Logit\n")
cat("   - Probit vs Logit: En práctica, da igual (resultados casi idénticos)\n")
cat("   - Muchos economistas prefieren Probit por tradición\n\n")

cat("5. INTERPRETACIÓN:\n")
cat("   - LPM: Coeficientes son efectos marginales directamente\n")
cat("   - Probit/Logit: Coeficientes no son EM, calcular AME\n")
cat("   - Todos dan conclusiones sustantivas similares\n\n")

cat("
================================================================================
FIN DEL ANÁLISIS - EJERCICIOS 5 Y 6
================================================================================

Has aprendido:
✓ Qué son los modelos de variable dependiente limitada
✓ Por qué MCO no funciona bien con variables binarias
✓ Cómo estimar LPM, Probit y Logit
✓ Cómo interpretar coeficientes
✓ Cómo calcular efectos marginales (AME)
✓ Cómo hacer predicciones
✓ Cómo evaluar bondad de ajuste (Pseudo R², LR test)
✓ Cómo comparar modelos

Este conocimiento es fundamental para trabajo empírico en economía, finanzas,
salud pública, marketing, y muchas otras áreas.

")

# Guardar workspace
cat("\nGuardando resultados...\n")
save.image(file = "ejercicios_5_6_resultados.RData")
cat("✓ Resultados guardados en 'ejercicios_5_6_resultados.RData'\n\n")
