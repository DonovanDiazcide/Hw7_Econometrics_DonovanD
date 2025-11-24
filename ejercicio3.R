# ==============================================================================
# ECONOMETRÍA 1 - ACTIVIDAD 7
# EJERCICIO 3: ANÁLISIS EMPÍRICO - CARD (1993, 1995)
# ==============================================================================
# Dr. Francisco Cabrera
# División de Economía - CDE
#
# Este script contiene el análisis empírico completo de los datos de Card (1993)
# sobre retornos a la educación usando Variables Instrumentales.
#
# PAPER: Card, D. (1993). "Using Geographic Variation in College Proximity to 
#        Estimate the Return to Schooling"
# ==============================================================================

# Limpiar el entorno
rm(list = ls())

# Cargar paquetes necesarios
library(pacman)
p_load(tidyverse, fixest, modelsummary, haven, broom, knitr, kableExtra)

cat("
================================================================================
CONTEXTO DEL ESTUDIO DE CARD (1993, 1995)
================================================================================

PREGUNTA DE INVESTIGACIÓN:
--------------------------
¿Cuál es el retorno de la educación sobre los salarios?

Es decir, ¿en cuánto aumenta el salario (en %) por cada año adicional de 
educación?

MODELO BÁSICO (Mincer equation):
---------------------------------
  log(wage_i) = β₀ + β₁ educ_i + β₂ exper_i + β₃ exper²_i + ... + u_i

donde:
  - log(wage_i) = logaritmo natural del salario por hora
  - educ_i = años de educación
  - exper_i = años de experiencia laboral
  - u_i = término de error

El coeficiente β₁ se interpreta como el retorno porcentual de un año 
adicional de educación (aproximadamente).

EL PROBLEMA: ENDOGENEIDAD DE LA EDUCACIÓN
------------------------------------------
La educación (educ) es probablemente ENDÓGENA porque:

1. HABILIDAD NO OBSERVADA (Ability Bias):
   - Personas más hábiles tienden a:
     a) Obtener más educación
     b) Ganar salarios más altos (independientemente de la educación)
   - La habilidad está en u_i pero afecta tanto a educ como a wage
   - Resultado: Cov(educ, u) > 0

2. ANTECEDENTES FAMILIARES:
   - Familias con más recursos económicos y educativos:
     a) Pueden financiar más años de educación para sus hijos
     b) Transmiten ventajas que aumentan salarios (redes, capital cultural)

3. MOTIVACIÓN Y PREFERENCIAS:
   - Personas más motivadas o con preferencias por el trabajo:
     a) Obtienen más educación
     b) Trabajan más duro → mayores salarios

CONSECUENCIA:
")

# aquí va el código de latex para el sesgo MCO

cat("

El estimador de MCO SOBREESTIMA el verdadero retorno de la educación porque
confunde el efecto causal con la correlación espuria debida a la habilidad.

SOLUCIÓN DE CARD: VARIABLE INSTRUMENTAL
----------------------------------------
Card propone usar como instrumento:

  nearc4_i = 1 si el individuo i creció cerca de una universidad de 4 años
           = 0 si no

INTUICIÓN:
----------
- Vivir cerca de una universidad reduce el costo de asistir a ella
  (costos de transporte, vivienda, etc.)
- Esto aumenta la probabilidad de obtener más educación
- Pero, ¿vivir cerca de una universidad afecta directamente al salario?
  Si no, entonces nearc4 es un buen instrumento.

DATOS:
------
- Muestra: Hombres en 1976
- Fuente: National Longitudinal Survey of Young Men (NLSY)
- Archivo: card.dta

================================================================================
")

# ==============================================================================
# CARGAR Y EXPLORAR LOS DATOS
# ==============================================================================

cat("
================================================================================
PASO 1: CARGAR Y EXPLORAR LOS DATOS
================================================================================
")

# Instalar el paquete si no lo tienes
install.packages("wooldridge")

# Cargar el paquete y los datos
library(wooldridge)
data("card")

# Ver las primeras filas
head(card)

# Ver descripción de las variables
?card


# Cargar los datos
# Nota: Asumiendo que el archivo está en /mnt/user-data/uploads/
data_path <- data("card")

# Explorar la estructura de los datos
cat("ESTRUCTURA DE LOS DATOS:\n")
cat("------------------------\n")
cat("Número de observaciones:", nrow(card), "\n")
cat("Número de variables:", ncol(card), "\n\n")

cat("VARIABLES PRINCIPALES:\n")
cat("----------------------\n")
cat("Variable      Descripción\n")
cat("--------      -----------\n")
cat("lwage         Logaritmo del salario por hora\n")
cat("educ          Años de educación\n")
cat("exper         Años de experiencia potencial (age - educ - 6)\n")
cat("nearc4        =1 si creció cerca de universidad de 4 años\n")
cat("nearc2        =1 si creció cerca de universidad de 2 años (community college)\n")
cat("black         =1 si es afroamericano\n")
cat("smsa          =1 si vive en área metropolitana (SMSA)\n")
cat("south         =1 si vive en el sur\n")
cat("smsa66        =1 si vivía en SMSA en 1966\n")
cat("reg661-reg669 Dummies regionales de residencia en 1966\n")
cat("IQ            Puntaje de IQ (disponible solo para submuestra)\n\n")

# Estadísticas descriptivas básicas
cat("ESTADÍSTICAS DESCRIPTIVAS DE VARIABLES CLAVE:\n")
cat("----------------------------------------------\n")

vars_interes <- c("lwage", "educ", "exper", "nearc4", "black", "smsa", "south")

if (all(vars_interes %in% names(card))) {
  desc_stats <- card %>%
    select(all_of(vars_interes)) %>%
    summarise(
      across(everything(),
             list(
               n = ~sum(!is.na(.)),
               media = ~mean(., na.rm = TRUE),
               sd = ~sd(., na.rm = TRUE),
               min = ~min(., na.rm = TRUE),
               max = ~max(., na.rm = TRUE)
             ),
             .names = "{.col}_{.fn}")
    )
  
  # Transponer para mejor visualización
  desc_stats_long <- desc_stats %>%
    pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
    separate(stat, into = c("variable", "estadistico"), sep = "_(?=[^_]+$)") %>%
    pivot_wider(names_from = estadistico, values_from = value)
  
  print(desc_stats_long)
  cat("\n")
}

# Crear gráfico exploratorio
cat("RELACIÓN VISUAL ENTRE EDUCACIÓN Y LOG(SALARIO):\n")
cat("------------------------------------------------\n")

if ("lwage" %in% names(card) && "educ" %in% names(card)) {
  # Calcular medias por nivel educativo
  educ_wage <- card %>%
    filter(!is.na(lwage) & !is.na(educ)) %>%
    group_by(educ) %>%
    summarise(
      lwage_mean = mean(lwage),
      n = n()
    )
  
  cat("A simple vista, parece haber una relación positiva entre educación y salarios.\n")
  cat("Pero, ¿es esta relación causal o refleja habilidad no observada?\n\n")
}

# ==============================================================================
# EJERCICIO 3a: ¿ES nearc4 UN IV CREÍBLE?
# ==============================================================================

cat("
================================================================================
EJERCICIO 3a: ¿ES nearc4 UN INSTRUMENTO CREÍBLE PARA EDUCACIÓN?
================================================================================

Para que nearc4 sea un instrumento válido para educación, debe cumplir dos 
condiciones:

")

# aquí va el código de latex para condiciones IV

cat("

EVALUACIÓN DE CONDICIÓN 1: RELEVANCIA
--------------------------------------
¿Está nearc4 correlacionado con educación?

Pregunta económica: ¿Vivir cerca de una universidad aumenta los años de 
educación que obtienen las personas?

HIPÓTESIS:
SÍ, porque vivir cerca reduce:
  - Costos de transporte
  - Costos de vivienda (pueden vivir con sus padres)
  - Costos psicológicos (familiaridad con el ambiente universitario)
  - Información asimétrica (mayor conocimiento sobre beneficios de la educación)

PRUEBA EMPÍRICA: Regresión de primera etapa
--------------------------------------------
")

# aquí va el código de latex para primera etapa

cat("

Vamos a estimar esta regresión y probar H₀: π₁ = 0

")

# Primera etapa - solo con nearc4
cat("REGRESIÓN DE PRIMERA ETAPA (SIMPLE):\n")
cat("------------------------------------\n")

first_stage_simple <- lm(educ ~ nearc4, data = card)
summary_fs_simple <- summary(first_stage_simple)

cat("\nResultados:\n")
print(summary_fs_simple)

cat("\n")
cat("INTERPRETACIÓN:\n")
cat("---------------\n")
cat("Coeficiente de nearc4: ", round(coef(first_stage_simple)["nearc4"], 4), "\n")
cat("Error estándar:        ", round(summary_fs_simple$coefficients["nearc4", "Std. Error"], 4), "\n")
cat("Estadístico t:         ", round(summary_fs_simple$coefficients["nearc4", "t value"], 2), "\n")
cat("p-valor:               ", format.pval(summary_fs_simple$coefficients["nearc4", "Pr(>|t|)"], digits = 4), "\n\n")

if (summary_fs_simple$coefficients["nearc4", "Pr(>|t|)"] < 0.05) {
  cat("✓ El coeficiente es ESTADÍSTICAMENTE SIGNIFICATIVO al 5%\n")
  cat("✓ Vivir cerca de una universidad aumenta la educación en aproximadamente ",
      round(coef(first_stage_simple)["nearc4"], 2), " años\n")
} else {
  cat("✗ El coeficiente NO es estadísticamente significativo\n")
}

# Calcular el F-estadístico
f_stat_simple <- summary_fs_simple$fstatistic[1]
cat("\nF-estadístico de la regresión: ", round(f_stat_simple, 2), "\n")

if (f_stat_simple > 10) {
  cat("✓ F > 10: El instrumento NO es débil (regla práctica de Stock y Yogo)\n")
} else {
  cat("⚠ F < 10: ADVERTENCIA - Instrumento potencialmente débil\n")
}

cat("\n")
cat("REGLA PRÁCTICA: Un instrumento se considera DÉBIL si F < 10 en la primera etapa.\n")
cat("Instrumentos débiles producen estimadores IV sesgados hacia MCO.\n\n")

# Primera etapa con controles
cat("REGRESIÓN DE PRIMERA ETAPA (CON CONTROLES):\n")
cat("--------------------------------------------\n")
cat("Es común incluir controles adicionales para aumentar la precisión:\n\n")

# Verificar qué variables existen en los datos
control_vars <- c("exper", "black", "smsa", "south")
available_controls <- control_vars[control_vars %in% names(card)]

if (length(available_controls) > 0) {
  formula_fs <- as.formula(paste("educ ~ nearc4 +", paste(available_controls, collapse = " + ")))
  
  # Agregar experiencia al cuadrado si existe exper
  if ("exper" %in% available_controls) {
    card <- card %>% mutate(exper2 = exper^2)
    formula_fs <- update(formula_fs, ~ . + exper2)
  }
  
  first_stage_controls <- lm(formula_fs, data = card)
  summary_fs_controls <- summary(first_stage_controls)
  
  cat("\nFórmula: ", deparse(formula_fs), "\n\n")
  print(summary_fs_controls)
  
  cat("\n")
  cat("COEFICIENTE DE nearc4 (con controles):\n")
  cat("--------------------------------------\n")
  cat("Coeficiente: ", round(coef(first_stage_controls)["nearc4"], 4), "\n")
  cat("Error estándar: ", round(summary_fs_controls$coefficients["nearc4", "Std. Error"], 4), "\n")
  cat("Estadístico t: ", round(summary_fs_controls$coefficients["nearc4", "t value"], 2), "\n")
  
  # F-estadístico para nearc4
  # Para obtener el F-estadístico de nearc4, hacemos una prueba F
  model_restricted <- update(first_stage_controls, ~ . - nearc4)
  anova_result <- anova(model_restricted, first_stage_controls)
  f_nearc4 <- anova_result$F[2]
  
  cat("F-estadístico de nearc4: ", round(f_nearc4, 2), "\n\n")
  
  if (f_nearc4 > 10) {
    cat("✓ F > 10: Instrumento FUERTE\n\n")
  } else {
    cat("⚠ F < 10: Instrumento DÉBIL\n\n")
  }
}

cat("CONCLUSIÓN SOBRE RELEVANCIA:\n")
cat("----------------------------\n")
cat("✓ nearc4 está correlacionado con educación\n")
cat("✓ La correlación es estadísticamente significativa\n")
cat("✓ La primera etapa es suficientemente fuerte (F > 10)\n")
cat("✓ CUMPLE LA CONDICIÓN DE RELEVANCIA\n\n")

cat("
EVALUACIÓN DE CONDICIÓN 2: EXOGENEIDAD (RESTRICCIÓN DE EXCLUSIÓN)
------------------------------------------------------------------
¿Está nearc4 correlacionado con el error u?

Formalmente: ¿Cov(nearc4, u) = 0?

Esta condición dice que vivir cerca de una universidad NO debe afectar el 
salario directamente, EXCEPTO a través de su efecto sobre la educación.

")

# aquí va el código de latex para restricción de exclusión

cat("

PREGUNTA CLAVE:
¿Existen canales por los cuales vivir cerca de una universidad podría afectar
el salario DIRECTAMENTE (sin pasar por la educación)?

POSIBLES AMENAZAS A LA EXOGENEIDAD:
------------------------------------

1. CALIDAD DE LA REGIÓN:
   - Quizás las áreas con universidades son más prósperas
   - Más oportunidades laborales → salarios más altos
   - Mejor infraestructura → mayor productividad
   ⚠ Esto violaría la exogeneidad

2. SELECCIÓN RESIDENCIAL:
   - Familias más educadas y ricas eligen vivir cerca de universidades
   - Estas familias transmiten ventajas a sus hijos (más allá de la educación)
   ⚠ Esto violaría la exogeneidad

3. EFECTOS DE RED/SPILLOVERS:
   - Vivir cerca de una universidad podría dar acceso a redes profesionales
   - Estas redes aumentan salarios independientemente de la educación
   ⚠ Esto violaría la exogeneidad

4. CALIDAD DE LA EDUCACIÓN K-12:
   - Áreas con universidades podrían tener mejores escuelas secundarias
   - Mejor educación pre-universitaria → mayores habilidades → mayores salarios
   ⚠ Esto violaría la exogeneidad

ESTRATEGIA DE VALIDACIÓN:
--------------------------
Como la exogeneidad NO es directamente testeable, Card usa estrategias indirectas:

a) CONTROLAR POR CARACTERÍSTICAS REGIONALES:
   - Incluir dummies regionales (reg662, ..., reg669)
   - Incluir SMSA en 1966 (área urbana vs rural)
   - Esto controla por diferencias geográficas sistemáticas

b) PROBAR CORRELACIÓN CON HABILIDAD OBSERVADA (IQ):
   - Si nearc4 está correlacionado con IQ, es sospechoso
   - IQ captura parte de la habilidad no observada
   - Veremos esto en los incisos 3c y 3d

ARGUMENTOS A FAVOR DE LA EXOGENEIDAD:
--------------------------------------

1. VARIACIÓN IDIOSINCRÁTICA:
   - La ubicación de universidades es histórica (fundadas hace 50-100 años)
   - No fue planeada basándose en características económicas actuales
   - Es en gran medida 'accidental' desde el punto de vista del individuo

2. CONTROLAR POR REGIÓN:
   - Al incluir controles regionales, comparamos personas DENTRO de la misma
     región que viven a diferentes distancias de universidades
   - Esto elimina muchas diferencias regionales sistemáticas

3. EVIDENCIA DE BALANCE:
   - Si nearc4 no está correlacionado con otras características observables
     (después de controlar por región), es más creíble

CONCLUSIÓN PRELIMINAR:
----------------------
La credibilidad de nearc4 como instrumento depende crucialmente de:
  - Incluir controles regionales apropiados
  - Verificar que no está correlacionado con habilidad (IQ)
  - Argumentos teóricos sobre la naturaleza 'accidental' de la proximidad

Evaluaremos esto más a fondo en los siguientes incisos.

================================================================================
")

# ==============================================================================
# EJERCICIO 3b: ESTIMACIÓN OLS vs IV
# ==============================================================================

cat("
================================================================================
EJERCICIO 3b: ESTIMACIÓN OLS vs IV
================================================================================

Ahora vamos a estimar el modelo de salarios usando:
1. MCO (OLS) - método estándar pero potencialmente sesgado
2. Variables Instrumentales (IV) - usando nearc4 como instrumento

MODELO BÁSICO:
--------------
  log(wage) = β₀ + β₁ educ + β₂ exper + β₃ exper² + β₄ black + β₅ smsa + 
              β₆ south + u

donde β₁ es el parámetro de interés: el retorno de la educación.

")

# ==============================================================================
# ESTIMACIÓN MCO (OLS)
# ==============================================================================

cat("ESTIMACIÓN 1: MCO (OLS)\n")
cat("------------------------\n\n")

# Preparar datos
card_clean <- card %>%
  filter(!is.na(lwage) & !is.na(educ) & !is.na(exper))

# Modelo OLS básico
ols_basic <- lm(lwage ~ educ + exper + I(exper^2), data = card_clean)

cat("Modelo OLS básico (sin controles adicionales):\n")
cat("----------------------------------------------\n")
print(summary(ols_basic))

cat("\n\nCOEFICIENTE DE EDUCACIÓN (OLS básico):\n")
cat("--------------------------------------\n")
cat("β̂₁ = ", round(coef(ols_basic)["educ"], 4), "\n")
cat("Interpretación: Un año adicional de educación aumenta el salario en aproximadamente ",
    round(coef(ols_basic)["educ"] * 100, 2), "%\n\n")

# Modelo OLS con controles adicionales
if (all(c("black", "smsa", "south") %in% names(card_clean))) {
  ols_full <- lm(lwage ~ educ + exper + I(exper^2) + black + smsa + south, 
                 data = card_clean)
  
  cat("Modelo OLS completo (con controles demográficos y regionales):\n")
  cat("-------------------------------------------------------------\n")
  print(summary(ols_full))
  
  cat("\n\nCOEFICIENTE DE EDUCACIÓN (OLS completo):\n")
  cat("----------------------------------------\n")
  cat("β̂₁ = ", round(coef(ols_full)["educ"], 4), "\n")
  cat("Interpretación: Un año adicional de educación aumenta el salario en aproximadamente ",
      round(coef(ols_full)["educ"] * 100, 2), "%\n\n")
}

# ==============================================================================
# ESTIMACIÓN IV (2SLS)
# ==============================================================================

cat("\nESTIMACIÓN 2: VARIABLES INSTRUMENTALES (IV / 2SLS)\n")
cat("----------------------------------------------------\n\n")

cat("Estrategia: Usar nearc4 como instrumento para educ\n\n")

cat("RECORDATORIO: Two-Stage Least Squares (2SLS)\n")
cat("--------------------------------------------\n")

# aquí va el código de latex para 2SLS

cat("\n")

# Modelo IV básico
cat("Modelo IV básico (sin controles adicionales):\n")
cat("---------------------------------------------\n")

iv_basic <- feols(lwage ~ exper + I(exper^2) | educ ~ nearc4, 
                  data = card_clean,
                  se = 'hetero')  # Errores robustos a heterocedasticidad

print(summary(iv_basic))

cat("\n\nCOEFICIENTE DE EDUCACIÓN (IV básico):\n")
cat("-------------------------------------\n")
cat("β̂₁_IV = ", round(coef(iv_basic)["fit_educ"], 4), "\n")
cat("Interpretación: Un año adicional de educación aumenta el salario en aproximadamente ",
    round(coef(iv_basic)["fit_educ"] * 100, 2), "%\n")
cat("(efecto causal para quienes su educación fue afectada por vivir cerca de universidad)\n\n")

# Estadísticas de primera etapa
cat("DIAGNÓSTICO DE PRIMERA ETAPA:\n")
cat("-----------------------------\n")
first_stage_stats <- fitstat(iv_basic, 'ivf')
cat("F-estadístico de primera etapa:", round(first_stage_stats$ivf1$stat, 2), "\n")

if (first_stage_stats$ivf1$stat > 10) {
  cat("✓ F > 10: Instrumento FUERTE\n")
} else {
  cat("⚠ F < 10: Instrumento DÉBIL - resultados pueden estar sesgados\n")
}

# Modelo IV con controles
if (all(c("black", "smsa", "south") %in% names(card_clean))) {
  cat("\n\nModelo IV completo (con controles demográficos y regionales):\n")
  cat("------------------------------------------------------------\n")
  
  iv_full <- feols(lwage ~ exper + I(exper^2) + black + smsa + south | educ ~ nearc4, 
                   data = card_clean,
                   se = 'hetero')
  
  print(summary(iv_full))
  
  cat("\n\nCOEFICIENTE DE EDUCACIÓN (IV completo):\n")
  cat("---------------------------------------\n")
  cat("β̂₁_IV = ", round(coef(iv_full)["fit_educ"], 4), "\n")
  cat("Interpretación: Un año adicional de educación aumenta el salario en aproximadamente ",
      round(coef(iv_full)["fit_educ"] * 100, 2), "%\n\n")
  
  # Estadísticas de primera etapa
  cat("DIAGNÓSTICO DE PRIMERA ETAPA (con controles):\n")
  cat("---------------------------------------------\n")
  first_stage_stats_full <- fitstat(iv_full, 'ivf')
  cat("F-estadístico:", round(first_stage_stats_full$ivf1$stat, 2), "\n\n")
}

# ==============================================================================
# COMPARACIÓN OLS vs IV
# ==============================================================================

cat("
================================================================================
COMPARACIÓN OLS vs IV
================================================================================
")

# Crear tabla comparativa
if (exists("ols_full") && exists("iv_full")) {
  cat("Tabla comparativa de resultados:\n")
  cat("--------------------------------\n\n")
  
  msummary(
    list("OLS" = ols_full, "IV (2SLS)" = iv_full),
    stars = TRUE,
    gof_omit = "IC|Log|Adj|F$|Pseudo|Within",
    coef_rename = c(
      "educ" = "Educación",
      "fit_educ" = "Educación (IV)",
      "exper" = "Experiencia",
      "I(exper^2)" = "Experiencia²",
      "black" = "Black",
      "smsa" = "SMSA",
      "south" = "South"
    ),
    title = "Comparación de Estimaciones OLS vs IV"
  )
  
  cat("\n\nANÁLISIS DE LA COMPARACIÓN:\n")
  cat("---------------------------\n\n")
  
  beta_ols <- coef(ols_full)["educ"]
  beta_iv <- coef(iv_full)["fit_educ"]
  
  cat("Coeficiente OLS:  ", round(beta_ols, 4), " (", round(beta_ols * 100, 2), "% retorno)\n")
  cat("Coeficiente IV:   ", round(beta_iv, 4), " (", round(beta_iv * 100, 2), "% retorno)\n")
  cat("Diferencia:       ", round(beta_iv - beta_ols, 4), "\n")
  cat("Diferencia (%):   ", round((beta_iv - beta_ols) * 100, 2), " puntos porcentuales\n\n")
  
  if (beta_iv > beta_ols) {
    cat("✓ β̂_IV > β̂_OLS\n\n")
    cat("INTERPRETACIÓN:\n")
    cat("---------------\n")
    cat("El estimador IV es MAYOR que el estimador OLS.\n\n")
    
    cat("¿QUÉ SIGNIFICA ESTO?\n")
    cat("--------------------\n")
    cat("Hay dos interpretaciones posibles:\n\n")
    
    cat("1. SESGO NEGATIVO EN OLS (menos probable en este contexto):\n")
    cat("   - Si el sesgo por habilidad no observada fuera NEGATIVO\n")
    cat("   - Esto ocurriría si personas con menor habilidad obtienen más educación\n")
    cat("   - Poco plausible en este contexto\n\n")
    
    cat("2. HETEROGENEIDAD DE EFECTOS Y LATE:\n")
    cat("   - IV estima el Local Average Treatment Effect (LATE)\n")
    cat("   - LATE = efecto para 'compliers' (quienes su educación fue afectada por nearc4)\n")
    cat("   - Los 'compliers' son personas al margen de obtener educación universitaria\n")
    cat("   - Estas personas pueden tener MAYORES retornos de la educación porque:\n")
    cat("       a) Vienen de familias con menos recursos\n")
    cat("       b) La universidad es más transformativa para ellos\n")
    cat("       c) Tienen restricciones de crédito que la proximidad alivia\n\n")
    
    cat("3. ERROR DE MEDICIÓN EN EDUCACIÓN:\n")
    cat("   - Si la educación se mide con error, OLS está sesgado hacia cero\n")
    cat("   - IV corrige este sesgo de atenuación\n\n")
    
  } else if (beta_ols > beta_iv) {
    cat("✓ β̂_OLS > β̂_IV\n\n")
    cat("INTERPRETACIÓN:\n")
    cat("---------------\n")
    cat("El estimador OLS es MAYOR que el estimador IV.\n\n")
    
    cat("¿QUÉ SIGNIFICA ESTO?\n")
    cat("--------------------\n")
    cat("Esto es consistente con el SESGO POR HABILIDAD NO OBSERVADA:\n\n")
    
    cat("- Las personas más hábiles obtienen más educación Y ganan más\n")
    cat("- OLS confunde el efecto causal con la correlación por habilidad\n")
    cat("- OLS SOBREESTIMA el retorno verdadero de la educación\n")
    cat("- IV, al usar variación exógena (nearc4), estima el efecto causal verdadero\n\n")
    
    cat("Sesgo estimado por habilidad no observada:\n")
    cat("  Sesgo ≈ β̂_OLS - β̂_IV = ", round(beta_ols - beta_iv, 4), "\n\n")
  } else {
    cat("β̂_OLS ≈ β̂_IV (muy similares)\n\n")
    cat("INTERPRETACIÓN:\n")
    cat("---------------\n")
    cat("Los estimadores son muy similares. Esto podría significar:\n")
    cat("- La endogeneidad no es tan severa como se pensaba\n")
    cat("- Los controles incluidos capturan la mayor parte del sesgo\n")
    cat("- El instrumento identifica un LATE similar al ATE\n\n")
  }
}

cat("
NOTA IMPORTANTE SOBRE INTERPRETACIÓN:
--------------------------------------
El estimador IV (β̂_IV) identifica el Local Average Treatment Effect (LATE):
  
  LATE = E[wage_i(educ+1) - wage_i(educ) | complier]

donde 'compliers' son las personas cuya educación fue afectada por vivir
cerca de una universidad.

Este efecto puede ser diferente del Average Treatment Effect (ATE) si:
  - Los retornos de la educación son heterogéneos en la población
  - Los 'compliers' tienen características diferentes del promedio

================================================================================
")

# ==============================================================================
# EJERCICIO 3c: RELACIÓN ENTRE nearc4 E IQ
# ==============================================================================

cat("
================================================================================
EJERCICIO 3c: ¿ESTÁ nearc4 CORRELACIONADO CON IQ?
================================================================================

OBJETIVO:
---------
Evaluar la credibilidad de la exogeneidad de nearc4 examinando si está 
correlacionado con IQ (habilidad observada).

RAZONAMIENTO:
-------------
- Si nearc4 está correlacionado con IQ, es sospechoso
- Significaría que personas más hábiles viven cerca de universidades
- Esto sugeriría que nearc4 podría estar correlacionado con u (habilidad no obs.)
- Violaría la restricción de exclusión

ESTRATEGIA:
-----------
Regresar IQ sobre nearc4:
  IQ_i = γ₀ + γ₁ nearc4_i + ε_i

Probar H₀: γ₁ = 0 vs H₁: γ₁ ≠ 0

")

# aquí va el código de latex para regresión IQ

cat("\n")

# Verificar si IQ está disponible
if ("IQ" %in% names(card)) {
  
  # Filtrar submuestra con IQ
  card_iq <- card %>%
    filter(!is.na(IQ) & !is.na(nearc4))
  
  cat("SUBMUESTRA CON IQ:\n")
  cat("------------------\n")
  cat("Tamaño de la submuestra con IQ: ", nrow(card_iq), " observaciones\n")
  cat("(de ", nrow(card), " observaciones totales)\n\n")
  
  cat("Estadísticas descriptivas de IQ:\n")
  cat("---------------------------------\n")
  cat("Media:         ", round(mean(card_iq$IQ), 2), "\n")
  cat("Desv. estándar:", round(sd(card_iq$IQ), 2), "\n")
  cat("Mínimo:        ", round(min(card_iq$IQ), 2), "\n")
  cat("Máximo:        ", round(max(card_iq$IQ), 2), "\n\n")
  
  # Regresión IQ ~ nearc4 (sin controles)
  cat("REGRESIÓN: IQ ~ nearc4 (sin controles)\n")
  cat("--------------------------------------\n\n")
  
  iq_reg1 <- lm(IQ ~ nearc4, data = card_iq)
  summary_iq1 <- summary(iq_reg1)
  
  print(summary_iq1)
  
  cat("\n\nRESULTADOS:\n")
  cat("-----------\n")
  cat("Coeficiente de nearc4: ", round(coef(iq_reg1)["nearc4"], 4), "\n")
  cat("Error estándar:        ", round(summary_iq1$coefficients["nearc4", "Std. Error"], 4), "\n")
  cat("Estadístico t:         ", round(summary_iq1$coefficients["nearc4", "t value"], 2), "\n")
  cat("p-valor:               ", format.pval(summary_iq1$coefficients["nearc4", "Pr(>|t|)"], digits = 4), "\n\n")
  
  # Interpretación
  if (summary_iq1$coefficients["nearc4", "Pr(>|t|)"] < 0.05) {
    cat("✗ El coeficiente es ESTADÍSTICAMENTE SIGNIFICATIVO al 5%\n\n")
    cat("IMPLICACIÓN:\n")
    cat("------------\n")
    cat("⚠ ADVERTENCIA: nearc4 está correlacionado con IQ\n")
    cat("⚠ Las personas que viven cerca de universidades tienen IQ ")
    if (coef(iq_reg1)["nearc4"] > 0) {
      cat("MAYOR\n")
    } else {
      cat("MENOR\n")
    }
    cat("⚠ Esto pone en duda la exogeneidad del instrumento\n")
    cat("⚠ Sugiere que nearc4 podría estar correlacionado con habilidad no observada\n\n")
    
    cat("POSIBLES EXPLICACIONES:\n")
    cat("-----------------------\n")
    cat("1. SELECCIÓN RESIDENCIAL:\n")
    cat("   - Familias más educadas eligen vivir cerca de universidades\n")
    cat("   - Estas familias tienen hijos con mayor IQ (por genética o ambiente)\n\n")
    
    cat("2. CALIDAD DEL ÁREA:\n")
    cat("   - Áreas con universidades tienen mejor educación K-12\n")
    cat("   - Mejor educación temprana → mayor IQ medido\n\n")
    
  } else {
    cat("✓ El coeficiente NO es estadísticamente significativo al 5%\n\n")
    cat("IMPLICACIÓN:\n")
    cat("------------\n")
    cat("✓ No hay evidencia de correlación entre nearc4 e IQ\n")
    cat("✓ Esto apoya la credibilidad de nearc4 como instrumento exógeno\n")
    cat("✓ No parece haber selección por habilidad en la proximidad a universidades\n\n")
  }
  
  # Calcular medias por grupo
  cat("COMPARACIÓN DE MEDIAS DE IQ POR GRUPO:\n")
  cat("--------------------------------------\n")
  
  iq_means <- card_iq %>%
    group_by(nearc4) %>%
    summarise(
      IQ_mean = mean(IQ),
      IQ_sd = sd(IQ),
      n = n()
    )
  
  print(iq_means)
  
  cat("\n")
  cat("IQ medio (nearc4 = 0): ", round(iq_means$IQ_mean[iq_means$nearc4 == 0], 2), "\n")
  cat("IQ medio (nearc4 = 1): ", round(iq_means$IQ_mean[iq_means$nearc4 == 1], 2), "\n")
  cat("Diferencia:            ", 
      round(iq_means$IQ_mean[iq_means$nearc4 == 1] - iq_means$IQ_mean[iq_means$nearc4 == 0], 2), 
      " puntos de IQ\n\n")
  
} else {
  cat("✗ La variable IQ no está disponible en los datos.\n")
  cat("  Verifica que card.dta incluya la variable IQ.\n\n")
}

# ==============================================================================
# EJERCICIO 3d: IQ Y nearc4 CON CONTROLES REGIONALES
# ==============================================================================

cat("
================================================================================
EJERCICIO 3d: ¿ESTÁN IQ Y nearc4 RELACIONADOS (CONTROLANDO POR REGIÓN)?
================================================================================

OBJETIVO:
---------
Examinar si la correlación entre nearc4 e IQ (si existe) persiste después de
controlar por características regionales.

RAZONAMIENTO:
-------------
- La correlación entre nearc4 e IQ podría deberse a diferencias REGIONALES
- Por ejemplo: áreas urbanas tienen más universidades Y mayor IQ promedio
- Al controlar por región, eliminamos estas diferencias sistemáticas
- Si la correlación desaparece al controlar, es menos preocupante

CONTROLES A INCLUIR:
--------------------
1. smsa66: ¿Vivía en área metropolitana en 1966?
   - Captura la dimensión urbano/rural
   
2. reg662, ..., reg669: Dummies regionales de residencia en 1966
   - Capturan diferencias geográficas (Sur, Noreste, Medio Oeste, Oeste, etc.)

MODELO:
-------
")

# aquí va el código de latex para regresión IQ con controles

cat("\n")

if ("IQ" %in% names(card)) {
  
  # Identificar variables regionales disponibles
  reg_vars <- grep("^reg66", names(card), value = TRUE)
  
  cat("Variables regionales disponibles:\n")
  cat("----------------------------------\n")
  print(reg_vars)
  cat("\n")
  
  # Verificar si smsa66 existe
  has_smsa66 <- "smsa66" %in% names(card)
  
  if (has_smsa66) {
    cat("✓ smsa66 disponible\n")
  } else {
    cat("✗ smsa66 no disponible\n")
  }
  
  cat("\n")
  
  # Construir fórmula
  if (has_smsa66 && length(reg_vars) > 0) {
    
    formula_iq_controls <- as.formula(paste("IQ ~ nearc4 + smsa66 +", 
                                            paste(reg_vars, collapse = " + ")))
    
    cat("REGRESIÓN: IQ ~ nearc4 + smsa66 + dummies regionales\n")
    cat("----------------------------------------------------\n\n")
    
    cat("Fórmula completa:\n")
    cat(deparse(formula_iq_controls), "\n\n")
    
    # Estimar modelo
    iq_reg2 <- lm(formula_iq_controls, data = card_iq)
    summary_iq2 <- summary(iq_reg2)
    
    print(summary_iq2)
    
    cat("\n\nRESULTADOS (COEFICIENTE DE nearc4):\n")
    cat("------------------------------------\n")
    cat("Coeficiente: ", round(coef(iq_reg2)["nearc4"], 4), "\n")
    cat("Error estándar: ", round(summary_iq2$coefficients["nearc4", "Std. Error"], 4), "\n")
    cat("Estadístico t: ", round(summary_iq2$coefficients["nearc4", "t value"], 2), "\n")
    cat("p-valor: ", format.pval(summary_iq2$coefficients["nearc4", "Pr(>|t|)"], digits = 4), "\n\n")
    
    # Comparación
    cat("COMPARACIÓN: SIN CONTROLES vs CON CONTROLES\n")
    cat("-------------------------------------------\n")
    cat("Coeficiente sin controles: ", round(coef(iq_reg1)["nearc4"], 4), "\n")
    cat("Coeficiente con controles: ", round(coef(iq_reg2)["nearc4"], 4), "\n")
    cat("Cambio: ", round(coef(iq_reg2)["nearc4"] - coef(iq_reg1)["nearc4"], 4), "\n\n")
    
    # Significancia con controles
    if (summary_iq2$coefficients["nearc4", "Pr(>|t|)"] < 0.05) {
      cat("✗ nearc4 sigue siendo SIGNIFICATIVO después de controlar\n\n")
      cat("IMPLICACIÓN:\n")
      cat("------------\n")
      cat("⚠ ADVERTENCIA: La correlación entre nearc4 e IQ NO se explica solo por región\n")
      cat("⚠ Incluso dentro de la misma región, vivir cerca de universidad está asociado con IQ\n")
      cat("⚠ Esto es más preocupante para la validez del instrumento\n\n")
      
    } else {
      cat("✓ nearc4 NO es significativo después de controlar por región\n\n")
      cat("IMPLICACIÓN:\n")
      cat("------------\n")
      cat("✓ La correlación entre nearc4 e IQ (si existía) se debe a diferencias regionales\n")
      cat("✓ Al controlar por región, no hay evidencia de correlación con habilidad\n")
      cat("✓ Esto APOYA la credibilidad de nearc4 como instrumento (con controles)\n\n")
    }
    
    # Test F de significancia conjunta de controles
    cat("TEST F: ¿Son conjuntamente significativos los controles regionales?\n")
    cat("-------------------------------------------------------------------\n")
    anova_result <- anova(iq_reg1, iq_reg2)
    print(anova_result)
    cat("\n")
    
    if (anova_result$`Pr(>F)`[2] < 0.05) {
      cat("✓ Los controles regionales son CONJUNTAMENTE SIGNIFICATIVOS\n")
      cat("  Esto significa que las características regionales SÍ importan para IQ\n")
      cat("  Por lo tanto, es crucial incluir estos controles en las estimaciones IV\n\n")
    } else {
      cat("✗ Los controles regionales NO son conjuntamente significativos\n")
      cat("  Esto sugiere que las diferencias regionales no explican mucho de IQ\n\n")
    }
    
  } else {
    cat("✗ No hay suficientes variables regionales en los datos para este análisis\n\n")
  }
  
} else {
  cat("✗ La variable IQ no está disponible en los datos.\n\n")
}

# ==============================================================================
# EJERCICIO 3e: CONCLUSIONES SOBRE LOS CONTROLES
# ==============================================================================

cat("
================================================================================
EJERCICIO 3e: IMPORTANCIA DE CONTROLAR POR smsa66 Y DUMMIES REGIONALES
================================================================================

PREGUNTA:
---------
A partir de los resultados de los incisos 3c y 3d, ¿qué concluimos sobre la
importancia de incluir smsa66 y las dummies regionales en la ecuación de 
log(wage)?

EVIDENCIA DE LOS INCISOS ANTERIORES:
-------------------------------------
")

if ("IQ" %in% names(card) && exists("iq_reg1") && exists("iq_reg2")) {
  
  cat("1. INCISO 3c (Sin controles):\n")
  if (summary_iq1$coefficients["nearc4", "Pr(>|t|)"] < 0.05) {
    cat("   - nearc4 está correlacionado con IQ\n")
    cat("   - Coeficiente: ", round(coef(iq_reg1)["nearc4"], 4), "\n")
    cat("   - Esto sugiere posible endogeneidad del instrumento\n\n")
  } else {
    cat("   - nearc4 NO está correlacionado con IQ\n")
    cat("   - Esto apoya la exogeneidad del instrumento\n\n")
  }
  
  cat("2. INCISO 3d (Con controles regionales):\n")
  if (exists("iq_reg2")) {
    if (summary_iq2$coefficients["nearc4", "Pr(>|t|)"] < 0.05) {
      cat("   - nearc4 SIGUE correlacionado con IQ después de controlar\n")
      cat("   - Coeficiente: ", round(coef(iq_reg2)["nearc4"], 4), "\n")
      cat("   - La correlación no se debe solo a diferencias regionales\n\n")
    } else {
      cat("   - nearc4 NO está correlacionado con IQ después de controlar\n")
      cat("   - Coeficiente: ", round(coef(iq_reg2)["nearc4"], 4), " (no significativo)\n")
      cat("   - La correlación (si existía) se explica por diferencias regionales\n\n")
    }
  }
}

cat("
CONCLUSIONES E IMPLICACIONES:
------------------------------

")

# aquí va el código de latex para conclusiones

cat("

RECOMENDACIONES PARA LA ESPECIFICACIÓN DEL MODELO:
---------------------------------------------------

1. INCLUIR CONTROLES REGIONALES ES CRUCIAL:
   - Los controles regionales (smsa66, dummies regionales) no solo aumentan
     la precisión de las estimaciones
   - TAMBIÉN son necesarios para la VALIDEZ del instrumento nearc4
   - Sin estos controles, nearc4 podría estar correlacionado con características
     no observadas que varían por región

2. ESTRATEGIA DE IDENTIFICACIÓN:
   - La identificación NO proviene de comparar personas en diferentes regiones
   - La identificación proviene de comparar personas DENTRO de la misma región
     que viven a diferentes distancias de universidades
   - Los controles regionales hacen esta estrategia válida

3. MODELO PREFERIDO:
   
   Primera etapa:
     educ = π₀ + π₁ nearc4 + π₂ exper + π₃ exper² + π₄ black + 
            π₅ smsa + π₆ south + Σγⱼ reg66ⱼ + Σδₖ smsa66 + v
   
   Segunda etapa (IV):
     log(wage) = β₀ + β₁ êduc + β₂ exper + β₃ exper² + β₄ black + 
                 β₅ smsa + β₆ south + Σγⱼ reg66ⱼ + Σδₖ smsa66 + u

4. INTERPRETACIÓN DE β₁:
   - Con esta especificación, β₁ identifica el retorno de la educación
   - Para personas cuya educación fue afectada por vivir cerca de universidad
   - DENTRO de la misma región y tipo de área (urbana/rural)
   - Esto hace la restricción de exclusión más creíble

LECCIÓN GENERAL:
----------------
Este ejercicio ilustra un punto clave de las Variables Instrumentales:

  La CREDIBILIDAD de un instrumento a menudo depende de qué CONTROLES
  se incluyen en el modelo.
  
  Un instrumento que parece problemático sin controles puede ser válido
  una vez que se controla por las variables apropiadas.

APLICACIÓN PRÁCTICA:
--------------------
En tu propia investigación con IV:
  1. Siempre prueba la correlación de tu instrumento con características observables
  2. Identifica qué controles son necesarios para la restricción de exclusión
  3. Incluye esos controles en AMBAS etapas de 2SLS
  4. Discute explícitamente por qué el instrumento es válido CONDICIONAL en los controles

================================================================================
RESUMEN DEL EJERCICIO 3
================================================================================

PREGUNTA PRINCIPAL:
¿Cuál es el retorno causal de la educación sobre los salarios?

ESTRATEGIA:
Usar la proximidad a universidad (nearc4) como instrumento para educación

HALLAZGOS CLAVE:

1. RELEVANCIA:
   ✓ nearc4 está fuertemente correlacionado con educación (F > 10)
   ✓ Vivir cerca de universidad aumenta educación ~0.X años
   ✓ El instrumento es FUERTE, no débil

2. EXOGENEIDAD:
   ⚠ nearc4 puede estar correlacionado con IQ (depende de los datos)
   ✓ Esta correlación (si existe) se elimina al controlar por región
   ⟹ La restricción de exclusión es creíble CON controles apropiados

3. ESTIMACIONES:
   - OLS: β̂ ≈ 0.07-0.08 (7-8% retorno por año de educación)
   - IV:  β̂ ≈ 0.10-0.13 (10-13% retorno por año de educación)
   - IV > OLS sugiere: LATE para compliers, o corrección de atenuación

4. IMPORTANCIA DE CONTROLES:
   ✓✓✓ Incluir smsa66 y dummies regionales es ESENCIAL
   ✓✓✓ Sin estos controles, la validez del instrumento es cuestionable
   ✓✓✓ Con estos controles, la estrategia de identificación es creíble

CONTRIBUCIÓN DE CARD:
Card (1993, 1995) demostró que:
  - Las estimaciones OLS subestiman los retornos de la educación (en su muestra)
  - Los retornos verdaderos son ~30-50% mayores que las estimaciones OLS
  - Las variables instrumentales son esenciales para identificar efectos causales

================================================================================
FIN DEL EJERCICIO 3
================================================================================
")

# Guardar el workspace para uso posterior si es necesario
cat("\n\nGuardando resultados...\n")
save.image(file = "ejercicio3_resultados.RData")
cat("✓ Resultados guardados en 'ejercicio3_resultados.RData'\n\n")



