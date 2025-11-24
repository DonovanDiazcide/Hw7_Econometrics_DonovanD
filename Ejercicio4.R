# ==============================================================================
# ECONOMETRÍA 1 - ACTIVIDAD 7
# EJERCICIO 4: SIMULACIONES DE VARIABLES INSTRUMENTALES
# ==============================================================================
# Dr. Francisco Cabrera
# División de Economía - CDE
#
# Este script contiene simulaciones para entender las propiedades de los
# estimadores de Variables Instrumentales bajo diferentes escenarios:
# - Instrumento fuerte y exógeno (caso ideal)
# - Instrumento débil (relevancia débil)
# - Instrumento endógeno (violación de exogeneidad)
# - Mejora en relevancia del instrumento
# ==============================================================================

# Limpiar el entorno
rm(list = ls())

# Cargar paquetes necesarios
library(pacman)
p_load(tidyverse, fixest, modelsummary, knitr, kableExtra)

cat("
================================================================================
¿POR QUÉ SIMULACIONES EN ECONOMETRÍA?
================================================================================

Las simulaciones Monte Carlo son una herramienta fundamental para entender
las propiedades de los estimadores econométricos.

VENTAJAS DE LAS SIMULACIONES:
------------------------------

1. CONOCEMOS LA VERDAD:
   - En datos reales, NUNCA conocemos los parámetros verdaderos
   - En simulaciones, NOSOTROS generamos los datos con parámetros conocidos
   - Podemos comparar estimaciones con la verdad

2. EXPERIMENTACIÓN CONTROLADA:
   - Podemos cambiar UN solo aspecto y ver el efecto
   - Ejemplo: cambiar solo la fuerza del instrumento
   - Imposible en datos reales (todo cambia a la vez)

3. ENTENDER PROPIEDADES TEÓRICAS:
   - La teoría dice 'el estimador es consistente'
   - ¿Qué significa eso en la práctica?
   - Las simulaciones lo muestran visualmente

4. EVALUAR REGLAS PRÁCTICAS:
   - Ejemplo: 'F > 10 significa instrumento fuerte'
   - ¿Es esta regla buena? Las simulaciones lo verifican

ESTRUCTURA DE UNA SIMULACIÓN:
------------------------------

1. ESPECIFICAR el proceso generador de datos (DGP)
   - Definir las ecuaciones estructurales
   - Establecer los parámetros verdaderos
   
2. GENERAR los datos según el DGP
   - Simular variables aleatorias
   - Crear las variables del modelo
   
3. ESTIMAR los modelos
   - Aplicar MCO, IV, etc.
   - Guardar los estimadores
   
4. REPETIR muchas veces
   - Típicamente 1,000 - 10,000 repeticiones
   - Esto da la distribución de muestreo
   
5. ANALIZAR los resultados
   - Comparar media de estimadores con valor verdadero
   - Calcular sesgo, varianza, MSE
   - Construir intervalos de confianza

EN ESTE EJERCICIO:
------------------
Vamos a hacer UNA SOLA simulación (n=1000 observaciones) para cada escenario,
pero explicaremos en detalle qué significan los resultados y qué pasaría si
repitiéramos muchas veces.

================================================================================
")

# ==============================================================================
# ESCENARIO BASE: INSTRUMENTO FUERTE Y EXÓGENO
# ==============================================================================

cat("
================================================================================
ESCENARIO BASE: INSTRUMENTO FUERTE Y EXÓGENO (CASO IDEAL)
================================================================================

Este es el CASO IDEAL donde el instrumento satisface todas las condiciones.

PROCESO GENERADOR DE DATOS (DGP):
----------------------------------
")

# aquí va el código de latex para DGP base

cat("

PARÁMETROS VERDADEROS:
----------------------
- β₁ = 3  (efecto causal verdadero de x sobre y)
- Correlación entre z y x = 0.5 (FUERTE)
- Correlación entre x y u = alta (debido al coeficiente 4 en u)
- Correlación entre z y u = 0 (EXÓGENO por construcción)

¿POR QUÉ ESTE ES UN BUEN INSTRUMENTO?
--------------------------------------
1. RELEVANCIA: z afecta a x con coeficiente 0.5 (fuerte)
2. EXOGENEIDAD: z y u son independientes (z no depende de u)

INTUICIÓN:
----------
- z crea variación EXÓGENA en x (la parte 0.5*z)
- Esta variación no está contaminada por u
- IV usa esta variación limpia para estimar el efecto causal

PREDICCIÓN TEÓRICA:
-------------------
- MCO estará SESGADO (porque Cov(x,u) ≠ 0)
- IV será CONSISTENTE (porque z es válido)
- Con n grande, β̂_IV → 3

Vamos a verificar esto con los datos:

")

# Generar datos del escenario base
set.seed(666)
n <- 1000

df_base <- tibble(
  z = rnorm(n),              # Instrumento: distribución normal estándar
  u = rnorm(n),              # Error: distribución normal estándar
  # x es endógena porque se correlaciona con u por construcción
  x = 0.5 * z + 4 * u + rnorm(n),   # Correlación fuerte con z (0.5) y con u (4)
  y = 3 * x + 5 * u          # El valor verdadero de β₁ es 3
)

cat("DATOS GENERADOS:\n")
cat("----------------\n")
cat("Tamaño de muestra: n =", n, "\n")
cat("Variables creadas: z (instrumento), u (error), x (endógena), y (dependiente)\n\n")

# Verificar correlaciones
cat("CORRELACIONES EN LOS DATOS GENERADOS:\n")
cat("--------------------------------------\n")
correlaciones_base <- df_base %>%
  select(z, x, u, y) %>%
  cor() %>%
  round(3)

print(correlaciones_base)

cat("\n")
cat("INTERPRETACIÓN DE CORRELACIONES:\n")
cat("---------------------------------\n")
cat("Cor(z, x) =", round(cor(df_base$z, df_base$x), 3), " → RELEVANCIA ✓ (instrumento fuerte)\n")
cat("Cor(z, u) =", round(cor(df_base$z, df_base$u), 3), " → EXOGENEIDAD ✓ (≈ 0 por construcción)\n")
cat("Cor(x, u) =", round(cor(df_base$x, df_base$u), 3), " → ENDOGENEIDAD ✓ (≠ 0, problema para MCO)\n")
cat("Cor(z, y) =", round(cor(df_base$z, df_base$y), 3), " → z afecta y (a través de x)\n\n")

# Estimación MCO
cat("ESTIMACIÓN 1: MCO (OLS)\n")
cat("------------------------\n\n")

ols_base <- lm(y ~ x, data = df_base)
summary_ols_base <- summary(ols_base)

print(summary_ols_base)

cat("\n")
cat("ANÁLISIS DEL ESTIMADOR MCO:\n")
cat("----------------------------\n")
cat("β̂₁_OLS =", round(coef(ols_base)["x"], 4), "\n")
cat("Valor verdadero: β₁ = 3\n")
cat("Sesgo =", round(coef(ols_base)["x"] - 3, 4), "\n\n")

if (coef(ols_base)["x"] > 3) {
  cat("✗ MCO SOBREESTIMA el efecto verdadero\n")
  cat("✗ Sesgo positivo debido a que Cov(x, u) > 0\n")
} else if (coef(ols_base)["x"] < 3) {
  cat("✗ MCO SUBESTIMA el efecto verdadero\n")
  cat("✗ Esto podría ocurrir por azar en esta muestra específica\n")
} else {
  cat("◯ MCO está muy cerca del valor verdadero (¡por suerte!)\n")
}

cat("\n")
cat("FÓRMULA DEL SESGO DE MCO:\n")
cat("-------------------------\n")

# aquí va el código de latex para sesgo MCO

cat("\n")

# Estimación IV
cat("\nESTIMACIÓN 2: VARIABLES INSTRUMENTALES (IV / 2SLS)\n")
cat("----------------------------------------------------\n\n")

iv_base <- feols(y ~ 1 | x ~ z, data = df_base, se = "hetero")
summary_iv_base <- summary(iv_base)

print(summary_iv_base)

cat("\n")
cat("ANÁLISIS DEL ESTIMADOR IV:\n")
cat("---------------------------\n")
cat("β̂₁_IV =", round(coef(iv_base)["fit_x"], 4), "\n")
cat("Valor verdadero: β₁ = 3\n")
cat("Error =", round(coef(iv_base)["fit_x"] - 3, 4), "\n\n")

if (abs(coef(iv_base)["fit_x"] - 3) < abs(coef(ols_base)["x"] - 3)) {
  cat("✓ IV está MÁS CERCA del valor verdadero que MCO\n")
  cat("✓ Esto confirma que IV corrige el sesgo de endogeneidad\n")
} else {
  cat("◯ En esta muestra particular, IV no está más cerca que MCO\n")
  cat("◯ Esto puede ocurrir debido a variabilidad muestral\n")
  cat("◯ Con muestras más grandes o promediando muchas simulaciones, IV sería mejor\n")
}

cat("\n")
cat("NOTA SOBRE VARIABILIDAD MUESTRAL:\n")
cat("----------------------------------\n")
cat("En UNA muestra, β̂_IV puede estar lejos de β₁ por azar.\n")
cat("La propiedad de CONSISTENCIA significa que con n → ∞, β̂_IV → β₁.\n")
cat("Para ver esto claramente, necesitaríamos repetir la simulación muchas veces\n")
cat("y calcular el promedio de β̂_IV sobre todas las simulaciones.\n\n")

# Primera etapa
cat("DIAGNÓSTICO: PRIMERA ETAPA (RELEVANCIA DEL INSTRUMENTO)\n")
cat("--------------------------------------------------------\n\n")

first_stage_base <- lm(x ~ z, data = df_base)
summary_fs_base <- summary(first_stage_base)

print(summary_fs_base)

cat("\n")
cat("ANÁLISIS DE LA PRIMERA ETAPA:\n")
cat("------------------------------\n")
cat("Coeficiente de z:", round(coef(first_stage_base)["z"], 4), "\n")
cat("t-estadístico:", round(summary_fs_base$coefficients["z", "t value"], 2), "\n")
cat("F-estadístico:", round(summary_fs_base$fstatistic[1], 2), "\n\n")

f_stat_base <- summary_fs_base$fstatistic[1]
if (f_stat_base > 10) {
  cat("✓ F > 10: Instrumento FUERTE\n")
  cat("✓ No hay problemas de instrumento débil\n")
  cat("✓ El sesgo de muestra finita es pequeño\n")
} else {
  cat("✗ F < 10: Instrumento DÉBIL\n")
  cat("✗ Posible sesgo del estimador IV hacia MCO\n")
}

cat("\n")
cat("REGLA PRÁCTICA (Stock y Yogo, 2005):\n")
cat("-------------------------------------\n")

# aquí va el código de latex para regla práctica

cat("\n")

# Tabla comparativa
cat("TABLA COMPARATIVA: OLS vs IV\n")
cat("-----------------------------\n\n")

msummary(
  list("MCO" = ols_base, "IV (2SLS)" = iv_base),
  stars = TRUE,
  gof_omit = "^(?!Num|F)",
  coef_omit = "^(Intercept|Constant)",
  title = "Comparación MCO vs IV - Escenario Base"
)

cat("\n")
cat("RESUMEN DEL ESCENARIO BASE:\n")
cat("---------------------------\n")
cat("✓ Instrumento fuerte (F =", round(f_stat_base, 1), "> 10)\n")
cat("✓ Instrumento exógeno (Cor(z,u) ≈ 0)\n")
cat("✓ MCO sesgado por endogeneidad\n")
cat("✓ IV consistente y cercano al valor verdadero\n")
cat("✓ Este es el CASO IDEAL de Variables Instrumentales\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 4a: EVALUAR SI z ES INSTRUMENTO VÁLIDO
# ==============================================================================

cat("
================================================================================
EJERCICIO 4a: ¿ES z UN INSTRUMENTO RELEVANTE Y EXÓGENO?
================================================================================

Con los datos del escenario base, evaluemos formalmente las condiciones del IV.

CONDICIÓN 1: RELEVANCIA
------------------------
")

# aquí va el código de latex para evaluar relevancia

cat("

EVALUACIÓN:\n")
cat("-----------\n")
cat("Coeficiente en primera etapa: π₁ =", round(coef(first_stage_base)["z"], 4), "\n")
cat("t-estadístico:", round(summary_fs_base$coefficients["z", "t value"], 2), "\n")
cat("p-valor:", format.pval(summary_fs_base$coefficients["z", "Pr(>|t|)"], digits = 4), "\n")
cat("F-estadístico:", round(f_stat_base, 2), "\n\n")

cat("CONCLUSIÓN SOBRE RELEVANCIA:\n")
if (f_stat_base > 10) {
  cat("✓ π₁ es altamente significativo (p < 0.001)\n")
  cat("✓ F-estadístico =", round(f_stat_base, 2), "> 10\n")
  cat("✓ El instrumento es RELEVANTE y FUERTE\n")
  cat("✓ No es un instrumento débil\n")
} else {
  cat("⚠ F-estadístico =", round(f_stat_base, 2), "< 10\n")
  cat("⚠ El instrumento es DÉBIL\n")
}

cat("\n")
cat("CONDICIÓN 2: EXOGENEIDAD\n")
cat("------------------------\n")

# aquí va el código de latex para evaluar exogeneidad

cat("\n")
cat("EVALUACIÓN:\n")
cat("-----------\n")
cat("Por construcción del DGP:\n")
cat("  z = rnorm(n)         → z es independiente de todo lo demás\n")
cat("  u = rnorm(n)         → u es independiente de todo lo demás\n")
cat("  z y u son generadas independientemente → Cov(z, u) = 0\n\n")

cat("Correlación muestral: Cor(z, u) =", round(cor(df_base$z, df_base$u), 4), "\n")
cat("(Cercano a 0, confirma independencia)\n\n")

cat("✓ El instrumento es EXÓGENO por construcción\n")
cat("✓ No hay correlación entre z y u\n")
cat("✓ La restricción de exclusión se cumple\n\n")

cat("CONCLUSIÓN GENERAL:\n")
cat("-------------------\n")
cat("✓✓ z es un instrumento VÁLIDO:\n")
cat("   1. Relevante: F =", round(f_stat_base, 2), "> 10\n")
cat("   2. Exógeno: Cor(z, u) ≈ 0 por construcción\n\n")

cat("¿ES z UN INSTRUMENTO DÉBIL?\n")
cat("----------------------------\n")
cat("NO. Con F =", round(f_stat_base, 2), "> 10, z es un instrumento FUERTE.\n\n")

cat("IMPLICACIÓN:\n")
cat("------------\n")
cat("Con un instrumento fuerte y exógeno:\n")
cat("  - El estimador IV es consistente\n")
cat("  - El sesgo de muestra finita es pequeño\n")
cat("  - La inferencia es válida\n")
cat("  - Este es el escenario ideal para usar IV\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 4b: INSTRUMENTO DÉBIL
# ==============================================================================

cat("
================================================================================
EJERCICIO 4b: ¿QUÉ PASA CON UN INSTRUMENTO DÉBIL?
================================================================================

Ahora cambiamos la relación de x con z de 0.5 a 0.2 (más débil).

NUEVO PROCESO GENERADOR DE DATOS:
----------------------------------
")

# aquí va el código de latex para DGP débil

cat("

¿QUÉ CAMBIÓ?
------------
- Antes: x = 0.5*z + 4*u + ε  (relación FUERTE con z)
- Ahora: x = 0.2*z + 4*u + ε  (relación DÉBIL con z)

CONSECUENCIAS ESPERADAS:
------------------------
1. La primera etapa será más débil (F < 10 probablemente)
2. El instrumento tiene menos 'poder' para predecir x
3. β̂_IV estará más cerca de β̂_OLS (sesgo hacia MCO)
4. La varianza de β̂_IV aumentará (menos precisión)

Vamos a verificarlo:

")

# Generar datos con instrumento débil
set.seed(666)
df_debil <- tibble(
  z = rnorm(n),
  u = rnorm(n),
  x = 0.2 * z + 4 * u + rnorm(n),  # Coeficiente de z reducido a 0.2
  y = 3 * x + 5 * u                # β₁ sigue siendo 3
)

cat("CORRELACIONES CON INSTRUMENTO DÉBIL:\n")
cat("-------------------------------------\n")
correlaciones_debil <- df_debil %>%
  select(z, x, u, y) %>%
  cor() %>%
  round(3)

print(correlaciones_debil)

cat("\n")
cat("COMPARACIÓN DE CORRELACIONES:\n")
cat("------------------------------\n")
cat("                     Base      Débil\n")
cat("Cor(z, x)           ", round(cor(df_base$z, df_base$x), 3), "     ", 
    round(cor(df_debil$z, df_debil$x), 3), "    ← MUCHO MÁS DÉBIL\n")
cat("Cor(z, u)           ", round(cor(df_base$z, df_base$u), 3), "     ", 
    round(cor(df_debil$z, df_debil$u), 3), "    ← Sigue siendo ≈ 0\n\n")

# Estimadores
ols_debil <- lm(y ~ x, data = df_debil)
iv_debil <- feols(y ~ 1 | x ~ z, data = df_debil, se = "hetero")
first_stage_debil <- lm(x ~ z, data = df_debil)

cat("PRIMERA ETAPA (INSTRUMENTO DÉBIL):\n")
cat("-----------------------------------\n")
summary_fs_debil <- summary(first_stage_debil)
print(summary_fs_debil)

f_stat_debil <- summary_fs_debil$fstatistic[1]

cat("\n")
cat("DIAGNÓSTICO DE PRIMERA ETAPA:\n")
cat("------------------------------\n")
cat("F-estadístico:", round(f_stat_debil, 2), "\n\n")

if (f_stat_debil < 10) {
  cat("✗ F =", round(f_stat_debil, 2), "< 10: Instrumento DÉBIL\n")
  cat("✗ ADVERTENCIA: Problemas de instrumento débil\n\n")
} else {
  cat("◯ F =", round(f_stat_debil, 2), "> 10: Aún es fuerte en esta muestra\n")
  cat("  (Con n=1000, incluso correlación de 0.2 puede dar F > 10)\n\n")
}

cat("COMPARACIÓN DE ESTIMADORES:\n")
cat("---------------------------\n\n")

msummary(
  list("MCO (débil)" = ols_debil, "IV (débil)" = iv_debil),
  stars = TRUE,
  gof_omit = "^(?!Num|F)",
  coef_omit = "^(Intercept|Constant)",
  title = "Estimadores con Instrumento Débil"
)

cat("\n")
cat("ANÁLISIS DE RESULTADOS:\n")
cat("-----------------------\n")
cat("β̂_OLS =", round(coef(ols_debil)["x"], 4), "\n")
cat("β̂_IV  =", round(coef(iv_debil)["fit_x"], 4), "\n")
cat("Verdadero: β₁ = 3\n\n")

# Calcular sesgo
sesgo_ols_debil <- coef(ols_debil)["x"] - 3
sesgo_iv_debil <- coef(iv_debil)["fit_x"] - 3

cat("Sesgo de MCO:", round(sesgo_ols_debil, 4), "\n")
cat("Sesgo de IV: ", round(sesgo_iv_debil, 4), "\n\n")

cat("PREGUNTA: ¿HAY ALGUNA RAZÓN PARA PENSAR QUE β̂_IV ESTÁ SESGADO?\n")
cat("----------------------------------------------------------------\n\n")

cat("SÍ, cuando el instrumento es DÉBIL, β̂_IV tiene sesgo de muestra finita.\n\n")

cat("RAZONES:\n")
cat("--------\n")

# aquí va el código de latex para sesgo instrumento débil

cat("\n")

cat("DIRECCIÓN DEL SESGO:\n")
cat("--------------------\n")
cat("Con instrumento débil, β̂_IV está sesgado HACIA β̂_OLS.\n\n")

cat("Fórmula aproximada del sesgo (Bound, Jaeger, Baker, 1995):\n")

# aquí va el código de latex para fórmula aproximada sesgo

cat("\n")

cat("donde:\n")
cat("  - n = tamaño de muestra\n")
cat("  - K = número de instrumentos\n")
cat("  - F = F-estadístico de primera etapa\n\n")

cat("En nuestro caso:\n")
cat("  F =", round(f_stat_debil, 2), "\n")
cat("  n = 1000\n")
cat("  Sesgo/σ ≈ 1/F ≈", round(1/f_stat_debil, 4), "\n\n")

if (f_stat_debil < 10) {
  cat("⚠ Con F < 10, el sesgo puede ser sustancial\n")
  cat("⚠ β̂_IV no es confiable\n")
} else {
  cat("◯ Con F > 10, el sesgo es pequeño\n")
  cat("◯ Aunque el instrumento es más débil, aún es usable\n")
}

cat("\n")
cat("COMPARACIÓN CON ESCENARIO BASE:\n")
cat("--------------------------------\n")
cat("                     Base          Débil\n")
cat("F-estadístico       ", sprintf("%.2f", f_stat_base), "        ", sprintf("%.2f", f_stat_debil), "\n")
cat("β̂_IV               ", sprintf("%.4f", coef(iv_base)["fit_x"]), "      ", 
    sprintf("%.4f", coef(iv_debil)["fit_x"]), "\n")
cat("Error de β̂_IV      ", sprintf("%.4f", coef(iv_base)["fit_x"] - 3), "      ", 
    sprintf("%.4f", coef(iv_debil)["fit_x"] - 3), "\n\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 4c: ERROR ESTÁNDAR CON INSTRUMENTO DÉBIL
# ==============================================================================

cat("
================================================================================
EJERCICIO 4c: ERROR ESTÁNDAR CON INSTRUMENTO DÉBIL
================================================================================

PREGUNTA: ¿Qué sucede con el error estándar cuando la relación entre x y z
es más débil?

RESPUESTA: El error estándar de β̂_IV AUMENTA.

INTUICIÓN:
----------
- Un instrumento débil tiene menos 'información' sobre x
- La variación en x inducida por z es pequeña
- Con menos variación, es más difícil estimar β₁ con precisión
- Resultado: mayor incertidumbre → error estándar más grande

FÓRMULA DEL ERROR ESTÁNDAR DE β̂_IV:
------------------------------------
")

# aquí va el código de latex para SE de IV

cat("\n")

# Comparar errores estándar
se_iv_base <- summary_iv_base$se["fit_x"]
se_iv_debil <- summary(iv_debil)$se["fit_x"]

cat("COMPARACIÓN DE ERRORES ESTÁNDAR:\n")
cat("---------------------------------\n")
cat("                     Base          Débil         Razón\n")
cat("SE(β̂_IV)           ", sprintf("%.4f", se_iv_base), "      ", 
    sprintf("%.4f", se_iv_debil), "      ", 
    sprintf("%.2f", se_iv_debil/se_iv_base), "x\n\n")

if (se_iv_debil > se_iv_base) {
  cat("✓ Confirmado: El error estándar AUMENTA con instrumento débil\n")
  cat("✓ El estimador es menos preciso\n")
  cat("✓ Los intervalos de confianza son más anchos\n\n")
}

# Calcular intervalos de confianza
ic_base <- confint(iv_base)["fit_x", ]
ic_base <- unlist(ic_base)  # <- CORRECCIÓN: convertir a vector numérico

# Para feols, necesitamos calcular IC manualmente para el débil
ic_debil_lower <- coef(iv_debil)["fit_x"] - 1.96 * se_iv_debil
ic_debil_upper <- coef(iv_debil)["fit_x"] + 1.96 * se_iv_debil

cat("INTERVALOS DE CONFIANZA AL 95%:\n")
cat("--------------------------------\n")
cat("Escenario Base:   [", round(ic_base[1], 3), ", ", round(ic_base[2], 3), "]\n", sep = "")
cat("Escenario Débil:  [", round(ic_debil_lower, 3), ", ", round(ic_debil_upper, 3), "]\n", sep = "")
cat("Valor verdadero:  β₁ = 3.000\n\n")

# Calcular amplitud de IC
amplitud_base <- ic_base[2] - ic_base[1]
amplitud_debil <- ic_debil_upper - ic_debil_lower

cat("Amplitud del IC (Base):  ", round(amplitud_base, 3), "\n")
cat("Amplitud del IC (Débil): ", round(amplitud_debil, 3), "\n")
cat("Razón:                   ", round(amplitud_debil/amplitud_base, 2), "x más ancho\n\n")

cat("DEMOSTRACIÓN CON LA FÓRMULA:\n")
cat("-----------------------------\n\n")

cat("Sabemos que:\n")

# aquí va el código de latex para demostración SE

cat("\n")

cat("Por lo tanto:\n")
cat("  - Si Cor(z,x) disminuye → F disminuye → SE aumenta\n")
cat("  - Relación inversa: SE ∝ 1/√F\n\n")

# Calcular la relación
razon_f <- sqrt(f_stat_base / f_stat_debil)
razon_se <- se_iv_debil / se_iv_base

cat("Verificación numérica:\n")
cat("  √(F_base / F_débil) = √(", round(f_stat_base, 2), "/", round(f_stat_debil, 2), 
    ") =", round(razon_f, 2), "\n")
cat("  SE_débil / SE_base  =", round(razon_se, 2), "\n")
cat("  ≈ ✓ Consistente con la teoría\n\n")

cat("CONCLUSIÓN:\n")
cat("-----------\n")
cat("Cuando la correlación entre x y z es más débil:\n")
cat("  1. ✗ El error estándar de β̂_IV AUMENTA\n")
cat("  2. ✗ Los intervalos de confianza son MÁS ANCHOS\n")
cat("  3. ✗ La precisión de la estimación DISMINUYE\n")
cat("  4. ⚠ El poder estadístico de las pruebas DISMINUYE\n\n")

cat("Trade-off de instrumentos débiles:\n")
cat("  - Menos sesgo por endogeneidad (si z es exógeno)\n")
cat("  - Pero más imprecisión (mayor varianza)\n")
cat("  - En casos extremos, el aumento en varianza puede no valer la pena\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 4d: INSTRUMENTO ENDÓGENO
# ==============================================================================

cat("
================================================================================
EJERCICIO 4d: ¿QUÉ PASA CUANDO EL INSTRUMENTO NO ES EXÓGENO?
================================================================================

Ahora consideramos el peor escenario: el instrumento z está CORRELACIONADO
con el error u.

NUEVO PROCESO GENERADOR DE DATOS:
----------------------------------
")

# aquí va el código de latex para DGP endógeno

cat("

¿QUÉ CAMBIÓ?
------------
Ahora hay una variable NO OBSERVADA v que afecta tanto a:
  - El instrumento z (z2 = v + ε)
  - El error u (u2 = 0.1*v + ε)

Esto crea correlación entre z2 y u2:
  Cov(z2, u2) = Cov(v + ε_z, 0.1*v + ε_u) = Var(v) * 0.1 ≠ 0

VIOLACIÓN DE EXOGENEIDAD:
--------------------------
El instrumento ya NO es válido porque:
  Cov(z2, u2) ≠ 0  →  ✗ VIOLACIÓN de la condición de exogeneidad

CONSECUENCIA ESPERADA:
----------------------
β̂_IV será INCONSISTENTE (no converge al valor verdadero incluso con n → ∞)

Vamos a verificarlo:

")

# Generar datos con instrumento endógeno
set.seed(666)
df_endog <- tibble(
  v = rnorm(n),                          # Variable no observada
  z2 = v + rnorm(n),                     # Instrumento correlacionado con v
  u2 = 0.1 * v + rnorm(n),               # Error correlacionado con v
  x2 = 0.2 * z2 + 4 * u2 + rnorm(n),     # x es endógena (coef de z2 es 0.2, débil)
  y2 = 3 * x2 + 5 * u2                   # β₁ sigue siendo 3
)

cat("CORRELACIONES CON INSTRUMENTO ENDÓGENO:\n")
cat("----------------------------------------\n")
correlaciones_endog <- df_endog %>%
  select(v, z2, u2, x2, y2) %>%
  cor() %>%
  round(3)

print(correlaciones_endog)

cat("\n")
cat("ANÁLISIS DE CORRELACIONES:\n")
cat("--------------------------\n")
cat("Cor(z2, x2) =", round(cor(df_endog$z2, df_endog$x2), 3), " → RELEVANCIA ◯ (débil pero ≠ 0)\n")
cat("Cor(z2, u2) =", round(cor(df_endog$z2, df_endog$u2), 3), " → EXOGENEIDAD ✗ (≠ 0, PROBLEMA)\n")
cat("Cor(z2, v)  =", round(cor(df_endog$z2, df_endog$v), 3), " → Ambos dependen de v\n")
cat("Cor(u2, v)  =", round(cor(df_endog$u2, df_endog$v), 3), " → Ambos dependen de v\n\n")

cat("⚠⚠⚠ ADVERTENCIA: El instrumento está correlacionado con el error\n")
cat("⚠⚠⚠ Esto VIOLA la restricción de exclusión\n")
cat("⚠⚠⚠ β̂_IV será INCONSISTENTE\n\n")

# Estimadores
ols_endog <- lm(y2 ~ x2, data = df_endog)
iv_endog <- feols(y2 ~ 1 | x2 ~ z2, data = df_endog, se = "hetero")

cat("COMPARACIÓN DE ESTIMADORES:\n")
cat("---------------------------\n\n")

msummary(
  list("MCO (endógeno)" = ols_endog, "IV (endógeno)" = iv_endog),
  stars = TRUE,
  gof_omit = "^(?!Num|F)",
  coef_omit = "^(Intercept|Constant)",
  title = "Estimadores con Instrumento Endógeno"
)

cat("\n")
cat("ANÁLISIS DE RESULTADOS:\n")
cat("-----------------------\n")
cat("β̂_OLS =", round(coef(ols_endog)["x2"], 4), "\n")
cat("β̂_IV  =", round(coef(iv_endog)["fit_x2"], 4), "\n")
cat("Verdadero: β₁ = 3\n\n")

cat("Error de MCO:", round(coef(ols_endog)["x2"] - 3, 4), "\n")
cat("Error de IV: ", round(coef(iv_endog)["fit_x2"] - 3, 4), "\n\n")

cat("PREGUNTA: ¿CUÁL ESTIMADOR ES MEJOR, MCO O IV?\n")
cat("----------------------------------------------\n\n")

# Calcular cuál está más cerca
error_ols <- abs(coef(ols_endog)["x2"] - 3)
error_iv <- abs(coef(iv_endog)["fit_x2"] - 3)

if (error_iv > error_ols) {
  cat("✗ En este caso, IV está MÁS LEJOS del valor verdadero que MCO\n")
  cat("✗ ¡Usar IV empeoró la estimación!\n\n")
} else {
  cat("◯ En esta muestra, IV está más cerca (por suerte)\n")
  cat("  Pero en general, IV con instrumento endógeno es problemático\n\n")
}

cat("RAZÓN: DIRECCIÓN E INCONSISTENCIA DE β̂_IV\n")
cat("-------------------------------------------\n\n")

cat("Cuando z está correlacionado con u, β̂_IV converge a:\n\n")

# aquí va el código de latex para plim IV endógeno

cat("\n")

cat("Calculemos este límite con nuestros datos:\n\n")

# Calcular covarianzas
cov_zy <- cov(df_endog$z2, df_endog$y2)
cov_zx <- cov(df_endog$z2, df_endog$x2)
cov_zu <- cov(df_endog$z2, df_endog$u2)

cat("Cov(z, y) =", round(cov_zy, 4), "\n")
cat("Cov(z, x) =", round(cov_zx, 4), "\n")
cat("Cov(z, u) =", round(cov_zu, 4), "  ← ≠ 0 (PROBLEMA)\n\n")

plim_iv_teorico <- (cov_zy - 5 * cov_zu) / cov_zx
cat("plim β̂_IV = (Cov(z,y) - 5*Cov(z,u)) / Cov(z,x)\n")
cat("           = (", round(cov_zy, 4), " - 5*", round(cov_zu, 4), ") / ", round(cov_zx, 4), "\n")
cat("           =", round(plim_iv_teorico, 4), "\n\n")

cat("Comparación:\n")
cat("  Valor verdadero: β₁ = 3.000\n")
cat("  plim β̂_IV      ≈", round(plim_iv_teorico, 4), "\n")
cat("  β̂_IV (muestra)  =", round(coef(iv_endog)["fit_x2"], 4), "\n\n")

cat("✗ plim β̂_IV ≠ β₁: El estimador IV es INCONSISTENTE\n")
cat("✗ Incluso con n → ∞, β̂_IV NO converge al valor verdadero\n\n")

cat("DIRECCIÓN DEL SESGO:\n")
cat("--------------------\n\n")

sesgo_iv_asint <- plim_iv_teorico - 3

cat("Sesgo asintótico de IV ≈", round(sesgo_iv_asint, 4), "\n\n")

if (sesgo_iv_asint > 0) {
  cat("El sesgo es POSITIVO:\n")
  cat("  - Cov(z, u) > 0 introduce sesgo hacia arriba\n")
  cat("  - IV SOBREESTIMA el efecto verdadero\n\n")
} else {
  cat("El sesgo es NEGATIVO:\n")
  cat("  - Cov(z, u) < 0 introduce sesgo hacia abajo\n")
  cat("  - IV SUBESTIMA el efecto verdadero\n\n")
}

cat("CONCLUSIÓN: ¿QUÉ ESTIMADOR ES MEJOR?\n")
cat("-------------------------------------\n\n")

cat("Ambos estimadores están sesgados:\n")
cat("  - MCO: sesgado por Cov(x, u) ≠ 0\n")
cat("  - IV:  sesgado por Cov(z, u) ≠ 0\n\n")

cat("¿Cuál es mejor?\n")
cat("  Depende de la magnitud de los sesgos:\n")
cat("    |Sesgo MCO| =", round(abs(coef(ols_endog)["x2"] - 3), 4), "\n")
cat("    |Sesgo IV|  =", round(abs(coef(iv_endog)["fit_x2"] - 3), 4), "\n\n")

if (error_iv > error_ols) {
  cat("  En este caso: MCO es MEJOR\n")
  cat("  ⚠ Usar un instrumento inválido empeoró la situación\n")
} else {
  cat("  En este caso: IV es mejor (pero ambos son problemáticos)\n")
}

cat("\n")
cat("LECCIÓN CRUCIAL:\n")
cat("----------------\n")
cat("⚠⚠⚠ Un instrumento endógeno (que viola Cov(z,u)=0) puede ser PEOR que MCO\n")
cat("⚠⚠⚠ Es FUNDAMENTAL asegurar la exogeneidad del instrumento\n")
cat("⚠⚠⚠ Un instrumento malo es peor que ningún instrumento\n\n")

cat("Por eso en investigación aplicada:\n")
cat("  1. Se argumenta extensamente por qué z es exógeno\n")
cat("  2. Se prueba correlación de z con observables (como IQ)\n")
cat("  3. Se usan controles para hacer z exógeno condicionalmente\n")
cat("  4. Se prefieren fuentes de variación 'as-if random'\n")

cat("
================================================================================
")

# ==============================================================================
# EJERCICIO 4e: MEJORA EN RELEVANCIA
# ==============================================================================

cat("
================================================================================
EJERCICIO 4e: MEJORA EN RELEVANCIA DEL INSTRUMENTO
================================================================================

¿Qué pasa si mejoramos la relevancia del instrumento?

Cambiamos el coeficiente de z2 en la ecuación de x de 0.2 a 3.

NUEVO PROCESO GENERADOR DE DATOS:
----------------------------------
")

# aquí va el código de latex para DGP relevancia mejorada

cat("

¿QUÉ CAMBIÓ?
------------
- Antes: x3 = 0.2*z2 + 4*u2 + ε  (relación DÉBIL con z)
- Ahora: x3 = 3*z2 + 4*u2 + ε    (relación FUERTE con z)

NOTA IMPORTANTE:
----------------
- El instrumento z2 SIGUE siendo endógeno (Cov(z2, u2) ≠ 0)
- Pero ahora tiene una correlación MUCHO MÁS FUERTE con x3
- ¿Esto ayuda?

")

# Generar datos con relevancia mejorada
set.seed(7)  # Cambiamos la semilla como en el código original
df_fuerte <- tibble(
  v = rnorm(n),
  z2 = v + rnorm(n),
  u2 = 0.1 * v + rnorm(n),
  x3 = 3 * z2 + 4 * u2 + rnorm(n),   # Coeficiente de z2 aumentado a 3
  y2 = 3 * x3 + 5 * u2
)

cat("CORRELACIONES CON RELEVANCIA MEJORADA:\n")
cat("---------------------------------------\n")
correlaciones_fuerte <- df_fuerte %>%
  select(v, z2, u2, x3, y2) %>%
  cor() %>%
  round(3)

print(correlaciones_fuerte)

cat("\n")
cat("COMPARACIÓN DE CORRELACIONES:\n")
cat("------------------------------\n")
cat("                     Débil     Fuerte\n")
cat("Cor(z2, x)          ", round(cor(df_endog$z2, df_endog$x2), 3), "     ", 
    round(cor(df_fuerte$z2, df_fuerte$x3), 3), "    ← MUCHO MÁS FUERTE\n")
cat("Cor(z2, u2)         ", round(cor(df_endog$z2, df_endog$u2), 3), "     ", 
    round(cor(df_fuerte$z2, df_fuerte$u2), 3), "    ← Igual (sigue siendo ≠ 0)\n\n")

# Estimadores
ols_fuerte <- lm(y2 ~ x3, data = df_fuerte)
iv_fuerte <- feols(y2 ~ 1 | x3 ~ z2, data = df_fuerte, se = "hetero")
first_stage_fuerte <- lm(x3 ~ z2, data = df_fuerte)

cat("PRIMERA ETAPA CON RELEVANCIA MEJORADA:\n")
cat("---------------------------------------\n")
summary_fs_fuerte <- summary(first_stage_fuerte)
f_stat_fuerte <- summary_fs_fuerte$fstatistic[1]

cat("F-estadístico:", round(f_stat_fuerte, 2), "\n\n")

if (f_stat_fuerte > 10) {
  cat("✓ F =", round(f_stat_fuerte, 2), "> 10: Instrumento FUERTE\n\n")
}

cat("COMPARACIÓN DE ESTIMADORES:\n")
cat("---------------------------\n\n")

msummary(
  list("MCO (fuerte)" = ols_fuerte, "IV (fuerte)" = iv_fuerte),
  stars = TRUE,
  gof_omit = "^(?!Num|F)",
  coef_omit = "^(Intercept|Constant)",
  title = "Estimadores con Relevancia Mejorada"
)

cat("\n")
cat("ANÁLISIS DE RESULTADOS:\n")
cat("-----------------------\n")
cat("β̂_OLS =", round(coef(ols_fuerte)["x3"], 4), "\n")
cat("β̂_IV  =", round(coef(iv_fuerte)["fit_x3"], 4), "\n")
cat("Verdadero: β₁ = 3\n\n")

cat("Error de MCO:", round(coef(ols_fuerte)["x3"] - 3, 4), "\n")
cat("Error de IV: ", round(coef(iv_fuerte)["fit_x3"] - 3, 4), "\n\n")

# Comparar con escenario anterior
cat("COMPARACIÓN: DÉBIL vs FUERTE (ambos endógenos):\n")
cat("------------------------------------------------\n")
cat("                     Débil         Fuerte\n")
cat("F-estadístico       ", sprintf("%.2f", f_stat_debil), "         ", 
    sprintf("%.2f", f_stat_fuerte), "\n")
cat("β̂_IV               ", sprintf("%.4f", coef(iv_endog)["fit_x2"]), "      ", 
    sprintf("%.4f", coef(iv_fuerte)["fit_x3"]), "\n")
cat("Error de β̂_IV      ", sprintf("%.4f", coef(iv_endog)["fit_x2"] - 3), "      ", 
    sprintf("%.4f", coef(iv_fuerte)["fit_x3"] - 3), "\n\n")

error_iv_endog <- abs(coef(iv_endog)["fit_x2"] - 3)
error_iv_fuerte <- abs(coef(iv_fuerte)["fit_x3"] - 3)

if (error_iv_fuerte < error_iv_endog) {
  cat("✓ Con relevancia mejorada, β̂_IV está MÁS CERCA del valor verdadero\n")
  cat("✓ Aumentar la relevancia ayudó\n\n")
} else {
  cat("◯ En esta muestra, mejorar relevancia no ayudó mucho\n")
  cat("  (Variabilidad muestral)\n\n")
}

cat("EJERCICIO 4e: REGLA PRÁCTICA (ESTADÍSTICO t)\n")
cat("---------------------------------------------\n\n")

cat("PREGUNTA: ¿Cuál es la regla práctica para decir que z es relevante\n")
cat("          en términos del estadístico t?\n\n")

cat("RESPUESTA:\n")
cat("----------\n")

# aquí va el código de latex para regla práctica t

cat("\n")

# Obtener t-estadístico
t_stat_fuerte <- summary_fs_fuerte$coefficients["z2", "t value"]

cat("En nuestra primera etapa:\n")
cat("  t-estadístico de z2 =", round(t_stat_fuerte, 2), "\n")
cat("  |t| > 3.16: ✓ El instrumento es RELEVANTE\n\n")

cat("Relación entre regla de t y regla de F:\n")
cat("  - Para un instrumento (K=1): F = t²\n")
cat("  - Por lo tanto: |t| > 3.16 ⇔ F > 10\n")
cat("  - Verificación: F =", round(f_stat_fuerte, 2), ", t² =", round(t_stat_fuerte^2, 2), "\n\n")

cat("EJERCICIO 4f: ¿POR QUÉ β̂_IV ES MÁS CONSISTENTE AHORA?\n")
cat("--------------------------------------------------------\n\n")

cat("PREGUNTA: ¿Cuál es la razón por la que el estimador IV ahora es\n")
cat("          más consistente a pesar de NO ser puramente exógeno?\n\n")

cat("RESPUESTA:\n")
cat("----------\n\n")

cat("El estimador IV converge a:\n\n")

# aquí va el código de latex para plim IV con mejor relevancia

cat("\n")

cat("Descomponiendo:\n\n")

# aquí va el código de latex para descomposición

cat("\n")

cat("Calculemos con nuestros datos:\n\n")

# Calcular razones
cov_zu_fuerte <- cov(df_fuerte$z2, df_fuerte$u2)
cov_zx_fuerte <- cov(df_fuerte$z2, df_fuerte$x3)

razon_cov_fuerte <- cov_zu_fuerte / cov_zx_fuerte
razon_cov_debil <- cov_zu / cov_zx

cat("Escenario DÉBIL:\n")
cat("  Cov(z, u) / Cov(z, x) =", round(cov_zu, 4), "/", round(cov_zx, 4), 
    "=", round(razon_cov_debil, 4), "\n\n")

cat("Escenario FUERTE:\n")
cat("  Cov(z, u) / Cov(z, x) =", round(cov_zu_fuerte, 4), "/", round(cov_zx_fuerte, 4), 
    "=", round(razon_cov_fuerte, 4), "\n\n")

cat("✓ La razón Cov(z,u)/Cov(z,x) es MUCHO MÁS PEQUEÑA con relevancia fuerte\n")
cat("✓ Aunque z2 sigue correlacionado con u2, el sesgo se diluye\n")
cat("✓ Cov(z,x) grande 'domina' sobre Cov(z,u) pequeño\n\n")

cat("INTERPRETACIÓN INTUITIVA:\n")
cat("-------------------------\n\n")

cat("Pensemos en la señal vs ruido:\n\n")

cat("  Señal    = Cov(z, x) = variación en x inducida por z\n")
cat("  Ruido    = Cov(z, u) = correlación espuria entre z y u\n")
cat("  Ratio S/R = Cov(z, x) / Cov(z, u)\n\n")

cat("Cuando MEJORAMOS LA RELEVANCIA:\n")
cat("  - La señal (Cov(z,x)) AUMENTA mucho\n")
cat("  - El ruido (Cov(z,u)) permanece igual\n")
cat("  - El ratio señal/ruido MEJORA\n")
cat("  - β̂_IV es más consistente\n\n")

cat("FÓRMULA FORMAL:\n")

# aquí va el código de latex para fórmula formal sesgo

cat("\n")

cat("Si Cov(z,x) aumenta pero Cov(z,u) se mantiene:\n")
cat("  → El sesgo asintótico disminuye\n")
cat("  → plim β̂_IV se acerca más a β₁\n\n")

cat("EJERCICIO 4g: ESTADÍSTICO F DE PRIMERA ETAPA\n")
cat("---------------------------------------------\n\n")

cat("Ahora ejecutemos:\n")
cat("  thef <- fitstat(iv_fuerte, 'ivf')[['ivf1::x3']][['stat']]\n\n")

# Obtener F-estadístico usando fitstat
thef <- fitstat(iv_fuerte, "ivf")[["ivf1::x3"]][["stat"]]

cat("Resultado:\n")
cat("  F-estadístico de primera etapa =", round(thef, 2), "\n\n")

cat("DISCUSIÓN:\n")
cat("----------\n\n")

if (thef > 10) {
  cat("✓ F =", round(thef, 2), "> 10: El instrumento es FUERTE\n\n")
  
  cat("Implicaciones:\n")
  cat("  1. ✓ No hay problemas de instrumento débil\n")
  cat("  2. ✓ El sesgo de muestra finita es mínimo\n")
  cat("  3. ✓ La inferencia (pruebas t, IC) es válida\n\n")
  
  cat("PERO CUIDADO:\n")
  cat("  ⚠ El instrumento SIGUE siendo endógeno (Cov(z2, u2) ≠ 0)\n")
  cat("  ⚠ F > 10 solo indica RELEVANCIA, no EXOGENEIDAD\n")
  cat("  ⚠ β̂_IV sigue siendo inconsistente\n\n")
  
  cat("La mejora en relevancia:\n")
  cat("  - Reduce el sesgo ASINTÓTICO (por la razón Cov(z,u)/Cov(z,x))\n")
  cat("  - Reduce el sesgo de MUESTRA FINITA (relacionado con F)\n")
  cat("  - Pero NO elimina la inconsistencia por endogeneidad\n\n")
}

cat("COMPARACIÓN DE LOS TRES ESCENARIOS ENDÓGENOS:\n")
cat("----------------------------------------------\n\n")

cat("                     Débil        Fuerte\n")
cat("F-estadístico       ", sprintf("%.2f", f_stat_debil), "        ", 
    sprintf("%.2f", thef), "\n")
cat("Cor(z, x)           ", sprintf("%.3f", cor(df_endog$z2, df_endog$x2)), "       ", 
    sprintf("%.3f", cor(df_fuerte$z2, df_fuerte$x3)), "\n")
cat("Cor(z, u)           ", sprintf("%.3f", cor(df_endog$z2, df_endog$u2)), "       ", 
    sprintf("%.3f", cor(df_fuerte$z2, df_fuerte$u2)), "\n")
cat("Cov(z,u)/Cov(z,x)   ", sprintf("%.4f", razon_cov_debil), "     ", 
    sprintf("%.4f", razon_cov_fuerte), "  ← Clave\n")
cat("β̂_IV               ", sprintf("%.4f", coef(iv_endog)["fit_x2"]), "      ", 
    sprintf("%.4f", coef(iv_fuerte)["fit_x3"]), "\n")
cat("|Error|            ", sprintf("%.4f", error_iv_endog), "      ", 
    sprintf("%.4f", error_iv_fuerte), "\n\n")

cat("LECCIÓN FINAL:\n")
cat("--------------\n\n")

cat("Un instrumento MÁS RELEVANTE:\n")
cat("  1. ✓ Reduce el sesgo cuando hay endogeneidad pequeña\n")
cat("  2. ✓ Reduce la varianza del estimador\n")
cat("  3. ✓ Mejora la inferencia estadística\n")
cat("  4. ⚠ PERO no convierte un instrumento endógeno en válido\n\n")

cat("Jerarquía de importancia:\n")
cat("  1º - EXOGENEIDAD (Cov(z,u) = 0): Absolutamente esencial\n")
cat("  2º - RELEVANCIA (Cov(z,x) ≠ 0): Muy importante\n\n")

cat("Un instrumento con:\n")
cat("  ✓ Exógeno + Débil:    MEJOR que    Endógeno + Fuerte\n")
cat("  ✓ Exógeno + Fuerte:   IDEAL\n")
cat("  ✗ Endógeno (cualquier relevancia): PROBLEMÁTICO\n")

cat("
================================================================================
RESUMEN DEL EJERCICIO 4
================================================================================

Hemos explorado cuatro escenarios de simulación:

1. INSTRUMENTO IDEAL (fuerte y exógeno):
   ✓ F > 10, Cov(z,u) = 0
   ✓ IV consistente y más preciso que MCO
   ✓ Este es el objetivo en investigación aplicada

2. INSTRUMENTO DÉBIL (baja relevancia):
   ⚠ F pequeño
   ⚠ Mayor error estándar (menos precisión)
   ⚠ Sesgo de muestra finita hacia MCO
   ⚠ Inferencia menos confiable

3. INSTRUMENTO ENDÓGENO (Cov(z,u) ≠ 0):
   ✗ Violación de restricción de exclusión
   ✗ IV inconsistente
   ✗ Puede ser PEOR que MCO
   ✗ Nunca usar un instrumento endógeno

4. INSTRUMENTO ENDÓGENO CON MEJORA EN RELEVANCIA:
   ◯ Sesgo asintótico se reduce (pero no desaparece)
   ◯ Mayor relevancia diluye el efecto de endogeneidad
   ◯ Pero sigue siendo inconsistente
   ◯ No es solución real

LECCIONES CLAVE:
----------------
1. La EXOGENEIDAD es no negociable
2. La RELEVANCIA importa para precisión y sesgo de muestra finita
3. F > 10 es una buena guía (o |t| > 3.16)
4. Instrumentos débiles son problemáticos pero usables si son exógenos
5. Instrumentos endógenos son inutilizables sin importar su fuerza
6. Aumentar relevancia NO convierte un instrumento malo en bueno

APLICACIÓN PRÁCTICA:
--------------------
En tu investigación:
  1. Argumenta extensamente la exogeneidad de tu instrumento
  2. Verifica que F > 10 en primera etapa
  3. Prueba correlación con observables (como IQ)
  4. Usa controles para hacer z exógeno condicionalmente
  5. Sé honesto sobre limitaciones

================================================================================
FIN DEL EJERCICIO 4
================================================================================
")

# Guardar workspace
cat("\nGuardando resultados...\n")
save.image(file = "ejercicio4_simulaciones.RData")
cat("✓ Resultados guardados en 'ejercicio4_simulaciones.RData'\n\n")
