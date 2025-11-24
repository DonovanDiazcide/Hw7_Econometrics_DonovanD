# ==============================================================================
# ECONOMETRÍA 1 - ACTIVIDAD 7
# EJERCICIOS 1-2: TEORÍA DE VARIABLES INSTRUMENTALES
# ==============================================================================
# Dr. Francisco Cabrera
# División de Economía - CDE
#
# Este script contiene las respuestas detalladas y didácticas de los ejercicios
# 1 y 2 sobre Variables Instrumentales (IV), endogeneidad y validez de instrumentos.
# ==============================================================================

# Cargar paquetes necesarios
library(pacman)
p_load(tidyverse)

# ==============================================================================
# EJERCICIO 1: EFECTO DE PC SOBRE GPA
# ==============================================================================
# Contexto: Queremos estimar el efecto de tener una computadora personal (PC)
# sobre el promedio de calificaciones universitarias (GPA):
#
#   GPA = β₀ + β₁ PC + u
#
# donde PC es una variable binaria (0 = no tiene PC, 1 = tiene PC)
# ==============================================================================

cat("
================================================================================
EJERCICIO 1a: ¿Por qué PC podría estar correlacionada con u?
================================================================================

CONCEPTO CLAVE: ENDOGENEIDAD
-----------------------------
Para que los estimadores de MCO (Mínimos Cuadrados Ordinarios) sean insesgados
y consistentes, necesitamos que se cumpla el SUPUESTO DE EXOGENEIDAD:

  E(u | PC) = 0    o equivalentemente    Cov(PC, u) = 0

Este supuesto dice que el error (u) no debe estar correlacionado con la 
variable explicativa (PC). Cuando este supuesto NO se cumple, decimos que
la variable es ENDÓGENA.

¿POR QUÉ PC PODRÍA SER ENDÓGENA?
---------------------------------
Hay varias razones por las cuales PC podría estar correlacionada con u:

1. HABILIDAD NO OBSERVADA (Ability Bias):
   - Los estudiantes más hábiles o motivados tienen mayor probabilidad de:
     a) Comprar/tener una PC (por iniciativa propia o apoyo familiar)
     b) Obtener mejores calificaciones (GPA más alto)
   - Esta 'habilidad' está en el término de error u, pero afecta tanto a PC
     como a GPA, creando una correlación espuria.

2. ANTECEDENTES FAMILIARES:
   - Familias con mayor capital cultural y económico:
     a) Pueden comprar PCs para sus hijos
     b) Proveen un mejor ambiente educativo (libros, tutorías, etc.)
   - Estos factores familiares están en u y afectan tanto PC como GPA.

3. MOTIVACIÓN Y PREFERENCIAS:
   - Estudiantes más motivados académicamente:
     a) Buscan herramientas como PCs para estudiar mejor
     b) Naturalmente obtienen mejores calificaciones
   - La motivación no se observa pero correlaciona PC con GPA.

CONSECUENCIA:
-------------
Si Cov(PC, u) ≠ 0, entonces el estimador de MCO será SESGADO e INCONSISTENTE:

")

# aquí va el código de latex para E1a

cat("
Esto significa que β̂₁_MCO NO estima el efecto causal verdadero de tener una PC
sobre el GPA, sino una mezcla del efecto causal y la correlación espuria.

================================================================================
")

cat("
================================================================================
EJERCICIO 1b: ¿Es el ingreso parental un buen instrumento (IV) para PC?
================================================================================

CONCEPTO CLAVE: VARIABLES INSTRUMENTALES (IV)
----------------------------------------------
Una Variable Instrumental (IV) es una variable Z que nos ayuda a 'limpiar' la
endogeneidad de X cuando queremos estimar el efecto de X sobre Y.

Para que Z sea un INSTRUMENTO VÁLIDO, debe cumplir DOS condiciones:

")

# aquí va el código de latex para condiciones IV

cat("

CONDICIÓN 1: RELEVANCIA (Relevance)
------------------------------------
El instrumento debe estar correlacionado con la variable endógena X.
En nuestro caso: ¿Está el ingreso parental correlacionado con PC?

RESPUESTA: SÍ, claramente.
- Familias con mayor ingreso tienen mayor probabilidad de comprar PCs
- Esta es una relación económica directa y obvia
- Podemos verificarlo con una regresión de 'primera etapa':
    PC = π₀ + π₁(ingreso_parental) + v

✓ CUMPLE LA CONDICIÓN DE RELEVANCIA

CONDICIÓN 2: EXOGENEIDAD (Exclusion Restriction)
-------------------------------------------------
El instrumento NO debe estar correlacionado con el error u, excepto a través
de su efecto sobre X. Es decir:

  Cov(ingreso_parental, u) = 0

Esto significa que el ingreso parental solo afecta al GPA a través de PC,
NO directamente.

PREGUNTA: ¿Es esto creíble?

RESPUESTA: NO, probablemente NO cumple esta condición.

RAZONES por las que el ingreso parental podría afectar directamente al GPA:

1. ACCESO A RECURSOS EDUCATIVOS:
   - Familias de mayor ingreso pueden pagar tutorías privadas
   - Pueden comprar más libros y materiales educativos
   - Pueden pagar colegios privados de mejor calidad
   - Estos factores mejoran el GPA independientemente de tener una PC

2. AMBIENTE FAMILIAR:
   - Mayor ingreso → menos estrés financiero familiar
   - Padres con más educación (correlacionado con ingreso)
   - Mejor nutrición y salud → mejor rendimiento académico
   
3. NETWORKING Y OPORTUNIDADES:
   - Conexiones sociales que ayudan al estudiante
   - Experiencias enriquecedoras (viajes, actividades extracurriculares)

4. EXPECTATIVAS Y PRESIÓN:
   - Diferentes expectativas familiares según nivel socioeconómico

TODOS estos factores están en el término de error u y son afectados por el
ingreso parental, violando la restricción de exclusión.

✗ NO CUMPLE LA CONDICIÓN DE EXOGENEIDAD

CONCLUSIÓN:
-----------
El ingreso parental NO es un buen instrumento para PC porque:
- ✓ Cumple relevancia (correlacionado con PC)
- ✗ NO cumple exogeneidad (afecta directamente al GPA por múltiples canales)

Este es un ejemplo clásico de un 'BAD INSTRUMENT': está correlacionado con
la variable endógena, pero también está correlacionado con el error.

================================================================================
")

cat("
================================================================================
EJERCICIO 1c: Usar grants aleatorios como Variable Instrumental
================================================================================

CONCEPTO CLAVE: ALEATORIZACIÓN COMO FUENTE DE EXOGENEIDAD
----------------------------------------------------------

CONTEXTO:
---------
Hace 4 años, la universidad otorgó apoyos (grants) para comprar computadoras
a aproximadamente la mitad de los estudiantes de nuevo ingreso, y estos
estudiantes fueron ELEGIDOS AL AZAR.

Llamemos a esta variable: GRANT (1 = recibió apoyo, 0 = no recibió apoyo)

¿POR QUÉ GRANT ES UN BUEN INSTRUMENTO?
---------------------------------------

La ALEATORIZACIÓN es la clave:

1. EXOGENEIDAD POR CONSTRUCCIÓN:
   - Como los grants se asignaron AL AZAR, la asignación es INDEPENDIENTE
     de todas las características de los estudiantes
   - No importa la habilidad, motivación, ingreso familiar, etc.
   - Por construcción: Cov(GRANT, u) = 0
   
   Esto es similar a un experimento clínico: el tratamiento (grant) es
   independiente de las características del paciente (estudiante).

2. RELEVANCIA:
   - Los estudiantes que recibieron grants tienen mayor probabilidad de
     tener una PC (pueden comprarla con el apoyo)
   - Cov(GRANT, PC) > 0
   - Podemos verificarlo empíricamente

")

# aquí va el código de latex para la estrategia IV

cat("

CÓMO CONSTRUIR Y USAR LA VARIABLE INSTRUMENTAL:
------------------------------------------------

PASO 1: Crear la variable instrumental
---------------------------------------
La variable GRANT ya está construida:
  GRANT_i = 1  si el estudiante i recibió el apoyo hace 4 años
  GRANT_i = 0  si el estudiante i NO recibió el apoyo

Esta variable es BINARIA (0 o 1).

PASO 2: Verificar las condiciones del instrumento
--------------------------------------------------

A) Verificar RELEVANCIA (testeable):
   - Correr la regresión de 'primera etapa':
     PC_i = π₀ + π₁ GRANT_i + v_i
   
   - Probar H₀: π₁ = 0 vs H₁: π₁ ≠ 0
   - Si π₁ es estadísticamente significativo y el F-estadístico > 10,
     entonces tenemos un instrumento RELEVANTE
   
   En R:
   first_stage <- lm(PC ~ GRANT, data = datos)
   summary(first_stage)

B) Verificar EXOGENEIDAD (no testeable directamente):
   - Por la aleatorización, asumimos que Cov(GRANT, u) = 0
   - Podemos hacer pruebas de balance: comparar características observables
     entre el grupo que recibió GRANT y el que no
   - Si las características son similares, es evidencia de que la
     aleatorización funcionó

PASO 3: Estimar el modelo usando IV
------------------------------------

Hay dos formas equivalentes de estimar el efecto causal de PC sobre GPA:

MÉTODO 1: Estimador de Wald (para instrumento binario)
------------------------------------------------------
")

# aquí va el código de latex para Wald estimator

cat("

En R:
# Calcular medias por grupo
media_GPA_grant1 <- mean(datos$GPA[datos$GRANT == 1])
media_GPA_grant0 <- mean(datos$GPA[datos$GRANT == 0])
media_PC_grant1  <- mean(datos$PC[datos$GRANT == 1])
media_PC_grant0  <- mean(datos$PC[datos$GRANT == 0])

# Estimador de Wald
beta1_Wald <- (media_GPA_grant1 - media_GPA_grant0) / 
              (media_PC_grant1 - media_PC_grant0)

MÉTODO 2: Estimador IV general (Two-Stage Least Squares - 2SLS)
----------------------------------------------------------------
")

# aquí va el código de latex para 2SLS

cat("

En R usando el paquete fixest:
library(fixest)
modelo_IV <- feols(GPA ~ 1 | PC ~ GRANT, data = datos)
summary(modelo_IV)

INTERPRETACIÓN DEL RESULTADO:
------------------------------
El coeficiente β₁ estimado por IV representa el EFECTO CAUSAL LOCAL (LATE)
de tener una PC sobre el GPA, específicamente para los 'compliers':

COMPLIERS = estudiantes cuya decisión de tener PC fue afectada por el grant
          = estudiantes que tienen PC porque recibieron el grant, pero que
            no habrían tenido PC sin el grant

Este es el grupo de estudiantes para quienes el instrumento es relevante.

NOTA IMPORTANTE:
----------------
El efecto causal estimado por IV puede ser diferente del efecto promedio
poblacional (ATE) si hay heterogeneidad en los efectos. IV estima el
Local Average Treatment Effect (LATE), no el ATE.

VENTAJAS DE ESTA ESTRATEGIA:
-----------------------------
✓ Exogeneidad garantizada por aleatorización
✓ No requiere supuestos adicionales fuertes
✓ Interpretación causal clara
✓ Similar a un experimento aleatorizado

DESVENTAJAS Y CONSIDERACIONES:
-------------------------------
- Solo podemos usar datos de estudiantes que ingresaron hace 4 años
- Pueden haber graduado, abandonado, o aún estar estudiando
- El efecto puede ser específico a ese cohorte
- Si muchos estudiantes con GRANT = 0 también compraron PCs (por otros medios),
  el instrumento puede ser DÉBIL (primera etapa débil)

================================================================================
")

cat("
================================================================================
RESUMEN DEL EJERCICIO 1
================================================================================

1. PC es ENDÓGENA porque está correlacionada con habilidad no observada,
   antecedentes familiares, y motivación (todos en el error u).

2. El ingreso parental NO es un buen IV porque aunque está correlacionado
   con PC (relevancia ✓), también afecta directamente al GPA por múltiples
   canales (exogeneidad ✗).

3. Los grants aleatorios SON un buen IV porque:
   - Relevancia ✓: afectan la probabilidad de tener PC
   - Exogeneidad ✓: por aleatorización, son independientes de u
   
   Podemos usar el estimador de Wald o 2SLS para estimar el efecto causal.

================================================================================
")

# ==============================================================================
# EJERCICIO 2: DERIVACIÓN FORMAL DEL ESTIMADOR IV
# ==============================================================================

cat("
================================================================================
EJERCICIO 2: DERIVACIONES ALGEBRAICAS DEL ESTIMADOR IV Y WALD
================================================================================

Este ejercicio muestra formalmente cómo funciona el estimador de Variables
Instrumentales y conecta la teoría general con el caso especial del estimador
de Wald para instrumentos binarios.

================================================================================
EJERCICIO 2a: Derivación del estimador IV
================================================================================

PROBLEMA:
---------
Queremos estimar:
  y = β₀ + β₁ x + u

pero x está correlacionada con u: Cov(x, u) ≠ 0 (endogeneidad)

Tenemos un instrumento z tal que:
  - Cov(z, u) = 0   (exogeneidad)
  - Cov(z, x) ≠ 0   (relevancia)

OBJETIVO:
---------
Mostrar que el estimador IV tiene la forma:
")

# aquí va el código de latex para la fórmula del estimador IV

cat("

DERIVACIÓN:
-----------

PASO 1: Partir de los momentos poblacionales
")

# aquí va el código de latex para momento poblacional

cat("

PASO 2: Usar el supuesto de exogeneidad del instrumento
")

# aquí va el código de latex para aplicar exogeneidad

cat("

PASO 3: Derivar el análogo muestral (método de momentos)
")

# aquí va el código de latex para análogo muestral

cat("

PASO 4: Manipulación algebraica
")

# aquí va el código de latex para manipulación algebraica

cat("

INTERPRETACIÓN:
---------------
El estimador IV tiene una forma intuitiva:

  β̂₁_IV = Cov(z,y) / Cov(z,x)

Numéricamente:
- Numerador: ¿Cuánto co-varía el instrumento z con el resultado y?
- Denominador: ¿Cuánto co-varía el instrumento z con la variable endógena x?

La razón de estas covarianzas nos da el efecto causal de x sobre y.

¿POR QUÉ FUNCIONA?
------------------
El instrumento z 'captura' la variación en x que es EXÓGENA (no correlacionada
con u). Esta variación exógena se usa para identificar el efecto causal.

================================================================================
EJERCICIO 2b: Estimador de Wald para instrumento binario
================================================================================

CONTEXTO:
---------
Ahora consideremos el caso especial donde z es una variable BINARIA (0 o 1).
Ejemplos: recibir un tratamiento, vivir cerca de una universidad, recibir
un grant, etc.

Este es exactamente el caso del Ejercicio 1c (GRANT es binario).

OBJETIVO:
---------
Mostrar que cuando z es binario, el estimador IV se simplifica al
estimador de Wald:
")

# aquí va el código de latex para el estimador de Wald

cat("

donde:
- ȳ₁, x̄₁ = promedios de y y x para el grupo con z = 1
- ȳ₀, x̄₀ = promedios de y y x para el grupo con z = 0

DERIVACIÓN:
-----------

PASO 1: Expresar la covarianza para variable binaria
")

# aquí va el código de latex para covarianza con binaria

cat("

PASO 2: Calcular E(z) cuando z es binaria
")

# aquí va el código de latex para E(z) binaria

cat("

PASO 3: Calcular Cov(z, y) cuando z es binaria
")

# aquí va el código de latex para Cov(z,y) binaria

cat("

PASO 4: Calcular Cov(z, x) de manera similar
")

# aquí va el código de latex para Cov(z,x) binaria

cat("

PASO 5: Sustituir en la fórmula del estimador IV
")

# aquí va el código de latex para sustituir y simplificar

cat("

INTERPRETACIÓN DEL ESTIMADOR DE WALD:
--------------------------------------

El estimador de Wald tiene una interpretación muy intuitiva:

  β̂₁ = [Diferencia en Y entre grupos] / [Diferencia en X entre grupos]
      = [Efecto reducido (reduced form)] / [Primera etapa (first stage)]

EFECTO REDUCIDO (Reduced Form):
  ȳ₁ - ȳ₀ = Diferencia promedio en el resultado Y entre quienes tienen z=1
            y quienes tienen z=0

PRIMERA ETAPA (First Stage):
  x̄₁ - x̄₀ = Diferencia promedio en el tratamiento X entre quienes tienen z=1
            y quienes tienen z=0

INTUICIÓN:
----------
Supongamos que el instrumento z aumenta la probabilidad de recibir el
tratamiento X. El estimador de Wald divide:
  - El efecto total del instrumento sobre Y (efecto reducido)
  - Por el efecto del instrumento sobre X (primera etapa)

Esto nos da el efecto de X sobre Y.

EJEMPLO NUMÉRICO:
-----------------
Supongamos (datos del ejercicio de grants en clase):
- Efecto reducido: Las personas con z=1 ganan $1000 más que las con z=0
- Primera etapa: Las personas con z=1 tienen 0.5 más años de educación

Entonces:
  β̂₁ = 1000 / 0.5 = 2000

Interpretación: Un año adicional de educación aumenta el salario en $2000.

CONEXIÓN CON 2SLS (Two-Stage Least Squares):
---------------------------------------------

El estimador de Wald es equivalente a estimar 2SLS:

ETAPA 1: Proyectar x sobre z
  x̂ᵢ = π̂₀ + π̂₁ zᵢ

ETAPA 2: Regresar y sobre x̂
  yᵢ = β̂₀ + β̂₁ x̂ᵢ + εᵢ

Cuando z es binario, β̂₁ de 2SLS = estimador de Wald.

VENTAJAS DEL ESTIMADOR DE WALD:
--------------------------------
1. SIMPLICIDAD: Solo requiere calcular 4 medias
2. INTERPRETACIÓN CLARA: Diferencia de diferencias
3. TRANSPARENCIA: Fácil de explicar y verificar
4. CONEXIÓN CON EXPERIMENTOS: Similar al análisis Intention-to-Treat (ITT)

APLICACIÓN AL EJERCICIO 1c:
----------------------------
Para el ejemplo de grants:
")

# aquí va el código de latex para aplicación al ejercicio 1c

cat("

Este es exactamente el código que usaríamos en R para implementarlo:

# Calcular medias por grupo
media_GPA_grant1 <- mean(datos$GPA[datos$GRANT == 1])
media_GPA_grant0 <- mean(datos$GPA[datos$GRANT == 0])
media_PC_grant1  <- mean(datos$PC[datos$GRANT == 1])
media_PC_grant0  <- mean(datos$PC[datos$GRANT == 0])

# Estimador de Wald
beta1_Wald <- (media_GPA_grant1 - media_GPA_grant0) / 
              (media_PC_grant1 - media_PC_grant0)

cat('Efecto estimado de PC sobre GPA:', beta1_Wald, '\\n')

================================================================================
")

cat("
================================================================================
RESUMEN DEL EJERCICIO 2
================================================================================

1. El estimador IV general tiene la forma:
   β̂₁ = Σ(zᵢ - z̄)(yᵢ - ȳ) / Σ(zᵢ - z̄)(xᵢ - x̄) = Cov(z,y) / Cov(z,x)

2. Cuando el instrumento z es BINARIO, el estimador IV se simplifica al
   estimador de WALD:
   β̂₁ = (ȳ₁ - ȳ₀) / (x̄₁ - x̄₀)

3. El estimador de Wald tiene una interpretación intuitiva:
   - Numerador: Efecto del instrumento sobre el resultado (efecto reducido)
   - Denominador: Efecto del instrumento sobre el tratamiento (primera etapa)

4. Esta derivación muestra por qué IV funciona: usa la variación EXÓGENA
   en x (inducida por z) para identificar el efecto causal.

5. El estimador de Wald fue sugerido por primera vez por Wald (1940) y
   sigue siendo fundamental en econometría aplicada, especialmente en
   el análisis de experimentos y diseños quasi-experimentales.

================================================================================
")

cat("
================================================================================
CONCEPTOS CLAVE PARA ESTUDIAR
================================================================================

ENDOGENEIDAD:
  Cuando Cov(x, u) ≠ 0, x está correlacionada con el error.
  Causas: variable omitida, error de medición, simultaneidad.

VARIABLE INSTRUMENTAL:
  Una variable z que satisface:
  1. Relevancia: Cov(z, x) ≠ 0
  2. Exogeneidad: Cov(z, u) = 0

ESTIMADOR IV:
  β̂₁ = Cov(z, y) / Cov(z, x)
  Usa variación exógena para identificar efectos causales.

ESTIMADOR DE WALD:
  Para z binario: β̂₁ = (ȳ₁ - ȳ₀) / (x̄₁ - x̄₀)
  Interpretación: efecto reducido / primera etapa

TWO-STAGE LEAST SQUARES (2SLS):
  Etapa 1: x̂ = π₀ + π₁ z
  Etapa 2: y = β₀ + β₁ x̂ + ε

LATE (Local Average Treatment Effect):
  IV identifica el efecto para 'compliers', no necesariamente el ATE.

INSTRUMENTOS BUENOS vs MALOS:
  Buenos: aleatorización, discontinuidades, shocks exógenos
  Malos: variables que afectan y directamente (violan exogeneidad)

================================================================================
FIN DEL SCRIPT - EJERCICIOS 1 Y 2
================================================================================
")