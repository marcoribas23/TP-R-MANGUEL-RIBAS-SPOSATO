TP – R-MANGUEL, RIBAS, SPOSATO

Cómo correr:

Instalar los paquetes necesarios:

install.packages(c("tidyverse", "lubridate", "ggplot2", "readr", "dplyr", "tidyr"))


En cada script está incluido un set.seed() y los pasos están comentados.

Correr los scripts en el siguiente orden:

Ejercicio 1: análisis de datos (ejercicio1_futbol.R)

Ejercicio 2: análisis econométrico (ejercicio2_gapminder.R)

Ejercicio 3: simulación (ejercicio3_teg.R)

Los gráficos se muestran directamente en la consola de RStudio y algunos resultados se imprimen con print() o View().

Aclaración: debido a que el TP fue hecho en diferentes computadoras, se debe tener en cuenta esto a la hora de subir los datasets para correr los comandos.

Ejercicio 1 — Análisis de datos

Dataset: International Football Results (1872–2025)
Se analizó un dataset de resultados de fútbol internacional.

Preguntas:

¿Cómo evolucionó el promedio de goles totales por década?

¿Ser local da ventaja en el resultado? ¿Cambió con el tiempo?

¿Qué torneos tienen más goles por partido?

Se usaron funciones de tidyverse para limpiar datos, crear variables (como total_goals), y graficar con ggplot2.
Incluye histogramas y líneas de tendencia por década.

Ejercicio 2 — Análisis econométrico (Gapminder)

Dataset: Gapminder
Se analizaron ingresos y esperanza de vida de distintos países.

Parte 1 — Ingreso per cápita:

Se graficó la evolución del ingreso de Argentina en el tiempo.
Se estimaron tres modelos (lineal, polinómico grado 2 y grado 10) y se compararon con RMSE para ver el sobreajuste.
Se calcularon correlaciones entre países sudamericanos y también entre sus tasas de crecimiento anual.

Parte 2 — Esperanza de vida y género:

Se graficó la relación entre la esperanza de vida general y femenina (año 1997).
Se hizo una regresión simple y luego una múltiple controlando por ingreso.
Se realizó un test t para verificar si la esperanza de vida femenina es mayor que la general.

Ejercicio 3 — Simulación (Juego TEG)

Se programó una simulación del ataque en el juego TEG.
Se creó una función resultado_ataque() que compara dados del atacante y defensor.
Con simular_batalla() se simula una batalla completa hasta que uno gana.
Finalmente, con probabilidad_ataque() se estimó la probabilidad de que el atacante gane cuando ambos comienzan con 5 fichas (usando 10.000 simulaciones).