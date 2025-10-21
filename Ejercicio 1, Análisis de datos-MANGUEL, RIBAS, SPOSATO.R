# ============================================
# ANÁLISIS DE DATASET
# Dataset: International Football Results (1872–2025)
# ============================================

# Cargamos librerías ---------------------------
library(tidyverse)
library(lubridate)

# Cargamos dataset -----------------------------
df <- read_csv("results.csv")

# 3. Cambiamos el formato de la fecha ----------------------------
df <- df %>%
  mutate(date = ymd(date))

# Verificamos que no tengamos NAs en los datos
df %>% summarise(across(everything(), ~ sum(is.na(.))))

# Estructura
glimpse(df)

# Variable de goles totales
df <- df %>%
  mutate(total_goals = home_score + away_score)

# 4. Grafico previo ----------------------
# Histograma de goles totales por partido
ggplot(df, aes(x = total_goals)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribución de goles totales por partido",
       x = "Goles totales",
       y = "Frecuencia") +
  theme_minimal()

# ================================
# PREGUNTA 1 ---------------------
# ¿Cómo evolucionó el promedio de goles totales por década?
# ================================

df_decades <- df %>%
  mutate(decade = floor(year(date) / 10) * 10) %>%
  group_by(decade) %>%
  summarise(avg_goals = mean(total_goals, na.rm = TRUE),
            n_matches = n()) %>%
  filter(!is.na(decade))

# Gráfico
ggplot(df_decades, aes(x = decade, y = avg_goals)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(size = 3, color = "red") +
  labs(title = "Promedio de goles por partido a lo largo del tiempo",
       subtitle = "Agrupado por décadas",
       x = "Década",
       y = "Promedio de goles") +
  theme_minimal()

# ================================
# PREGUNTA 2 ---------------------
# ¿Ser local hace que tengas mejor ventaja de goles? ¿Cambio con el tiempo?
# ================================

df_diff <- df %>%
  mutate(goal_diff = home_score - away_score,
         decade = floor(year(date) / 10) * 10) %>%
  group_by(decade) %>%
  summarise(avg_diff = mean(goal_diff, na.rm = TRUE),
            n = n()) %>%
  filter(!is.na(decade))

# Gráfico 1
ggplot(df_diff, aes(x = decade, y = avg_diff)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 3, color = "orange") +
  labs(title = "Evolución de la ventaja del local (diferencia de goles)",
       x = "Década",
       y = "Promedio de (home_score - away_score)") +
  theme_minimal()
# ================================
# PREGUNTA 3 ---------------------
# ¿Hay torneos donde se hacen mas goles que otros?
# ================================

# Seleccionamos los 10 torneos que más aparecen
top_tournaments <- df %>%
  count(tournament, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(tournament)

df_tourney <- df %>%
  filter(tournament %in% top_tournaments)

# Gráfico 2
ggplot(df_tourney, aes(x = reorder(tournament, total_goals, median), y = total_goals)) +
  geom_violin(fill = "coral", alpha = 0.7) +
  coord_flip() +
  labs(title = "Distribución de goles totales por tipo de torneo",
       x = "Torneo",
       y = "Goles totales por partido") +
  theme_minimal()
# ================================
