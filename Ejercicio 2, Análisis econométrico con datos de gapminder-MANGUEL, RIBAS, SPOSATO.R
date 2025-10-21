# EJERCICIO 2 #


#descargamos el dataset y las librerias que vamos a usar

library(tidyverse)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
gapminder <- read_csv("/Users/alanmanguel/Downloads/gapminder (2).xlsx")
gapminder <- gapminder_2_

View(gapminder) #confirmamos que el dataset esta descargado

# Inciso 1 #

# Filtramos para usar solo los datos de Argentina

datos_argentina <- gapminder %>% filter(country == "Argentina") %>% 
  select(year,income_per_person)
View(datos_argentina)
# Obtenemos una tabla con el GDP per capita de Argentina y los años 

# Graficamos la evolución del ingreso per capita en Argentina

ggplot(datos_argentina, aes(x = year, y = income_per_person)) +
  geom_line(color = "blue", linewidth = 1.5) +
  labs(
    title = "Evolución del ingreso per cápita en Argentina",
    x = "Año",
    y = "Ingreso per capita"
  ) 

# Inciso 2 #

# Separamos los datos entre train y test

# Para test: elegimos los últimos 10 años
test_data <- tail(datos_argentina, 10)

# Para train usamos el resto de los años
train_data <- head(datos_argentina, - 10)

# Nombramos las variables dependientes e independientes de test y de train
x_train <- train_data$year
y_train <- train_data$income_per_person
x_test  <- test_data$year
y_test  <- test_data$income_per_person

# Estimamos los tres modelos, regresando a income_per_person sobre
# el tiempo.

# Modelo lineal
fit_lineal <- lm(y_train ~ x_train)
summary (fit_lineal)

# Modelo polinómico grado 2
fit_poly2 <- lm(y_train ~ poly(x_train, 2, raw = TRUE))
summary(fit_poly2)

# Modelo polinómico grado 10
fit_poly10 <- lm(y_train ~ poly(x_train, 10, raw = TRUE))
summary(fit_poly10)


# Obtenemos el resumen de los datos de cada regresion

# Creamos una tabla de años para lograr visualizarlo mejor
x_grid <- seq(min(datos_argentina$year), max(datos_argentina$year), length.out = 400)
grid <- data.frame(year = x_grid)

# A partir de la tabla predecimos los modelos
grid$lineal <- predict(fit_lineal, newdata = data.frame(x_train = x_grid))
grid$poly2  <- predict(fit_poly2 ,  newdata = data.frame(x_train = x_grid))
grid$poly10 <- predict(fit_poly10, newdata = data.frame(x_train = x_grid))

# Graficamos los modelos en train e incluimos datos de train y de test 
ggplot() +
  geom_point(data = train_data, aes(x = year, y = income_per_person), color = "green", size = 3, alpha = 0.7) +
  geom_point(data = test_data, aes(x = year, y = income_per_person), color = "black", size = 3, alpha = 0.7) +
  geom_line(data = grid, aes(x = year, y = lineal, color = "Lineal"), size = 1.5) +
  geom_line(data = grid, aes(x = year, y = poly2, color = "Grado 2"), size = 1.5) +
  geom_line(data = grid, aes(x = year, y = poly10, color = "Grado 10"), size = 1.5) +
  labs(
    title = "Income per person en Argentina",
    subtitle = "Verde = train, Negro = test",
    x = "Año",
    y = "Ingreso per capita",
    color = "Modelo"
  ) +
  theme_minimal()


# para ver si hay overfitting, usamos la raíz del error cuadrático medio
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))

rmse_train <- c(
  Lineal = rmse(y_train, fitted(fit_lineal)),
  Grado2 = rmse(y_train, fitted(fit_poly2)),
  Grado10 = rmse(y_train, fitted(fit_poly10))
)

rmse_test <- c(
  Lineal = rmse(y_test, predict(fit_lineal, newdata = data.frame(x_train = x_test))),
  Grado2 = rmse(y_test, predict(fit_poly2,  newdata = data.frame(x_train = x_test))),
  Grado10 = rmse(y_test, predict(fit_poly10, newdata = data.frame(x_train = x_test)))
)

cat("\nRMSE en TRAIN:\n"); print(rmse_train)
cat("\nRMSE en TEST:\n"); print(rmse_test)

#Notamos que el modelo polinómico de grado 10 tiene overfitting.

# Inciso 3 #

# Elegimos Colombia, Chile, Bolivia y Ecuador

#Filtramos el dataset para los paises que elegimos
paises <- c("Argentina", "Colombia", "Bolivia", "Chile", "Ecuador")
sudamericanos <- gapminder %>% filter(country %in% paises)


# Poenmos los elegidos en formato ancho
sudamericanos<- sudamericanos %>%
  select(year, country, income_per_person) %>%
  pivot_wider(names_from = country, values_from = income_per_person)
View(sudamericanos_ancho)

# Armamos la matriz de correlaciones (entre los elegidos y Argentina)
cor_ingresos_sudamericanos <- cor(sudamericanos_ancho[ , -1])  # Sacamos la columna 'year'
print(cor_ingresos_sudamericanos)

# Inciso B, calculamos las variaciones porcentuales anuales

# Calculamos el crecimiento interanual para cada país
crecimiento_sudamericanos <- sudamericanos_ancho %>%
  arrange(year) %>%
  mutate(across(-year, ~ (.-lag(.)) / lag(.))) #realiza el lag a los ingresos per capita excepto a la columna year

# Eliminamos los NA
crecimiento_sudamericanos <- na.omit(crecimiento_sudamericanos)

# Calculamos la matriz de correlación del crecimiento
cor_crecimiento <- cor(crecimiento_sudamericanos[ , -1])
print(cor_crecimiento)

## PARTE 2 ##

#Inciso 5
# Elegimos el año 1997
#filtramos el dataset por año, life_expectancy y life_expectancy_female

gapminder_97 <- gapminder %>%
  filter(year == 1997) %>%  
  na.omit() %>%   
  mutate(
    life_expectancy = as.numeric(life_expectancy), 
    life_expectancy_female = as.numeric(life_expectancy_female),
    income_per_person = as.numeric(income_per_person) 
  ) 

View(gapminder_97)

# Excluimos los NAs y nos aseguramos de que las variables 
# sean números


#Graficamos 

ggplot(gapminder_97, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(title = "Relación entre esperanza de vida de las mujeres y en general (1997)",
       x = "Esperanza de vida - Mujeres",
       y = "Esperanza de vida - General") +
  theme_minimal()

#Inciso 6
# Hacemos la regresion lineal simple
modelo_simple <-  lm(life_expectancy ~ life_expectancy_female, data = gapminder_97)
summary(modelo_simple)

#Inciso 7
# Creamos una nueva variable que sea la diferencia entre life_expectancy y life_expectancy_female

gapminder_97 <- gapminder_97 %>% mutate(diferencia = life_expectancy_female - life_expectancy
)

# Realizamos un Test de T de la muestra sobre la diferencia
t_test <- t.test(gapminder_97$diferencia, mu = 0, alternative = "greater")

print(t_test)

# Rechazamos H0 (a un nivel de significancia de 5% ), por lo tanto life expectancy female es mayor que life expectancy

#Inciso 8
# Estimamos la regresion multiple de de life expectancy sobre life expectancy female e income per person.
modelo_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = gapminder_97)
summary(modelo_multiple)

#Comparamos ambos modelos
modelo_simple <-  lm(life_expectancy ~ life_expectancy_female, data = gapminder_97)
summary(modelo_simple)
modelo_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person, data = gapminder_97)
summary(modelo_multiple)

#Inciso 9

#elegimos income_per_person y life_expectancy_male
gapminder_97 <- gapminder_97 %>%
  filter(year == 1997) %>%
  mutate(
    income_per_person = as.numeric(income_per_person),
    life_expectancy_male = as.numeric(life_expectancy_male),
  ) %>% na.omit()

modelo_doble <- lm(life_expectancy ~ life_expectancy_male + income_per_person, data = gapminder_97)
summary(modelo_doble)

