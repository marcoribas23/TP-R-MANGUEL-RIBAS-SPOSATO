#Ejercicio 3: Simulación de ataque en juego TEG
# Simulación de ataque en TEG

set.seed(1234)

# Inciso 1
# Función: resultado_ataque(att_dice, def_dice)
# Devuelve un vector c(perdidas_atacante, perdidas_defensor) para UNA ronda.
# Reglas:
# - Se tiran "att_dice" y "def_dice" dados (1..6).
# - Se ordenan de mayor a menor y se comparan por posición.
# - En cada comparación, si hay empate o el dado del defensor es mayor,
#   pierde 1 ficha el atacante; si el del atacante es mayor, pierde 1 el defensor.

resultado_ataque <- function(att_dice, def_dice) {
  if (att_dice <= 0 || def_dice <= 0) return(c(0L, 0L))
  a <- sort(sample(1:6, att_dice, replace = TRUE), decreasing = TRUE)
  d <- sort(sample(1:6, def_dice, replace = TRUE), decreasing = TRUE)
  n <- min(length(a), length(d))
  comp <- a[1:n] - d[1:n]
  perdidas_atacante <- sum(comp <= 0)
  perdidas_defensor <- sum(comp > 0)
  c(perdidas_atacante = as.integer(perdidas_atacante),
    perdidas_defensor = as.integer(perdidas_defensor))
}

#Pruebo los comandos con datos:
# Ejecutar una comparación con 3 dados atacante y 2 del defensor
resultado_ataque(3, 2)
# Resultado esperado: vector c(perdidas_atacante = X, perdidas_defensor = Y)

# Inciso 2
# Función: simular_batalla(fichas_atacante, fichas_defensor)
# Simula una batalla completa.
# Reglas:
# - El atacante ataca hasta conquistar (defensor llega a 0) o quedarse sin poder atacar (<= 1 ficha).
# - Por ronda: atacante tira hasta 3 dados pero debe dejar una ficha atrás -> min(3, fichas_atacante - 1)
#              defensor tira hasta 3 dados -> min(3, fichas_defensor)
# Devuelve una lista con el resultado final.

simular_batalla <- function(fichas_atacante, fichas_defensor) {
  fa <- as.integer(fichas_atacante)
  fd <- as.integer(fichas_defensor)
  if (fa < 1 || fd < 0) stop("Entradas inválidas.")
  while (fa > 1 && fd > 0) {
    dados_a <- min(3L, fa - 1L)
    dados_d <- min(3L, fd)
    perdidas <- resultado_ataque(dados_a, dados_d)
    fa <- fa - perdidas["perdidas_atacante"]
    fd <- fd - perdidas["perdidas_defensor"]
  }
  list(
    fichas_atacante_final = fa,
    fichas_defensor_final = fd,
    atacante_conquista = (fd == 0L)
  )
}

#Pruebo los comandos con datos:
# Simular batalla empezando con 5 fichas atacante y 3 defensor
simular_batalla(5, 3)
# Devuelve una lista: fichas_atacante_final, fichas_defensor_final, atacante_conquista (TRUE/FALSE)

# Inciso 3
# Función: probabilidad_ataque(n_sim = 1000, atacante_inicial = 5, defensor_inicial = 5)
# Corre n_sim batallas y estima P(atacante conquista el territorio).
# Devuelve la proporción de conquistas.

probabilidad_ataque <- function(n_sim = 1000, atacante_inicial = 5, defensor_inicial = 5) {
  conquistas <- 0L
  for (i in seq_len(n_sim)) {
    res <- simular_batalla(atacante_inicial, defensor_inicial)
    conquistas <- conquistas + as.integer(res$atacante_conquista)
  }
  conquistas / n_sim
}

# Estimación precisa (10000 simulaciones)
probabilidad_ataque(10000, 5, 5)


