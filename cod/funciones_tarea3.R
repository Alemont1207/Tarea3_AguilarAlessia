# alerta.movil
# Recibe:   df         data frame de transacciones (columnas fecha, id_tx)
#           ventana    número de días anteriores para medir lo "normal"
#           k.alerta   número de desviaciones estándar sobre la media
# Devuelve: data frame con fecha, n.tx, media, desv, alerta
#           (los primeros 'ventana' días quedan en NA: no se evalúan)
alerta.movil <- function(df, ventana, k.alerta) {
  conteo <- aggregate(id_tx ~ fecha, data = df, FUN = length)
  names(conteo) <- c("fecha", "n.tx")
  conteo$fecha <- as.Date(conteo$fecha)
  conteo <- conteo[order(conteo$fecha), ]
  
  n <- nrow(conteo)
  conteo$media  <- NA_real_
  conteo$desv   <- NA_real_
  conteo$alerta <- NA
  
  for (t in (ventana + 1):n) {
    x <- conteo$n.tx[(t - ventana):(t - 1)]
    conteo$media[t]  <- mean(x)
    conteo$desv[t]   <- sd(x)
    conteo$alerta[t] <- conteo$n.tx[t] > conteo$media[t] + k.alerta * conteo$desv[t]
  }
  conteo
}

# benford.mad
# Recibe:   x  vector numérico de montos
# Devuelve: un número, la MAD (desviación absoluta media) entre la proporción
#           observada de primeros dígitos (1 a 9) y la proporción de Benford
benford.mad <- function(x) {
  x <- x[!is.na(x) & x > 0]
  digito <- x %/% 10^floor(log10(x))
  p.obs <- tabulate(as.integer(digito), nbins = 9) / length(digito)
  p.benford <- log10(1 + 1 / (1:9))
  mean(abs(p.obs - p.benford))
}

# benford.canton
# Recibe:   df        data frame de transacciones (cod_canton, canal, monto_crc)
#           canal     nombre del canal a auditar (texto)
#           cantones  data frame con cod_canton y canton
# Devuelve: data frame con cod_canton, canton, n, mad y clase (Nigrini),
#           ordenado de mayor a menor MAD
benford.canton <- function(df, canal, cantones) {
  d <- df[df$canal == canal, ]
  partes <- split(d$monto_crc, d$cod_canton)
  
  res <- data.frame(
    cod_canton = as.integer(names(partes)),
    n          = as.integer(unname(sapply(partes, length))),
    mad        = as.numeric(unname(sapply(partes, benford.mad)))
  )
  
  res$clase <- as.character(cut(
    res$mad,
    breaks = c(-Inf, 0.006, 0.012, 0.015, Inf),
    labels = c("conformidad cercana", "conformidad aceptable",
               "conformidad marginal", "no conformidad")
  ))
  
  res <- merge(res, cantones[, c("cod_canton", "canton")], by = "cod_canton")
  res <- res[order(-res$mad), c("cod_canton", "canton", "n", "mad", "clase")]
  rownames(res) <- NULL
  res
}