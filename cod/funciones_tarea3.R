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