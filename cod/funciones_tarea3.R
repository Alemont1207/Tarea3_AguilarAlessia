# alerta.movil
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
benford.mad <- function(x) {
  x <- x[!is.na(x) & x > 0]
  digito <- x %/% 10^floor(log10(x))
  p.obs <- tabulate(as.integer(digito), nbins = 9) / length(digito)
  p.benford <- log10(1 + 1 / (1:9))
  mean(abs(p.obs - p.benford))
}

# benford.canton
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

# valor.acumulado
valor.acumulado <- function(df, tasa.anual, canal) {
  d <- df[df$canal == canal, ]
  mes <- as.integer(format(as.Date(d$fecha), "%m"))
  
  monto <- tapply(as.numeric(d$monto_crc), factor(mes, levels = 1:12), sum)
  monto <- as.numeric(monto)
  monto[is.na(monto)] <- 0
  
  tasa.mensual <- (1 + tasa.anual)^(1 / 12) - 1
  fac <- (1 + tasa.mensual)^(12 - 1:12)
  
  tabla <- data.frame(mes = 1:12, monto = monto, factor = fac)
  va <- sum(tabla$monto * tabla$factor)
  
  list(tabla = tabla,
       tasa.mensual = tasa.mensual,
       va = va,
       interes = va - sum(tabla$monto))
}