
# La función sensitivity recrea el data frame que se alimenta a la función de 
# tornado para generar el análisis visual
# high: espera recibir una lista de números igual a la diferencia entre el caso 
# alto de la variable en cuestión y el caso base
# low: espera recibir la lista de números del caso bajo
sensitivity <- function(variables = c("A","B","C","D","E","F"), 
                        high = rnorm(length(variables),95, sd = 50),
                        low = rnorm(length(variables),-95, sd = 50)){
  
  # variables, high, and low
  variable.label <- rep(variables,2)
  type <- c(rep("Alto",length(variables)),rep("Bajo",length(variables)))
  df <- data.frame(x = variable.label, type = type, val = c(high, low))
  df$absval <- rep(abs(high)+abs(low),2)
  df
}

# La función tornado genera el gráfico que muestra qué tan susceptible es el 
# resultado a las variaciones de las variables de entrada 
tornado <- function(df) {
  
  bplot <- ggplot(df, aes(val, reorder(x,absval), fill = type)) + 
    geom_bar(position="identity", stat="identity")
  bplot <- bplot + theme_bw() +
    # label_dollar le da formato de porcentaje al eje X
    scale_x_continuous(labels = label_percent())
  bplot
}


# La función sensitivity_npv está diseñada para llamar la función de npv
# para calcular el valor presente neto utilizando los casos alto y bajo
# especificados para el problema
# El diseño de ésta puede ser mejorado en una siguiente iteración
sensitivity_npv <- function(base, variation, variable, type){
  ult_period = base$valor[base$variable == "Periodo"]
  
  low_tib <- tibble(
    periodo = c(0:(ult_period - 1)),
    cashflow =
    if (variable == "Inversion.Capital") {
        c(as.numeric(-base$valor[base$variable == variable] * 
                       (1 + variation[variation$variable == variable, 
                                      names(variation) == type])), 
          rep(0, ult_period - 1)) +
        (rep(base$valor[base$variable == "Ahorro.Anual"], ult_period) -
           rep(base$valor[base$variable == "Gasto.Anual"], ult_period))
    } else if (variable == "Ahorro.Anual") {
      c(-base$valor[base$variable == "Inversion.Capital"], 
                   rep(0, ult_period - 1)) +
        (rep(as.numeric(base$valor[base$variable == variable] * (1 + 
               variation[variation$variable == variable, 
                         names(variation) == type])), ult_period) -
           rep(base$valor[base$variable == "Gasto.Anual"], ult_period))
    } else if (variable == "Gasto.Anual") {
      c(-base$valor[base$variable == "Inversion.Capital"], 
                   rep(0, ult_period - 1)) +
        (rep(base$valor[base$variable == "Ahorro.Anual"], ult_period) -
           rep(as.numeric(base$valor[base$variable == variable] * (1 +
                 variation[variation$variable == variable, 
                           names(variation) == type])), ult_period))
    } else {
      c(-base_case$valor[base_case$variable == "Inversion.Capital"], 
        rep(0, ult_periodo - 1)) +
        (rep(base_case$valor[base_case$variable == "Ahorro.Anual"], ult_periodo) -
           rep(base_case$valor[base_case$variable == "Gasto.Anual"], ult_periodo))
    }
  )
  
  tmar <- if (variable == "TMAR") {
    as.numeric(base$valor[base$variable == variable] * 
                 (1 +variation[variation$variable == variable, 
                               names(variation) == type]))
  } else {
    base$valor[base$variable == "TMAR"]
  }
    sen_npv <- npv(rate = tmar,
                   cashflow = low_tib$cashflow, period = low_tib$periodo)

    return(sen_npv)
}


# Función que calcula el VPN del proyecto de Tabletas Personalizables
calcular_npv_sens <- function(variable, escenario, n) {
  
  # Preparar indice de escenario
  indice_esc <- switch(escenario, Bajo=2, Alto=3)
  
  # Preparar indices
  indice_valores <- rep(1, n)
  
  # Ajustar indice al caso correspondiente
  if (variable == col_sens[2]) {
    indice_valores[2] <- indice_esc
  } else if (variable == col_sens[3]) {
    indice_valores[3] <- indice_esc
  } else if (variable == col_sens[4]) {
    indice_valores[4] <- indice_esc
  } else if (variable == col_sens[5]) {
    indice_valores[5] <- indice_esc
  } else if (variable == col_sens[6]) {
    indice_valores[6] <- indice_esc
  } else if (variable == col_sens[7]) {
    indice_valores[7] <- indice_esc
  }
  
  # Asignar los valores individuales
  inversion<- as.numeric(input_sens[indice_valores[2], 2])
  tabletas <- as.numeric(input_sens[indice_valores[3], 3])
  markup <- as.numeric(input_sens[indice_valores[4], 4])
  costo <- as.numeric(input_sens[indice_valores[5], 5])
  renta <- as.numeric(input_sens[indice_valores[6], 6])
  descuento <- as.numeric(input_sens[indice_valores[7], 7])
  
  # Definir flujos anuales
  inv_flujo <- c(inversion, rep(0, 10))
  costo_flujo <- c(0, rep(costo * tabletas, 10))
  ingreso_flujo <- costo_flujo * (1+markup)
  renta_flujo <- c(0, rep(renta, 10))
  depr_flujo <- c(0, MACRS(investment=inversion, 
                           recovery_period = 5, 
                           n.period = 10))
  periodos <- 0:length(n)
  
  # Calcular Flujo de Caja Libre
  fcf_sens <- fcf(revenues = ingreso_flujo,
                  costs = costo_flujo + renta_flujo,
                  depreciation = depr_flujo,
                  capital_expenditures = inv_flujo,
                  change_net_working_capital = cambio_CTN,
                  tax_rate = tasa_gravable)
  
  # Calcular Valor Presente Neto
  vpn_sens <- npv(rate = descuento, cashflow = fcf_sens, period = periodos)
  
  return(vpn_sens)
}


