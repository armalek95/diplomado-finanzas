
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
  type <- c(rep("Max",length(variables)),rep("Min",length(variables)))
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
    # label_dollar le da formato de moneda al eje X
    scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale()))
  bplot
}

gen_values <- function(values = rnorm(6,50, sd = 1)){
  
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
