# Este Script contiene todas las funciones básicas financieras que se utilizan
# a lo largo de este trabajo

## función que calcula el valor presente neto dado un número de periodos, 
## la tasa  de descuento, el flujo de efectivo y el periodo de inicio
npv <- function(rate = 0.09, cashflow = 0, period = 0){
  ## rate es la tasa de descuento, tasa mínima aceptable de retorno 
  ## o el costo promedio de capital
  ## cashflow es el flujo de efectivo a evaluar (para esto se puede 
  ## utilizar la función "fcf")
  ## period es el periodo correspondiente al flujo de efectivo
  
  # calcula el valor presente neto
  net_present_value <- sum(cashflow/(1+rate)^period) 
  
  # regresa el valor presente neto
  net_present_value 
}


## función que calcula el flujo de efectivo "libre" tomando en cuenta:
##los ingresos, costos, depreciación, inversiones, cambios en el capital 
## de trabajo y la tasa gravable
fcf <- function(revenues, 
                costs, 
                depreciation, 
                capital_expenditures, 
                change_net_working_capital = 0, 
                tax_rate = 0.3){
  ## revenues representa el vector de ingresos
  ## costs es el vector de los costos
  ## depreciation es el vector de depreciación
  ## capital_expenditures es el costo de la inversión a lo largo del proyecto
  ## change_net_working_capital es el cambio de capital de trabajo 
  ## (el valor de default es 0)
  ## tax_rate es la tasa de impuestos (el valor default es 30%)
  
  # cálculo del flujo de efectivo
  free_cash_flow <- (revenues - costs- depreciation)*(1-tax_rate)
  +depreciation-capital_expenditures-change_net_working_capital
  
  free_cash_flow # regresa el valor del flujo de efectivo
}


## función que regresa el vector de depreciación utilizando 
## MACRS (Modified Accelerated Cost Recovery System)
MACRS <- function(investment = 0, recovery_period = 10, n.period = 10) {
  
  # if statements que nos permiten utilizar el porcentaje adecuado para 
  # el cálculo de la depreciación de cada año
  if (recovery_period == 10){ # MACRS 10 años
    macrs <- c(0.1,0.18,0.144,0.1152, 0.0922, 0.0737,
               0.0655, 0.0655,0.0656,0.0655,0.0328)
  } else if (recovery_period < 10) {
    if (recovery_period == 5) { # MACRS 5 años
      macrs <- c(0.2, 0.32, 0.192, 0.1152, 0.1152, 0.0576)
    } else if (recovery_period == 3) { # MACRS 3 años
      macrs <- c(0.3333,0.4445,0.1481,0.0741)
    } else if (recovery_period == 7) { # MACRS 7 años
      macrs <- c(0.1429,0.2449,0.1749,0.1249,0.0893,0.0892,0.0893,0.0446)
    }
  } else {
    if (recovery_period == 15){ # MACRS 15 años
      macrs <- c(0.05,0.095,0.0855,0.077,0.0693,0.0623,0.0590,0.0590,0.0591,
                 0.0590,0.0591,0.0590,0.0591,0.0590,0.0591,0.0295)
    } else if (recovery_period == 20) { # MACRS 20 años
      macrs <- c(0.0375,0.07219,0.06677,0.06177,0.05713,0.05285,0.04888,
                 0.04522,0.04462,0.04461,0.04462,0.04461,0.04462,0.04461,
                 0.04462,0.04461,0.04462,0.04461,0.04462,0.04461,0.02231)
    }
  }
  
  if (length(macrs) > n.period){
    macrs <- macrs[1:n.period]
  } else {
    x <- n.period - length(macrs)
    macrs <- append(macrs, rep(0,x))
  }
  depreciation <- macrs*investment
  depreciation
}
