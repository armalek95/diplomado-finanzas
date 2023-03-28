## Este script va a contener las funciones necesarias para realizar la 
## simulación Monte Carlo

# Dependencia
#source("scripts/funciones_financieras.R")

## función que genera las distribuciones de probabilidad para el 
## cálculo de los valores a evaluar
random_values <- function(n.period = 10, 
                          distribution = "normal",
                          mlv = 0, 
                          sd = 1, 
                          shape = 1, 
                          scale = 1, 
                          shape2 = shape*5){
  
  if (distribution == "normal"){
    values <- rnorm(n.period, mlv, sd)
  } else if (distribution == "weibull"){
    values <- rweibull(n.period, shape = shape, scale = scale)
  } else if (distribution == "cauchy"){
    values <- rcauchy(n.period,location = mlv, scale = scale)
  } else if (distribution == "poisson"){
    values <- rpois(n.period, mlv)
  } else if (distribution == "beta"){
    values <- rbeta(n.period, shape1 = shape, shape2 = shape2, ncp = 0)
  }
  
  return(values) # regresa los valores generados de forma aleatoria
}

monte_carlo <- function(df, 
                        simulations = 10, 
                        n = 10, 
                        seed = 1,
                        discount_rate = 0.09,
                        tax_rate = 0.3, inflation = 0) {
  # Generar los valores para todos los periodos considerados
  # Podriamos agregar funciones de forecasting para los precios y el volumen 
  # esperado
  
  # Generar valores pronosticados para el periodo
  for (i in seq_along(1:(n-1))) {
    forecast_df <- df %>%
      filter(periodo == max(periodo)) %>%
      mutate(precio = precio * (1 + inflation),
             periodo = periodo + 1)
    
    df <- bind_rows(df, forecast_df)
  }
  
  monte_df <- tibble(
    sim = c(1:simulations),
    npv = rep(0, simulations)
  )
  
  for (i in seq_along(1:(simulations))) {
    # Generar valores aleatorios con base en el punto anterior
    
    rand <- df %>%
      rowwise() %>%
      mutate(
        volumen_r = random_values(n.period = 1, mlv = volumen, 
                                  distribution = dist_vol),
        precio_r = random_values(n.period = 1, mlv = precio, 
                                 distribution = dist_pre)
      )
    
    # Calcular variables necesarias para calcular FCF
    
    sim_revenue <- revenues(products_volume = filter(rand, tipo == "Producto") %>% 
                           select(volumen_r), 
                         products_price = filter(rand, tipo == "Producto") %>% 
                           select(precio_r),
                         periods = filter(rand, tipo == "Producto") %>% 
                           select(periodo))
    
    
    sim_costs <- total_costs(
      period_costs = 0,
      product_costs = product_costs(
        direct_materials = direct_materials(
          # Inician inputs para funcion de materiales directos
          raw_materials = filter(rand, tipo == "Material") %>% 
            select(volumen_r),
          raw_mat_cost = filter(rand, tipo == "Material") %>%
            select(precio_r),
          period = filter(rand, tipo == "Material") %>%
            select(periodo)
          # Terminan inputs para funcion de materiales directos
        ),
        direct_labor = 0,
        overhead = overhead(
          #Inician inputs para funcion de overhead
          utility_vol = filter(rand, tipo == "Utility") %>%
            select(volumen_r),
          utility_price = filter(rand, tipo == "Utility") %>%
            select(precio_r),
          maintenance_cost = 0,
          periods = filter(rand, tipo == "Utility") %>%
            select(periodo)
          # Terminan inputs para funcion de overhead
        )
      )
    )
    
    sim_fcf <- tibble(
      periodo = unique(rand$periodo),
      ingresos = sim_revenue$tot_revenue,
      costos = sim_costs$tot_cost
    )
    
    # Calcular el Flujo de Caja Libre (FCF)
    
    sim_fcf <- sim_fcf %>%
      mutate(fcf = fcf(revenues = ingresos, 
                       costs = costos, 
                       depreciation = 0, 
                       capital_expenditures = 0, 
                       change_net_working_capital = 0, 
                       tax_rate = tax_rate))

    #write_csv(sim_fcf, file = "monte_test.csv", append = TRUE)
    
    # Calcular Valor Presente Neto
    monte_df[i,2] = npv(rate = discount_rate, 
                        cashflow = sim_fcf$fcf, 
                        period = sim_fcf$periodo)
  }
  
  return(monte_df)
}


