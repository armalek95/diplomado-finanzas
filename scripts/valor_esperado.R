## Este script va a contener las funciones necesarias para realizar la 
## evaluación de Valor Monetario Esperado

# Calcula el VPN del arbol de decision
w_npv<- function(node) {
  result <- node$prob * node$npv
  if(length(result) == 0) result <- sum(sapply(node$children, w_npv))
  return (result)
}


## función para calcular los ingresos esperados por la ejecución del 
## proyecto de inversión

revenues <- function(products_volume = c(0,0,0), 
                     products_price = c(0,0,0),
                     periods = c(1,1,1)){
  # products_volume es una lista de valores que representa el 
  # volumen de producción de productos vendibles
  # products_price es una lista de valores que representan el 
  # precio unitario de cada uno de los productos
  ## es importante que las unidades de volumen de venta de productos y 
  ## del precio de estos concuerde
  ## ejemplo: volumen en L y precio en $/L, o volumen en kg y precio en $/kg
  
  # Genera una tibble con la información recibida
  rev <- tibble(
    prod_vol = products_volume, 
    prod_price = products_price,
    per = periods
  )
  
  rev <- rev %>%
    # Calcula el costo de cada material
    mutate(revenue = prod_vol * prod_price) %>% 
    group_by(per) %>%
    # Calcula el costo total de material del periodo
    summarise(tot_revenue = sum(revenue)) 
  
  rev %>%
    select(tot_revenue)
}

## función que calcula los costos totales 
## (utiliza dos funciones que se llaman desde la función)
## estas funciones se describen posteriormente

total_costs <- function(period_costs = 0, 
                        product_costs = 0) {
  # period_costs son los costos no relacionados a produccion que se generan
  # en el periodo
  # product_costs son los costos relacionados a la produccion del producto
  
  total_costs <- (product_costs + period_costs)
  
  total_costs
}

# función que calcula los costos de producto
product_costs <- function(direct_materials = 0, 
                          direct_labor = 0, 
                          overhead = 0){
  
  product_costs <- direct_materials + direct_labor + overhead
  
  product_costs
}

# función que calcula el costo de los materiales directos como función del 
# producto del volumen de los mismos y su precio
direct_materials <- function(raw_materials = c(0,0,0), raw_mat_cost = c(0,0,0), 
                             period = c(1,1,1)){
  # Genera una tibble con la información recibida
  dir_mat <- tibble(
    raw_mat = raw_materials,
    raw_cost = raw_mat_cost,
    per = period
  )
  
  
  dir_mat <- dir_mat %>%
    # Calcula el costo de cada material
    mutate(cost = raw_cost*raw_mat) %>% 
    group_by(per) %>%
    # Calcula el costo total de material del periodo
    summarise(tot_cost = sum(cost)) 
  
  dir_mat %>%
    select(tot_cost)
}

# función que calcula el costo de la labor necesaria para fabricar el producto
# para fines de este trabajo se consideran sólo tres puestos
direct_labor <- function(roles = c("Supervisor", "Technician", "Janitor"),
                         number = c(0,0,0),
                         wage = c(0,0,0),
                         period = c(1,1,1)) {
  
  # Genera una tibble con la información recibida
  dir_lab <- tibble(
    dir_rol = roles,
    dir_num = number,
    dir_wag = wage,
    per = period
  )
  
  dir_lab <- dir_lab %>%
    # Calcula el costo de cada rol
    mutate(cost = dir_rol*dir_wag*dir_num) %>% 
    group_by(per) %>%
    # Calcula el costo total de labor del periodo
    summarise(tot_cost = sum(cost)) 
  
  dir_lab %>%
    select(tot_cost)
}

# función que calcula los los costos del overhead de la planta
# para fines de este trabajo sólo estamos considerando el calor, la electricidad 
# y los costos de mantenimiento de los equipos
overhead <- function(utility_vol = c(0,0,0),
                     utility_price = c(0,0,0),
                     maintenance_cost = c(0,0,0),
                     periods = c(1,1,1)) {
  
  # Genera una tibble con la información recibida
  over <- tibble(
    util = utility_vol, 
    price = utility_price,
    per = periods
  )
  
  over <- over %>%
    # Calcula el costo de cada periodo de cada utility
    mutate(cost = util * price) %>% 
    group_by(per) %>%
    # Calcula el costo total de labor del periodo
    summarise(tot_cost = sum(cost)) 
  
  over$tot_cost <- over$tot_cost + maintenance_cost 
  
  over %>%
    select(tot_cost)
}

# función que calcula los costos de periodo, para la parte de gastos 
# administrativos se considera 0 si el proyecto no conlleva gastos 
# administrativos adicionales a la empresa que lleva a cabo el proyecto
period_costs <- function(sales_costs = c(0,0,0), 
                         admin_expenses = c(0,0,0),
                         period = c(1,1,1)) {
  
  period_costs <- sales_costs + admin_expenses
  period_costs
}

# función que calcula los costos de venta, para el análisis de este trabajo 
# sólo se están considerando 
# los costos relacionados al transporte de los productos a la ubicación de venta
sales_costs <- function(product_volume = c(0,0,0), 
                        transport_cost = c(0,0,0),
                        period = c(1,1,1)) {
  
  # Genera una tibble con la información recibida
  sales_cost <- tibble(
    prod_vol = product_volume, 
    trans_cost = transport_cost,
    per = periods
  )
  
  sales_cost <- sales_cost %>%
    # Calcula el costo del envío de cada producto
    mutate(cost = prod_vol * trans_cost) %>% 
    group_by(per) %>%
    # Calcula el costo total envío del periodo
    summarise(tot_cost = sum(cost)) 
  
  sales_cost %>%
    select(tot_cost)
}

