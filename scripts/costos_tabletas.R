
# funcion que da como resultado las dimensiones de la tableta
rand_dimensiones <- function(formulacion){
  if (formulacion == "E1") {
    return(tibble(variable=c("z", "kappa", "alpha", "omega"), 
                  valor=c(0.6, 0.05, 2, 0.05))) 
  } else if (formulacion == "E2") {
    return (tibble(variable=c("z", "kappa", "alpha", "omega"), 
                   valor=c(0.6, 0.05, 2, 0.05)))
  }
}

# Funcion que da como resultado la composicion dependiendo de la formulacion
# de la tableta
ret_composicion <- function(formulacion){
  if (formulacion == "E1") {
    return(composicion[,c("Compound", "Composicion_E1")] 
           %>% filter(Composicion_E1 >0) %>% rename(Composicion=Composicion_E1))
  } else if (formulacion == "E2") {
    return(composicion[,c("Compound", "Composicion_E2")] 
           %>% filter(Composicion_E2 >0) %>% rename(Composicion=Composicion_E2))
  }
}

# Funcion que genera la simulación de órdenes personalizadas
ordenes_personalizadas <- function(sim_num) {
  # Crear tabla para iniciar la simulacion
  simulaciones <- tibble(sim = 1:sim_num, 
                         formulacion=sample(formulaciones, sim_num, replace = TRUE))
  
  # Agregar compuesto activo
  simulaciones <- simulaciones %>%
    mutate(compuesto_activo = map_chr(formulacion, compuesto))
  
  # Agregar dosis y composicion
  simulaciones <- simulaciones %>%
    mutate(dosis = abs(map_dbl(compuesto_activo, dosis)),
           dimensiones = map(.f=rand_dimensiones, formulacion),
           composicion = map(.f=ret_composicion, formulacion))
  
  # Calcular los materiales que componen a la tableta de acuerdo a las 
  # especificaciones de la misma
  simulaciones <- simulaciones %>% 
    mutate(materiales=
             pmap(list(formulacion, 
                       compuesto_activo, 
                       dosis, dimensiones, composicion), 
                  .f=requerimiento_material)) 
  return(simulaciones)
}

# Funcion para calcular el costo de los materiales

costo_materiales <- function(materiales, tabletas) {
  costo <- materiales %>% 
    mutate(costo=masa*Precio_mxn_g) %>%
    select(costo) %>%
    sum()
  
  return(costo + costo_frasco / tabletas)
}

# Funcion para calcular el costo de labor asociado

costo_labor <- function(tabletas, t_fabricacion) {
  costo <- (t_fabricacion/tabletas) * salario_trabajador / eficiencia_trabajador
  return(costo)
}

# Funcion para calcular el costo energetico

costo_energia <- function(materiales, t_impresion, 
                          costo_electricidad, consumo_energetico_impresora, 
                          formulacion, T0=25, T1=82, T2=65) {
  calor_matriz <- 0
  if (formulacion == "E1") {
    calor_matriz <- materiales %>% 
      filter(Componente == "MI" | Componente == "ME") %>% 
      mutate(energia = masa * Specific_Heat * (T1 - T0) + masa * Fusion_Heat) %>%
      select(energia) %>% sum()
  }
  
  calor_cubierta <- materiales %>%
    filter(Componente == "CU") %>%
    mutate(energia = masa * Specific_Heat * (T2 - T0) + masa * Fusion_Heat) %>%
    select(energia) %>% sum()
  
  calor_total <- calor_matriz + calor_cubierta
  
  energia_impresora <- t_impresion * consumo_energetico_impresora
  
  costo_energia <- (calor_total / eficiencia_calentador + 
                      energia_impresora) * costo_electricidad
  
  return(costo_energia)
}

# Funcion para calcular el costo de distribucion
costo_distribucion <- function(t_transporte, salario_trabajador, 
                               distancia, precio_gasolina, eficiencia_vehiculo,
                               numero_tabletas_viaje) {
  costo <- (t_transporte * salario_trabajador + 
              (distancia * precio_gasolina)/eficiencia_vehiculo) / 
    numero_tabletas_viaje
  
  return(costo)
}
