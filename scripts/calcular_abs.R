# Este script va a alojar el procedimiento para calcular la masa de ABS
# requerida para imprimir los moldes para los componentes de las tabletas
# personalizables de formulacion E1

# Paso 1.a: calcular área transversal de la matriz con componente activo
area_maca <- function(masa_comp, densidad_comp, z){
  # masa_comp es la masa de cada compuesto presente en la matriz con compuesto
  # activo en [g]
  # densidad_comp es la densidad de cada compuesto presente en la matriz con 
  # compuesto activo en [g/cm3]
  # z es la longitud de la matriz a lo largo de ese eje
  masa <- sum(masa_comp/densidad_comp)
  
  area_maca <- masa/z
  
  area_maca
}

# Paso 1.b: Calcular x e y
xy_length <- function(alpha, area_maca){
  x <- sqrt(area_maca / (alpha))
  y <- alpha * sqrt(area_maca/(alpha))
  
  c(x, y)
}

# Paso 2:
m_maca_abs <- function(x, y, z, kappa, densidad_abs){
  masa_maca_abs <- 2*kappa*(x+y+2*kappa)*densidad_abs
  masa_maca_abs
}

# Paso 3:
v_mas_gamma <- function(masa_comp, densidad_comp, x, y, z){
  v_mas <- sum(masa_comp/densidad_comp)
  gamma <- (-2*(x+y)+sqrt(4*(x+y)^2+16*(sum(masa_comp/densidad_comp)/z)))/8
  c(v_mas, gamma)
}

# Paso 4:
m_mas_abs <- function(densidad_abs, x, y, z, gamma, kappa, v_mas){
  v_mas_molde <- ((x+2*(gamma+kappa))*(y+(gamma+kappa))) * z - v_mas
  masa_abs_mas <- v_mas_molde * densidad_abs
  masa_abs_mas
}

# Paso 5:
mv_cera <- function(x, y, z, gamma, omega, densidad_cera){
  volumen_cera <- 2*omega*(x+z+2*(gamma+omega))*(y+2*gamma)
  masa_cera <- volumen_cera * densidad_cera
  c(masa_cera, volumen_cera)
}

# Paso 6:
m_cubierta_abs <- function(x, y, z, omega, kappa , gamma, 
                           densidad_abs, v_cera){
  v_molde_cubierta <- 
    ((x+2*(gamma+omega+kappa))*(z+2*(omega+kappa)))*(y+2*gamma)-v_cera
  masa_abs_cubierta <- v_molde_cubierta * densidad_abs
  masa_abs_cubierta
}

# Flujo completo E1:
masa_abs_cera <- function(z, kappa, densidad_comp_mi, masa_comp_mi, 
                          densidad_comp_me, masa_comp_me, alpha, omega, 
                       densidad_abs, densidad_cera){
  
  #Paso 1:
  a_maca <- area_maca(masa_comp_mi, densidad_comp_mi, z)
  
  xy <- xy_length(alpha, a_maca)
  
  x <- xy[1]
  y <- xy[2]
  
  #Paso 2:
  masa_abs_maca <- m_maca_abs(x, y, z, kappa, densidad_abs)
  
  #Paso 3:
  v_mas_g <- v_mas_gamma(masa_comp_me, densidad_comp_me, x=x, y=y, z=z)
  v_mas <- v_mas_g[1]
  gamma <- v_mas_g[2]
  
  #Paso 4:
  masa_abs_mas <- m_mas_abs(densidad_abs, x, y, z, gamma, kappa, v_mas)
  
  #Paso 5:
  m_v_cera <- mv_cera(x, y, z, gamma, omega, densidad_cera)
  masa_cera <- m_v_cera[1]
  volumen_cera <- m_v_cera[2]
  
  #Paso 6:
  masa_abs_cubierta <- m_cubierta_abs(x, y, z, omega, kappa , gamma, 
                                  densidad_abs, volumen_cera)
  
  masa_abs_total <- masa_abs_maca + masa_abs_mas + masa_abs_cubierta
  c(masa_abs_total, masa_cera)
}
