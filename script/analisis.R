# ------------------------------------------------------------------------- #
#                         Proyección de ventas 2023                         #
# ------------------------------------------------------------------------- #

# Instructor: Alex Bajaña (alexvbr@ergostats.org)
# Organización: Fernando Arevalo (fdarevalo@lacamaradequito.com)
# Fecha del taller: Diciembre 15, 2022



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)

# Lectura de los datos: ---------------------------------------------------

# Ventas:

ventas_df <- read_csv("data/ventas_comercio.txt")

# Diccionario:

diccionario_df <- read_csv("data/diccionario_ventas_comercio.txt")


# Transformación a objeto de tipo date: -----------------------------------

#### Ejemplos:

# Formato clásico:

ymd("2012-10-07")

# Formato de excel:

dmy("1/1/2021")

# Extraer partes de una fecha:

fechas <- ymd(c("20120101","20150407","20180909"))

month(fechas,label = TRUE,abbr = FALSE)

# Operaciones con fechas:

quarter(fechas,type = "year.quarter")

#### Aplicación sobre nuestra tabla:

# Solución cuando tienes en ingles, solo para windows:

Sys.setlocale("LC_TIME", "Spanish")

# EDA: --------------------------------------------------------------------
# 1. Calcular las ventas totales (mutate)
# 2. La proporción de ventas gravadas sobre las ventas totales (en el mismo mutate)
# 3. Resumenes anuales: (group_by por año)
#    (summarise: ) 
#    3.1. Ventas promedio 
#    3.2. Ventas totales
#    3.3. Ventas medianas
#    3.4. Desviación estandar
#    3.5. Otro


# Objetos: ts() para series de tiempo -------------------------------------



as_tsibble(x = ventas_df,)

