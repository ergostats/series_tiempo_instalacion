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

ventas_df <- read_csv("data/diccionario_ventas_comercio.txt")


# Transformación a objeto de tipo date: -----------------------------------

#### Ejemplos:

# Formato clásico:

ymd("2012-10-07")

# Formato de excel:

dmy("1/1/2021")

# Otro formato de excel:

my("Ene-2019")

# Extraer partes de una fecha:

fechas <- ymd(c("20120101","20150407","20180909"))

month(fechas,label = TRUE,abbr = FALSE)

# Operaciones con fechas:

quarter(fechas,type = "year.quarter")

#### Aplicación sobre nuestra tabla:

    


# Objetos: ts() para series de tiempo -------------------------------------



as_tsibble(x = ventas_df,)

