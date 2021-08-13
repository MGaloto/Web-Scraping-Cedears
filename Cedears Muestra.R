
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Web Scraping Cedears
#
# From: https://es-us.finanzas.yahoo.com/
# --------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------- #

# Librerias

library(corrplot) 
library(quantmod)
library(highcharter)
library(tidyr)
library(miscset)
library(ggplot2)
library(readr)  
library(dplyr) 
library(plotly)
library(crayon) 
library(modeest)
library(readxl)
library(plotly)
library(ggthemes)
library(reshape)

# --------------------------------------------------------------------------------------- #

# Importar acciones

CAT=getSymbols("CAT.BA" , src = 'yahoo', auto.assign = F, from = "2001-01-02", to = Sys.Date(), periodicity = "daily")
MELI=getSymbols("MELI.BA" , src = 'yahoo', auto.assign = F, from = "2001-01-02", to = Sys.Date(), periodicity = "daily")
TSLA=getSymbols("TSLA.BA" , src = 'yahoo', auto.assign = F, from = "2001-01-02", to = Sys.Date(), periodicity = "daily")
BABA=getSymbols("BABA.BA" , src = 'yahoo', auto.assign = F, from = "2001-01-02", to = Sys.Date(), periodicity = "daily")
AAPL=getSymbols("AAPL.BA" , src = 'yahoo', auto.assign = F, from = "2001-01-02", to = Sys.Date(), periodicity = "daily")
AMZN=getSymbols("AMZN.BA" , src = 'yahoo', auto.assign = F, from = "2001-01-02", to = Sys.Date(), periodicity = "daily")

# --------------------------------------------------------------------------------------- #

# Se crean los Data Frames


CAT = as.data.frame(CAT)
MELI = as.data.frame(MELI)
TSLA = as.data.frame(TSLA)
BABA = as.data.frame(BABA)
AAPL = as.data.frame(AAPL)
AMZN = as.data.frame(AMZN)

# --------------------------------------------------------------------------------------- #

# Se agregan las Fechas como variables


CAT$Fecha <- row.names(CAT)
MELI$Fecha <- row.names(MELI)
TSLA$Fecha <- row.names(TSLA)
BABA$Fecha <- row.names(BABA)
AAPL$Fecha <- row.names(AAPL)
AMZN$Fecha <- row.names(AMZN)

# --------------------------------------------------------------------------------------- #

# Se omiten los nulos


CAT = na.omit(CAT)
MELI = na.omit(MELI)
TSLA = na.omit(TSLA)
BABA = na.omit(BABA)
AAPL = na.omit(AAPL)
AMZN = na.omit(AMZN)

# --------------------------------------------------------------------------------------- #

# Se mofidican las columnas

colnames(CAT) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(MELI) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(TSLA) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(BABA) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(AAPL) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")
colnames(AMZN) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")

# --------------------------------------------------------------------------------------- #

# Se numeran las filas

rownames(CAT) = 1:nrow(CAT)
rownames(MELI) = 1:nrow(MELI)
rownames(TSLA) = 1:nrow(TSLA)
rownames(BABA) = 1:nrow(BABA)
rownames(AAPL) = 1:nrow(AAPL)
rownames(AMZN) = 1:nrow(AMZN)

# --------------------------------------------------------------------------------------- #

# Se crean variables las variaciones % de los ultimos 5 dias

CATdis = round( ( CAT$Close[length(CAT$Close)] - CAT$Close[length(CAT$Close) - 5] ) / CAT$Close[length(CAT$Close) - 5] ,4)*100 
MELIdis = round( ( MELI$Close[length(MELI$Close)] - MELI$Close[length(MELI$Close) - 5] ) / MELI$Close[length(MELI$Close) - 5] ,4)*100 
TSLAdis = round( ( TSLA$Close[length(TSLA$Close)] - TSLA$Close[length(TSLA$Close) - 5] ) / TSLA$Close[length(TSLA$Close) - 5] ,4)*100 
BABAdis = round( ( BABA$Close[length(BABA$Close)] - BABA$Close[length(BABA$Close) - 5] ) / BABA$Close[length(BABA$Close) - 5] ,4)*100 
AAPLdis = round( ( AAPL$Close[length(AAPL$Close)] - AAPL$Close[length(AAPL$Close) - 5] ) / AAPL$Close[length(AAPL$Close) - 5] ,4)*100 
AMZNdis = round( ( AMZN$Close[length(AMZN$Close)] - AMZN$Close[length(AMZN$Close) - 5] ) / AMZN$Close[length(AMZN$Close) - 5] ,4)*100 

# --------------------------------------------------------------------------------------- #

# Se crea un data frame con los valores de las variaciones


Cedears = c(
  "CAT",
  "MELI",
  "TSLA",
  "BABA",
  "AAPL",
  "AMZN")



Distancias = c(
  CATdis,
  MELIdis,
  TSLAdis,
  BABAdis,
  AAPLdis,
  AMZNdis)




df = data.frame(Cedears, Distancias)

View(df)


# --------------------------------------------------------------------------------------- #

# Se ordena el data frame de mayor a menor y se sacan las primeras 5

dfmax = df[order(-df$Distancias),]


dfmax = dfmax[1:5,]

# --------------------------------------------------------------------------------------- #

# Se ordena el data frame de menor a mayor y se sacan las primeras 5

dfmin = df[order(df$Distancias),]


dfmin = dfmin[1:5,]

# --------------------------------------------------------------------------------------- #

# Grafico serie de tiempo de prueba


Apple = plot_ly()
Apple = fig %>% add_lines(y = log(AAPL$Close) , name = "AAPL", x=AAPL$Fecha ,line = list(shape = "spline"))
Apple = fig %>% layout(title = "AAPL en Log", xaxis = list(title = 'Periodo'), yaxis = list (title = 'AAPL'))
Apple

# --------------------------------------------------------------------------------------- #

# Grafico de barras de prueba de las que mas subieron o menos bajaron de la muestra

plot_ly() %>% 
  add_trace(x= dfmax$Cedears, y=dfmax$Distancias, type = "bar", text = paste0(dfmax$Distancias, "%"), 
            textposition = 'auto',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
  layout(title = "Variacion % Semanal Cedears",
         barmode = 'group', 
         xaxis = list(title = "Cedears"),
         yaxis = list(title = "%"))

# --------------------------------------------------------------------------------------- #



