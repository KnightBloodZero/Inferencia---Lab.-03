#Librerias
library(xlsx)
require(ggplot2)
require(grid)
require(gridExtra)

#Carga de Datos
#Por Equipos
equipo1 <- read.xlsx("prueba.xlsx",1)
equipo2 <- read.xlsx("prueba.xlsx",2)
#Por Columnas
ComplejidadEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=2)
HorasHombreEquipo1 <- read.xlsx("prueba.xlsx",1,startRow=2,colIndex=3)
ValorNegocioEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=4)
BugsEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=5)
CalidadCodigoEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=6)
DeudaTecnicaEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=7)
NLCEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=8)
RendimientoEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=9)
BeneficioEquipo1 <- read.xlsx("prueba.xlsx",1,colIndex=10)
ComplejidadEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=2)
HorasHombreEquipo2 <- read.xlsx("prueba.xlsx",2,startRow=2,colIndex=3)
ValorNegocioEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=4)
BugsEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=5)
CalidadCodigoEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=6)
DeudaTecnicaEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=7)
NLCEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=8)
RendimientoEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=9)
BeneficioEquipo2 <- read.xlsx("prueba.xlsx",2,colIndex=10)

#Establecimiento de vectores Unidimensionales con los datos
unidimensionalComplejidadesEquipo1 <- ComplejidadEquipo1[,1]
unidimensionalHorasHombreEquipo1 <- HorasHombreEquipo1[,1]
unidimensionalValorNegocioEquipo1 <- ValorNegocioEquipo1[,1]
unidimensionalBugsEquipo1 <- BugsEquipo1[,1]
unidimensionalCalidadCodigoEquipo1 <- CalidadCodigoEquipo1[,1]
unidimensionalDeudaTecnicaEquipo1 <- DeudaTecnicaEquipo1[,1]
unidimensionalNLCEquipo1 <- NLCEquipo1[,1]
unidimensionalRendimientoEquipo1 <- RendimientoEquipo1[,1]
unidimensionalBeneficioEquipo1 <- BeneficioEquipo1[,1]
unidimensionalComplejidadesEquipo2 <- ComplejidadEquipo2[,1]
unidimensionalHorasHombreEquipo2 <- HorasHombreEquipo2[,1]
unidimensionalValorNegocioEquipo2 <- ValorNegocioEquipo2[,1]
unidimensionalBugsEquipo2 <- BugsEquipo2[,1]
unidimensionalCalidadCodigoEquipo2 <- CalidadCodigoEquipo2[,1]
unidimensionalDeudaTecnicaEquipo2 <- DeudaTecnicaEquipo2[,1]
unidimensionalNLCEquipo2 <- NLCEquipo2[,1]
unidimensionalRendimientoEquipo2 <- RendimientoEquipo2[,1]
unidimensionalBeneficioEquipo2 <- BeneficioEquipo2[,1]

#Frecuencias Absolutas
FrecuenciaAbsolutaComplejidadesEquipo1 <- hist(unidimensionalComplejidadesEquipo1, freq=TRUE)
FrecuenciaAbsolutaHorasHombreEquipo1 <- hist(unidimensionalHorasHombreEquipo1, freq=TRUE)
FrecuenciaAbsolutaValorNegocioEquipo1 <- hist(unidimensionalValorNegocioEquipo1, freq=TRUE)
FrecuenciaAbsolutaBugsEquipo1 <- hist(unidimensionalBugsEquipo1, freq=TRUE)
FrecuenciaAbsolutaNLCEquipo1 <- hist(unidimensionalNLCEquipo1, freq=TRUE)
FrecuenciaAbsolutaRendimientoEquipo1 <- hist(unidimensionalRendimientoEquipo1, freq=TRUE)
FrecuenciaAbsolutaBeneficioEquipo1 <- hist(unidimensionalBeneficioEquipo1, freq=TRUE)
FrecuenciaAbsolutaComplejidadesEquipo2 <- hist(unidimensionalComplejidadesEquipo2, freq=TRUE)
FrecuenciaAbsolutaHorasHombreEquipo2 <- hist(unidimensionalHorasHombreEquipo2, freq=TRUE)
FrecuenciaAbsolutaValorNegocioEquipo2 <- hist(unidimensionalValorNegocioEquipo2, freq=TRUE)
FrecuenciaAbsolutaBugsEquipo2 <- hist(unidimensionalBugsEquipo2, freq=TRUE)
FrecuenciaAbsolutaCalidadCodigoEquipo2 <- hist(unidimensionalCalidadCodigoEquipo2, freq=TRUE)
FrecuenciaAbsolutaDeudaTecnicaEquipo2 <- hist(unidimensionalDeudaTecnicaEquipo2, freq=TRUE)
FrecuenciaAbsolutaNLCEquipo2 <- hist(unidimensionalNLCEquipo2, freq=TRUE)
FrecuenciaAbsolutaRendimientoEquipo2 <- hist(unidimensionalRendimientoEquipo2, freq=TRUE)
FrecuenciaAbsolutaBeneficioEquipo2 <- hist(unidimensionalBeneficioEquipo2, freq=TRUE)
#Por omision el freq es TRUE dando Frecuencia Absoluta, si es False da las Frecuencias Relativas. Al la propiedad "probability" se consigue el mismo efecto pero usando
#la logica booleana de forma inversa.

#Frecuencias Relativas
FrecuenciaRelativaComplejidadesEquipo1 <- hist(unidimensionalComplejidadesEquipo1, freq=FALSE)
FrecuenciaRelativaHorasHombreEquipo1 <- hist(unidimensionalHorasHombreEquipo1, freq=FALSE)
FrecuenciaRelativaValorNegocioEquipo1 <- hist(unidimensionalValorNegocioEquipo1, freq=FALSE)
FrecuenciaRelativaBugsEquipo1 <- hist(unidimensionalBugsEquipo1, freq=FALSE)
FrecuenciaRelativaNLCEquipo1 <- hist(unidimensionalNLCEquipo1, freq=FALSE)
FrecuenciaRelativaRendimientoEquipo1 <- hist(unidimensionalRendimientoEquipo1, freq=FALSE)
FrecuenciaRelativaBeneficioEquipo1 <- hist(unidimensionalBeneficioEquipo1, freq=FALSE)
FrecuenciaRelativaComplejidadesEquipo2 <- hist(unidimensionalComplejidadesEquipo2, freq=FALSE)
FrecuenciaRelativaHorasHombreEquipo2 <- hist(unidimensionalHorasHombreEquipo2, freq=FALSE)
FrecuenciaRelativaValorNegocioEquipo2 <- hist(unidimensionalValorNegocioEquipo2, freq=FALSE)
FrecuenciaRelativaBugsEquipo2 <- hist(unidimensionalBugsEquipo2, freq=FALSE)
FrecuenciaRelativaCalidadCodigoEquipo2 <- hist(unidimensionalCalidadCodigoEquipo2, freq=FALSE)
FrecuenciaRelativaDeudaTecnicaEquipo2 <- hist(unidimensionalDeudaTecnicaEquipo2, freq=FALSE)
FrecuenciaRelativaNLCEquipo2 <- hist(unidimensionalNLCEquipo2, freq=FALSE)
FrecuenciaRelativaRendimientoEquipo2 <- hist(unidimensionalRendimientoEquipo2, freq=FALSE)
FrecuenciaRelativaBeneficioEquipo2 <- hist(unidimensionalBeneficioEquipo2, freq=FALSE)

#Calculos de Medias
mediaComplejidadEquipo1 <- sapply(ComplejidadEquipo1, mean, na.rm=TRUE)
mediaHorasHombreEquipo1 <- sapply(HorasHombreEquipo1, mean, na.rm=TRUE)
mediaValorNegocioEquipo1 <- sapply(ValorNegocioEquipo1, mean, na.rm=TRUE)
mediaBugsEquipo1 <- sapply(BugsEquipo1, mean, na.rm=TRUE)
mediaNLCEquipo1 <- sapply(NLCEquipo1, mean, na.rm=TRUE)
mediaRendimientoEquipo1 <- sapply(RendimientoEquipo1, mean, na.rm=TRUE)
mediaBeneficioEquipo1 <- sapply(BeneficioEquipo1, mean, na.rm=TRUE)
mediaComplejidadEquipo2 <- sapply(ComplejidadEquipo2, mean, na.rm=TRUE)
mediaHorasHombreEquipo2 <- sapply(HorasHombreEquipo2, mean, na.rm=TRUE)
mediaValorNegocioEquipo2 <- sapply(ValorNegocioEquipo2, mean, na.rm=TRUE)
mediaBugsEquipo2 <- sapply(BugsEquipo2, mean, na.rm=TRUE)
mediaNLCEquipo2 <- sapply(NLCEquipo2, mean, na.rm=TRUE)
mediaRendimientoEquipo2 <- sapply(RendimientoEquipo2, mean, na.rm=TRUE)
mediaBeneficioEquipo2 <- sapply(BeneficioEquipo2, mean, na.rm=TRUE)

#Calculos de Medianas
medianaComplejidadEquipo1 <- sapply(ComplejidadEquipo1, median, na.rm=TRUE)
medianaHorasHombreEquipo1 <- sapply(HorasHombreEquipo1, median, na.rm=TRUE)
medianaValorNegocioEquipo1 <- sapply(ValorNegocioEquipo1, median, na.rm=TRUE)
medianaBugsEquipo1 <- sapply(BugsEquipo1, median, na.rm=TRUE)
medianaNLCEquipo1 <- sapply(NLCEquipo1, median, na.rm=TRUE)
medianaRendimientoEquipo1 <- sapply(RendimientoEquipo1, median, na.rm=TRUE)
medianaBeneficioEquipo1 <- sapply(BeneficioEquipo1, median, na.rm=TRUE)
medianaComplejidadEquipo2 <- sapply(ComplejidadEquipo2, median, na.rm=TRUE)
medianaHorasHombreEquipo2 <- sapply(HorasHombreEquipo2, median, na.rm=TRUE)
medianaValorNegocioEquipo2 <- sapply(ValorNegocioEquipo2, median, na.rm=TRUE)
medianaBugsEquipo2 <- sapply(BugsEquipo2, median, na.rm=TRUE)
medianaNLCEquipo2 <- sapply(NLCEquipo2, median, na.rm=TRUE)
medianaRendimientoEquipo2 <- sapply(RendimientoEquipo2, median, na.rm=TRUE)
medianaBeneficioEquipo2 <- sapply(BeneficioEquipo2, median, na.rm=TRUE)

#Calculo de Modas

#Para MultiModal
Moda <- function(x){
 temp <- table(as.vector(x))
 names(temp)[temp == max(temp)]
}

modaComplejidadEquipo1 <- Moda(unidimensionalComplejidadesEquipo1)
modaHorasHombreEquipo1 <- Moda(unidimensionalHorasHombreEquipo1)
modaValorNegocioEquipo1 <- Moda(unidimensionalValorNegocioEquipo1)
modaBugsEquipo1 <- Moda(unidimensionalBugsEquipo1)
modaCalidadCodigoEquipo1 <- Moda(unidimensionalCalidadCodigoEquipo1)
modaDeudaTecnicaEquipo1 <- Moda(unidimensionalDeudaTecnicaEquipo1)
modaNLCEquipo1 <- Moda(unidimensionalNLCEquipo1)
modaRendimientoEquipo1 <- Moda(unidimensionalRendimientoEquipo1)
modaBeneficioEquipo1 <- Moda(unidimensionalBeneficioEquipo1)
modaComplejidadEquipo2 <- Moda(unidimensionalComplejidadesEquipo2)
modaHorasHombreEquipo2 <- Moda(unidimensionalHorasHombreEquipo2)
modaValorNegocioEquipo2 <- Moda(unidimensionalValorNegocioEquipo2)
modaBugsEquipo2 <- Moda(unidimensionalBugsEquipo2)
modaCalidadCodigoEquipo2 <- Moda(unidimensionalCalidadCodigoEquipo2)
modaDeudaTecnicaEquipo2 <- Moda(unidimensionalDeudaTecnicaEquipo2)
modaNLCEquipo2 <- Moda(unidimensionalNLCEquipo2)
modaRendimientoEquipo2 <- Moda(unidimensionalRendimientoEquipo2)
modaBeneficioEquipo2 <- Moda(unidimensionalBeneficioEquipo2)

#Convertir las modas obtenidas a un string concatenado para poder colocar en casilla en una tabla
modaComplejidadEquipo1 <- paste(modaComplejidadEquipo1, collapse='-')
modaHorasHombreEquipo1 <- paste(modaHorasHombreEquipo1, collapse='-')
modaValorNegocioEquipo1 <- paste(modaValorNegocioEquipo1, collapse='-')
modaBugsEquipo1 <- paste(modaBugsEquipo1, collapse='-')
modaCalidadCodigoEquipo1 <- paste(modaCalidadCodigoEquipo1, collapse='-')
modaDeudaTecnicaEquipo1 <- paste(modaDeudaTecnicaEquipo1, collapse='-')
modaNLCEquipo1 <- paste(modaNLCEquipo1, collapse='-')
modaRendimientoEquipo1 <- paste(modaRendimientoEquipo1, collapse='-')
modaBeneficioEquipo1 <- paste(modaBeneficioEquipo1, collapse='-')
modaComplejidadEquipo2 <- paste(modaComplejidadEquipo2, collapse='-')
modaHorasHombreEquipo2 <- paste(modaHorasHombreEquipo2, collapse='-')
modaValorNegocioEquipo2 <- paste(modaValorNegocioEquipo2, collapse='-')
modaBugsEquipo2 <- paste(modaBugsEquipo2, collapse='-')
modaCalidadCodigoEquipo2 <- paste(modaCalidadCodigoEquipo2, collapse='-')
modaDeudaTecnicaEquipo2 <- paste(modaDeudaTecnicaEquipo2, collapse='-')
modaNLCEquipo2 <- paste(modaNLCEquipo2, collapse='-')
modaRendimientoEquipo2 <- paste(modaRendimientoEquipo2, collapse='-')
modaBeneficioEquipo2 <- paste(modaBeneficioEquipo2, collapse='-')

#Calculos de Desviaciones Medias
desviacionMediaComplejidadEquipo1 <- sapply(ComplejidadEquipo1, sd, na.rm=TRUE)
desviacionMediaHorasHombreEquipo1 <- sapply(HorasHombreEquipo1, sd, na.rm=TRUE)
desviacionMediaValorNegocioEquipo1 <- sapply(ValorNegocioEquipo1, sd, na.rm=TRUE)
desviacionMediaBugsEquipo1 <- sapply(BugsEquipo1, sd, na.rm=TRUE)
desviacionMediaNLCEquipo1 <- sapply(NLCEquipo1, sd, na.rm=TRUE)
desviacionMediaRendimientoEquipo1 <- sapply(RendimientoEquipo1, sd, na.rm=TRUE)
desviacionMediaBeneficioEquipo1 <- sapply(BeneficioEquipo1, sd, na.rm=TRUE)
desviacionMediaComplejidadEquipo2 <- sapply(ComplejidadEquipo2, sd, na.rm=TRUE)
desviacionMediaHorasHombreEquipo2 <- sapply(HorasHombreEquipo2, sd, na.rm=TRUE)
desviacionMediaValorNegocioEquipo2 <- sapply(ValorNegocioEquipo2, sd, na.rm=TRUE)
desviacionMediaBugsEquipo2 <- sapply(BugsEquipo2, sd, na.rm=TRUE)
desviacionMediaNLCEquipo2 <- sapply(NLCEquipo2, sd, na.rm=TRUE)
desviacionMediaRendimientoEquipo2 <- sapply(RendimientoEquipo2, sd, na.rm=TRUE)
desviacionMediaBeneficioEquipo2 <- sapply(BeneficioEquipo2, sd, na.rm=TRUE)

#Calculos de Varianza
varianzaComplejidadEquipo1 <- sapply(ComplejidadEquipo1, var, na.rm=TRUE)
varianzaHorasHombreEquipo1 <- sapply(HorasHombreEquipo1, var, na.rm=TRUE)
varianzaValorNegocioEquipo1 <- sapply(ValorNegocioEquipo1, var, na.rm=TRUE)
varianzaBugsEquipo1 <- sapply(BugsEquipo1, var, na.rm=TRUE)
varianzaNLCEquipo1 <- sapply(NLCEquipo1, var, na.rm=TRUE)
varianzaRendimientoEquipo1 <- sapply(RendimientoEquipo1, var, na.rm=TRUE)
varianzaBeneficioEquipo1 <- sapply(BeneficioEquipo1, var, na.rm=TRUE)
varianzaComplejidadEquipo2 <- sapply(ComplejidadEquipo2, var, na.rm=TRUE)
varianzaHorasHombreEquipo2 <- sapply(HorasHombreEquipo2, var, na.rm=TRUE)
varianzaValorNegocioEquipo2 <- sapply(ValorNegocioEquipo2, var, na.rm=TRUE)
varianzaBugsEquipo2 <- sapply(BugsEquipo2, var, na.rm=TRUE)
varianzaNLCEquipo2 <- sapply(NLCEquipo2, var, na.rm=TRUE)
varianzaRendimientoEquipo2 <- sapply(RendimientoEquipo2, var, na.rm=TRUE)
varianzaBeneficioEquipo2 <- sapply(BeneficioEquipo2, var, na.rm=TRUE)

#Creación de Tablas
#install.packages("data.table")
library(data.table)

tablaDeMedias <- data.table(Equipo1=c(mediaComplejidadEquipo1,mediaHorasHombreEquipo1, mediaValorNegocioEquipo1, mediaBugsEquipo1, mediaNLCEquipo1, mediaRendimientoEquipo1,  mediaBeneficioEquipo1),Equipo.2=c(mediaComplejidadEquipo2,mediaHorasHombreEquipo2, mediaValorNegocioEquipo2, mediaBugsEquipo2, mediaNLCEquipo2, mediaRendimientoEquipo2,  mediaBeneficioEquipo2))
colnames(tablaDeMedias) <- c("Equipo 1", "Equipo 2")
rownames(tablaDeMedias) <- c("Complejidad","Horas Hombre","Valor De Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio")
tablaDeMedianas <- data.table(Equipo1=c(medianaComplejidadEquipo1,medianaHorasHombreEquipo1, medianaValorNegocioEquipo1, medianaBugsEquipo1, medianaNLCEquipo1, medianaRendimientoEquipo1,  medianaBeneficioEquipo1),Equipo.2=c(medianaComplejidadEquipo2,medianaHorasHombreEquipo2, medianaValorNegocioEquipo2, medianaBugsEquipo2, medianaNLCEquipo2, medianaRendimientoEquipo2,  medianaBeneficioEquipo2))
colnames(tablaDeMedianas) <- c("Equipo 1", "Equipo 2")
tablaDeDesviacionMedia <- data.table(Equipo1=c(desviacionMediaComplejidadEquipo1,desviacionMediaHorasHombreEquipo1, desviacionMediaValorNegocioEquipo1, desviacionMediaBugsEquipo1, desviacionMediaNLCEquipo1, desviacionMediaRendimientoEquipo1,  desviacionMediaBeneficioEquipo1),Equipo.2=c(desviacionMediaComplejidadEquipo2,desviacionMediaHorasHombreEquipo2, desviacionMediaValorNegocioEquipo2, desviacionMediaBugsEquipo2, desviacionMediaNLCEquipo2, desviacionMediaRendimientoEquipo2,  desviacionMediaBeneficioEquipo2))
colnames(tablaDeDesviacionMedia) <- c("Equipo 1", "Equipo 2")
tablaDeVarianza <- data.table(Equipo1=c(varianzaComplejidadEquipo1,varianzaHorasHombreEquipo1, varianzaValorNegocioEquipo1, varianzaBugsEquipo1, varianzaNLCEquipo1, varianzaRendimientoEquipo1,  varianzaBeneficioEquipo1),Equipo.2=c(varianzaComplejidadEquipo2,varianzaHorasHombreEquipo2, varianzaValorNegocioEquipo2, varianzaBugsEquipo2, varianzaNLCEquipo2, varianzaRendimientoEquipo2,  varianzaBeneficioEquipo2))
colnames(tablaDeVarianza) <- c("Equipo 1", "Equipo 2")
tablaMedias <- data.frame(Categoria=c("Complejidad","Horas Hombre","Valor De Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio"), Media=tablaDeMedias)
tablaMedianas <- data.frame(Categoria=c("Complejidad","Horas Hombre","Valor De Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio"), Mediana=tablaDeMedianas)
tablaDesviacionEstandar <- data.frame(Categoria=c("Complejidad","Horas Hombre","Valor De Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio"), DesviacionMedia=tablaDeDesviacionMedia)
tablaVarianza <-  data.frame(Categoria=c("Complejidad","Horas Hombre","Valor De Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio"),Varianza=tablaDeVarianza)
tablaDeModasPorEquipos <- data.table(Categorias=c("Complejidad","Horas Hombre","Valor De Negocio", "Bugs", "NLC", "Calidad de Codigo", "Deuda Tecnica","Rendimiento", "Beneficio"),Equipo1=c(modaComplejidadEquipo1, modaHorasHombreEquipo1, modaValorNegocioEquipo1, modaBugsEquipo1, modaNLCEquipo1, modaCalidadCodigoEquipo1, modaDeudaTecnicaEquipo1, modaRendimientoEquipo1, modaBeneficioEquipo1),Equipo2=c(modaComplejidadEquipo2, modaHorasHombreEquipo2, modaValorNegocioEquipo2, modaBugsEquipo2, modaNLCEquipo2, modaCalidadCodigoEquipo2, modaDeudaTecnicaEquipo2, modaRendimientoEquipo2, modaBeneficioEquipo2))
xtable(tablaDeModasPorEquipos)

#Coeficientes
nombres = c("Complejidad", "Horas Hombre", "Valor de Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio")
datosParaCorrelacionEquipo1 = data.frame(unidimensionalComplejidadesEquipo1, unidimensionalHorasHombreEquipo1, unidimensionalValorNegocioEquipo1,unidimensionalBugsEquipo1, unidimensionalNLCEquipo1, unidimensionalRendimientoEquipo1, unidimensionalBeneficioEquipo1 )
colnames(datosParaCorrelacionEquipo1) <- nombres
tablaCorrelacionKendallEquipo1 = cor(datosParaCorrelacionEquipo1,use="complete.obs", method="kendall")
tablaCorrelacionSpearmanEquipo1 = cor(datosParaCorrelacionEquipo1,use="complete.obs", method="spearman")
tablaCorrelacionPearsonEquipo1 = cor(datosParaCorrelacionEquipo1,use="complete.obs", method="pearson")

nombres = c("Complejidad", "Horas Hombre", "Valor de Negocio", "Bugs", "NLC", "Rendimiento", "Beneficio")
datosParaCorrelacionEquipo2 = data.frame(unidimensionalComplejidadesEquipo2, unidimensionalHorasHombreEquipo2, unidimensionalValorNegocioEquipo2,unidimensionalBugsEquipo2, unidimensionalNLCEquipo2, unidimensionalRendimientoEquipo2, unidimensionalBeneficioEquipo2)
colnames(datosParaCorrelacionEquipo2) <- nombres
tablaCorrelacionKendallEquipo2 = cor(datosParaCorrelacionEquipo2,use="complete.obs", method="kendall")
tablaCorrelacionSpearmanEquipo2 = cor(datosParaCorrelacionEquipo2,use="complete.obs", method="spearman")
tablaCorrelacionPearsonEquipo2 = cor(datosParaCorrelacionEquipo2,use="complete.obs", method="pearson")

#Para pasar las tablas a Latex.
#install.packages("xtable")
#library(xtable)
#xtable(tablaGeneral)

#Dataframe con los datos para graficar
Team1 <- data.frame(unidimensionalComplejidadesEquipo1, unidimensionalHorasHombreEquipo1, unidimensionalValorNegocioEquipo1, unidimensionalBugsEquipo1, unidimensionalCalidadCodigoEquipo1, unidimensionalDeudaTecnicaEquipo1, unidimensionalNLCEquipo1, unidimensionalRendimientoEquipo1, unidimensionalBeneficioEquipo1)
Team2 <- data.frame(unidimensionalComplejidadesEquipo2, unidimensionalHorasHombreEquipo2, unidimensionalValorNegocioEquipo2, unidimensionalBugsEquipo2, unidimensionalCalidadCodigoEquipo2, unidimensionalDeudaTecnicaEquipo2, unidimensionalNLCEquipo2, unidimensionalRendimientoEquipo2, unidimensionalBeneficioEquipo2)
juntos <- data.frame()
juntos <- rbind(juntos, data.frame(n=1:length(unidimensionalComplejidadesEquipo1),eje=unidimensionalComplejidadesEquipo1, tipo="Team1"))
juntos <- rbind(juntos, data.frame(n=1:length(unidimensionalComplejidadesEquipo2),eje=unidimensionalComplejidadesEquipo2, tipo="Team2"))

geomB1 <- data.frame()
geomB1 <- rbind(geomB1, data.frame(n=1:length(unidimensionalHorasHombreEquipo1), ejex=unidimensionalHorasHombreEquipo1,ejey=unidimensionalBeneficioEquipo1, tipo="Team1"))
geomB1 <- rbind(geomB1, data.frame(n=1:length(unidimensionalHorasHombreEquipo2), ejex=unidimensionalHorasHombreEquipo2,ejey=unidimensionalBeneficioEquipo2, tipo="Team2"))

geomB2 <- data.frame()
geomB2 <- rbind(geomB2, data.frame(n=1:length(unidimensionalNLCEquipo1), ejex=unidimensionalNLCEquipo1,ejey=unidimensionalRendimientoEquipo1, tipo="Team1"))
geomB2 <- rbind(geomB2, data.frame(n=1:length(unidimensionalNLCEquipo2), ejex=unidimensionalNLCEquipo2,ejey=unidimensionalRendimientoEquipo2, tipo="Team2"))

#Graficos de las nuevas funciones

#Histograma
#Valor de Negocio vs Calidad de Codigo Team1
g1 <- ggplot(data = Team1, aes(x = unidimensionalValorNegocioEquipo1, fill=unidimensionalCalidadCodigoEquipo1)) 
g1 <- g1 + geom_histogram()
g1 <- g1 + ggtitle("Complejidad por dia de trabajo")
g1 <- g1 + labs(x = "Valor de Negocio")
g1 <- g1 + labs(y = "Frecuencia")
g1 <- g1 + ggtitle("Valor de Negocio vs Frecuencia")
grid.draw(g1)

#Valor de Negocio vs Calidad de Codigo Team2
g2 <- ggplot(data = Team2, aes(x = unidimensionalValorNegocioEquipo2, fill=unidimensionalCalidadCodigoEquipo2)) 
g2 <- g2 + geom_histogram()
g2 <- g2 + ggtitle("Complejidad por dia de trabajo")
g2 <- g2 + labs(x = "Valor de Negocio")
g2 <- g2 + labs(y = "Frecuencia")
g2 <- g2 + ggtitle("Valor de Negocio vs Frecuencia")
grid.draw(g2)

#Boxplot
#Bugs vs Deuda Tecnica Team1
g3 <- ggplot(data = Team1, aes(x = unidimensionalDeudaTecnicaEquipo1))
g3 <- g3 + geom_boxplot(aes(y=unidimensionalBugsEquipo1))
g3 <- g3 + ggtitle("Deuda Tecnica vs Bugs Equipo1")
g3 <- g3 + labs(x = "Deuda Tecnica")
g3 <- g3 + labs(y = "Bugs")
grid.draw(g3)

#Bugs vs Deuda Tecnica Team2
g4 <- ggplot(data = Team2, aes(x = unidimensionalDeudaTecnicaEquipo2))
g4 <- g4 + geom_boxplot(aes(y=unidimensionalBugsEquipo2))
g4 <- g4 + ggtitle("Deuda Tecnica vs Bugs Equipo2")
g4 <- g4 + labs(x = "Deuda Tecnica")
g4 <- g4 + labs(y = "Bugs")
grid.draw(g4)

#Geonbar
#Horas Hombre vs Beneficio 
g5 <- ggplot(data = geomB1, aes(x = ejex, y=ejey, fill=tipo))
g5 <- g5 + geom_bar(stat="identity", position="dodge", color="black") 
g5 <- g5 + ggtitle("Horas Hombre vs Beneficio")
g5 <- g5 + labs(x = "Horas Hombre")
g5 <- g5 + labs(y = "Beneficio")
g5 <- g5 + labs(fill = "Equipos")
grid.draw(g5)

#Horas Rendimiento vs NLC
g6 <- ggplot(data = geomB2, aes(x = ejex, y=ejey, fill=tipo))
g6 <- g6 + geom_bar(stat="identity", position="dodge", color="black") 
g6 <- g6 + ggtitle("Rendimiento vs NLC")
g6 <- g6 + labs(x = "NLC")
g6 <- g6 + labs(y = "Rendimiento")
g6 <- g6 + labs(fill = "Equipos")
grid.draw(g6)

#Line
#Complejidad vs Dias de Trabajo
g9<- ggplot(data = juntos, aes(x=n, y=eje, fill=tipo)) + geom_bar(stat="identity", position="dodge")
g9 <- g9 + ggtitle("Complejidad por Equipo")
g9 <- g9 + labs(x = "Dias de trabajo")
g9 <- g9 + labs(y = "Complejidad")
g6 <- g6 + labs(fill = "Beneficio")
grid.draw(g9)
