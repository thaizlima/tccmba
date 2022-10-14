pacotes <- c("prophet","tidymodels","fpp2","tidyverse","tseries","itsmr",
             "timetk","modeltime","glmnet","h2o","modeltime.h2o", "seasonal", "fable",
             "quantmod", "gridExtra", "dygraphs","readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal")

##capotes padrão - Thaiz
library (readxl)
library(forecast)
library(tseries)
library (foreign)
library (xts)
library (astsa)
library (plotly)
library (urca)
library(lubridate)
library(tidyverse)
library(car)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(car)
library(mlogit)
library(dplyr)
library(tidyr)
library(fpp2)
library(stats)
library(quantmod)
library(tsibble)
library(gridExtra)
library(xtable)
library(scales)
library(tsutils)
library(caret)
library(feasts)
library(fpp3)
library(ExpDes.pt)



library(readxl)
bel <- read_excel("C:/Users/Thaiz Rodrigues/Desktop/Materiais USP - TCC/BANCO DE DADOS - BASE/bel.xlsx", 
                  col_types = c("date", "numeric"))


#######################################
#Convertendo o conjunto de dados para time series (ts)
belts=ts(bel$temp, start =c(1971, 7), end = c(2022, 9),frequency = 12)
plot(belts, ylab="Temperatura", mean="Conjunto de Dados ts", xlab='Ano')

seasonplot(belts, col = rainbow(12), year.labels = TRUE)

#######################################
#Dividindo janelas
beltreino=window(belts, start=c(1971,7), end=c(2019,12)) #base utilisada para analisar e prever
belval=window(belts, start= c(2020,1), end=c(2022,9))#Esta é a base que vamos prever + um h a frente

length(beltreino)
length(belval)

autoplot(beltreino) +
  autolayer(beltreino, series="Treino") +
  autolayer(belval, series="Validação") +
  scale_color_viridis_d() +
  theme_bw()

plot(beltreino, ylab="Temperatura", mean="Temperatura Máxima Média, Mensal", xlab='Ano')


#######################################
#Medidas de Tendencias da series: Media, Max, Min
sd(beltreino) 
var(beltreino)

#Parametros de Estima
summary(beltreino) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#27.72   29.82   30.66   30.84   31.72   35.03

#Gráficos da base treino
plot(beltreino, xlab="Ano", ylab="Temperatura", main="Análise Visual e Descritiva da Série Treino", las=1)
axis(side=1, at=seq(1970, 2000, 2020))
axis(side=2, at=seq(0, 30, 40), las=1)

abline(mean(beltreino), 0, col="red")
abline(median(beltreino), 0, col="blue")
abline(max(beltreino), 0, col="purple")
abline(min(beltreino), 0, col="purple3")

########################################
#Gráfico com as tendencias para base teste
#Medidas de Tendencias da series: Media, Max, Min
mean(belval)
sd(belval)
var(belval) 
summary(belval)


#graficos da base teste
plot(belval, xlab="Ano", ylab="Temperatura", main="Análise Visual e Descritiva da Série Teste", las=1)
axis(side=1, at=seq(1970, 2000, 2020))
axis(side=2, at=seq(0, 30, 40), las=1)

abline(mean(belval), 0, col="red")
abline(median(belval), 0, col="blue")
abline(max(belval), 0, col="purple")
abline(min(belval), 0, col="purple3")

#######################################
#grafico de ACF e PACF da base de treino
ggtsdisplay(beltreino)
acf(beltreino)
pacf(beltreino)

#teste de estacionaridade de Dickey-Fuller
testedf=ur.df(beltreino)
summary(testedf)


#teste de estacionaridade de KPSS 
testeKPSS=ur.kpss(beltreino)
summary(testeKPSS)
 


#######################################
#Verificando a quantidade de diferenciações necessarias:
ndiffs(beltreino)

#Fazendo a diferenciação:
dif_beltreino=diff(beltreino)
ggtsdisplay(dif_beltreino)

#######################################
#teste de estacionaridade de Dickey-Fuller - Diferenciação
test_beldif=ur.df(dif_beltreino)
summary(test_beldif)


#teste de estacionaridade de KPSS - Diferenciação
test_beldif=ur.kpss(dif_beltreino)
summary(test_beldif)


#######################################
#Indicando o melho modelo:
arima_bel=auto.arima(beltreino, trace=T
arima_bel$coef #mostra os coeficientes
arima_bel$var.coef
arima_bel$aic
arima_bel$bic

#validando diagnostico
#teste de teste Kolgomorov-Smirnov (KS):
ks.test(arima_bel$residuals, "pnorm", mean(arima_bel$residuals),
        sd(arima_bel$residuals))


#######################################
# Previsao para a série
prev_bel=forecast::forecast(arima_bel, h=48) #prevendo 48 meses a frente (4 anos)
prev_bel

autoplot(prev_bel) +
  theme_bw()

#Calculando os erros
accuracy(prev_bel, belval)


ggplotly(
  autoplot(beltreino)+
    autolayer(belval,serie="Valores Reais")+
    autolayer(prev_bel$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)

#Teste de Média:
t.test(belval, prev_bel, paired = TRUE)

#Update: o script do R utilizado aqui e o arquivo com os dados aqui.
###########################################################################
########FIIIIMM#########
