library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao m�s(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma s�rie temporal
selic = ts(arq$meta_selic,start = c(1999,3), end=c(2019,12), frequency=12)

plot(selic)

summary(selic)

#-------------------------------------------------------------------------------------

autoplot(selic)
#A s�rie n�o aparenta ter sazonalidade, possui alguma varia��o e parece ter uma
#tend�ncia de queda

#-------------------------------------------------------------------------------------

#Histograma para ver como os dados est�o distribu�dos
hist(selic)
#A maior parte do tempo a taxa estava entre 10% e 20%.

#-------------------------------------------------------------------------------------
#Gerar um boxplot para entender como que a ocupa��o est� distribu�da
boxplot(selic)
#Como visto no histograma a mediana est� por volta de 13% e uma taxa acima de
#26%(em torno de 26/27%) j� � considerada alta(Outlier)

#-------------------------------------------------------------------------------------
#Vamos decompor a s�rie para entender melhor como se comporta a selic
dec = decompose(selic)
autoplot(dec)  
#Existe um padr�o de sazonalidade. Pelo gr�fco ela � bem regular  
#A tend�ncia tem forte varia��o por�m com queda durante o per�odo  

#-------------------------------------------------------------------------------------
#At� agora nos vimos que a taxa m�dia da selic � de cerca de 13%aa.
#Vimos que taxas acima de 25% � exce��o e n�o regra;
#Existe um padr�o sazonal frequente
#No per�odo analisado existe uma tendencia de queda da taxa(Not�cia boa)

#-------------------------------------------------------------------------------------
#Vamos analisar a tend�ncia com mais cuidado:
autoplot(dec$trend)
#A taxa vem caindo ao longo dos anos, por�m em meados de 2013 aprasentou um forte aumento que conincide com
#o in�cio da crise econ�mica. E, 2016 iniciou-se uma queda acentuada.
autoplot(window(dec$trend, start=c(2016,01)))
#Essa tend�ncia de queda da taxa Selici come�ou no in�cio de 2016, estabilizou no segundo semestre de
# 2018 e em 2019 voltou a cair chegando a 4,5%aa, taxa in�dida no pa�s.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupa��o sazonal 2000-2020 (cada ano � uma cor)
ggseasonplot(selic)  
#Aqui se confirma que existe um padr�o bem regular na sazonalidade  
#Apenas dois anos apresentam n�o lineares 1999 e 2003
#Nos demais anos h� diferenca de taxa, por�m essas se mant�m com certa estabilidade durante o ano
#As linhas demosntram, tamb�m, uma queda ao longo dos anos.

#Ver a ocupa��o a partir de 2016:
ggseasonplot(window(selic, start=c(1999,01), end = c(2003, 12)))
#Confirma��o dos dados anteriores, apresentando apenas dados mais limpos

#-------------------------------------------------------------------------------------
#Gerar o modelo preditivo para a Selic:
#Previs�o para os pr�ximos dois anos (Forecast de dois anos)

#1� - ARIMA---------------------------------------------------------------------------
#Usando a fun��o auto.arima

#a)Testar se os dados s�o estacion�rios ou n�o(Precisam de diferencia��o?):
est = ur.kpss(selic)
print(est)
#Resultado:
####################################### 
# KPSS Unit Root / Cointegration Test # 
####################################### 
#The value of the test statistic is: 2.9205 

#o valor de p est� muito acima de 0.05, n�o utilizaremos Arima para fins de predi��o! Apenas an�lise
ndiffs(selic)
#Resultado [1] 1
#Com esse resultado deveremos fazer a diferencia��o uma vez
selicdiff = diff(selic)
ndiffs(selicdiff)

#Vamos ver o diagrama de autocorrela��o dos dados:
#tssdisplay que gera os dois gr�ficos
tsdisplay(selicdiff)
#T�pico de dados altamente sazonais
#Diagrama de correla��o parcial, PACF, indica v�rios ponto de autocorrela��o

#Partindo para o modelo ARIMA:
#Usar os par�metros "trace=T", para n�o limitar o modelo
modelo = auto.arima(selicdiff, trace=T, stepwise = F, approximation = F )
#Retornou como melhor modelo:
#Best model: ARIMA(4,0,0)(1,0,0)[12] with zero mean  
#Melhor modelo Arima(400) para a parte n�o sazonal
#E a frequ�ncia do arquivo � 12
print(modelo)
#Resultado:
#Series: selicdiff 
#ARIMA(4,0,0)(1,0,0)[12] with zero mean 
#
#Coefficients:
#  ar1     ar2      ar3      ar4     sar1
#0.8713  0.4452  -0.1922  -0.1955  -0.1410
#s.e.  0.0641  0.0959   0.0940   0.0867   0.0807
#
#sigma^2 estimated as 0.3988:  log likelihood=-238.97
#AIC=489.94   AICc=490.28   BIC=511.07

#Verificar se os res�duos est�o adequados. Se ficou informa��o nos res�duos que deveriam estar no modelo
#Verificando os res�duos do modelo:
checkresiduals(modelo)
#Resultado:
#         Ljung-Box test
#
#data:  Residuals from ARIMA(4,0,0)(1,0,0)[12] with zero mean
#Q* = 39.839, df = 19, p-value = 0.003435
#
#Model df: 5.   Total lags used: 24

#J� podemos observar, em 1� lugar, que o p-value � 0.003435
#Isso indica que n�o existe correla��o entre os res�duos (Eles s�o white noise)

#Vamos olhar os gr�ficos:
#N�o se v� nenhum padr�o aparente nos residuais
#Existe uma legs onde h� signific�ncia no diagrama de autocorrela��o (ACF), mas � apenas uma
#No histograma os dados, aparentemente, distribu�dos normalmente

#OBs:Porque no checkresiduals n�s passamos o modelo e no shapiro.test passamos o objeto modelo$residuals?
#Isso ocorre porque o checkresiduals j� encontra os residuais dentro do modelo e o shapiro.test n�o,
#ele � uma fun��o gen�rica de teste de normalidade, ent�o temos que explicitar para a fun��o o que se
#quer testar.

#Fazer mais um teste para ver se os dados est�o distribu�dos normalmente:
shapiro.test(modelo$residuals)
#Resposta:
#Shapiro-Wilk normality test
#
#data:  modelo$residuals
#W = 0.80281, p-value < 2.2e-16

#N�s temos um p-value de 2.17 que � bastante alto, bem mais alto que 0.05, entendemos assim que
#os dados est�o normalmente distribu�dos.

#Podemos checar a Vari�ncia dos res�duos e checar a m�dia
var(modelo$residuals)
#[1] 0.3923729
mean(modelo$residuals)
#[1] -0.008154001
#A vari�ncia � baixa e a m�dia bem pr�xima de zero

#Ent�o todos eses s�o indicativos de que criamos um bom modelo que vai ser eficiente para prever
#a situa��o do hotel para os pr�ximos dois anos

#Partindo para a PREVIS�O:
previsao = forecast(previsao, h=24)
print(previsao)
autoplot(previsao)
#########################################
#
#  meu problema esta aqui!
#
#       Como fazer para plotar o gr�fico desta previs�o de arima com diferencia��o
#       com os valores "normais", ou seja, a taxa varia na s�rie entre 40%aa e 4,5%aa(valor atual)
#       gostaria que minhas previs�es ficassem nessa faixa e n�o variando entre -10 e 10.
#
#########################################


#Analisando o Gr�fico verificamos que a previs�o capturou correntamente a sazonalidade da s�rie
#para os pr�ximos dois anos(Modelobem pr�ximo do ideal)


#OBS: Para comparar os dois modelos n�s n�o poderemos olhar o �ndice de preformance do modelo.
#Vimos que o AIC do modelo acima foi 706.25
#Eu n�o posso gerar um modelo de suaviza��o exponencial com ets, por exemplo, e  comparar o 
#AIC dele ou o BIC com este modelo
#O que teremos que fazer para comparar a performance destes dois modelos � um processo de separar
#os dados em treino e teste e depois usar um �ndice de avalia��o de performance e n�o um �ndice de 
#avalia��o de modelo.

#2� - Modelo ets com Suaviza��o Exponencial-----------------------------------------------
#Usando a fun��o ets:

#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os dois modelos, fazer a previs�o accurancy para comparar os �ndices
#OBS: Como vamos dividir os dados entre treino e teste, vamos ter que recriar o modelo ARIMA
#N�o seria justo usar o moodelo anterior paraa fazer a compara��o com ets.
#Para criar uma an�lise mais justa temos que criar modelos com os mesmos dados.

#Primeiro passo: Dividir a s�rie temporal entre treino e teste:
#Dados de 2003-2017
#Vamos usar dados de 2003-2015 para treino e de 2016-2017 para teste
#13 anos para treino e 2 anos para teste

selictreino = ts(arq$meta_selic, start = c(1999,1), end = c(2017,12), frequency = 12)
selicteste = ts(arq$meta_selic, start = c(2018,1), end = c(2019,12), frequency = 12)

#Criando modelo Arima
modeloarima = auto.arima(selictreino, trace = T, stepwise = F, approximation = F, lambda="auto")
#Result:
#Best model: ARIMA(2,0,0)(0,1,2)[12] 
#Observamos aqui que o modelo � diferente de quando os dados n�o foram particionados
#Arima (2,0,0) (na parte n�o sazonal)
#Criar previsao para 2016,1-2017,12 
Preverarima = forecast(modeloarima, h=24)

#Criando modelo ets
modeloets = ets(selictreino)
preverets = forecast(modeloets, h=24)
modelonnetar = nnetar(selictreino)
prevernnetar = forecast(modelonnetar, h=24)


#Ver graficamente os dois modelos:
plot(selictreino)
lines(preverets$mean, col="red")
lines(prevernnetar$mean, col="blue")
lines(Preverarima$mean, col="green")


#Os dois modelos s�o extremamente parecidos

#Podemos comparar as duas previs�es com os dados de treino

#3� - Comparar os dois modelos--------------------------------------------------------
#Qual � o mais adequado?????

accuracy(prevernnetar, selicteste)
#                      ME     RMSE      MAE         MPE     MAPE      MASE         ACF1 Theil's U
#Training set  0.08376679 1.696600 1.327916  0.04804535 4.207832 0.8206864  0.007874157        NA
#Test set     -0.62580455 2.847596 2.476824 -2.21883870 8.188865 1.5307408 -0.014641481 0.3100383

accuracy(preverets, selicteste)
#                      ME     RMSE      MAE        MPE     MAPE      MASE      ACF1 Theil's U
#Training set 0.003681829 1.782438 1.382404 -0.4102375 4.357550 0.8543612 0.2100046        NA
#Test set     0.409191024 3.128761 2.677909  1.8017713 8.397247 1.6550166 0.1051434 0.3481312
accuracy(Preverarima, selicteste)

#Observando o ME os valores para o ARIMA est�o menores
#Observando o RMSE os valores para ARIMA est�o menores
#Observando o MAE os valores para ARIMA est�o menores
#Observando o MPE os valores para ARIMA est�o menores
#Observando o MAPE os valores para ARIMA est�o menores
#Observando o MASE os valores para ARIMA est�o menores
#Observando o ACF1 os valores para ARIMA est�o menores

#Todos os �ndices de performance d�o "ganho" para o modelo ARIMA
#Sendo que de fato o arima � o modelo mais poderoso utilizado.

plot(selic)
lines(prevernnetar$mean, col="red")
lines(preverets$mean, col="green")
lines(Preverarima$mean, col="blue")

modelo = nnetar(co2)
print(modelo)

prev = forecast(dados, h=24)
print(prev)

autoplot(prev)

