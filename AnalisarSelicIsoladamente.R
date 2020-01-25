library(forecast)
library(ggplot2)
library(seasonal)
library(seasonalview)
library(urca)

#Ler o arquivo com os dados da taxa selic ao mês(03/1999 - 01/2020 Mensal)
arq = read.csv(file.choose(), header = T, sep = ";", dec = ",")
print(arq)

#Transformar o arquivo em uma série temporal
selic = ts(arq$meta_selic,start = c(1999,3), end=c(2019,12), frequency=12)

plot(selic)

summary(selic)

#-------------------------------------------------------------------------------------

autoplot(selic)
#A série não aparenta ter sazonalidade, possui alguma variação e parece ter uma
#tendência de queda

#-------------------------------------------------------------------------------------

#Histograma para ver como os dados estão distribuídos
hist(selic)
#A maior parte do tempo a taxa estava entre 10% e 20%.

#-------------------------------------------------------------------------------------
#Gerar um boxplot para entender como que a ocupação está distribuída
boxplot(selic)
#Como visto no histograma a mediana está por volta de 13% e uma taxa acima de
#26%(em torno de 26/27%) já é considerada alta(Outlier)

#-------------------------------------------------------------------------------------
#Vamos decompor a série para entender melhor como se comporta a selic
dec = decompose(selic)
autoplot(dec)  
#Existe um padrão de sazonalidade. Pelo gráfco ela é bem regular  
#A tendência tem forte variação porém com queda durante o período  

#-------------------------------------------------------------------------------------
#Até agora nos vimos que a taxa média da selic é de cerca de 13%aa.
#Vimos que taxas acima de 25% é exceção e não regra;
#Existe um padrão sazonal frequente
#No período analisado existe uma tendencia de queda da taxa(Notícia boa)

#-------------------------------------------------------------------------------------
#Vamos analisar a tendência com mais cuidado:
autoplot(dec$trend)
#A taxa vem caindo ao longo dos anos, porém em meados de 2013 aprasentou um forte aumento que conincide com
#o início da crise econômica. E, 2016 iniciou-se uma queda acentuada.
autoplot(window(dec$trend, start=c(2016,01)))
#Essa tendência de queda da taxa Selici começou no início de 2016, estabilizou no segundo semestre de
# 2018 e em 2019 voltou a cair chegando a 4,5%aa, taxa inédida no país.


#-------------------------------------------------------------------------------------
#Vamos analisar a sazonalidade com mais cuidado:
#Gera um plote com a ocupação sazonal 2000-2020 (cada ano é uma cor)
ggseasonplot(selic)  
#Aqui se confirma que existe um padrão bem regular na sazonalidade  
#Apenas dois anos apresentam não lineares 1999 e 2003
#Nos demais anos há diferenca de taxa, porém essas se mantém com certa estabilidade durante o ano
#As linhas demosntram, também, uma queda ao longo dos anos.

#Ver a ocupação a partir de 2016:
ggseasonplot(window(selic, start=c(1999,01), end = c(2003, 12)))
#Confirmação dos dados anteriores, apresentando apenas dados mais limpos

#-------------------------------------------------------------------------------------
#Gerar o modelo preditivo para a Selic:
#Previsão para os próximos dois anos (Forecast de dois anos)

#1º - ARIMA---------------------------------------------------------------------------
#Usando a função auto.arima

#a)Testar se os dados são estacionários ou não(Precisam de diferenciação?):
est = ur.kpss(selic)
print(est)
#Resultado:
####################################### 
# KPSS Unit Root / Cointegration Test # 
####################################### 
#The value of the test statistic is: 2.9205 

#o valor de p está muito acima de 0.05, não utilizaremos Arima para fins de predição! Apenas análise
ndiffs(selic)
#Resultado [1] 1
#Com esse resultado deveremos fazer a diferenciação uma vez
selicdiff = diff(selic)
ndiffs(selicdiff)

#Vamos ver o diagrama de autocorrelação dos dados:
#tssdisplay que gera os dois gráficos
tsdisplay(selicdiff)
#Típico de dados altamente sazonais
#Diagrama de correlação parcial, PACF, indica vários ponto de autocorrelação

#Partindo para o modelo ARIMA:
#Usar os parâmetros "trace=T", para não limitar o modelo
modelo = auto.arima(selicdiff, trace=T, stepwise = F, approximation = F )
#Retornou como melhor modelo:
#Best model: ARIMA(4,0,0)(1,0,0)[12] with zero mean  
#Melhor modelo Arima(400) para a parte não sazonal
#E a frequência do arquivo é 12
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

#Verificar se os resíduos estão adequados. Se ficou informação nos resíduos que deveriam estar no modelo
#Verificando os resíduos do modelo:
checkresiduals(modelo)
#Resultado:
#         Ljung-Box test
#
#data:  Residuals from ARIMA(4,0,0)(1,0,0)[12] with zero mean
#Q* = 39.839, df = 19, p-value = 0.003435
#
#Model df: 5.   Total lags used: 24

#Já podemos observar, em 1º lugar, que o p-value é 0.003435
#Isso indica que não existe correlação entre os resíduos (Eles são white noise)

#Vamos olhar os gráficos:
#Não se vê nenhum padrão aparente nos residuais
#Existe uma legs onde há significância no diagrama de autocorrelação (ACF), mas é apenas uma
#No histograma os dados, aparentemente, distribuídos normalmente

#OBs:Porque no checkresiduals nós passamos o modelo e no shapiro.test passamos o objeto modelo$residuals?
#Isso ocorre porque o checkresiduals já encontra os residuais dentro do modelo e o shapiro.test não,
#ele é uma função genérica de teste de normalidade, então temos que explicitar para a função o que se
#quer testar.

#Fazer mais um teste para ver se os dados estão distribuídos normalmente:
shapiro.test(modelo$residuals)
#Resposta:
#Shapiro-Wilk normality test
#
#data:  modelo$residuals
#W = 0.80281, p-value < 2.2e-16

#Nós temos um p-value de 2.17 que é bastante alto, bem mais alto que 0.05, entendemos assim que
#os dados estão normalmente distribuídos.

#Podemos checar a Variância dos resíduos e checar a média
var(modelo$residuals)
#[1] 0.3923729
mean(modelo$residuals)
#[1] -0.008154001
#A variância é baixa e a média bem próxima de zero

#Então todos eses são indicativos de que criamos um bom modelo que vai ser eficiente para prever
#a situação do hotel para os próximos dois anos

#Partindo para a PREVISÃO:
previsao = forecast(previsao, h=24)
print(previsao)
autoplot(previsao)
#########################################
#
#  meu problema esta aqui!
#
#       Como fazer para plotar o gráfico desta previsão de arima com diferenciação
#       com os valores "normais", ou seja, a taxa variam na série entre 40%aa e 4,5%aa(valor atual)
#       gostaria que minhas previsões ficassem nessa faixa e não variando entre -10 e 10.
#
#########################################


#Analisando o Gráfico verificamos que a previsão capturou correntamente a sazonalidade da série
#para os próximos dois anos(Modelobem próximo do ideal)


#OBS: Para comparar os dois modelos nós não poderemos olhar o índice de preformance do modelo.
#Vimos que o AIC do modelo acima foi 706.25
#Eu não posso gerar um modelo de suavização exponencial com ets, por exemplo, e  comparar o 
#AIC dele ou o BIC com este modelo
#O que teremos que fazer para comparar a performance destes dois modelos é um processo de separar
#os dados em treino e teste e depois usar um índice de avaliação de performance e não um índice de 
#avaliação de modelo.

#2º - Modelo ets com Suavização Exponencial-----------------------------------------------
#Usando a função ets:

#Faremos um processo de Hold-out (Dividir os dados entre treino e teste)
#Criaremos os dois modelos, fazer a previsão accurancy para comparar os índices
#OBS: Como vamos dividir os dados entre treino e teste, vamos ter que recriar o modelo ARIMA
#Não seria justo usar o moodelo anterior paraa fazer a comparação com ets.
#Para criar uma análise mais justa temos que criar modelos com os mesmos dados.

#Primeiro passo: Dividir a série temporal entre treino e teste:
#Dados de 2003-2017
#Vamos usar dados de 2003-2015 para treino e de 2016-2017 para teste
#13 anos para treino e 2 anos para teste

selictreino = ts(arq$meta_selic, start = c(1999,1), end = c(2017,12), frequency = 12)
selicteste = ts(arq$meta_selic, start = c(2018,1), end = c(2019,12), frequency = 12)

#Criando modelo Arima
modeloarima = auto.arima(selictreino, trace = T, stepwise = F, approximation = F, lambda="auto")
#Result:
#Best model: ARIMA(2,0,0)(0,1,2)[12] 
#Observamos aqui que o modelo é diferente de quando os dados não foram particionados
#Arima (2,0,0) (na parte não sazonal)
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


#Os dois modelos são extremamente parecidos

#Podemos comparar as duas previsões com os dados de treino

#3º - Comparar os dois modelos--------------------------------------------------------
#Qual é o mais adequado?????

accuracy(prevernnetar, selicteste)
#                      ME     RMSE      MAE         MPE     MAPE      MASE         ACF1 Theil's U
#Training set  0.08376679 1.696600 1.327916  0.04804535 4.207832 0.8206864  0.007874157        NA
#Test set     -0.62580455 2.847596 2.476824 -2.21883870 8.188865 1.5307408 -0.014641481 0.3100383

accuracy(preverets, selicteste)
#                      ME     RMSE      MAE        MPE     MAPE      MASE      ACF1 Theil's U
#Training set 0.003681829 1.782438 1.382404 -0.4102375 4.357550 0.8543612 0.2100046        NA
#Test set     0.409191024 3.128761 2.677909  1.8017713 8.397247 1.6550166 0.1051434 0.3481312
accuracy(Preverarima, selicteste)

#Observando o ME os valores para o ARIMA estão menores
#Observando o RMSE os valores para ARIMA estão menores
#Observando o MAE os valores para ARIMA estão menores
#Observando o MPE os valores para ARIMA estão menores
#Observando o MAPE os valores para ARIMA estão menores
#Observando o MASE os valores para ARIMA estão menores
#Observando o ACF1 os valores para ARIMA estão menores

#Todos os índices de performance dão "ganho" para o modelo ARIMA
#Sendo que de fato o arima é o modelo mais poderoso utilizado.

plot(selic)
lines(prevernnetar$mean, col="red")
lines(preverets$mean, col="green")
lines(Preverarima$mean, col="blue")

modelo = nnetar(co2)
print(modelo)

prev = forecast(dados, h=24)
print(prev)

autoplot(prev)

