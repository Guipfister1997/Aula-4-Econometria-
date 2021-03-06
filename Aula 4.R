install.packages("urca") #instala o pacote urca
library("urca") #carrega o pacote
library(readxl)
interdaay <- read_excel("C:/Econometria/A1/interdaay.xls", 
                        +     col_types = c("date", "numeric", "numeric", 
                                            +         "numeric")) #carrega a tabela de excel interdaay do computador
View(interdaay) #vizualiza a tabela interdaay
interdaay <- interdaay[,-1] #retirar a primeira coluna

dados_diarios <- ts(interdaay, start = 2017-01-10, frequency = 365) #cria uma s�rie temporal com os dados da tabela que se inicia em 2017-01-10 com frequ�ncia d�ria
Variacao <- ts(interdaay$Variacao, start = 2017-01-10, frequency = 365) #cria uma s�rie temporal Varia��o
Ibovespa <- ts(interdaay$Ibovespa, start = 2017-01-10, frequency = 365) #cria s�rie temporal Ibovespa
Quantidade <- ts(interdaay$Quantidade, start = 2017-01-10, frequency = 365) #cria uma s�rie temporal Quantidade
plot(dados_diarios, col= "blue", main="Dados do Indice Bovespa", xlab="Dias") #cria um gr�fico dos dados di�rios
plot(Variacao, main="Percentual de Varia��o") #cria um gr�fico da Varia��o com t�tulo Percentual de Varia��o
plot(Ibovespa, main="Indice do Dia",col="red") #cria um gr�fico do Ibovespa com t�tulo Indice do Dia e cor vermelha
plot(Quantidade, main="Indice do Dia", xlab="Dias", col="blue") #cria um gr�fico da Quantidade com t�tulo Indice do dia descri��o do eixo x como Dias e cor azul

# Teste de Dick-Fuller

TesteDF_Variacao_none <- ur.df(Variacao, "none",lags = 0) #teste 1 DF-DickFuller sem drift e sem tend�ncia
TesteDF_Variacao_none #n�o cont�m o valor cr�tico do teste
summary(TesteDF_Variacao_none) #resumo estat�stico do teste cont�m o valor cr�tico com 1% 5% e 10% de signific�ncia

TesteDF_Variacao_drift <- ur.df(Variacao, "drift", lags=0) #teste 2 com drift
TesteDF_Variacao_drift
summary(TesteDF_Variacao_drift)
TesteDF_Variacao_trend <- ur.df(Variacao, "trend", lags = 0) #teste 3 com tend�ncia e com drift
TesteDF_Variacao_trend
summary(TesteDF_Variacao_trend)

TesteDF_Ibovespa_none <- ur.df(Ibovespa, "none",lags = 0)
TesteDF_Ibovespa_none
summary(TesteDF_Ibovespa_none)
TesteDF_Ibovespa_drift <- ur.df(Ibovespa, "drift", lags=0)
TesteDF_Ibovespa_drift
summary(TesteDF_Ibovespa_drift)
TesteDF_Ibovespa_trend <- ur.df(Ibovespa, "trend", lags = 0)
TesteDF_Ibovespa_trend
summary(TesteDF_Ibovespa_trend)
