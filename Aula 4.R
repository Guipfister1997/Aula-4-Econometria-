install.packages("urca") #instala o pacote urca
library("urca") #carrega o pacote
library(readxl)
interdaay <- read_excel("C:/Econometria/A1/interdaay.xls", 
                        +     col_types = c("date", "numeric", "numeric", 
                                            +         "numeric")) #carrega a tabela de excel interdaay do computador
View(interdaay) #vizualiza a tabela interdaay
interdaay <- interdaay[,-1] #retirar a primeira coluna

dados_diarios <- ts(interdaay, start = 2017-01-10, frequency = 365) #cria uma série temporal com os dados da tabela que se inicia em 2017-01-10 com frequência dária
Variacao <- ts(interdaay$Variacao, start = 2017-01-10, frequency = 365) #cria uma série temporal Variação
Ibovespa <- ts(interdaay$Ibovespa, start = 2017-01-10, frequency = 365) #cria série temporal Ibovespa
Quantidade <- ts(interdaay$Quantidade, start = 2017-01-10, frequency = 365) #cria uma série temporal Quantidade
plot(dados_diarios, col= "blue", main="Dados do Indice Bovespa", xlab="Dias") #cria um gráfico dos dados diários
plot(Variacao, main="Percentual de Variação") #cria um gráfico da Variação com título Percentual de Variação
plot(Ibovespa, main="Indice do Dia",col="red") #cria um gráfico do Ibovespa com título Indice do Dia e cor vermelha
plot(Quantidade, main="Indice do Dia", xlab="Dias", col="blue") #cria um gráfico da Quantidade com título Indice do dia descrição do eixo x como Dias e cor azul

# Teste de Dick-Fuller

TesteDF_Variacao_none <- ur.df(Variacao, "none",lags = 0) #teste 1 DF-DickFuller sem drift e sem tendência
TesteDF_Variacao_none #não contém o valor crítico do teste
summary(TesteDF_Variacao_none) #resumo estatístico do teste contém o valor crítico com 1% 5% e 10% de significância

TesteDF_Variacao_drift <- ur.df(Variacao, "drift", lags=0) #teste 2 com drift
TesteDF_Variacao_drift
summary(TesteDF_Variacao_drift)
TesteDF_Variacao_trend <- ur.df(Variacao, "trend", lags = 0) #teste 3 com tendência e com drift
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
