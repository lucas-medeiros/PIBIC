#comando pra limpar a memoria
rm(list=ls(all=TRUE)) 

#bibliotecas
require(graphics)
library(forecast)
library(TTR)
library(glmnet) 
library(dplyr)   
library(psych)   
library(mvtnorm)
library(excelR)
library(elmNNRcpp)
library(readxl)
library(nnfor)

##define diret�rio correto: (modificar para a pasta que est�o os arquivos)
#arquivos .R devem estar na mesma pasta que todos os .txt
setwd("~/Facul/Inicia��o Cient�fica/C�digos R")

#suprime warning mensages
options(warn=-1)


#lendo fun��es auxiliares
source("COA.R")

#verifica se bibliotecas est�o instaladas
requiredPackages<-c("tcltk","graphics","forecast","TTR","graphics","ggplot2",
                    "dplyr","psych","metaheuristicOpt","pso","psoptim","tidyr",
                    "mvtnorm","excelR","elmNNRcpp","stringr","DEoptim","kernlab","nnfor")

#carrega os pacotes
for (p in requiredPackages){
  if(!require(p,character.only = TRUE)) 
    install.packages(p)
  library(p,character.only = TRUE)
}

#-----------------------------------------------------------------------------------

# M�todo de HoltWinters

##Modelo gen�rico
#leitura da base de dados: (valores em arquivo txt)
dados <- read.csv("dataset_energia eletrica_brasil.txt", header = T, sep = "\t", dec = ",")

total <- dados$Total
norte <- dados$Norte
nordeste <- dados$Nordeste
sudeste <- dados$Sudeste
sul <- dados$Sul
centro_oeste <- dados$Centro_Oeste

#formata��o da s�rie temporal
dados_ts <- ts(total, start = c(2004,1), end = c(2019,6), frequency = 12)
dados_ts <- ts(norte, start = c(2004,1), end = c(2019,6), frequency = 12)
dados_ts <- ts(nordeste, start = c(2004,1), end = c(2019,6), frequency = 12)
dados_ts <- ts(sudeste, start = c(2004,1), end = c(2019,6), frequency = 12)
dados_ts <- ts(sul, start = c(2004,1), end = c(2019,6), frequency = 12)
dados_ts <- ts(centro_oeste, start = c(2004,1), end = c(2019,6), frequency = 12)

#plot da s�rie temporal original
plot.ts(dados_ts, ylab = "Consumo de Energia El�trica (MWh)", xlab = "Ano")
#aplica��o do m�todo de Holt-Winters aditivo
dados_HW <- HoltWinters(dados_ts, seasonal = c("additive"))
plot(dados_HW, ylab = "Consumo de Energia El�trica (MWh)", xlab = "Ano")
legend("bottomright",legend=c("Valores medidos","Valores calculados"),col=c("black","red"), lty=c(1,2,3))
#plot de cada componente da s�rie separadamente
plot(fitted(dados_HW), ylab = "Consumo de Energia El�trica (MWh)", xlab = "Ano")
#biblioteca utiliza para previs�o com base no m�todo de Holt-Winters
dados_forecast <- forecast:::forecast.HoltWinters(dados_HW, h=12)
forecast:::plot.forecast(dados_forecast, ylab = "Consumo de Energia El�trica (MWh)", xlab = "Ano")
legend("bottomright",legend=c("Valores reais","Valores previstos"),col=c("black","blue"), lty=c(1,2,3))
plot.ts(dados_forecast$residuals) #para estimar a qualidade do modelo



#--------------------------------------------------------------------------------------

# M�todo LASSO

#sele��o dos dados: seleciona uma coluna em y e as demais para x
y <- dados %>% select(Total) %>% as.matrix()
x <- dados %>% select(-Total) %>% as.matrix()

dados07 = round(nrow(y)*0.7)                          #retorna n�mero inteiro que representa 70% dos valores dispon�veis

x_train = x[1:dados07, ]                                  #seleciona parte de treinamento
x_test  = x[(dados07 + 1):nrow(x), ]                      #seleciona parte de teste

y_train = matrix(y[1:dados07, ], ncol = 1)                #seleciona parte de treinamento de y
y_test  = matrix(y[(dados07 + 1):nrow(y), ], ncol = 1)    #seleciona parte de teste de y


set.seed(123)
# Valida��o cruzada de 10 etapas para selecionar o lambda
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# alpha = 1 para -> lasso regression
lasso_cv <- cv.glmnet(x, y, family = "gaussian", alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot do resultado da valida��o cruzada, destacando o valor m�nimo do erro m�dio quadr�tico
plot(lasso_cv)

# seleciona o melhor lambda (menor)
lambda_cv <- lasso_cv$lambda.min
# Realiza o modelo final utilizando o melhor lambda
model_cv <- glmnet(x=x_train, y=y_train, family = "gaussian", alpha = 1, lambda = lambda_cv, standardize = TRUE)

predict_train <- predict(model_cv, newx = x_train, s=lambda_cv) #previs�o com base no modelo final gerado

mse <- mean((y_train - predict_train)^2)

rsq_lasso_cv <- cor(y_train, predict_train)^2

cat('mse:',mse,'\n')
cat('rsq:',rsq_lasso_cv,'\n')

#Realiza o plot dos coeficientes para cada vari�vel, com legenda
#O coeficientes dimunuem a medida que o lambda aumenta
res <- glmnet(x_train, y_train, family = "gaussian", alpha = 1, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")

legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)
legend("topright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7) #usar se a legenda ficar na frente
legend("topleft", lwd = 1, col = 1:6, legend = colnames(x), cex = .7) #usar se a legenda ficar na frente


#--------------------------------------------------------------------------------------

# M�todo ELM

#leitura da base de dados: (valores em arquivo txt)
dataset_energia_eletrica_MATRIX <- read_excel("~/Facul/Inicia��o Cient�fica/C�digos R/dataset_energia_eletrica_MATRIX.xlsx")
dados <- dataset_energia_eletrica_MATRIX


#IMPORTANTE escolher os dados de acordo com o que ser� entrada e sa�da no arquivo dataset_wind.txt
input  <- dados %>% select(input) %>% as.matrix()
output <- dados %>% select(output) %>% as.matrix()


##################################################################

typeof(input)

problem <- 2

#divide dataset em duas partes - p% dos dados <- treinamento e (1-p)% <- teste
p       <- 0.7
tam     <- round(nrow(output)*p)                      #retorna n�mero inteiro que representa 70% dos valores dispon�veis

if (problem == 1) {   # regress�o
  cat("Problema de regress�o\n\n")
  input_tr <- input[1:tam, ]                            #seleciona parte de treinamento
  input_te <- input[(tam + 1):nrow(input), ]            #seleciona parte de teste
  
  output_tr <- matrix(output[1:tam, ], ncol = 1)               #seleciona parte de treinamento de y
  output_te <- matrix(output[(tam + 1):nrow(output), ], ncol = 1)    #seleciona parte de teste de y
}  


if (problem == 2) {   #s�rie temporal
  
  
  cat("Problema de previs�o de s�ries temporais\n\n")
  
  atraso       <- 1   # atraso no sinal de entrada do modelo
  
  # define o tipo e tamanho das matrizes
  input_tr   <- matrix(nrow=tam-atraso                         , ncol=dim(input)[2])
  output_tr  <- matrix(nrow=tam-atraso                         , ncol=ncol(output)) 
  
  input_te   <- matrix(nrow=dim(input)[1]-atraso-tam           , ncol=dim(input)[2])
  output_te  <- matrix(nrow=dim(output)[1]-atraso-tam          , ncol=ncol(output)) 
  
  #atribui os valores
  input_tr   <- matrix( input [(atraso+1):tam, ]               , ncol = ncol(input) )
  output_tr  <- matrix( output[(atraso+1):tam, ]               , ncol = ncol(output) )
  
  input_te   <- matrix( input [(atraso+tam):nrow(input)-1, ]   , ncol = ncol(input) )
  output_te  <- matrix( output[(atraso+tam):nrow(input), ]     , ncol = ncol(output) )
  
  cat("\nDimens�o do sinal de entrada (treinamento) : ", dim(input_tr))
  cat("\nDimens�o do sinal de sa�da  (teste)        : ", dim(input_te))
  
  
}  

# Definicoes para os otimizadores
# lb - (l - lower) limite inferior das variaveis de projeto
# ub - (u - upper) limite superior das variaveis de projeto
# [no. de neuronios na camada oculta (de 10 a 50), tipo da funcao de ativacao, tipo de inicializa��o, bias (polarizacao)]

lb        <- c(10,1,1,1)
ub        <- c(100,10,3,2)
numVar    <- length(lb)   # numero de variaveis de projeto da rede neural, que ser�o otimizadas

rangeVar  <- matrix(c(lb,ub), byrow=TRUE,nrow=2)
Iter      <- 250 # numero maximo de iteracoes do metodo de otimizacao
Pop       <- length(lb)*10  # numero da popula��o do metodo de otimizacao - 4*10


#RESULTADOS DA OTIMIZA��O
# Params armazena as vari�veis de projeto de otimiza��o, 4 vari�veis que deram o menor
# valor para a funcao objetivo

Params           <- matrix(nrow=6,ncol=length(lb),byrow=TRUE)
colnames(Params) <- paste("Params_",1:length(lb),sep="")
rownames(Params) <- c("DE","GWO","PSO","DA","MFO","COA")

#------------------------------------------------------------------------
# DE - DIFFERENTIAL EVOLUTION
#------------------------------------------------------------------------
cat("DE optimization starts! \n")
source('elm_obj.R')

set.seed(1234)
elm_de_opt       <- DEoptim(fn = elm_obj, lower = lb, upper = ub,
                            control = DEoptim.control(reltol=1e-3,steptol =10,itermax = Iter))

fitted_params_DE <- elm_de_opt$optim$bestmem
best_obj_DE      <- elm_de_opt$optim$bestval
Params[1,]       <- floor(fitted_params_DE)

cat('Parameters for DE         :',Params[1,],'\n')
cat('Objective function for DE :',best_obj_DE,'\n')

source('elm_graphics.R')

aux  <- elm_graphics(Params[1,],1)    # train
aux  <- elm_graphics(Params[1,],2)    # test

legend("bottomright", legend=c("Valor real","Valor previsto"),col=c("blue","red"),pch=c("-","-"), lty=c(1,2,3))

#----------------------------------------------------------------
# PSO - PARTICLE SWARM OPTIMIZATION
#----------------------------------------------------------------
Vmax <- 2;  ci <- 1.5;  cg <- 1.5;  w <- 0.9   #par�metros do PSO
cat("PSO optimization starts! \n")
Params[3,]<-PSO(elm_obj, optimType="MIN", numVar, numPopulation=Pop,
                maxIter=Iter, rangeVar, Vmax, ci, cg, w)
floor(Params[3,])
elm_obj(Params[3,])
cat('Parameters for PSO:',Params[3,],'\n')

cat('Parameters for PSO         :',Params[3,],'\n')

source('elm_graphics.R')

aux  <- elm_graphics(Params[3,],1)    # train
aux  <- elm_graphics(Params[3,],2)    # test


#------------------------------------------------------------------
# COA - COYOTE OPTIMIZATION ALGORITHM
#------------------------------------------------------------------
source('COA.R')
cat("COA optimization starts! \n")

# Objective function parameters
D     <- length(ub)
lu    <- matrix(lb,2,D,byrow=TRUE)
lu[2,]<- ub

# -- COA parameters
n_packs = 5
n_coy = 10
nfevalMAX = n_packs*n_coy*3

# -- Run the experiments
n_exper = 1; 	 # Number of experiments
y = matrix(0,1,n_exper)
t <- Sys.time()
for (i in 1:n_exper){
  otm_COA = COA(elm_obj,lu,nfevalMAX,n_packs,n_coy)
  print(Sys.time() - t)
  y[1,i] = otm_COA$globalMin
  t <- Sys.time()
}
Params[6,]<-otm_COA$globalParams
cat('Parameters for COA:',Params[6,],'\n')
# -- Show the statistics
v = c(min(y), mean(y), median(y), max(y), sd(y))
print('Statistics (min., avg., median, max., std.)')
print(v)
cat('Optimization process done! \n')

cat('Parameters for COA         :',Params[6,],'\n')

source('elm_graphics.R')

aux  <- elm_graphics(Params[6,],1)    # train
aux  <- elm_graphics(Params[6,],2)    # test


##################################################################

#M�todo ELM 2 - usando o pacote nnfor - para previs�o de s�ries temporais

#leitura dos dados
dados <- read.csv("dataset_energia eletrica_brasil.txt", header = T, sep = "\t", dec = ",")

total <- dados$Total

#formata��o da s�rie temporal
dados_ts <- ts(total, start = c(2004,1), end = c(2019,6), frequency = 12)

#cria o objeto elm - aplica o m�todo lasso para sele��o de vari�veis
dados_elm <- elm(dados_ts, m = frequency(dados_ts), hd = NULL, type = c("lasso"), reps = 20, comb = "mean", lags = NULL)

plot(dados_elm)

#realiza a previs�o da s�rie temporal com base no modelo ELM gerado
forecast_elm <- forecast(dados_elm, h = 12)

#plot da previs�o calculada
forecast:::plot.forecast(forecast_elm, ylab = "Consumo de Energia El�trica (MWh)", xlab = "Ano")
legend("bottomright",legend=c("Valores reais","Valores previstos"),col=c("black","blue"), lty=c(1,2,3))
plot.ts(forecast_elm$residuals) #para estimar a qualidade do modelo

