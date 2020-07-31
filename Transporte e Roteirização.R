#--------------------------------------------------------------------------------
# Problema a resolver
#-------------------------------------------------------------------------------

# Tres fabricas devem distribuir os produtos produzidos entre 5 depositos, e 
# queremos saber quanto devemos enviar de cada fabrica para cada deposito, 
# minimizando os custos de transporte. A tabela dos custos de transporte por 
# unidade transportada segue abaixo:
  
#  Depositos	| Fabrica X  | Fabrica Y	| Fabrica Z
#     A	      |   1,50	   |   1,05	    |   0,93
#     B	      |   0,70	   |   0,99	    |   0,97
#     C	      |   1,25	   |   1,15	    |   1,00
#     D	      |   0,95	   |   1,00	    |   1,35
#     E	      |   1,00	   |   1,00	    |   1,20

# Tambem temos a demanda em cada deposito, 
# A: 10.000 unidades
# B: 15.000 unidades
# C: 25.000 unidades
# D: 20.000 unidades
# E: 17.500 unidades
# E as capacidades de cada uma das fabricas, 
# X: 35.000 unidades
# Y: 30.000 unidades 
# Z: 30.000 unidades

#------------------------------------------------------------------------
# Modelando o problema
#------------------------------------------------------------------------

# Minimizar o custo de transporte 

# a = Fabrica X
# b = Fabrica Y
# c = Fabrica Z
# 1 = Deposito A
# 2 = Deposito B
# 3 = Deposito C
# 4 = Deposito D
# 5 = Deposito E

# Funcao objetivo
# MinC= 1.50Xa1 + 0.70Xa2 + 1.25Xa3 + 0.95Xa4 + 1.00Xa5
#       1.05Xb1 + 0.99Xb2 + 1.15Xb3 + 1.00Xb4 + 1.00Xb5
#       0.93Xc1 + 0.97Xc2 + 1.00Xc3 + 1.35Xc4 + 1.20Xc5  (R$)

# Restricoes de capacidade
# Xa1 + Xa2 + Xa3 + Xa4 + Xa5 <= 35000 (unidades)
# Xb1 + Xb2 + Xb3 + Xb4 + Xb5 <= 30000 (unidades)
# Xc1 + Xc2 + Xc3 + Xc4 + Xc5 <= 30000 (unidades)
# Restricoes de demanda
# Xa1 + Xb1 + Xc1 >= 10000 (unidades)
# Xa2 + Xb2 + Xc2 >= 15000 (unidades)
# Xa3 + Xb3 + Xc3 >= 25000 (unidades)
# Xa4 + Xb4 + Xc4 >= 20000 (unidades)
# Xa5 + Xb5 + Xc5 >= 17500 (unidades)

#-----------------------------------------------------------------------
# Implementando o problema 
#-----------------------------------------------------------------------

# install.packages("lpSolve")
library(lpSolve)

funcao_objetivo = matrix(c(1.50, 0.70, 1.25, 0.95, 1.00,
                           1.05, 0.99, 1.15, 1.00, 1.00,
                           0.93, 0.97, 1.00, 1.35, 1.20), ncol = 5, byrow = T)

sinal_linha = c("<=",
                "<=",
                "<=")

param_linha = c(35000,
                30000,
                30000)

sinal_coluna = c(">=",
                 ">=",
                 ">=",
                 ">=",
                 ">=")

param_coluna = c(10000,
                 15000,
                 25000,
                 20000,
                 17500)

res_modelo = lp.transport(funcao_objetivo,     # funcao objetivo
                          "min",               # minimizar a funcao objetivo
                          sinal_linha,         # sinais das linhas
                          param_linha,         # param. das linhas
                          sinal_coluna,        # sinal das colunas
                          param_coluna,        # param. das colunas
                          integers = T,        # variavies inteiras
                          compute.sens = T)    # computa sensibilidade

#------------------------------------------------------------------------
# Resultado
#------------------------------------------------------------------------

res_modelo                            # custo em R$
res_modelo$solution                   # qtd de unidades 
res_modelo$sens.coef.from             # valores min dos parametros
res_modelo$sens.coef.to               # valores max dos parametros
res_modelo$duals                      # preco sombra das restricoes
res_modelo$duals.from                 # preco sombra min das restricoes
res_modelo$duals.to                   # preco sombra max das restricoes 
