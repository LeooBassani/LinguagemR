#-------------------------------------------------
# Problema a ser resolvido
#------------------------------------------------

# A empresa opera de segunda a sexta-feira e fabrica dois modelos de cintos 
# de couro, Social e Esporte. A disponibilidade do couro permite fabricar até 
# 800 cintos por dia. Os cintos empregam fivelas diferentes, cuja disponibilidade
# semanal é de 2000 para cintos Social e 1400 para cintos Esporte. Os lucros
# unitários são de R$ 5 para cada cinto Social e R$3 para cada cinto Esporte. 
# A quantidade de horas necessárias para confeccionar uma centena de cada produto,
# assim como o número total de horas disponíveis em cada departamento são 
# apresentados na tabela a seguir. Determine o plano de produção semanal que 
# maximiza o lucro.

# Etapa	    | Disponibilidade |Horas necessárias p/ 100 unidades
#           | (horas / semana)|	  Social	   |    Esporte
# Tingimento|      80	        |     2	       |      2,5
# Corte	    |      60	        |    1,5	     |       1
# Costura	  |      120	      |     4	       |       2
# Embalagem	|      40	        |    0,5	     |      0,3

#--------------------------------------------------
# Modelando o problema
#-------------------------------------------------

# PROGRAMAÇÃO LINEAR

# Maximizar o lucro

# Xs = Cinto social
# Xe = Cinto esportivo

# Funcao objetivo
# MaxL= 5Xs + 3Xe    R$

# Restricoes
# 1.000Xs + 1.000Xe <= 4000   cintos/semana = 800 dia = 4000 semana
# 1.000Xs + 0.000Xe <= 2000   cintos/semana = disp. fivelas Xs
# 0.000Xs + 1.000Xe <= 1400   cintos/semana = disp. fivelas Xe
# 0.020Xs + 0.025Xe <= 80     cintos/semana = horas/100uni
# 0.015Xs + 0.010Xe <= 60     cintos/semana = horas/100uni
# 0.040Xs + 0.020Xe <= 120    cintos/semana = horas/100uni
# 0.005Xs + 0.003Xe <= 40     cintos/semana = horas/100uni

#---------------------------------------------------------
# Implementando o problema
#---------------------------------------------------------

# Instalar e carregar pacote lpSolve
# install.packages("lpSolve")
library(lpSolve)

# Criando o modelo
funcao_objetivo = c(5,3)
restricoes = matrix(c(1.000, 1.000,
                      1.000, 0.000,
                      0.000, 1.000,
                      0.020, 0.025,
                      0.015, 0.010,
                      0.040, 0.020,
                      0.005, 0.003),ncol = 2, byrow = T)

restricoes_dir = c("<=",
                   "<=",
                   "<=",
                   "<=",
                   "<=",
                   "<=",
                   "<=")

restricoes_rhs = c(4000,
                   2000,
                   1400,
                   80,
                   60,
                   120,
                   40)

res_modelo = lp("max",             # maximizar o lucro
                funcao_objetivo,   # funcao objetivo
                restricoes,        # matriz de restricoes
                restricoes_dir,    # sinais relacionados a restricao
                restricoes_rhs,    # vetor das restricoes (rhs)
                all.int = T,       # variaveis inteiras
                compute.sens = T)  # computa a sensibilidade

#-------------------------------------------------------------
# Resultados
#-------------------------------------------------------------

res_modelo                 # (R$)
res_modelo$solution        # (qtd cintos/semana)
res_modelo$sens.coef.from  # valores min dos parametros
res_modelo$sens.coef.to    # valores max dos parametros
res_modelo$duals           # preco sombra
res_modelo$duals.from      # preco sombra min
res_modelo$duals.to        # preco sombra max