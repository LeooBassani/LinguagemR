#------------------------------------------------------------------------------
# Problema a ser resolvido
#-----------------------------------------------------------------------------

# Ha 9 itens na lista de compras, sendo que cada um dos itens oferece um grau de 
# satisfação e tem preço conforme a tabela a seguir: 
  
#  Item	    | A	| B	| C	| D	| E	| F	| G	| H	| I
# Preço	    | 2	| 5	| 1	| 3	| 7	| 4	| 2	| 5	| 3
# Satisfacao|10 |15	| 5	| 8	|12	| 2	| 4	| 6	| 0

# Caso compre produto G, tambem deve comprar o produto D. Se comprar o produto D, 
# não deve comprar o produto I. Considere que ha apenas $ 40 para as compras. 
# Encontre a solução que maximiza a satisfação considerando que as variáveis de 
# decisão são do tipo binária. 

#--------------------------------------------------
# Modelando o problema
#-------------------------------------------------

# Funcao objetivo
# MaxS = 10Xa + 15Xb + 5Xc + 8Xd + 12Xe + 2Xf + 4Xg + 6Xh + 0Xi

# Restricoes
# 2Xa + 5Xb + 1Xc + 3Xd + 7Xe + 4Xf + 2Xg + 5Xh + 3Xi <= 40
#                 + 1Xd             + 1Xg              = 2
#                 + 1Xd                         + 1Xi  = 1

#---------------------------------------------------------
# Implementando o problema
#---------------------------------------------------------

# install.packages("lpSolve")
library(lpSolve)

funcao_objetivo = c(10, 15, 5, 8, 12, 2, 4, 6, 0)

restricoes = matrix(c(2, 5, 1, 3, 7, 4, 2, 5, 3, 
                      0, 0, 0, 1, 0, 0, 1, 0, 0,
                      0, 0, 0, 1, 0, 0, 0, 0, 1),ncol = 9, byrow = T)

restricoes_dir = c("<=",
                   "=",
                   "=")

restricoes_rhs = c(40, 2, 1)

res_modelo = lp("max",              # maximizar a funcao objetivo
                funcao_objetivo,    # funcao objetivo
                restricoes,         # restricoes
                restricoes_dir,     # sinais das restricoes
                restricoes_rhs,     # vetores das restricoes
                all.bin = T,        # variaveis inteiras   
                compute.sens = T)   # computa sensibilidade

#-------------------------------------------------------------
# Resultados
#-------------------------------------------------------------

res_modelo                 # Satisfação
res_modelo$solution        # itens selecionados
res_modelo$sens.coef.from  # valores min dos parametros
res_modelo$sens.coef.to    # valores max dos parametros
res_modelo$duals           # preco sombra
res_modelo$duals.from      # preco sombra min
res_modelo$duals.to        # preco sombra max

