#-----------------------------------------------------------------------------
# Problema a ser resolvido
#-----------------------------------------------------------------------------

# Um determinado processo produtivo possui 4 produtos e 3 recursos criticos. 
# A tabela a seguir apresenta os tempos operacionais (min/peça) e os respectivos 
# lucros dos produtos:
  
#  Produto	|Tempos operacionais em minutos / peça|	 Lucro 
#           |   Máq. A	 |   Máq. B	 |   Máq. C	  |(R$/peça)
# Produto 1	|     10	   |     15	   |     20	    |  20,00
# Produto 2	|     12	   |     10	   |     15	    |  30,00
# Produto 3	|     20	   |     25	   |     30	    |  40,00
# Produto 4	|     25	   |     30	   |     40     |  60,00

# As maquinas A, B e C possuem disponibilidade de 2000, 1800 e 1750 minutos/mes 
# respectivamente. 

#------------------------------------------------------------------------
# Modelando o problema
#------------------------------------------------------------------------

# X1 = Produto 1
# X2 = Produto 2
# X3 = Protudo 3
# X4 = Produto 4

# Funcao objetivo
# MaxL = 20X1 + 30X2 + 40X3 + 60X4

# Restricoes
# 10x1 + 12x2 + 20x3 + 25x4 <= 2000 peca/mes
# 15x1 + 10x2 + 25x3 + 30x4 <= 1800 peca/mes
# 20x1 + 15x2 + 30x3 + 40x4 <= 1750 peca/mes

#------------------------------------------------------------------------
# Implementando o probelma 
#------------------------------------------------------------------------

# install.packages("lpSolve")
library(lpSolve)

funcao_objetivo = c(20, 30, 40, 60)

restricoes = matrix(c(10, 12, 20, 25,
                      15, 10, 25, 30,
                      20, 15, 30, 40), ncol = 4, byrow = T)

restricoes_dir = c("<=",
                   "<=",
                   "<=")

restricoes_rhs = c(2000,
                   1800,
                   1750)

res_modelo = lp ("max",              # maximizar a funcao objetivo
                 funcao_objetivo,    # funcao objetivo
                 restricoes,         # matriz de restricoes
                 restricoes_dir,     # sinais das restricoes
                 restricoes_rhs,     # vetores das restricoes
                 all.int = T,        # variaveis inteiras
                 compute.sens = T)   # computa sensibilidade

#----------------------------------------------------------------------
# Resultados
#---------------------------------------------------------------------

res_modelo                 # R$/mes
res_modelo$solution        # produto/mes
res_modelo$sens.coef.from  # valores min dos parametros
res_modelo$sens.coef.to    # valores max dos parametros
res_modelo$duals           # preco sombra
res_modelo$duals.from      # preco sombra min
res_modelo$duals.to        # preco sombra max

