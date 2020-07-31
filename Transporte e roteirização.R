#-----------------------------------------------------------------------------
# MOdelagem do problema
#----------------------------------------------------------------------------

# Variaveis = rota percorrida

# Funcao Objetivo
# MinD = 4Xab + 2Xac + 1Xbc + 8Xbd + 5Xbe + 6Xcd + 7Xce + 1Xdf + 2Xed + 4Xef

# Restricoes
# Xab + Xac                                                 = 1 (trajeto)
# Xab       - Xbc - Xbd - Xbe                               = 0 (trajeto)
#       Xac + Xbc             - Xcd - Xce                   = 0 (trajeto)
#                   Xbd       + Xcd       - Xdf + Xed       = 0 (trajeto)
#                         Xbe       + Xce       - Xed - Xef = 0 (trajeto)
#                                           Xdf       + Xef = 1 (trajeto)

#-----------------------------------------------------------------------------
# Implementando o problema
#-----------------------------------------------------------------------------

library(lpSolve)

funcao_objetivo = c(4, 2, 1, 8, 5, 6, 7, 1, 2, 4)

restricoes = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                      1, 0,-1,-1,-1, 0, 0, 0, 0, 0,
                      0, 1, 1, 0, 0,-1,-1, 0, 0, 0,
                      0, 0, 0, 1, 0, 1, 0,-1, 1, 0,
                      0, 0, 0, 0, 1, 0, 1, 0,-1,-1,
                      0, 0, 0, 0, 0, 0, 0, 1, 0, 1),ncol = 10, byrow = T)

restricoes_dir = c("=",
                   "=",
                   "=",
                   "=",
                   "=",
                   "=")

restricoes_rhs = c(1, 0, 0, 0, 0, 1)

res_modelo = lp("min",               # minimizar a distancia 
                funcao_objetivo,     # funcao objetivo
                restricoes,          # restricoes
                restricoes_dir,      # sinais das restricoes
                restricoes_rhs,      # valor das restricoes
                all.int = T,         # valores inteiros
                compute.sens = T)    # computa sensibilidade

#------------------------------------------------------------------------------
# Resultados
#------------------------------------------------------------------------------

res_modelo                    # km percorridos
res_modelo$solution           # rota percorrida
res_modelo$sens.coef.from     # valores min dos parametros
res_modelo$sens.coef.to       # valores max dos parametros
res_modelo$duals              # preco sombra das restricoes
res_modelo$duals.from         # preco sombra min das restricoes
res_modelo$duals.to           # preco sombra max das restricoes