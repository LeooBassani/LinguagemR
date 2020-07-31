#-----------------------------------------------------------------------------
# Problema a ser resolvido
#----------------------------------------------------------------------------

# Uma refinaria de petroleo produz gasolina para aviacao, gasolina aditivada e
# gasolina comum. A refinaria utiliza 3 ingredientes para tanto: Gasolina Pura,
# Aditivo e Octana. A tabela a seguir apresenta a composicao de cada uma das
# gasolinas considerando cada ingrediente, bem como os estoques disponiveis
# de cada um dos ingredientes (em milhoes de litros) disponiveis para a
# producao no inicio da proxima semana.

# Ingrediente |  Gas. Aviação |Gas. Aditivada |  Gas. Comum |Estoque
# Gas. Pura   |      20%      |     50%       |     75%     |  10
# Aditivo     |      30%      |     30%       |     5%      |  3
# Octana      |      50%      |     20%       |     20%     |  7

# Deseja-se produzir no minimo 8 milhoes de litro de gasolina comum. Sabese que os 
# lucros dos produtos, na ordem definida acima são de 0,03, 0,05 e 0,02 reais por 
# litro, respectivamente. Deseja-se o esquema de produção que maximizará o lucro 
# da empresa.


#-----------------------------------------------------------------------------
# Modelando o problema
#----------------------------------------------------------------------------

# Variaveis = qtd. de litros

# Funcao objetivo
# MaxL = 0.03Xa + 0.05Xd + 0.02Xc (mi/R$)

# Restricoes
#                   1.00Xc >= 8   (mi/L)
# 0.20Xa + 0.50Xd + 0.75Xc <= 10  (mi/L)
# 0.30Xa + 0.30Xd + 0.20Xc <= 3   (mi/L)
# 0.50Xa + 0.20Xd + 0.20Xc <= 7   (mi/L)

#------------------------------------------------------------------------------
# Implementando o problema
#------------------------------------------------------------------------------

# install.packages("lpSolve")
library(lpSolve)

# Criando o modelo
funcao_objetivo = c(0.03, 0.05, 0.02)

restricoes = matrix(c(0.00, 0.00, 1.00,
                      0.20, 0.50, 0.75,
                      0.30, 0.30, 0.05,
                      0.50, 0.20, 0.20), ncol = 3, byrow = T)

restricoes_dir = c(">=",
                   "<=",
                   "<=",
                   "<=")

restricoes_rhs = c(8,
                   10,
                   3,
                   7)

res_modelo = lp("max",                 # maximizar funcao objetivo
                funcao_objetivo,       # funcao objetivo
                restricoes,            # matriz de restricoes
                restricoes_dir,        # sinais relcacionados as restricoes
                restricoes_rhs,        # vetor de restricoes
                all.int = F,           # indica que todas as variaveis de decisao podem nÃ£o ser inteiras
                compute.sens = T)      # computa a sensibilidade

#-----------------------------------------------------------------------
# Resultados
#-----------------------------------------------------------------------
res_modelo                            # lucro [miR$]
res_modelo$solution                   # qtd milhoes litros produzidos
res_modelo$sens.coef.from             # valores min dos parametros
res_modelo$sens.coef.to               # valores max dos parametros
res_modelo$duals                      # preco sombra das restricoes
res_modelo$duals.from                 # preco sombra min das restricoes
res_modelo$duals.to                   # preco sombra max das restricoes 