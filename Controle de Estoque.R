# Programa controle de estoques

# Funcao para adicionar produto e itens no estoque
adicao = function(){
  
  # Decis�o adicionar produto novo ou item
  cat("Você está na aba de adicionar produtos do estoque\n")
  cat("Caso queira adicionar um produto existente, pressione 1\n")
  cat("Caso queira adicionar um novo produto, pressione 2\n")
  cat("Para voltar, pressione 3\n")
  
  decisao = as.integer(readline(prompt = "Informe decisao: "))
  
  # Adicionar item
  if (decisao == 1){
    produto = readline(prompt = "Informe qual produto você quer adicionar: ")
    quantidade = as.integer(readline(prompt = "Informe a quantidade que você quer adicionar: "))
    
    for (n in 1:nrow(dados)){
      if (produto == dados[n,2]){
        dados[n,3] <<- dados[n,3] + quantidade
        cat("Produto adicionado!\n")
        Sys.sleep(2)
        
        cat("----------------------------------------------\n")
        cat("Caso queira adicionar mais um produto, pressione 1\n")
        cat("Caso queira voltar ao menu principal, pressione 2\n")
        decisao = as.integer(readline(prompt = "Informe a decisao: "))
        
        while (decisao != 1 & decisao != 2){
          cat("Opção inválida, tente novamente\n")
          Sys.sleep(2)
          cat("-------------------------------------------------------")
          cat("Caso queira adicionar mais um produto, pressione 1\n")
          cat("Caso queira voltar ao menu principal, pressione 2\n")
          decisao = as.integer(readline(prompt = "Informe a decisao: "))
        }
        if (decisao == 1){
          cat("Voltando para pagina de adicionar produtos!\n")
          cat("----------------------------------------------------------------\n")
          Sys.sleep(2)
          adicao()
        }else{
          cat("Voltando para o menu principal!\n")
          cat("----------------------------------------------------------------\n")
          Sys.sleep(2)
          menu()
        }
        
      }else if(n == nrow(dados)){
        cat("Coloque um produto válido!\n")
        cat("----------------------------------------------\n")
        Sys.sleep(2)
        adicao()
      }
    }
  # Adicionar produto novo
  }else if (decisao == 2){
    produto = readline(prompt = "Informe o produto que você quer adicionar: ")
    estoque = as.integer(readline(prompt = "Informe a quantidade do estoque atual para esse produto: "))
    estoque_min = as.integer(readline(prompt = "Informe o estoque minimo para esse produto: "))
    
    tamanho = nrow(dados)
    
    new_role = data.frame(tamanho + 1, produto, estoque, estoque_min)
    names(new_role)=c("X","Produtos","Estoque", "Estoque.min")
    dados <<- rbind(dados,new_role)
    
    cat("----------------------------------------------\n")
    cat("Caso queira adicionar mais um produto, pressione 1\n")
    cat("Caso queira voltar ao menu principal, pressione 2\n")
    decisao = as.integer(readline(prompt = "Informe a decisao: "))
    
    while (decisao != 1 & decisao != 2){
      cat("Opção inválida, tente novamente\n")
      Sys.sleep(2)
      cat("-------------------------------------------------------n")
      cat("Caso queira adicionar mais um produto, pressione 1\n")
      cat("Caso queira voltar ao menu principal, pressione 2\n")
      decisao = as.integer(readline(prompt = "Informe a decisao: "))
    }
    if (decisao == 1){
      cat("Voltando para pagina de adicionar produtos!\n")
      cat("----------------------------------------------------------------\n")
      Sys.sleep(2)
      adicao()
    }else{
      cat("Voltando para o menu principal!\n")
      cat("----------------------------------------------------------------\n")
      Sys.sleep(2)
      menu()
    }
    
  }else if ( decisao == 3){
    cat("Você está voltando para aba anterior.")
    Sys.sleep(2)
    menu()
  }else{
    cat("Informe um opção válida\n")
    Sys.sleep(2)
    adicao()
  }
}

# Funcao para remover itens no estoque ou excluir produto
remocao = function(){
  
  cat("Você está na aba de remover produtos do estoque\n")
  cat("Caso queira permanecer, pressione 1\n")
  cat("Caso queira sair, pressione 2")
  
  decisao = as.integer(readline(prompt = "Informe decisao: "))
  
  if (decisao == 1){
    
    # Decisao remover itens ou remover produto
    cat("Você quer remover uma certa quantidade de um produto, pressione 1\n")
    cat("Você quer remover um produto por completo da lista, pressione 2\n")
    decisao = as.integer(readline(prompt = "Informe decisao: "))
    
    if (decisao == 1){
      
      # Remover item
      produto = readline(prompt = "Informe qual produto você quer retirar: ")
      quantidade = as.integer(readline(prompt = "Informe a quantidade que você quer retirar: "))
      
      for (n in 1:nrow(dados)){
        if (produto == dados[n, 2]){
          
          if (quantidade > dados[n,3]){
            faltante = quantidade - dados[n,3]
            dados[n,3] <<- 0
            
            cat("Quantidade solicita maior que quantidade do produto no estoque\n")
            cat(paste("O seu estoque está zerado, faltou ", faltante, "para completar sua solicitação\n"))
            
            Sys.sleep(2)
            menu()
            
          }else{
            dados[n,3] <<- dados[n,3] - quantidade
            print(paste("Retirado quantidade solicitada, o produto", produto, "está com ", dados[n,3], "em estoque."))
            
            Sys.sleep(2)
            menu()
          }
        }
        if(n == nrow(dados)){
          cat("Coloque um produto válido!\n")
          Sys.sleep(2)
          remocao()
        }
      }
    # Remover produto
    }else if( decisao == 2){
      
      produto = readline(prompt = "Informe qual produto você quer retirar: ")
      
      for (n in 1:nrow(dados)){
        if (produto == dados[n,2]){
          dados <<- dados[-n, ]
          cat(paste("O produto", produto, "foi removido da base de dados"))
          Sys.sleep(3)
          menu()
        }
      }
      if(n == nrow(dados)){
        cat("Coloque um produto válido!\n")
        Sys.sleep(2)
        remocao()
      }
    }
  }else if ( decisao == 2){
    cat("Você está voltando para aba anterior.")
    Sys.sleep(2)
    menu()
  }else{
    cat("Pressione um valor valido.")
    Sys.sleep(2)
    reducao()
  }
}

# Funcao atualizar planilha estoque
atualizado = function(){
  write.csv2(dados,"estoque.csv")
  cat("Atualizado quantidade de produtos no estoque.\n")
  Sys.sleep(2)
  
  cat("----------------------------------------------------------------------------------------\n")
  cat("Oque você deseja fazer\n")
  cat("1 - Continuar no controle de estoque.\n")
  cat("2 - Sair do programa.")
  
  opcao = as.integer(readline(prompt = "Informe opcao desejada: "))
  
  if (opcao == 1){
    menu()
  }else if(opcao == 2){
    cat("Volte sempre!")
    Sys.sleep(2)
    quit()
  }else{
    cat("Informe uma opcao valida.")
    Sys.sleep()
    atualizado()
  }
}

# Funcao exibir lista estoque
lista = function(){
  print(dados)
  Sys.sleep(2)
  cat("Deseja voltar ao menu?\n")
  cat("1 - Voltar ao menu.")
eleito  = as.integer(readline(prompt = "Informe opcao desejada: "))

  if(eleito == 1){
    menu()
  }else{
    cat("Informe uma opcao valida.")
    Sys.sleep(1)
    lista()
  }
  
  
}

# Funcao exibir produtos com estoque abaixo do estoque minimo
relatorio = function(){
  print(dados[, c(2,3,4)])
  Sys.sleep(2)
  for (n in 1:nrow(dados)){
    if (dados[n,4] > dados[n,3]){
    cat(paste("O produto", dados[n,2] , "esta com o estoque abaixo do minimo\n"))
    }else{
      cat((paste("O produto", dados[n,2] , "não esta com o estoque abaixo do minimo\n")))
      Sys.sleep(2)
      
    }
    }
  
      
  
  
  cat("Deseja voltar ao menu?\n")
  cat("1 - Voltar ao menu.")
  eleito  = as.integer(readline(prompt = "Informe opcao desejada: "))
  
  if(eleito == 1){
    menu()
  }
}

# Menu principal
menu = function(){
  cat("----------------------------------------------------------------------------------------\n")
  cat("Seja bem vindo ao controle de estoque!\n")
  cat("----------------------------------------------------------------------------------------\n")
  cat("1 - Permitir a adicao de novos produtos ao estoque\n")
  cat("2 - Permitir a remocao de produtos do estoque\n")
  cat("3 - Permitir a atualizacao da quantidade de produtos no estoque\n")
  cat("4 - Exibir a lista completa de produtos em estoque e suas quantidades\n")
  cat("5 - Exibir um relatorio que apresente os produtos que estao com estoque abaixo do minimo\n")
  cat("6 - Sair!")
  
  # Decisao menu pricipal
  opcao = as.integer(readline(prompt = "Escreva opcao desejada: "))
  
  if(opcao == 1){
    adicao()
  } else if(opcao == 2){
    remocao()
  } else if(opcao == 3){
    atualizado()
  } else if(opcao == 4){
    lista()
  } else if(opcao == 5){
    relatorio()
  } else if(opcao == 6){
    print("Volte sempre!")
    Sys.sleep(2)
    quit()
  } else{
    print("Opcao invalida!")
    menu()
  }
} 

# Tela inicial programa
main = function(){
  cat("----------------------------------------------------------------------------------------\n")
  cat("Seja bem vindo ao programa!\n")
  cat("----------------------------------------------------------------------------------------\n")
  cat("1 - Entrar no programa\n")
  cat("2 - Sair do programa")
  
  escolha = as.integer(readline(prompt = "Digite a escolha desejada: "))
  
  if (escolha == 1){
    read.csv2("estoque.csv")
    dados <<- read.csv2("estoque.csv")
    menu()
  }else if (escolha == 2){
    cat("Volte sempre!")
    Sys.sleep(2)
    quit()
  }else{
    cat("Opcao invalida!")
    Sys.sleep(2)
    main()
  }
}

if (interactive()) {
  main()
}


