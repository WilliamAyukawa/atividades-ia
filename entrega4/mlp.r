# função de net
f <- function(net) {
  return (1/(1 + exp(-net)))
}

# derivada de net
df_dnet <- function (f_net) {
  return (f_net * (1 - f_net))
}

# arquitetura
# camada de entrada, camada escondida, camada de saída
# nro de entradas, nro de neuronios da camada escondida, nro de neuronios das saídas
mlp.architecture <- function(input.length=2, hidden.length=2, output.length=1, activation.function=f, d_activation.function=df_dnet){
  #setup das variáveis
  model = list()
  model$input.length = input.length
  model$hidden.length = hidden.length
  model$output.length = output.length
  
  
  #representação dos pesos e thetas da camada escondida
  model$hidden = matrix(runif(min=-0.5, max=0.5, hidden.length*(input.length+1)), #geração de valores aleatórios do theta
                        nrow = hidden.length,ncol=input.length+1) #nro de neuronios da camada escondida
  
  #representação dos pesos e thetas da camada de saída
  model$output = matrix(runif(min = -0.5, max = 0.5,
                              output.length*(hidden.length+1)), #geração de valores aleatórios do theta
                                nrow=output.length, ncol=hidden.length+1) #nro de neuronios da camada de saída
  #treinamento dos pesos
  model$f = activation.function
  #derivada
  model$df_dnet = d_activation.function
  
  return (model)
}

#fase de utilização do algoritmo
# entrada: modelo e um padrão de entrada (exemplo: XOR)
mlp.forward <- function(model, Xp) {
  #camada escondida
  net_h_p = model$hidden %*% c(Xp, 1)
  f_net_h_p = model$f(net_h_p)
  
  #camada output
  net_o_p = model$output %*% c(as.numeric(f_net_h_p), 1)
  f_net_o_p = model$f(net_o_p)
  
  #resultados
  ret = list()
  ret$net_h_p = net_h_p
  ret$net_o_p = net_o_p
  ret$f_net_h_p = f_net_h_p
  ret$f_net_o_p = f_net_o_p
  
  return (ret)
}

#fase de treinamento
mlp.backpropagation <- function(model, dataset, eta=0.1,
                                threshold=1e-3){
  squaredError = 2 * threshold
  counter = 0
  
  while (squaredError > threshold) {
    squaredError= 0
    
    for (p in 1:nrow(dataset)) {
      Xp = as.numeric(dataset[p, 1:model$input.length])
      Yp = as.numeric(dataset[p,
                              (model$input.length+1):(ncol(dataset))])
    
      
      results = mlp.forward(model, Xp)
      Op = results$f_net_o_p
      
      #calculo do erro
      #diferença entre o esperado e o obtido
      error = Yp - Op
      
      squaredError = squaredError + sum(error^2)
      
      #treinamento do output
      delta_o_p = error * model$df_dnet(results$f_net_o_p)
      
      #treinamento do hidden
      w_o_kj = model$output[,1:model$hidden.length]
      delta_h_p = as.numeric(model$df_dnet(results$f_net_h_p))*(as.numeric(delta_o_p) %*% w_o_kj)
      
      #treinamento
      model$output = model$output + eta*(delta_o_p%*%as.vector(c(results$f_net_h_p,1)))
      model$hidden = model$hidden + eta*(t(delta_h_p)%*%as.vector(c(Xp,1)))
    }
    
    squaredError = squaredError / nrow(dataset)
    
    cat(squaredError, "\n")
    
    counter = counter + 1
  }
  
  ret = list()
  ret$model = model
  ret$counter = counter
  
  return (ret)
}