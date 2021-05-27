# William Hideyuki Ayukawa
#nro USP: 10346892

# Importando dados csv
# dataCsv = read.csv("./archive/dataset_5secondWindow%5B3%5D.csv",sep=",", dec=".", header=TRUE)
dataCsv = read.table("./archive/iris.data",sep=",", dec=".", header=FALSE)

# normalizando os dados
fnorm <- function(x){
  return (format(round(((x - min(x))/(max(x)-min(x))), 4), nsmall = 4))
}
for (colName in names(dataCsv)) {
  if(class(dataCsv[,colName]) == 'integer' | class(dataCsv[,colName]) == 'numeric') {
    dataCsv[,colName] <- fnorm(dataCsv[,colName])
  }
}

require(nnet)
dataset = cbind(dataCsv[,1:4], class.ind(dataCsv[,5]))

source("C:/Users/cintia/Documents/IA/entrega4/mlp.r")



model = mlp.architecture(input.length = 4, hidden.length = 3, output.length = 3)

ids = sample(1:nrow(dataset), size = 50)
training = dataset[ids,]
test = dataset[-ids,]

trained = mlp.backpropagation(dataset=training, eta=0.1, model=model, threshold=0.03)

# mlp.forward(model = trained$model, Xp=)
R = NULL;
for (i in 1:nrow(test)) {
  R = rbind(R,t(round(mlp.forward(model = trained$model, Xp = as.numeric(test[i,1:4]))$f_net_o_p)))

}