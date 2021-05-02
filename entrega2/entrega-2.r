# Importando dados csv
dataCsv = read.csv("./archive/dataset_5secondWindow%5B3%5D.csv",sep=",", dec=".", header=TRUE)

# normalizando os dados
fnorm <- function(x){
  return (format(round(((x - min(x))/(max(x)-min(x))), 4), nsmall = 4))
}

for (colName in names(dataCsv)) {
  if(class(dataCsv[,colName]) == 'integer' | class(dataCsv[,colName]) == 'numeric') {
    dataCsv[,colName] <- fnorm(dataCsv[,colName])
  }
}

#algoritmo knn com options: distância euclidiana e manhattan
knn <- function(dataset, query, k=1, typeDist){
  numCol = ncol(dataset)
  E = apply(dataset, 1, function(row){
        # √(x1-y1)²+(xn-yn)²
        # calculo distancia euclidiana
        if(typeDist == 'euclidiano'){
          sqrt(sum((query - as.numeric(row[1:(numCol-1)]))^2))
        }else{
        # (x1-y2)+...+(xn-xn)
        # calculo distancia de manhattan
          sum(abs(query - as.numeric(row[1:(numCol-1)])))
        }
      })
  #sort na lista, calculo do nro de votos das classes
  ids = sort.list(E, dec=F)[1:k]
  classes = dataset[ids, numCol]
  U = unique(classes)
  R = rep(0, length(U))
  
  for (i in 1:length(U)) {
    R[i] = sum(U[i] == classes)
  }
  
  ret = list()
  ret$U = U
  ret$R = R
  
  return (ret)
}

#dados copiados da dataset
# print("EUCLIDIANA")
# knn(dataCsv, c(55.0,11.051009365899999,5.20328476548,19.9210683779,4.74509773505,0.91981457686,0.892571376377,0.944020322503,0.0194591155439,1.45440841261,0.288313483422,3.2727975432300003,0.882147237915,1.34904080646,0.39462171816,2.63569658992,0.710472707746,3.4040956334400003,1.86219882664,4.619432445959999,1.05728428313,260.716039971,144.04642352,401.65293857300003,128.62282074,0.990901693396,0.985061298458,0.996007642629,0.00399601428257,89.71244133409998,89.71244133409998,89.71244133409998,0.00639984580339,0.75,0.75,0.75,0.0), k=7, 'euclidiano')
# print("MANHATTAN")
# knn(dataCsv, c(55.0,11.051009365899999,5.20328476548,19.9210683779,4.74509773505,0.91981457686,0.892571376377,0.944020322503,0.0194591155439,1.45440841261,0.288313483422,3.2727975432300003,0.882147237915,1.34904080646,0.39462171816,2.63569658992,0.710472707746,3.4040956334400003,1.86219882664,4.619432445959999,1.05728428313,260.716039971,144.04642352,401.65293857300003,128.62282074,0.990901693396,0.985061298458,0.996007642629,0.00399601428257,89.71244133409998,89.71244133409998,89.71244133409998,0.00639984580339,0.75,0.75,0.75,0.0), k=7, 'manhattan')

#bus x walking (k=1, k=10, k=100, k=500)
# print("EUCLIDIANA")
# knn(dataCsv,c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10), k=1000, 'euclidiano')
# print("MANHATTAN")
# knn(dataCsv,c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10), k=1000, 'manhattan')
