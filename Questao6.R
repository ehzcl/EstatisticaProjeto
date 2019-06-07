# Ewerton Henrique Zeferino Cruz de Lima (ehzcl)
# Paulo Victor da Silva Aragao (pvsa)
# Pedro Cesar Campos Rodrigues (pccr)

planilha = read.csv("GOTFinal.csv" , stringsAsFactors = FALSE)

TEMPORADA = (planilha$Temporada)
EPISODIO = (planilha$Episodio)
NOTA = (planilha$Nota)
ELENCO = (planilha$Personagens)
AUDIENCIA = (planilha$Audiencia.Em.milhoes.)

menordesvio = function(matriz, n) { # n =  numero de temporadas
  desvios = c()
  for(i in 1:n) {
    contador = i    # ira servir para procurar as temporadas
    elementos = c() # ira armazenar os valores da audiencia
    for(j in 1:length(matriz$Temporada)) {
      if(matriz$Temporada[j] == contador) {
        elementos = c(elementos, matriz$Audiencia.Em.milhoes.[j])
      }
    }
    # calculo da media da temporada
    acumulador = 0
    for(j in 1:length(elementos)) {
      acumulador = acumulador + elementos[j]
    }
    media = acumulador/ length(elementos)
    
    # calculo do desvio padrao da temporada
    acumulador = 0
    for(j in 1:length(elementos)) {
      acumulador = acumulador + ((elementos[j] - media)^2)
    }
    desvio = acumulador/(length(elementos) - 1)
    desvio = sqrt(desvio)
    
    # armazenando o desvio padrao da temporada
    desvios = c(desvios, desvio)
  }
  
  # procurando o menor desvio padrao, optamos por nao ordenar para conseguir
  # informar a temporada com o menor desvio tambem
  menor = Inf
  temporada = 0
  for(i in 1:length(desvios)) {
    if(desvios[i] < menor) {
      menor = desvios[i]
      temporada = i
    }
  }
  saida = c(temporada, menor)
  return(saida)
}

resposta = menordesvio(planilha, 4)
print("temporada | desvio padrao")
print(resposta)