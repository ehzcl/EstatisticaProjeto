# Ewerton Henrique Zeferino Cruz de Lima (ehzcl)
# Paulo Victor da Silva Aragao (pvsa)
# Pedro Cesar Campos Rodrigues (pccr)

planilha = read.csv("GOTFinal.csv" , stringsAsFactors = FALSE)

TEMPORADA = (planilha$Temporada)
EPISODIO = (planilha$Episodio)
NOTA = (planilha$Nota)
ELENCO = (planilha$Personagens)
AUDIENCIA = (planilha$Audiencia.Em.milhoes.)

# ------------------ questao 2 ------------------ #
# Nesta questao utilizamos um laço para percorrer a coluna das notas, somando-as 
# em um acumulador para calculo da media, uso do acumulador (zerado) para calculo
# do desvio padrao

# inicio do calculo da media
nota = NOTA
ACUMULADOR = 0
for(i in 1:length(NOTA)) {
  ACUMULADOR = ACUMULADOR + NOTA[i]
  # soma das notas
}
MEDIA = ACUMULADOR/length(NOTA)
# soma das notas/ quantidade de notas

print("Media das notas")
print(MEDIA)

# inicio do calculo do desvio padrao

ACUMULADOR = 0
for(i in NOTA) {
  ACUMULADOR = ACUMULADOR + ((i - MEDIA)^2)
}
DESVIO = ACUMULADOR/(length(NOTA)-1)
DESVIO = sqrt(DESVIO)
print("Desvio padrao")
print(DESVIO)

# inicio do calculo da moda

MODA = 0
# valor que mais se repete

APARICOESMODA = 0
# quantidade de vezes que a moda apareceu

sort(nota)
# ordena o vetor de notas

for (i in 1:length(nota)) {
  APARICOES = 0
  # quantidade de vezes que um candidato a moda apareceu
  
  for(j in 1:length(nota)) {
    if(nota[i] == nota[j]) {
      APARICOES = APARICOES + 1
      # se a nota se repete incrementa-se o numero de aparicoes
    }
  }
  if(APARICOES > APARICOESMODA) {
    APARICOESMODA = APARICOES
    MODA = nota[i]
    # se o numero de aparicoes de um candidato a moda e maior que a quantidade
    # de vezes que a moda atual se repete, ele torna-se a nova moda e com isso
    # a quantidade de vezes que a moda repete-se torna-se a quantidade de vezes
    # que o candidato se repetiu.
    
  }
}
print("Moda")
print(MODA)
