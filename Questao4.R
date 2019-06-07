# Ewerton Henrique Zeferino Cruz de Lima (ehzcl)
# Paulo Victor da Silva Aragao (pvsa)
# Pedro Cesar Campos Rodrigues (pccr)

planilha = read.csv("GOTFinal.csv" , stringsAsFactors = FALSE)

TEMPORADA = (planilha$Temporada)
EPISODIO = (planilha$Episodio)
NOTA = (planilha$Nota)
ELENCO = (planilha$Personagens)
AUDIENCIA = (planilha$Audiencia.Em.milhoes.)
RESPOSTA = c()

# ------------------ questao 4 ------------------ #
# Nesta questao utilizamos um for para percorrer a coluna das notas 
# e um if para checar se a nota e maior ou igual a 9 e se for 
# adiciona-a num vetor

for(i in 1:73) {
  if(NOTA[i] >= 9) {
    RESPOSTA = c(RESPOSTA, EPISODIO[i])
  }
}

print(RESPOSTA)