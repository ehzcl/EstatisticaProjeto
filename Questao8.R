# Ewerton Henrique Zeferino Cruz de Lima (ehzcl)
# Paulo Victor da Silva Aragao (pvsa)
# Pedro Cesar Campos Rodrigues (pccr)

planilha = read.csv("GOTFinal.csv" , stringsAsFactors = FALSE)


# início da questão (8)
  # nesta questao, o problema de identificar quais personagens aparecem apenas uma
  # vez na quarta temporada foi resolvido com o uso da funcao strsplit para separacao
  # dos personagens na coluna elenco e da funcao unique para possibilitar o manuseio
  # simplificado do vetor/lista, utilizamos variaveis "booleanas" para verificar se
  # um personagem ja havia aparecido na temporada, junto com um vetor de personagens
  # que ja haviam aparecido.

apareceumaveztemp = function(matriz, n){
# funcao que retorna personagens que apareceram uma unica vez na temporada
  # matriz seria a planilha e n o numero da temporada
  
  aparecem_uma_unica_vez = c() 
  # personagens que aparecem uma unica vez na temporada
  
  personagens_que_apareceram = c()
  #personagens que ja apareceram
  
  vezes = c()
  # vezes que determinado personagem apareceu
  
  elenco = (matriz$Personagens)
  # elenco da matriz passada como parametro
  
  temporada = (matriz$Temporada)
  # temporadas da matriz passada como parametro
  
  for(y in 1:length(temporada)) {
    if(temporada[y] == n) {
    # procura-se a temporada desejada
      
      elenco_episodio = strsplit(elenco[y], ",")[[1]]
      # separamos a string do elenco onde cada ator estava dividido por ',' e separamos-os em uma lista
      
      unique(elenco_episodio)
      # usamos unique para podermos usar o indice para identificar cada ator

      for(z in 1:length(elenco_episodio)) {
      # procura-se os atores que participaram do episodio
        
        apareceu = FALSE
        # "boolean" para verificar se o personagem ja apareceu na temporada
        
        if(length(personagens_que_apareceram) > 0){
        # verificacao se algum personagem ja foi descoberto na temporada
          
          for(zx in 1:length(personagens_que_apareceram)) {
          # procura-se o personagem nos personagens que ja apareceram nesta temporada
            
            if(elenco_episodio[z] == personagens_que_apareceram[zx]) {
            # se o personagem ja apareceu incrementa-se a quantidade de vezes
            # que ele ja apareceu e muda-se a variavel "booleana" para que nao adicione-se
            # o pesonagem novamente
              vezes[zx] = vezes[zx] + 1
              apareceu = TRUE
            }
          }
        }
        if(apareceu == FALSE) {
        # se o personagem nao apareceu ainda, adiciona-o ao vetor de 
        # personagens que apareceram na temporada com quantidade 1
          personagens_que_apareceram = c(personagens_que_apareceram, elenco_episodio[z])
          vezes = c(vezes,1)
        }
      }
    }
  }
  
  # verificacao de vezes que o personagem apareceu na temporada
  for(i in 1:length(personagens_que_apareceram)) {
    if(vezes[i] == 1) {
      # esta parte sera omitida pois verificava-se em todas as temporadas
      
      #apareceu_em_outra_temporada = FALSE
      #if(length(aparecem_uma_unica_vez) > 0) {
      #  for(j in 1:length(aparecem_uma_unica_vez)){
      #    if(personagens_que_apareceram[i] == aparecem_uma_unica_vez[j]){
      #     apareceu_em_outra_temporada = TRUE
      #    }
      #  }
      #}
      #if(apareceu_em_outra_temporada == FALSE){
        aparecem_uma_unica_vez = c(aparecem_uma_unica_vez, personagens_que_apareceram[i])
      #}
    }
  }
  
  return(aparecem_uma_unica_vez)
}

unicosnaquarta = function(matriz) {
  c = apareceumaveztemp(matriz,4)
  # nao foi passada como parametro do return para facilitar a leitura
  
  return(c)
}

print(unicosnaquarta(planilha))
# impressao do resultado da funcao