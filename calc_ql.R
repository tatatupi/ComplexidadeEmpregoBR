calc_ql <- function(dados,limite) {

linhas <- dados[,1] #para saber o código de cada microrregião (primeira coluna da tabela)
dados <- dados[-1] #remove a primeira coluna da tabela
row.names(dados) <- linhas #renomeia as linhas de acordo com os códigos

colunas <- colnames(dados) #guarda uma lista dos códigos dos setores

total_setores <- colSums(dados) #calcula o total de empregos por setor
total_regioes <- rowSums(dados) #calcula o total de empregos por regiões

dados$total <- total_regioes #cria uma nova coluna com o total de empregos por regiões
dados<-rbind(dados,total_setores) #cria uma nova linha com o total de empregos por setor

row.names(dados)[nrow(dados)]<-'Total' #renomeia a linha recém-criada

ql <- matrix (nrow=nrow(dados)-1,ncol=ncol(dados)-1) #inicializa a matriz QL, pra ser do tamanho da tabela de dados, sem os totais (por isso -1)


for (i in 1:nrow(ql)) {
  for (j in 1:ncol(ql)) {
    if (dados[nrow(dados),j]==0) { #caso não haja nenhum emprego em nenhuma região para um dado setor, atribui o valor 0 (para não dar divisão por 0)
      ql[i,j]<-0
    } else {
      ql[i,j]<-(dados[i,j]/dados$total[i]) / (dados[nrow(dados),j]/sum(dados$total)) #calcula o QL
      j=j+1
    }
  }
}

ql <- data.frame(ql) #transforma em data.frame
row.names(ql)<-linhas #renomeia as linhas
colnames(ql)<-colunas #renomeia as colunas 


ql<-ifelse(ql>limite,1,0) #atribui 1 ou 0, baseado no valor da célula e do limite especificado

return(ql)

}