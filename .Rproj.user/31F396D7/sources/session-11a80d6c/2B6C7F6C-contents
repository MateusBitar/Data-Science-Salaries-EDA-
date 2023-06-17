#instalação de bibliotecas
install.packages("dplyr")
install.packages("ggplot2")
install.packages("RColorBrewer")
library(RColorBrewer)
library(dplyr)
library(ggplot2)
# Leitura de Dados
salarios = read.csv('ds_salaries.csv')


#grafico dos nomes dos cargos
coresbasicas = c(1:8)
Frequencia_de_nomes = -sort(-table(salarios$job_title))
barplot(Frequencia_de_nomes,
        main= "Nomes dos cargos designados de Data Science",
        ylab= "Frequência", xlab="Nomes Utilizados", col = coresbasicas)


#grafico dos nomes utilizados para os cargos, filtrando para o top 6

barplot(head(Frequencia_de_nomes),
        main= "Nomes dos cargos designados de Data Science(top 6)",
        ylab= "Frequência", xlab="Nomes  Utilizados",
        names.arg = head(names(Frequencia_de_nomes)),cex.names = 0.4, cex.axis = 1, col = coresbasicas
        )
         
 
#gráfico de setor sobre em quais moedas correntes se é pago os salários

com_US = -sort(-table(salarios$salary_currency))
sem_US = -sort(-table(salarios$salary_currency))[-1]
porcentagem.US = round((com_US/sum(com_US))*100, 2)
porcentagem.semUS = round((sem_US/sum(sem_US))*100, 2)

pie(com_US,col = rainbow(length(com_US)))

legend("topright", legend = porcentagem.US, fill = rainbow(length(com_US)), cex = 0.5)

pie(sem_US, col = rainbow(length(sem_US)))

legend("topright", legend = porcentagem.semUS, fill = rainbow(length(sem_US)), cex = 0.5)


qtd.US = sum(-sort(-table(salarios$company_location))[1])
outros = sum(-sort(-table(salarios$company_location))[-1])+
  
grafico = c(qtd.US,outros)


#gráfico da relação da localidade das empresas comparando Estados Unidos com o resto

nomes = c("EUA", "Outros")
names(grafico) = nomes
barplot(grafico, main = "Localização das empresas", 
        col = c("orange", "blue"), horiz = TRUE)

legend("topright", legend = round((grafico/sum(grafico))*100, 2),
       fill = c("orange", "blue"), cex = 1)



#gráfico do tamanho das empresas

tamanho.empresas = table(salarios$company_size)
porcentagem.tamanho = round((tamanho.empresas/sum(tamanho.empresas)*100),2)

 
 pie( tamanho.empresas,main = "Tamanho das empresas",col = brewer.pal(3, "Blues"))

legend("topright", legend = paste( c("1000+ -> ", "500 -> ", "100 -> "),
                                   porcentagem.tamanho,"%"), fill = brewer.pal(3, "Blues"),cex = 0.8)


#grafico de niveis de experiencia

experiencia = c(table(salarios$experience_level)[1],
                table(salarios$experience_level)[3],
                table(salarios$experience_level)[4],
                table(salarios$experience_level)[2]
                )
barplot(experiencia,main = "nivel de experiência", col = (brewer.pal(3,"OrRd")))

legend("topright", legend = c("EN -> Inicial ", "MI -> Pleno", "SE -> Sênior", "EX -> Executivo"),
         fill = brewer.pal(3, "OrRd"),cex = 0.8)

#gráfico de trabalho remoto
table(salarios$remote_ratio)

trabalho_remoto = c(table(salarios$remote_ratio)[1], sum(table(salarios$remote_ratio)[-1]))
nomes = c("Presencial", "Remoto")
names(trabalho_remoto) = nomes
porcentagem.remoto = round((trabalho_remoto/sum(trabalho_remoto)*100),2)

pie(trabalho_remoto,main = "proporção Presencial/Remoto", col = c("tomato" , "orange"))

legend("topright", legend = paste( c("Presencial -> ", "Remoto -> "),
                                   porcentagem.remoto,"%"), fill = c("tomato" , "orange"),cex = 0.8)


#gráfico de trabalho remoto nos EUA
tabela.US = filter(salarios, company_location == "US" )
table(tabela.US$remote_ratio)

trabalho_remoto = c(table(tabela.US$remote_ratio)[1], sum(table(tabela.US$remote_ratio)[-1]))
nomes = c("Presencial", "Remoto")
names(trabalho_remoto) = nomes
porcentagem.remoto = round((trabalho_remoto/sum(trabalho_remoto)*100),2)

pie(trabalho_remoto, main = "proporção Presencial/Remoto nos EUA", col = c("tomato" , "orange"))

legend("topright", legend = paste( c("Presencial -> ", "Remoto -> "),
                                   porcentagem.remoto,"%"), fill = c("tomato" , "orange"),cex = 0.8)


