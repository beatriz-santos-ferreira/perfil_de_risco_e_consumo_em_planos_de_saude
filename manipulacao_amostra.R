# Instalar pacote e realizar apenas uma vez
install.packages("readxl")
library(readxl)

dados <- read_excel("saude_amostra.xlsx")

# Verificar estrutura inicial da base sobre saúde:
head(dados) # Exibe primeiras linhas
str(dados) # Exibe estrutura e tipos de variáveis
colSums(is.na(dados)) # Verificar se há valores ausentes (NA) na base

# Converter variáveis específicas para inteiro, ou seja, quantitativa discreta:
dados$Idade <- as.integer(dados$Idade)
dados$Tempo_Plano_Meses <- as.integer(dados$Tempo_Plano_Meses)
dados$Qtd_Consultas_Ano <- as.integer(dados$Qtd_Consultas_Ano)
dados$Qtd_Internacoes_Ano <- as.integer(dados$Qtd_Internacoes_Ano)

# Converter para qualitativa nominal:
dados$Tipo_Plano <- as.factor(dados$Tipo_Plano)
dados$Regiao <- as.factor(dados$Regiao)
dados$Canal_Aquisicao <- as.factor(dados$Canal_Aquisicao)
dados$Possui_Pre_Existencia <- as.factor(dados$Possui_Pre_Existencia)
dados$Cancelou_Plano <- as.factor(dados$Cancelou_Plano)


# Converter para qualitativa ordinal com especificação dos níveis em ordem
dados$Nivel_Sinistralidade <- factor(
  dados$Nivel_Sinistralidade,
  levels = c("Baixo", "Medio", "Alto"),
  ordered = TRUE
)

dados$Perfil_Utilizacao <- factor(
  dados$Perfil_Utilizacao,
  levels = c("Raramente", "Ocasional", "Frequente", "Critico"),
  ordered = TRUE
)

# Verificar estrutura inicial da base sobre saúde:
head(dados) # Exibe primeiras linhas
str(dados) # Exibe estrutura e tipos de variáveis
colSums(is.na(dados)) # Verificar se há valores ausentes (NA) na base