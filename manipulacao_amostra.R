# I. PREPARAÇÃO DA BASE DE DADOS:
# Instalar pacote e realizar apenas uma vez
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)

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

# -----------------------------------------------------------------------------
# II. PARTE 2.1 - TABELA CRUZADA ENTRE TIPO DE PLANO E CANAL DE AQUISIÇÃO:

# Tabela de frequência relativa, que precisa da de frequência absoluta:
print("Parte 2: Tipos de Plano e Canal de Aquisição estão associados?")
print("Parte 2.1:")
(tab_freq_relativa <- round(prop.table(table(dados$Tipo_Plano, dados$Canal_Aquisicao), margin = 2) * 100, 2))

# -----------------------------------------------------------------------------
# III. PARTE 3 - USO DO PLANO POR NÍVEL DE SINISTRALIDADE:

print("Parte 3: Uso do plano por nível de sinistralidade")
print("Parte 3.1: Custo Anual e Quantidade de Consultas No Ano por Nível De Sinistralidade.")

# Summary: calcula Mínimo, 1º Quartil, Mediana, Média e 3º Quartil
# Calcular medidas numéricas e o desvio-padrão do Custo Anual:
aggregate(Custo_Anual_Operadora_K ~ Nivel_Sinistralidade, data = dados, summary)
aggregate(Custo_Anual_Operadora_K ~ Nivel_Sinistralidade, data = dados, sd)

# Gráfico Custo por Sinistralidade
boxplot(Custo_Anual_Operadora_K ~ Nivel_Sinistralidade, data = dados,
        main = "Custo Anual por Nível de Sinistralidade",
        xlab = "Nível de Sinistralidade", 
        ylab = "Custo Anual (R$ mil)",
        col = "#40cf2d")

# Calcular medidas numéricas e o desvio-padrão da Quantidade de Consultas:
aggregate(Qtd_Consultas_Ano ~ Nivel_Sinistralidade, data = dados, summary)
aggregate(Qtd_Consultas_Ano ~ Nivel_Sinistralidade, data = dados, sd)

# Gráfico Quantidade de Consultas por Sinistralidade
boxplot(Qtd_Consultas_Ano ~ Nivel_Sinistralidade, data = dados,
        main = "Quantidade de Consultas por Nível de Sinistralidade",
        xlab = "Nível de Sinistralidade", 
        ylab = "Qtd. de Consultas",
        col = "#49ee9c")


# -----------------------------------------------------------------------------
# V. PARTE 5 - DOENÇA PRÉ-EXISTENTE E SENSIBILIDADE DO CUSTO ÀS CONSULTAS:

# Subset: Filtra a planilha de dados criando um subconjunto com base em uma regra
dados_com_preex <- subset(dados, Possui_Pre_Existencia == "Sim")
dados_sem_preex <- subset(dados, Possui_Pre_Existencia == "Nao")

# Lm: Ajusta o modelo de regressão linear
mod_com_preex <- lm(Custo_Anual_Operadora_K ~ Qtd_Consultas_Ano, data = dados_com_preex)
mod_sem_preex <- lm(Custo_Anual_Operadora_K ~ Qtd_Consultas_Ano, data = dados_sem_preex)

# Coef: Exibe os dois valores numéricos para escrever a equação da reta:
# Intercep: Coeficiente Linear
# Qtd_Consultas_Ano: Coeficiente Angular
coef(mod_com_preex)
coef(mod_sem_preex)

# Calcular o Erro Quadrático Médio (EQM):
# Desvio total entre os custos reais dos clientes e o custo previsto pela linha reta.
eqm_com_preex <- sum(residuals(mod_com_preex)^2)
eqm_sem_preex <- sum(residuals(mod_sem_preex)^2)

# Exibir os resultados formatados no console
cat("EQM - Com Doença Pré-Existente:", round(eqm_com_preex, 2), "\n")
cat("EQM - Sem Doença Pré-Existente:", round(eqm_sem_preex, 2), "\n")

# Gráfico Beneficiários COM Doença Pré-Existente
plot(dados_com_preex$Qtd_Consultas_Ano, dados_com_preex$Custo_Anual_Operadora_K,
     main = "Custo Anual x Consultas Anuais (Com Doença Pré-Existente)",
     xlab = "Quantidade de Consultas no Ano",
     ylab = "Custo Anual (R$ mil)")
abline(mod_com_preex, col = "red")
legend("bottomright", 
       legend = paste0("Custo = ", round(coef(mod_com_preex)[1], 2), " + ", round(coef(mod_com_preex)[2], 2), " * Consultas"), 
       bty = "n", 
       text.col = "#6d0606",
       cex = 1,
       inset = c(0.2, 0.01))

# Gráfico Beneficiários SEM Doença Pré-Existente
plot(dados_sem_preex$Qtd_Consultas_Ano, dados_sem_preex$Custo_Anual_Operadora_K,
     main = "Custo Anual x Consultas Anuais (Sem Doença Pré-Existente)",
     xlab = "Quantidade de Consultas no Ano",
     ylab = "Custo Anual (R$ mil)")
abline(mod_sem_preex, col = "blue")
legend("topleft", 
       legend = paste0("Custo = ", round(coef(mod_sem_preex)[1], 2), " + ", round(coef(mod_sem_preex)[2], 2), " * Consultas"), 
       bty = "n", 
       text.col = "#020270",
       cex = 1,
       inset = c(0.02, 0.01))

