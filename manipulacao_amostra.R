# -----------------------------------------------------------------------------
# I. PREPARAÇÃO DA BASE DE DADOS:

print("Parte 0: Preparação da base de dados")

# Instalar pacote APENAS UMA VEZ e carregar pacotes
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
# II. PARTE 1 - VALIDANDO A CONSISTÊNCIA DOS DADOS:

print("Parte 1: Validando a consistência dos dados")
print("Parte 1.2:")

# Contar quantidade de IDs de beneficiários duplicados na base de dados.
sum(duplicated(dados$ID_Beneficiario))

# Criar um vetor com o nome das variáveis quantitativas e exibir frequência de todas
# Busca os valores mínimos e máximos, o desvio padrão e exibir cada variável quantitativa
cols_quantitativas <- c("Idade", "Tempo_Plano_Meses", "Qtd_Consultas_Ano", 
                        "Qtd_Internacoes_Ano", "Custo_Anual_Operadora_K", 
                        "Mensalidade_K")
summary(dados[, cols_quantitativas])
sapply(dados[, cols_quantitativas], range, na.rm = TRUE)
sapply(dados[, cols_quantitativas], sd, na.rm = TRUE)

# Verificar se os valores passam os limites estipulados no enunciado.
# Qtd de Consultas: 0 a 45
# Qtd Internações: 0 a 8
sum(dados$Qtd_Consultas_Ano < 0 | dados$Qtd_Consultas_Ano > 45, na.rm = TRUE)
sum(dados$Qtd_Internacoes_Ano < 0 | dados$Qtd_Internacoes_Ano > 8, na.rm = TRUE)

# Verificar se as variáveis quantitativas discretas possuem valores decimais, que estão incorretos
sum(dados$Idade %% 1 != 0, na.rm = TRUE)
sum(dados$Tempo_Plano_Meses %% 1 != 0, na.rm = TRUE)
sum(dados$Qtd_Consultas_Ano %% 1 != 0, na.rm = TRUE)
sum(dados$Qtd_Internacoes_Ano %% 1 != 0, na.rm = TRUE)

# Função que calcula os outliers usando as mesmas regras do summary()
outliers_summary <- function(x) {
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limite_inf <- q1 - 1.5 * iqr
  limite_sup <- q3 + 1.5 * iqr
  # Retorna os valores que ultrapassam os limites
  x[x < limite_inf | x > limite_sup]}

# Listar os valores discrepantes de todas as variáveis pela regra do summary
lapply(dados[cols_quantitativas], outliers_summary)

# Contar a quantidade exata de outliers por variável pela regra do summary
sapply(dados[cols_quantitativas], function(x) length(outliers_summary(x)))

# Criar um vetor com o nome das variáveis qualitativas e exibir os valores de cada categoria de todas
cols_qualitativas <- c("Tipo_Plano", "Regiao", "Possui_Pre_Existencia", 
                        "Canal_Aquisicao", "Nivel_Sinistralidade", 
                        "Cancelou_Plano", "Perfil_Utilizacao")
summary(dados[, cols_qualitativas])

# Calcular frequência relativa de cada categoria de cada variável qualitativa
round(prop.table(table(dados$Tipo_Plano)) * 100, 1)
round(prop.table(table(dados$Regiao)) * 100, 1)
round(prop.table(table(dados$Possui_Pre_Existencia)) * 100, 1)
round(prop.table(table(dados$Canal_Aquisicao)) * 100, 1)
round(prop.table(table(dados$Nivel_Sinistralidade)) * 100, 1)
round(prop.table(table(dados$Cancelou_Plano)) * 100, 1)
round(prop.table(table(dados$Perfil_Utilizacao)) * 100, 1)

# -----------------------------------------------------------------------------
# III. PARTE 2 - TABELA CRUZADA ENTRE TIPO DE PLANO E CANAL DE AQUISIÇÃO:

# Tabela de frequência relativa, que precisa da de frequência absoluta:
print("Parte 2: Tipos de Plano e Canal de Aquisição estão associados?")
print("Parte 2.1:")
(round(prop.table(table(dados$Tipo_Plano, dados$Canal_Aquisicao), margin = 2) * 100, 2))

# -----------------------------------------------------------------------------
# IV. PARTE 3 - USO DO PLANO POR NÍVEL DE SINISTRALIDADE:

print("Parte 3: Uso do plano por nível de sinistralidade")
print("Parte 3.1: Custo Anual e Quantidade de Consultas No Ano por Nível De Sinistralidade.")

# Summary: calcula Mínimo, 1º Quartil, Mediana, Média e 3º Quartil
# Calcular medidas numéricas e o desvio-padrão do Custo Anual:
aggregate(Custo_Anual_Operadora_K ~ Nivel_Sinistralidade, data = dados, summary)
aggregate(Custo_Anual_Operadora_K ~ Nivel_Sinistralidade, data = dados, sd)

# Gráfico Custo por Sinistralidade
png("boxplot_custo_sinistralidade.png", width = 1000, height = 750, res = 130)
boxplot(Custo_Anual_Operadora_K ~ Nivel_Sinistralidade, data = dados,
        main = "Custo Anual por Nível de Sinistralidade",
        xlab = "Nível de Sinistralidade", 
        ylab = "Custo Anual (R$ mil)",
        col = "#40cf2d")
dev.off()

# Calcular medidas numéricas e o desvio-padrão da Quantidade de Consultas:
aggregate(Qtd_Consultas_Ano ~ Nivel_Sinistralidade, data = dados, summary)
aggregate(Qtd_Consultas_Ano ~ Nivel_Sinistralidade, data = dados, sd)

# Gráfico Quantidade de Consultas por Sinistralidade
png("boxplot_consultas_sinistralidade.png", width = 1000, height = 750, res = 130)
boxplot(Qtd_Consultas_Ano ~ Nivel_Sinistralidade, data = dados,
        main = "Quantidade de Consultas por Nível de Sinistralidade",
        xlab = "Nível de Sinistralidade", 
        ylab = "Qtd. de Consultas",
        col = "#49ee9c")
dev.off()

print("Parte 3.2: Existe indício de que o padrão de uso do plano está associado ao nível de # Tabela cruzada de frequência relativa (% em cada nível de sinistralidade)")

# Criar frequência relativa da Utilização do Perfil em relação ao Nível de Sinistralidade
round(prop.table(table(dados$Perfil_Utilizacao, dados$Nivel_Sinistralidade), margin = 2) * 100, 2)

# -----------------------------------------------------------------------------
# V. PARTE 4 - HÁ INDÍCIOS DE QUE O TEMPO DE PLANO INFLUÊNCIA A MENSALIDADE?

print("Parte 4: Há indícios de que o tempo de plano influencia a mensalidade?")
print("Parte 4.1: Custo Anual e Quantidade de Internações no ano ")

# Calcular frequência relativa de Quantidade de Internações no Ano
(round(prop.table(table(dados$Qtd_Internacoes_Ano)) * 100, 1))

# Gráfico Custo Anual por Quantidade de Internações
png("boxplot_custo_internacoes.png", width = 1000, height = 750, res = 130)
boxplot(Custo_Anual_Operadora_K ~ Qtd_Internacoes_Ano, data = dados,
        main = "Custo Anual por Quantidade de Internações",
        xlab = "Qtd de Internações no Ano",
        ylab = "Custo Anual (R$ mil)",
        col = "#beb0f0")
dev.off()

# Calcula o resumo do Custo separando por quantidade de Internações (0, 1, 2...)
tapply(dados$Custo_Anual_Operadora_K, dados$Qtd_Internacoes_Ano, summary)
# Desvio-padrão do Custo Anual para cada quantidade de internações
tapply(dados$Custo_Anual_Operadora_K, dados$Qtd_Internacoes_Ano, sd)

# Calcular coeficiente de relação linear entre Custo Anual e Qtd de Internações
# Próximo de 1: Relação positiva
# Próximo de 0: Nenhuma relação aparente
# Próximo de -1: Relação inversa
cor(dados$Custo_Anual_Operadora_K, dados$Qtd_Internacoes_Ano)

print("Parte 4.2: Mensalidade por tempo de plano ")

# Gráfico Mensalidade por Tempo de Vínculo
png("tempo_mensalidade.png", width = 1000, height = 750, res = 130)
plot(dados$Tempo_Plano_Meses, dados$Mensalidade_K,
     main = "Mensalidade por Tempo de Vínculo",
     xlab = "Tempo de Vínculo (meses)",
     ylab = "Mensalidade (R$ mil)",
     col = "#5511a3",
     pch = 19)
abline(lm(Mensalidade_K ~ Tempo_Plano_Meses, data = dados), col = "red", lwd = 2)
dev.off()

# Calcular coeficiente de relação linear entre Tempo de Vínculo e Mensalidade
cor(dados$Tempo_Plano_Meses, dados$Mensalidade_K)

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
print("EQM - COM Doença Pré-Existente:")
(eqm_com_preex <- sum(residuals(mod_com_preex)^2))
print("EQM - SEM Doença Pré-Existente:")
(eqm_sem_preex <- sum(residuals(mod_sem_preex)^2))

# Gráfico Beneficiários COM Doença Pré-Existente
png("custo_com_doenca_preex.png", width = 1000, height = 750, res = 130)
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
dev.off()

# Gráfico Beneficiários SEM Doença Pré-Existente
png("custo_sem_doenca_preex.png", width = 1000, height = 750, res = 130)
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
dev.off()

