# Data Wrangling no R

# Atividade de Análise nº 4 - Dataset Comissão de Valores Mobiliários (CVM)
# O dataset contém informações financeiras de companhias abertas brasileiras

# Vamos investigar como foi a variação nas vendas e no lucro das empresas
# No dataset, temos dois anos de informações: 2019 e 2020
# Como evoluiu a receita de vendas e o lucro líquido no 1º ano da pandemia?
# Foi diferente entre setores?

library(tidyverse)

library(readxl)

base_cvm <- read_excel("(4.2) CVM Resultado Wrangling.xlsx")

View(base_cvm)

# Vamos organizar o dataset, mantendo apenas informações pertinentes

# Quais contas estão sendo apresentadas no dataset?

options(max.print = 3000)
unique(base_cvm$`DS_CONTA`)

# Em análise detalhada, identificou-se pelo código das contas (CD_CONTA)
# A receita de vendas é 3.01 e lucro/prejuízo líquido é 3.11

base_cvm <- base_cvm %>% filter(CD_CONTA=="3.01"|CD_CONTA=="3.11")

# Temos informações para 2019 e 2020, vamos coloca-lás juntas para cada empresa
# Para melhor organização, vamos separar as contas de receitas e lucros

base_cvm_aj <- base_cvm %>% 
  group_by(CD_CONTA, CD_CVM) %>% 
  arrange(base_cvm, DT_REFER, .by_group = T) %>% 
  ungroup () %>% droplevels(.)

# A variável com informações sobre os valores (VL_CONTA) está em caracteres
# Vamos tranformá-la em numérica
# Em uma análise prévia, vimos que a variável VL_CONTA está com resíduo (zeros)
# Para obter os valores adequados, deve ser dividida por 10.000.000.000

valor_conta <- as.numeric(base_cvm_aj$VL_CONTA)

base_cvm_aj <- base_cvm_aj %>% mutate(VALORES = valor_conta / 10000000000)

# Há linhas duplicadas no dataset, vamos excluí-las para evitar duplicidade

base_cvm_aj <- distinct(base_cvm_aj)

# No "distinct" (tidyverse), como não especificamos variáveis, considerou todas
# Vamos verificar se há 4 informações de cada empresa (2 para cada conta)

contagem <- base_cvm_aj %>% count(CD_CVM, CD_CONTA)

# Há um resíduo no dataset, a empresa com CD_CVM = 26034 tem uma duplicidade
# Em análise adicional, verificou-se que é a observação com VALORES = 451228
# Vamos excluí-la do dataset

base_cvm_aj <- base_cvm_aj %>% filter(!(CD_CVM=="26034"&VALORES==451228))

# Uma nova contagem para cerificar

contagem <- base_cvm_aj %>% count(CD_CVM, CD_CONTA)

# Vamos adicionar os setores das empresas para fazer análises mais específicas

cadastrais <- read_excel("(4.3) CVM Dados Cadastrais.xlsx")

# Ambas as bases, por serem da mesma fonte, têm uma variável com comum (CD_CVM)
# Vamos utilizá-la para um merge (join)

# Como existem muitas variáveis de dados cadastrais, vamos selecionar antes

cadastrais <- cadastrais %>% select (CD_CVM, SETOR_ATIV)
base_cvm_aj <- base_cvm_aj %>% left_join(cadastrais, by = "CD_CVM")

unique(base_cvm_aj$SETOR_ATIV)

# Por fim, vamos calcular a variação percentual e gerar a variável de interesse
# Aqui utilizaremos a função lag(), que traz valores anteriores para o cálculo

base_cvm_aj <- base_cvm_aj %>% 
  group_by(CD_CVM, CD_CONTA) %>%
  mutate(VARIACAO = ((VALORES - lag(VALORES, n = 1L))/lag(VALORES, n = 1L))) %>% 
  ungroup () %>% droplevels(.)

# Vamos utilizar o summarise e verificar algumas informações preliminares

summarise(base_cvm_aj,
          observações=n(),
          média=mean(VARIACAO, na.rm = TRUE))

# O cálculo da variação gerou valores que poluíram a variável (NaN, infinitos)
# Tais elementos não permitem os cálculos no "summarise"

# Vamos limpar a variável utilizando os procedimentos, a seguir, e o "filter"

infinitos <- is.infinite(base_cvm_aj$VARIACAO)
nan <- is.nan(base_cvm_aj$VARIACAO)

base_cvm_aj <- base_cvm_aj %>% mutate(INF = infinitos, NAN = nan) %>% 
                               filter(INF == FALSE & NAN == FALSE)

# Note que sobraram os NAs, mas estes vamos ajustar diretamente no summarise
# Um novo comando de summarise:

summarise(base_cvm_aj,
          observações=n(),
          média=mean(VARIACAO, na.rm = TRUE))

# Pode-se notar que existem valores extremos influenciando as descritivas
# O valor calculado para a média é irreal

# Sem maiores análises ou fundamentação, vamos apenas excluir grandes variações
# Por exemplo, excluindo variações maiores do que 100% e menores do que -100%
# São indícios de variações significativas nos fundamentos da empresa

base_cvm_excl <- base_cvm_aj %>% filter(!(VARIACAO > 1|VARIACAO < -1))

summarise(base_cvm_excl,
          observações=n(),
          média=mean(VARIACAO, na.rm = TRUE))

# Vamos gerar algumas informações mais detalhadas

base_cvm_excl %>% group_by(CD_CONTA) %>% 
  summarise(média=mean(VARIACAO, na.rm = TRUE)) %>% 
  ungroup () %>% droplevels(.)

tabela <- base_cvm_excl %>% group_by(CD_CONTA, SETOR_ATIV) %>% 
  summarise(média=mean(VARIACAO, na.rm = TRUE)) %>% 
  ungroup () %>% droplevels(.)

print (tabela, n=200)

# Os números setoriais indicam que existem análises mais específicas a fazer
# Por exemplo, alguns setores podem ter poucas observações (média com viés)
# Mas, para fins de exemplos, podemos parar neste ponto

# Uma forma mais adequada para comparação dos valores seria em termos reais
# A inflação no ano de 2020, medida pelo IPCA, foi 4,52% a.a.
# Vamos refazer os cálculos, atualizando os valores de 2019 pela inflação

base_cvm_atual <- base_cvm_aj %>% 
  mutate(ano = substr(DT_FIM_EXERC, 1, 4),
         VALORES_ATUAL = if_else(ano == "2019",
                                 VALORES*1.0452,
                                 VALORES*1)) %>%
  group_by(CD_CVM, CD_CONTA) %>%
  mutate(VARIACAO_ATUAL = ((VALORES_ATUAL - lag(VALORES_ATUAL, n = 1L))/lag(VALORES_ATUAL, n = 1L))) %>% 
  ungroup () %>% droplevels(.) %>%
  filter((!(VARIACAO_ATUAL > 1|VARIACAO_ATUAL < -1)))

# As novas descritivas, em termos reais, são:

base_cvm_atual %>% group_by(CD_CONTA) %>% 
  summarise(média=mean(VARIACAO_ATUAL, na.rm = TRUE)) %>% 
  ungroup () %>% droplevels(.)

