######################################
# Vamos trabalhar com a base titanic #
# cuja fonte está na library(titanic)#

titanic %>% head

#################################
# Nosso objetivo:
#      Classificar passageiros sobreviventes de acordo 
#      somente com variáveis do registro deles
################################


#################################### 
# Vamos fazer uma breve descritiva #

# Vamos criar uma base temporária para manter a base original intacta
tmp <- titanic
tmp$survived <- as.integer(titanic$Survived=="Y")

##########################################
# Função para fazer a análise descritiva #
# Vamos avaliar a distribuição de sobreviventes por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

descritiva("Sex")
descritiva("Pclass")
descritiva("Embarked")
descritiva("SibSp")
descritiva("Parch")

# Vamos categorizar as variáveis contínuas para analisar
tmp$cat_age <- quantcut(tmp$Age, 20)
descritiva("cat_age")

tmp$cat_fare <- quantcut(tmp$Fare, 10)
descritiva("cat_fare")

# Listagem das variáveis com algumas características
titanic %>% str

#############################################
# Vamos construir a árvore de classificação #
arvore <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=titanic,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de sobreviver
prob = predict(arvore, titanic)

# Classificação dos sobreviventes
class = prob[,2]>.5
# Matriz de confusão
tab <- table(class, titanic$Survived)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc
