#########################################
# Cross-validation ao longo dos CPs     #

# Plotando AUC vs CP por treino e teste
#Inicializa o objeto stats, que vai guardar o AUC dos modelos
stats <- data.frame(NULL)
# Loop ao longo dos valores de CP
for (cp in cptab[2:dim(cptab)[1],'CP']){
  
  # Treinar a árvore alterando o cp
  arvore <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                  data=treino,
                  method='class',
                  control = rpart.control(cp = cp, 
                                          minsplit = 1, 
                                          maxdepth = 30),
                  xval=0
  )
  
  # Avaliar a árvore na base de treino
  p_treino = predict(arvore, treino)
  c_treino = factor(ifelse(p_treino[,2]>.5, "Y", "N"))
  
  aval_treino <- data.frame(obs=treino$Survived, 
                            pred=c_treino,
                            Y = p_treino[,2],
                            N = 1-p_treino[,2]
  )
  aval_treino
  av_treino <- twoClassSummary(aval_treino, lev=levels(aval_treino$obs))
  
  # Avaliar base de teste  
  p_teste = predict(arvore, teste)
  c_teste = factor(ifelse(p_teste[,2]>.5, "Y", "N"))
  aval_teste <- data.frame(obs=teste$Survived, 
                           pred=c_teste,
                           Y = p_teste[,2],
                           N = 1-p_teste[,2]
  )
  
  av_teste <- twoClassSummary(aval_teste, lev=levels(aval_teste$obs))
  
  # Acumular as informações de cp e AUC para cada árvore  
  stat <- cbind(cp, ROC_treino=av_treino[1], ROC_teste=av_teste[1])
  stats <- rbind(stats, stat)
  
}
stats

ggplot(stats) +
  geom_point(aes(x=cp, y=ROC_treino, col='treino')) +
  geom_point(aes(x=cp, y=ROC_teste, col='teste')) +
  scale_color_viridis_d(begin=.4, end=.8) +
  theme(legend.position = "bottom") +
  ggtitle("Curva ROC - base de treino") +
  ylab("AUC") +
  labs(colour='Base') 


