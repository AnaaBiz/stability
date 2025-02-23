# DATA ANALYSIS - STABILITY STUDIES

# PACOTES
library(Metrics)
library(ggplot2)
library(multcompView)
library(car)

# EXPERIMENTAL DATA

# Time (months)
meses <- c(0, 3, 6, 9, 12, 15, 18)

# Concentration
concentracao <- c(180.40, 182, 175, 161.15, 157, 158.40, 148)

# Graph
plot(meses, concentracao)

# LINEAR REGRESSION (intercept = coeficiente linear / y = coeficiente angular)
lm(meses ~ concentracao)

# GGPLOT2
ggplot(mapping = aes(meses, concentracao)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 18, 1)) +  #início do eixo, final do eixo, intervalo do eixo
  scale_y_continuous(breaks = seq(0, 200, 10)) +
  labs (x = "Months",
        y = "Concentration (mg/100g)")

# DATA TABLE
dados <- data.frame(x, y)

#Criando um vetor no formato pilha
dat <- stack(dados)

#ANOVA (APENAS UM CRITÉRIO DE CLASSIFICAÇÃO - ONE WAY)
model <- aov(concentracao ~ meses, data = dados)

model

#Resultado da ANOVA
summary(model)

#Se o valor de p (Pr(>F)) for menor que 0.05, há diferença estatística entre as concentrações.

#NORMALIDADE DOS RESÍDUOS
shapiro.test(resid(model))

#Se o p -value do Shapiro for maior que 0.05 = a distribuição dos não residuos não difere da distribuição normal.

#LINEARIDADE = grau de associação entre as variáveis testadas - coeficiente de determinação
cor(concentracao, meses)
