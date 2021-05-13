setwd("C:\\Users\\marco\\Desktop\\Estatistica\\Semestres\\Semestre 5\\Análise de Regressão Linear\\Trabalho")
require(ggplot2)
require(leaps)
require(readr)
require(MASS)
require(cowplot)
require(car)
options(scipen = 999)
auto_mpg <- read_table("auto-mpg.data", col_names = FALSE)
auto_mpg$X9 <- sub("([0-9])\\t.*", "\\1", auto_mpg$X8)
auto_mpg$X8 <- sub('"',"",sub('([0-9])\\t\\"(.*)', "\\2", auto_mpg$X8))
str(auto_mpg)
auto_mpg <- auto_mpg[auto_mpg$X4 != "?",]
auto_mpg$X4 <- as.numeric(auto_mpg$X4); auto_mpg$X9 <- as.numeric(auto_mpg$X9)
str(auto_mpg)
auto_mpg <- data.frame(auto_mpg)
names(auto_mpg) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model year", "car name", "origin")
auto_mpg$`model year` <- 83 - auto_mpg$`model year`; names(auto_mpg)[7] <- "age"
auto_mpg$Japan <- as.numeric(auto_mpg$origin == 3); auto_mpg$Europe <- as.numeric(auto_mpg$origin == 2)
auto_mpg <- auto_mpg[,-c(8,9)]
set.seed(69)
id <- sample(1:nrow(auto_mpg), size = nrow(auto_mpg)/2)
desenv <- auto_mpg[id,]
valid <- auto_mpg[-id,]
desenv$origin <- NA
for(i in 1:196) {
  if(desenv$Japan[i] == 1) {desenv$origin[i] <- "Japão"} else if (desenv$Europe[i] == 1) {desenv$origin[i] <- "Europa"} else {desenv$origin[i] <- "EUA"}
}
desenv$origin <- factor(desenv$origin)

# Descritiva Univariada
quadro <- data.frame(matrix(NA, 7, 8))
names(quadro) <- c("Variável", "Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Desvio padrão")
for(i in 1:7){
  nome <- names(desenv)[i]
  valores <- round(summary(desenv[[i]]),2)
  dp <- round(sd(desenv[[i]]),2)
  quadro[i,1] <- nome
  for(j in 2:8){
    quadro[i,j] <- c(valores,dp)[j-1]
  }
}
a <- c()
for(i in 1:7){
  a[i] <- paste(sub("\\.",",",paste(quadro[i,])), collapse = " & ")
}
paste(a, collapse = " \\ ")
names(desenv)
mpg <- ggplot(desenv, aes(x=factor(""), y=mpg)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Consumo (milhas/galão)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
cyl <- ggplot(desenv, aes(x = cylinders)) + geom_bar(fill="#003366") +
  labs(x="Nº de cilindros", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 3:8)
dis <- ggplot(desenv, aes(x=factor(""), y=displacement)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Deslocamento (pol³)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
hor <- ggplot(desenv, aes(x=factor(""), y=horsepower)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Potência (cv)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
wei <- ggplot(desenv, aes(x=factor(""), y=weight)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Peso (libras)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
acc <- ggplot(desenv, aes(x=factor(""), y=acceleration)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Aceleração (0-60 milhas)")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
age <- ggplot(desenv, aes(x=factor(""), y=age)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Idade em 1983")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
ori <- ggplot(desenv, aes(x = origin)) + geom_bar(fill="#003366", width = 0.5) +
  labs(x="Origem", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
plot_grid(mpg, dis, hor, nrow = 1)
ggsave("univariada1.png", width = 158, height = 93, units = "mm")
plot_grid(wei,acc, age, nrow = 1)
ggsave("univariada2.png", width = 158, height = 93, units = "mm")
plot_grid(cyl, ori, nrow = 1)
ggsave("univariada3.png", width = 158, height = 93, units = "mm")

# Descritiva Bivariada
cyl <- ggplot(desenv, aes(x=cylinders, y=mpg)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Nº de cilindros", y="Consumo (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
dis <- ggplot(desenv, aes(x=displacement, y=mpg)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Deslocamento (pol³)", y="Consumo (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 
hor <- ggplot(desenv, aes(x=horsepower, y=mpg)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Potência (cv)", y="Consumo (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
wei <- ggplot(desenv, aes(x=weight, y=mpg)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Peso (libras)", y="Consumo (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = seq(2000, 5000, 1500))
acc <- ggplot(desenv, aes(x=acceleration, y=mpg)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Aceleração (0-60 milhas)", y="Consumo (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 
age <- ggplot(desenv, aes(x=age, y=mpg)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Idade em 1983", y="Consumo (milhas/galão)") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 
ori <- ggplot(desenv, aes(x=origin, y=mpg)) +
  geom_boxplot(fill=c("#003366"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Origem", y="Consumo (milhas/galão)") +
  stat_summary(geom = "crossbar", width=0.5, fatten=0, color="red", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))
plot_grid(cyl, dis, hor, wei, nrow = 1)
ggsave("bivariada1.png", width = 158, height = 93, units = "mm")
plot_grid(acc, age, ori, nrow = 1)
ggsave("bivariada2.png", width = 158, height = 93, units = "mm")
r2 <- numeric(7)
for(i in 1:7){
  r2[i] <- 1-mean(tapply(desenv[[i]], desenv$origin, var))/var(desenv[[i]])
}
names(r2) <- names(desenv)[1:7]
paste(sub("\\.",",",paste(round(r2,3))), collapse = " & ")
desenv <- desenv[,-c(10)]
cor <- data.frame(cor(desenv)[1:7,1:7])
for(i in 1:7){
  print(gsub("\\.",",",paste(round(cor[i,],3), collapse = " & ")))
}
cor(desenv)
plot(desenv)

# Modelo
mod <- lm(mpg ~ ., data = desenv)
summary(mod)
# Resíduo
res <- mod$residuals
# Resíduo studentizado
res_stud <- rstudent(mod)
# Independência
plot(res_stud)
df <- data.frame(indice = 1:196, res, res_stud, fitt = mod$fitted.values)
ind <- ggplot(df, aes(x=indice, y=res_stud)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Índice", y="Resíduo Studentizado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
# Homocedasticidade e Linearidade
plot(mod$fitted.values, res_stud)
lin <- ggplot(df, aes(x=fitt, y=res_stud)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Valores Ajustados", y="Resíduo Studentizado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
plot(mod$fitted.values, abs(res_stud))
linabs <- ggplot(df, aes(x=fitt, y=abs(res_stud))) + geom_point(colour="#003366", size=1.5) +
  labs(x="Valores Ajustados", y="Resíduo Studentizado Absoluto") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
# Normalidade
boxplot(res_stud)
qqnorm(res_stud)
qqline(res_stud)

y <- quantile(res, c(0.25, 0.75))
x <- qnorm(c(0.25,0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
d <- data.frame(resids = res)
qq <- ggplot(d, aes(sample = resids)) + stat_qq(colour = "#003366", size = 1) + 
  geom_abline(slope = slope, intercept = int, size = .5, colour = "#A11D21")+
  xlab("Quantis da Normal")+ylab("Quantis dos Resíduos") + 
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
plot_grid(ind, lin, linabs, qq, nrow = 2)
ggsave("diagnostico_completo.png", width = 158, height = 93, units = "mm")
# Testes
shapiro.test(res)
modbp <- lm(((res)^2) ~ . - mpg, data = desenv)
SQReg <- sum(anova(modbp)[1:8,2])
SQRes <- anova(mod)[9,2]
testchi <- (SQReg/9)/((SQRes/length(desenv$mpg))^2)
(pvalor <- 1-pchisq(testchi, 8))

# Transformação: log(mpg)
desenv$mpgl <- log(desenv$mpg)
desenv <- desenv[,-1]
# Modelo
mod <- lm(mpgl ~ ., data = desenv)
summary(mod)
# Resíduo
res <- mod$residuals
# Resíduo studentizado
res_stud <- rstudent(mod)
# Independência
plot(res_stud)
df <- data.frame(indice = 1:196, res, res_stud, fitt = mod$fitted.values)
ind <- ggplot(df, aes(x=indice, y=res_stud)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Índice", y="Resíduo Studentizado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
# Homocedasticidade e Linearidade
plot(mod$fitted.values, res_stud)
lin <- ggplot(df, aes(x=fitt, y=res_stud)) + geom_point(colour="#003366", size=1.5) +
  labs(x="Valores Ajustados", y="Resíduo Studentizado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
plot(mod$fitted.values, abs(res_stud))
linabs <- ggplot(df, aes(x=fitt, y=abs(res_stud))) + geom_point(colour="#003366", size=1.5) +
  labs(x="Valores Ajustados", y="Resíduo Studentizado Absoluto") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
# Normalidade
boxplot(res_stud)
qqnorm(res_stud)
qqline(res_stud)

y <- quantile(res, c(0.25, 0.75))
x <- qnorm(c(0.25,0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
d <- data.frame(resids = res)
qq <- ggplot(d, aes(sample = resids)) + stat_qq(colour = "#003366", size = 1) + 
  geom_abline(slope = slope, intercept = int, size = .5, colour = "#A11D21")+
  xlab("Quantis da Normal")+ylab("Quantis dos Resíduos") + 
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
plot_grid(ind, lin, linabs, qq, nrow = 2)
ggsave("diagnostico_transformado.png", width = 158, height = 93, units = "mm")
# Testes
shapiro.test(res)
modbp <- lm(((res)^2) ~ . - mpgl, data = desenv)
SQReg <- sum(anova(modbp)[1:8,2])
SQRes <- anova(mod)[9,2]
testchi <- (SQReg/9)/((SQRes/length(desenv$mpgl))^2)
(pvalor <- 1-pchisq(testchi, 8))

# Fazer seleção de variáveis
# Medidas: R2, R2adj, BIC, Cm
sele1 <- regsubsets(mpgl ~ ., data = desenv, nbest = 10)
names(summary(sele1))
n_var_exp <- as.numeric(rownames(summary(sele1)$which))
df <- data.frame(n_var_exp, r2 = summary(sele1)$rsq, r2adj = summary(sele1)$adjr2, bic = summary(sele1)$bic, cm = summary(sele1)$cp)
plot(n_var_exp, summary(sele1)$rsq, xlab = "Nº de variáveis explicativas", ylab = "R^2") # 2, 3, 4
r2 <- ggplot(df, aes(x=n_var_exp, y=r2)) + geom_point(colour="#003366", size=1.5, alpha = 0.5) +
  labs(x="Número de variáveis explicativas", y="R²") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)
plot(n_var_exp, summary(sele1)$adjr2, xlab = "Nº de variáveis explicativas", ylab = "R^2adj") # 2, 3, 4, 5, 6
r2adj <- ggplot(df, aes(x=n_var_exp, y=r2adj)) + geom_point(colour="#003366", size=1.5, alpha = 0.5) +
  labs(x="Número de variáveis explicativas", y="R² Ajustado") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)
plot(n_var_exp, summary(sele1)$bic, xlab = "Nº de variáveis explicativas", ylab = "BIC") # 2, 3, 4
bic <- ggplot(df, aes(x=n_var_exp, y=bic)) + geom_point(colour="#003366", size=1.5, alpha = 0.5) +
  labs(x="Número de variáveis explicativas", y="BIC") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)
plot(n_var_exp, summary(sele1)$cp, xlab = "Nº de variáveis explicativas", ylab = "Cm") # 2, 3, 4
cm <- ggplot(df, aes(x=n_var_exp, y=cm)) + geom_point(colour="#003366", size=1.5, alpha = 0.5) +
  labs(x="Número de variáveis explicativas", y="C de Mallows") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=8),
        axis.text = element_text(colour = "black", size=7.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks = 1:8)
plot_grid(r2, r2adj, bic, cm, nrow = 2)
ggsave("selecao.png", width = 158, height = 93, units = "mm")
mod1 <- lm(mpgl ~ age + weight, data = desenv) # 2 variáveis
mod2 <- lm(mpgl ~ age + weight + Japan, data = desenv) # 3 variáveis
mod3 <- lm(mpgl ~ age + weight + Japan + Europe, data = desenv) # 4 variáveis
mod4 <- lm(mpgl ~ age + weight + Japan + Europe + horsepower, data = desenv) # 5 variáveis
mod5 <- lm(mpgl ~ age + weight + Japan + Europe + horsepower + displacement, data = desenv) # 6 variáveis
r <- round(cbind(summary(sele1)$which, summary(sele1)$rsq, summary(sele1)$adjr2, summary(sele1)$bic, summary(sele1)$cp),4)
r <- r[c(9,19,29,39,49,59),10:13]
colnames(r) <- names(summary(sele1))[c(2,4,5,6)]
b <- c()
for(i in 1:6){
  b[i] <- paste(i, paste(r[i,], collapse = " "))
}
gsub("\\.",",",b)
# Implementar Backward
summary(lm(mpgl ~ ., data = desenv)) # Retira-se acceleration
summary(lm(mpgl ~ . - acceleration, data = desenv)) # Retira-se cylinders
summary(lm(mpgl ~ . - acceleration - cylinders, data = desenv)) # Retira-se displacement
summary(lm(mpgl ~ . - acceleration - cylinders - displacement, data = desenv)) # Modelo final igual ao mod4
# Métodos automáticos
full.model <- lm(mpgl ~ ., data = desenv)
back.model <- stepAIC(full.model, direction = "backward", trace = F)
summary(back.model)
mod6 <- lm(mpgl ~ age + weight + Japan + Europe + horsepower + displacement + cylinders, data = desenv)
forw.model <- stepAIC(lm(mpgl ~ 1, data=desenv), direction="forward", scope=(~ cylinders + displacement + horsepower + weight +
                                                                                   acceleration + age + Japan + Europe),
                      trace=F)
summary(forw.model) # Modelo final igual ao mod4
step.model <- stepAIC(lm(mpgl ~ 1, data=desenv), direction="both", scope=list(lower=lm(mpgl ~ 1, data=desenv), upper = full.model), trace=F)
summary(step.model) # Modelo final igual ao mod4
# Definindo os modelos candidatos
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5) # Descarta-se o mod5, pois displacement não é significativo
summary(mod6) # Descarta-se o mod6, pois displacement e cylinders não são significativos

# Análise completa de diagnóstico do mod4
desenv2 <- desenv[,c(3,4,6:9)]
# Os Pressupostos estão validados?
summary(mod4)
# Resíduo
res <- mod4$residuals
# Resíduo studentizado
res_stud <- rstudent(mod4)
# Independência
plot(res_stud)
# Homocedasticidade e Linearidade
plot(mod4$fitted.values, res_stud)
plot(mod4$fitted.values, abs(res_stud))
# Normalidade
boxplot(res_stud)
qqnorm(res_stud)
qqline(res_stud)
# Testes
shapiro.test(res)
modbp <- lm(((res)^2) ~ . - mpgl, data = desenv2)
SQReg <- sum(anova(modbp)[1:5,2])
SQRes <- anova(mod)[6,2]
testchi <- (SQReg/6)/((SQRes/length(desenv2$mpgl))^2)
(pvalor <- 1-pchisq(testchi, 5))

# Medidas Influentes
medidas <- as.data.frame(influence.measures(mod4)[[1]])
plot(1:196, medidas$hat)
identify(1:196, medidas$hat) # 136
plot(1:196, abs(medidas$dffit))
identify(1:196, abs(medidas$dffit)) # 35
plot(1:196, medidas$cook.d)
identify(1:196, medidas$cook.d) # 10, 35
plot(1:196, abs(medidas$dfb.age))
identify(1:196, abs(medidas$dfb.age)) # 25, 151
plot(1:196, abs(medidas$dfb.wght))
identify(1:196, abs(medidas$dfb.wght)) # 35, 68, 136
plot(1:196, abs(medidas$dfb.Japn))
identify(1:196, abs(medidas$dfb.Japn)) # 1, 27, 142, 150, 154, 160
plot(1:196, abs(medidas$dfb.Eurp))
identify(1:196, abs(medidas$dfb.Eurp)) # 6, 56, 70, 76, 191
plot(1:196, abs(medidas$dfb.hrsp))
identify(1:196, abs(medidas$dfb.hrsp)) # 10, 35, 46, 68, 136
# 10, 35, 68, 136
summary(lm(mpgl ~ ., data = desenv2))
summary(lm(mpgl ~ ., data = desenv2[-10,]))
summary(lm(mpgl ~ ., data = desenv2[-35,])) # Horsepower
summary(lm(mpgl ~ ., data = desenv2[-68,]))
summary(lm(mpgl ~ ., data = desenv2[-136,])) # Horsepower
summary(lm(mpgl ~ ., data = desenv2[-c(10,35),]))
summary(lm(mpgl ~ ., data = desenv2[-c(10,68),]))
summary(lm(mpgl ~ ., data = desenv2[-c(10,136),]))
summary(lm(mpgl ~ ., data = desenv2[-c(35,68),]))
summary(lm(mpgl ~ ., data = desenv2[-c(35,136),])) # Horsepower
summary(lm(mpgl ~ ., data = desenv2[-c(68,136),]))
summary(lm(mpgl ~ ., data = desenv2[-c(10,35,68),]))
summary(lm(mpgl ~ ., data = desenv2[-c(10,35,136),])) # Horsepower
summary(lm(mpgl ~ ., data = desenv2[-c(10,68,136),]))
summary(lm(mpgl ~ ., data = desenv2[-c(35,68,136),])) # Horsepower
summary(lm(mpgl ~ ., data = desenv2[-c(10,35,68,136),])) # Horsepower

# Multicolinearidade
(vi<-vif(mod4))
mean(vi)

# Análise completa de diagnóstico do mod3
desenv3 <- desenv[,c(4,6:9)]
# Os Pressupostos estão validados?
summary(mod3)
# Resíduo
res <- mod3$residuals
# Resíduo studentizado
res_stud <- rstudent(mod3)
# Independência
plot(res_stud)
# Homocedasticidade e Linearidade
plot(mod3$fitted.values, res_stud)
plot(mod3$fitted.values, abs(res_stud))
# Normalidade
boxplot(res_stud)
qqnorm(res_stud)
qqline(res_stud)
# Testes
shapiro.test(res)
modbp <- lm(((res)^2) ~ . - mpgl, data = desenv3)
SQReg <- sum(anova(modbp)[1:4,2])
SQRes <- anova(mod3)[5,2]
testchi <- (SQReg/5)/((SQRes/length(desenv3$mpgl))^2)
(pvalor <- 1-pchisq(testchi, 4))

medidas <- as.data.frame(influence.measures(mod3)[[1]])
plot(1:196, medidas$hat)
identify(1:196, medidas$hat) # 126
plot(1:196, abs(medidas$dffit))
identify(1:196, abs(medidas$dffit)) # 150, 151
plot(1:196, medidas$cook.d)
identify(1:196, medidas$cook.d) # 150, 151
plot(1:196, abs(medidas$dfb.age))
identify(1:196, abs(medidas$dfb.age)) # 151
plot(1:196, abs(medidas$dfb.wght))
identify(1:196, abs(medidas$dfb.wght)) # 155, 170
plot(1:196, abs(medidas$dfb.Japn))
identify(1:196, abs(medidas$dfb.Japn)) # 1, 142, 150
plot(1:196, abs(medidas$dfb.Eurp))
identify(1:196, abs(medidas$dfb.Eurp)) # 70, 191
# 150, 151
summary(lm(mpgl ~ ., data = desenv3))
summary(lm(mpgl ~ ., data = desenv3[-150,]))
summary(lm(mpgl ~ ., data = desenv3[-151,]))
summary(lm(mpgl ~ ., data = desenv3[-c(150,151),]))

# Multicolinearidade
(vi<-vif(mod3))
mean(vi)


# Validação do modelo
# Modelo: mod4
valid$mpgl <- log(valid$mpg); valid$mpg <- NULL
mod4_valid <- lm(mpgl ~ age + weight + Japan + Europe + horsepower, data = valid) 
summary(mod4_valid) # Japão e Horsepower deixaram de ser significativos
summary(mod4)
coef(mod4_valid)
coef(mod4) # Intercepto, Age, Weight e Horsepower são próximos. Japão e Europa são distantes, mas pouco.

confint(mod4, level = .99) #Todos dentro a 99%

# Intervalo de confiança de 95%
confianca_mod4 <- predict(mod4, valid[,-9], interval = "confidence", level = .95)
confidence_rate_mod4 <- c()
for(i in seq_len(nrow(confianca_mod4))){
  confidence_rate_mod4[i] <- valid[i,"mpgl"] >= confianca_mod4[i,"lwr"] & valid[i,"mpgl"] <= confianca_mod4[i,"upr"]
}
mean(confidence_rate_mod4)

predicao_mod4 <- predict(mod4, valid[,-9], interval = "prediction", level = .95)
predict_rate_mod4 <- c()
for(i in seq_len(nrow(confianca_mod4))){
  predict_rate_mod4[i] <- valid[i,"mpgl"] >= predicao_mod4[i,"lwr"] & valid[i,"mpgl"] <= predicao_mod4[i,"upr"]
}
mean(predict_rate_mod4)

mpgl_hat <- predict(mod4, valid[,-9])
MSPR <- mean((valid[,"mpgl"] - mpgl_hat)^2)
anova(mod4)["Residuals","Mean Sq"]
MSPR # Estão próximos

# Modelo: mod3
mod3_valid <- lm(mpgl ~ age + weight + Japan + Europe, data = valid)
summary(mod3_valid) # Japão deixou de ser significativo
summary(mod3)
coef(mod3_valid)
coef(mod3) # Intercepto, Age, Weight e Europa são próximos. Japão está distante.

confint(mod3, level = .99)

# Intervalo de confiança de 95%
confianca_mod3 <- predict(mod3, valid[,-9], interval = "confidence", level = .95)
confidence_rate_mod3 <- c()
for(i in seq_len(nrow(confianca_mod3))){
  confidence_rate_mod3[i] <- valid[i,"mpgl"] >= confianca_mod3[i,"lwr"] & valid[i,"mpgl"] <= confianca_mod3[i,"upr"]
}
mean(confidence_rate_mod3)

predicao_mod3 <- predict(mod3, valid[,-9], interval = "prediction", level = .95)
predict_rate_mod3 <- c()
for(i in seq_len(nrow(confianca_mod3))){
  predict_rate_mod3[i] <- valid[i,"mpgl"] >= predicao_mod3[i,"lwr"] & valid[i,"mpgl"] <= predicao_mod3[i,"upr"]
}
mean(predict_rate_mod3)

mpgl_hat <- predict(mod3, valid[,-9])
MSPR <- mean((valid[,"mpgl"] - mpgl_hat)^2)
anova(mod3)["Residuals","Mean Sq"]
MSPR # Estão próximos