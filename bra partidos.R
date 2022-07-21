# bra
library(mirt)
library(readr)
mirtCluster()
library(psych)
library(tidyverse)

#bra 02 m

bra02m <- read_csv("bra02m.csv")
summary(bra02m)#
xbra02d <- fa(bra02m[,1:15], nfactors = 1, rotate = "varimax")#QUANTOS FACTORES?
fa.plot(xbra02d, cut = 0.9)
bra02_mod1 <- mirt(bra02m[,1:15], 1, itemtype = NULL)
summary(bra02_mod1)
factor_bra2 <- fscores(bra02_mod1)
bra02m$factor <- factor_bra2
summary(bra02m$factor)

# recods

bra02m <- bra02m %>%
  mutate(Mulher = case_when(X001 == 2 ~ 1,
                            TRUE ~ 0))
bra02m$Mulher <- as.factor(bra02m$Mulher)

bra02m <- bra02m %>%
  mutate(pt = case_when(E179WVS == 76002 ~ 1,
                            TRUE ~ 0))
summary(bra02m)#10% de escolha
library(coefplot)
library(sjPlot)

bra2model <-glm(pt ~ factor, data=bra02m,family=binomial(link=logit)) 
bra2full <- glm(pt ~ factor+Mulher+X003+X025R,data=bra02m,family=binomial(link=logit))
tab_model(bra2model, bra2full, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#peso do factor em escolher o pt (modelo full)
#(OR-1)*100
(1.93-1)*100#93% de chance de o pt ser a primeira escolha

#bra03m
bra03m <- read_csv("bra03m.csv")
summary(bra03m)#
summary(bra03m[,1:14])
xbra03d <- fa(bra03m[,1:14], nfactors = 1, rotate = "varimax")#QUANTOS FACTORES?
fa.plot(xbra03d, cut = 0.9)
bra03_mod1 <- mirt(bra03m[,1:14], 1, itemtype = NULL)
summary(bra03_mod1, suppress=0.31)
factor_bra03 <- fscores(bra03_mod1)
bra03m$factor <- factor_bra03
summary(bra03m$factor)
bra03m <- bra03m %>%
  mutate(Mulher = case_when(X001 == 2 ~ 1,
                            TRUE ~ 0))
bra03m$Mulher <- as.factor(bra03m$Mulher)
bra03m <- bra03m %>%
  mutate(pt = case_when(E179WVS == 76002 ~ 1,
                        TRUE ~ 0))
summary(bra03m)#23% de escolha

bra3model <-glm(pt ~ factor, data=bra03m,family=binomial(link=logit)) 
bra3full <- glm(pt ~ factor+Mulher+X003+X025R,data=bra03m,family=binomial(link=logit))
tab_model(bra5model, bra5full, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#peso do factor em escolher o pt (modelo full) positivo e sig

#(OR-1)*100
(1.46-1)*100#factor aumenta chance de o pt ser a primeira escolha


(0.98-1)*100#idade reduz de chance de o pt ser a primeira escolha



#bra05m
bra05m <- read_csv("bra05m.csv")
summary(bra05m)#
summary(bra05m[,1:13])
xbra05d <- fa(bra05m[,1:13], nfactors = 1, rotate = "varimax")#QUANTOS FACTORES?
fa.plot(xbra05d, cut = 0.9)
bra05_mod1 <- mirt(bra05m[,1:13], 1, itemtype = NULL)
summary(bra05_mod1, suppress=0.31)
factor_bra05 <- fscores(bra05_mod1)
bra05m$factor <- factor_bra05
summary(bra05m$factor)
bra05m <- bra05m %>%
  mutate(Mulher = case_when(X001 == 2 ~ 1,
                            TRUE ~ 0))
bra05m$Mulher <- as.factor(bra05m$Mulher)
bra05m <- bra05m %>%
  mutate(pt = case_when(E179WVS == 76002 ~ 1,
                        TRUE ~ 0))
summary(bra05m)#38% de escolha

bra5model <-glm(pt ~ factor, data=bra05m,family=binomial(link=logit)) 
bra5full <- glm(pt ~ factor+Mulher+X003+X025R,data=bra05m,family=binomial(link=logit))
tab_model(bra5model, bra5full, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#peso do factor em escolher o pt (modelo full) 
# nao teve efeito - perdeu o efeito-
# mudança de base petista? - os sentidos do Lulismo?

#(OR-1)*100
(0.99-1)*100#idade reduz de chance de o pt ser a primeira escolha
(0.68-1)*100#elevação da escolaridade reduz chande de o pt ser a primeira escolha


#bra07m
bra07m <- read_csv("br7m.csv")
summary(bra07m)#
summary(bra07m[,1:15])
xbra07d <- fa(bra07m[,1:15], nfactors = 1, rotate = "varimax")#QUANTOS FACTORES?
xbra07d$loadings
fa.plot(xbra07d, cut = 0.9)
bra07_mod1 <- mirt(bra07m[,1:15], 1, itemtype = NULL)
summary(bra07_mod1, suppress=0.31)
factor_bra07 <- fscores(bra07_mod1)
bra07m$factor <- factor_bra07
summary(bra07m$factor)

bra07m <- bra07m %>%
  mutate(Mulher = case_when(X001 == 2 ~ 1,
                            TRUE ~ 0))
bra07m$Mulher <- as.factor(bra07m$Mulher)

bra07m <- bra07m %>%
  mutate(pt = case_when(E179WVS == 76002 ~ 1,
                        TRUE ~ 0))
summary(bra07m)#21% de escolha

bra7model <-glm(pt ~ factor, data=bra07m,family=binomial(link=logit)) 
bra7full <- glm(pt ~ factor+Mulher+X003+X025R,data=bra07m,family=binomial(link=logit))
tab_model(bra7model, bra7full, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
#peso do factor em escolher o pt (modelo full) 
# nao teve

#(OR-1)*100
(0.99-1)*100#idade reduz de chance de o pt ser a primeira escolha
(0.65-1)*100#escolaridade reduz de chance de o pt ser a primeira escolha

# concl
#ser liberal nos costumes era signficativo com o voto no pt e com o tempo foi perdendo efeito

o91 <- ggplot(bra02m, aes(x=factor, y= as.factor(pt))) + geom_boxplot() + coord_flip() +
  xlab("Liberal nos costumes->>") + ylab("PT primeira escolha?") + labs(title = "Brasil 1991")

o91+ scale_fill_grey() + theme_classic()

o97<- ggplot(bra03m, aes(x=factor, y= as.factor(pt))) + geom_boxplot() + coord_flip() +
  xlab("Liberal nos costumes->>") + ylab("PT primeira escolha?") + labs(title = "Brasil 1997")

o06<- ggplot(bra05m, aes(x=factor, y= as.factor(pt))) + geom_boxplot() + coord_flip() +
  xlab("Liberal nos costumes->>") + ylab("PT primeira escolha?") + labs(title = "Brasil 2006")

o18 <-ggplot(bra07m, aes(x=factor, y= as.factor(pt))) + geom_boxplot() + coord_flip() +
  xlab("Liberal nos costumes ->>") + ylab("PT primeira escolha?") + labs(title = "Brasil 2018")
library(gridExtra)
grid.arrange(o91, o97,o06, o18, ncol = 4)



#inverter dep com indep  -modelos simples

library(marginaleffects)

bra02m$PT <- as.factor(bra02m$pt)
bra03m$PT <- as.factor(bra03m$pt)
bra05m$PT <- as.factor(bra05m$pt)
bra07m$PT <- as.factor(bra07m$pt)


mod91 <- lm(factor ~ PT, data=bra02m)
mod97 <- lm(factor ~ PT, data=bra03m)
mod06 <- lm(factor ~ PT, data=bra05m)
mod18 <- lm(factor ~ PT, data=bra07m)

bra02plotcap <- plot_cap(mod91, condition = c("PT"))
bra03plotcap <- plot_cap(mod97, condition = c("PT"))
bra05plotcap <- plot_cap(mod06, condition = c("PT"))
bra07plotcap <- plot_cap(mod18, condition = c("PT"))



bra02plotcap
bra03plotcap
bra05plotcap
bra07plotcap

grid.arrange(bra02plotcap, bra03plotcap,bra05plotcap, bra07plotcap, ncol = 4)


#

library(hrbrthemes)
library(viridis)
levels(bra03m$PT) <- c('Não','Sim')
levels(bra07m$PT) <- c('Não','Sim')
bra03m$ValoresLiberais <- bra03m$factor
g <- bra03m %>%
  ggplot( aes(x=PT, y=ValoresLiberais, fill=PT)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("1997") +
  xlab("")
bra07m$ValoresLiberais <- bra07m$factor
h <- bra07m %>%
  ggplot( aes(x=PT, y=ValoresLiberais, fill=PT)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) + 
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("2018") +
  xlab("")

g
h
grid.arrange(g,h, ncol = 2)
grid.arrange(g,h, nrow = 2)
