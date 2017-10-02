# Pacotes

library(gdata)
library(vars)
library(mFilter)
library(knitr)
library(readxl)
library(dygraphs)
library(plotly)
library(BETS)

## Lendo os dados que serão utilizados

dados <- read_excel("~/blogibre.xlsx", 
                    sheet = "Planilha1")
iiebr <- BETS::BETS.get("ST_100.0")
data_bloom <- na.omit(read_excel("~/blogibre.xlsx", 
                                 sheet = "Planilha2"))

#Observe que a série de investimentos é trimestral enquanto a série do 
#iiebr é mensal. Como forma de padronização, escolheu-se fazer a média
#por trimestre do iiebr a fim de se ter a série trimestral

## Transformando em série Temporal
dados_ts <- ts(dados[,-1], start=c(1996,1), freq = 4)
fbcf_pib <- dados_ts[,1:2]
colnames(fbcf_pib) <- c("PIB", "FBCF")
dados_inv <- window(dados_ts[,3:10], start = c(2002,1), end =c(2017,1), freq = 4)

iiebr_ts <- ts(iiebr[,-1], start = c(2000,1), freq = 12)
iiebrtri <- aggregate(iiebr_ts, nfrequency=4, mean)
iiebrtri <- window(iiebrtri, start=c(2002,1), freq=4)

data_bloom <- ts(data_bloom[,-1], start = c(2003,1), freq = 12)
data_bloom <- aggregate(data_bloom, nfrequency=4, mean)
data_bloom <- window(data_bloom, start = c(2003,1), end=c(2017,1), freq = 4)

data3 <- window(dados_ts, start = c(2003,1), end=c(2017,1), freq = 4)
iiebr_qoq <- data3[,12]
iiebr_110 <- data3[,13]
seriefinal<- data3[,10]

## Parte 1: FBCF

## Ajuste Sazonal

# FBCF
ajuste_fbcf <- seas(x = fbcf_pib[,2])
qs(ajuste_fbcf)
summary(ajuste_fbcf)
plot(ajuste_fbcf)
dess_fbcf <- series(ajuste_fbcf,"s11") # série dessazonalizada

# FBCF YoY

fbcf_yoy <- window(dados_ts[,11]*100, start=c(2000,1), freq = 4)

#Observe que pode ser feito o ajuste das duas ao mesmo tempo. 
#Fez-se separado unicamente com o intuito de facilitar a visualização

# Análises

base_fbcf <- 70524.84     #Data: 2014.1 como base

fbcf_fim <- (dess_fbcf/base_fbcf)*100 #tranformando em numero índice

## Gráfico

# FBCF

datas <- dados[,1]
data_fbcf <- data.frame(datas,as.numeric(fbcf_fim))

base2017 <- list(
  x = data_fbcf[86,1],
  y = data_fbcf[86,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('70.52'),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 1,
  ax = -20,
  ay = 20)

base2014 <- list(
  x = data_fbcf[73,1],
  y = data_fbcf[73,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('100'),
  xref = "x",
  yref = "y",
  showarrow =TRUE
)


plot_ly(data_fbcf, x = ~data_fbcf[,1],y = ~data_fbcf[,2], type = 'scatter', mode = 'lines', name= "FBCF") %>%
  add_trace(y = ~70, line = list(color = 'rgba(67,67,67,1)', width = 2,  dash = 'dash', name= "ffff"))  %>%
  layout(title = "FBCF (2014.I = 100, com aj. sazonal)",
         xaxis = list(title = "Ano"),
         yaxis = list (title = " "),
         annotations = base2017) %>%
  layout(annotations = base2014) %>%
  layout(showlegend = FALSE)

## Parte 2: INV e IIEBR

## Ajuste Sazonal

# Série em função do PIB
ajuste_inv_pib <- seas(x = dados_inv[,7]*100)
qs(ajuste_inv_pib)
summary(ajuste_inv_pib)
plot(ajuste_inv_pib)
dess_inv_pib <- series(ajuste_inv_pib,"s11") # série dessazonalizada

## Criando índice
base_inv_pib <- 16.04005 #Data: 2014.1 como base
inv_fim_pib <- (dess_inv_pib/base_inv_pib)*100 

## Criando um data.frame com esses dados
datas_inv <- seq(as.Date('01/03/2002', "%d/%m/%Y"), as.Date('01/03/2017', "%d/%m/%Y"),by = "quarter")
data_inv_pib<- data.frame(datas_inv,as.numeric(inv_fim_pib))
data_inv_pib2<- data.frame(datas_inv,as.numeric(dess_inv_pib))

# Série Real
ajuste_inv <- seas(x = dados_inv[,8])
qs(ajuste_inv)
summary(ajuste_inv)
plot(ajuste_inv)
dess_inv <- series(ajuste_inv,"s11") # série dessazonalizada

## Criando índice
base_inv <- 93667.60 #Data: 2014.1 como base
inv_fim <- (dess_inv/base_inv)*100 

## Criando um data.frame com esses dados
datas_inv <- seq(as.Date('01/03/2002', "%d/%m/%Y"), as.Date('01/03/2017', "%d/%m/%Y"),by = "quarter")
data_inv<- data.frame(datas_inv,as.numeric(inv_fim))

## Gráficos

#Série em função do PIB

plot_ly(data_inv_pib2, x = ~data_inv_pib2[,1],y = ~data_inv_pib2[,2], type = 'scatter', mode = 'lines', name= "INV") %>%
  layout(title = "Investimentos Privados (2014.I = 100, com aj. sazonal)",
         xaxis = list(title = "Ano"),
         yaxis = list(title = " "))


base2017_inv <- list(
  x = data_inv_pib[61,1],
  y = data_inv_pib[61,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('83.44'),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 1,
  ax = -30,
  ay = 20)

base2014_inv <- list(
  x = data_inv_pib[49,1],
  y = data_inv_pib[49,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('100'),
  xref = "x",
  yref = "y",
  showarrow =TRUE
)

plot_ly(data_inv_pib, x = ~data_inv_pib[,1],y = ~data_inv_pib[,2], type = 'scatter', mode = 'lines', name= "INV") %>%
  add_trace(y = ~83, line = list(color = 'rgba(67,67,67,1)', width = 2,  dash = 'dash', name= "ffff"))  %>%
  layout(title = "Investimentos Privados (2014.I = 100, com aj. sazonal)",
         xaxis = list(title = "Ano"),
         yaxis = list (title = " ", range =c(40,120)),
         annotations = base2017_inv) %>%
  layout(annotations = base2014_inv) %>%
  layout(showlegend = FALSE)


#Série Real
base2017_inv <- list(
  x = data_inv[61,1],
  y = data_inv[61,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('78.89'),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 1,
  ax = -30,
  ay = 20)

base2014_inv <- list(
  x = data_inv[49,1],
  y = data_inv[49,2],
  xanchor = 'right',
  yanchor = 'middle',
  text = ~paste('100'),
  xref = "x",
  yref = "y",
  showarrow =TRUE
)

plot_ly(data_inv, x = ~data_inv[,1],y = ~data_inv[,2], type = 'scatter', mode = 'lines', name= "INV") %>%
  add_trace(y = ~80, line = list(color = 'rgba(67,67,67,1)', width = 2,  dash = 'dash', name= "ffff"))  %>%
  layout(title = "Investimentos Privados (2014.I = 100, com aj. sazonal)",
         xaxis = list(title = "Ano"),
         yaxis = list (title = " ", range =c(40,120)),
         annotations = base2017_inv) %>%
  layout(annotations = base2014_inv) %>%
  layout(showlegend = FALSE)


# Criando uma base do iiebr + serie de investimentos privados
data_inv_iiebr<- data.frame(datas_inv,as.numeric(iiebrtri[-c(62,63)]),dess_inv_pib)


## Gráficos

# IIEBR
plot_ly(iiebr, x = ~iiebr[,1]) %>%
  add_trace(y = ~iiebr[,2], name = 'IIEBR',type="scatter",mode = 'lines') %>%
  layout(
    title = "IIEBR ",
    xaxis = list(title = "", showgrid = TRUE),
    yaxis = list(title = "", showgrid = TRUE)
  )

## IIEBR e Série de Investimentos

ay <- list(
  overlaying = "y",
  side = "right",
  autorange = "reversed",
  showgrid = FALSE
)

plot_ly(data_inv_iiebr, x = ~data_inv_iiebr[,1]) %>%
  add_trace(y = ~data_inv_iiebr[,3], type="scatter", mode="lines", name = "INV") %>%
  add_trace(y = ~data_inv_iiebr[,2], type="scatter", mode="lines", name = "IIEBR", yaxis = "y2") %>%
  layout(
    title = "", yaxis2 = ay,
    xaxis = list(title="Data"),
    yaxis = list(title = " "),
    shapes = list(
      list(type = "rect",
           fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
           x0 = "2002-06-01", x1 = "2003-12-01", xref = "x",
           y0 = 12, y1 = 17, yref = "y"),
      
      list(type = "rect",
           fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
           x0 = "2008-06-01", x1 = "2010-03-01", xref = "x",
           y0 = 12, y1 = 17, yref = "y")
      ,
      
      list(type = "rect",
           fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
           x0 = "2011-06-01", x1 = "2012-03-01", xref = "x",
           y0 = 12, y1 = 17, yref = "y")
      ,
      
      list(type = "rect",
           fillcolor = "gray", line = list(color = "gray"), opacity = 0.2,
           x0 = "2015-06-01", x1 = "2017-03-01", xref = "x",
           y0 = 12, y1 = 17, yref = "y")
    )
  )

# Modelo VAR  (Metodologia Bloom 2009)

#Selecionando as variáveis

ord <- c( "SELIC_R","CUT", "IBC", "NUCI")
data_var <- na.omit(cbind(iiebr_110,data_bloom[,ord], seriefinal))
colnames(data_var) <-  c("iiebr", "SELIC_R","CUT", "IBC", "NUCI","Inv")

var_c <- data_var

#selic_r
data_var[,"SELIC_R"] <- log(data_var[,"SELIC_R"])
var_aux <- hpfilter(data_var[,"SELIC_R"])
var_c[,"SELIC_R"] <- var_aux$cycle

#cut
data_var[,"CUT"] <- log(data_var[,"CUT"])
var_aux <- hpfilter(data_var[,"CUT"])
var_c[,"CUT"] <- var_aux$cycle

#IBC
data_var[,"IBC"] <- log(data_var[,"IBC"])
var_aux <- hpfilter(data_var[,"IBC"])
var_c[,"IBC"] <- var_aux$cycle

#nuci
data_var[,"NUCI"] <- log(data_var[,"NUCI"])
var_aux <- hpfilter(data_var[,"NUCI"])
var_c[,"NUCI"] <- var_aux$cycle


#INV
ajuste_inv <- seas(x = data_var[,"Inv"])
qs(ajuste_inv)
summary(ajuste_inv)
plot(ajuste_inv)
dess_inv <- series(ajuste_inv,"s11") # série dessazonalizada

var_aux <- hpfilter(log(dess_inv))
var_c[,"Inv"] <- var_aux$cycle

plot(ajuste_inv)
plot(var_c[,"Inv"])

# Exógenas
data_bloom[,"COMD"] <- log(data_bloom[,"COMD"])

#Rodando o Modelo 
VARselect(var_c, lag.max = 3, type = "const", exogen = data_bloom[,"COMD"])

(model1 <- VAR(var_c, p =1, type = "const", exogen = data_bloom[,"COMD"]))


# resposta do choque 

plot(irf(model1,impulse = "iiebr", response = c("Inv"),
         n.ahead = 20, ci =0.95, runs= 100))

