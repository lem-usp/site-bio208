##############################
# Instalando pacotes necessarios #
##############################
if(!require(RCurl)){install.packages('RCurl'); library(RCurl)} # verifica se o pacote RCurl esta instalado. Se nao estiver, instala e carrega.
if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} # com acima para o pacote ggplot2.
if(!require(reshape2)){install.packages('reshape2'); library(reshape2)} # similar ao de cima, mudando os parametros
if(!require(plyr)){install.packages('plyr'); library(plyr)} # similar ao de cima, mudando os parametros
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} # similar ao de cima, mudando os parametros


################################################
# Obtencao dos dados da planilha do Diurno e Noturno  #
################################################
d_c1=getURL("https://docs.google.com/spreadsheets/d/1jZbxJzstOdgaNADw6n1ov4pKo0l0dAD1uEcefAlFplI/export?gid=0&format=csv", ssl.verifypeer=FALSE) # obtem os dados do cenario 1 do diurno e grava na variavel d_c1.
d_c2=getURL("https://docs.google.com/spreadsheets/d/1jZbxJzstOdgaNADw6n1ov4pKo0l0dAD1uEcefAlFplI/export?gid=1488503095&format=csv", ssl.verifypeer=FALSE) # similar ao de cima, mudando os parametros
d_c3=getURL("https://docs.google.com/spreadsheets/d/1jZbxJzstOdgaNADw6n1ov4pKo0l0dAD1uEcefAlFplI/export?gid=1830499974&format=csv", ssl.verifypeer=FALSE) # similar ao de cima, mudando os parametros
d_c4=getURL("https://docs.google.com/spreadsheets/d/1jZbxJzstOdgaNADw6n1ov4pKo0l0dAD1uEcefAlFplI/export?gid=1573305252&format=csv", ssl.verifypeer=FALSE) # similar ao de cima, mudando os parametros
n_c1=getURL("https://docs.google.com/spreadsheets/d/1Ohp7eJ9RQnDUsMamCgX_hvuPLSlQLAGrktFVR-p8VyI/export?gid=0&format=csv", ssl.verifypeer=FALSE)  # obtem os dados do cenario 1 do noturno e grava na variavel d_c1.
n_c2=getURL("https://docs.google.com/spreadsheets/d/1Ohp7eJ9RQnDUsMamCgX_hvuPLSlQLAGrktFVR-p8VyI/export?gid=316756507&format=csv", ssl.verifypeer=FALSE)  # similar ao de cima, mudando os parametros
n_c3=getURL("https://docs.google.com/spreadsheets/d/1Ohp7eJ9RQnDUsMamCgX_hvuPLSlQLAGrktFVR-p8VyI/export?gid=574934489&format=csv", ssl.verifypeer=FALSE)  # similar ao de cima, mudando os parametros
n_c4=getURL("https://docs.google.com/spreadsheets/d/1Ohp7eJ9RQnDUsMamCgX_hvuPLSlQLAGrktFVR-p8VyI/export?gid=12088454&format=csv", ssl.verifypeer=FALSE)  # similar ao de cima, mudando os parametros

##############################################
# Juntando os dados da planilha do Diurno e Noturno  #
##############################################
cenarios <- list()
cenarios[[1]] <- rbind(data.frame(read.csv(textConnection(d_c1))[-1], periodo = "diurno"), data.frame(read.csv(textConnection(n_c1))[-1], periodo = "noturno")) # juntando os dados de cenario 1 do diurno e noturno
cenarios[[2]] <- rbind(data.frame(read.csv(textConnection(d_c2))[-1], periodo = "diurno"), data.frame(read.csv(textConnection(n_c2))[-1], periodo = "noturno")) # similar ao de cima, mudando os parametros
cenarios[[3]] <- rbind(data.frame(read.csv(textConnection(d_c3))[-1], periodo = "diurno"), data.frame(read.csv(textConnection(n_c3))[-1], periodo = "noturno")) # similar ao de cima, mudando os parametros
cenarios[[4]] <- rbind(data.frame(read.csv(textConnection(d_c4))[-1], periodo = "diurno"), data.frame(read.csv(textConnection(n_c4))[-1], periodo = "noturno")) # similar ao de cima, mudando os parametros

########################################
# Funcao para calculo da Matriz de transicao  #
########################################
TransitionMatrix <- function(n){ # Definindo uma funcao chamada TransitionMatrix. Esta funcao recebe 1 paramentro: n
    t.mat <- matrix(NA, n+1, n+1)
    for(i in 1:(n+1)){
        t.mat[,i] <- pbinom(0:n, n, (i-1)/n) - c(0, pbinom(0:(n-1), n, (i-1)/n))
    }
    t.mat
}

########################################
# Funcao para calculo da populacao esperada  #
########################################
ExpectedPops <- function(m, n, p, gen){ # Definindo uma funcao chamada ExpectedPops. Esta funcao recebe 4 paramentro: m, n, p, gen
    pops = numeric(n+1)
    pops[(p*n)+1] <- m
    t.mat <- TransitionMatrix(n)
    for(i in 1:gen){
        pops <- t.mat %*% pops
    }
    data.frame(n.pretos = 0:n, pops)
}

############################################################
# Funcao para desenhar histogramas dos dados com a linha esperada  #
############################################################
PlotHistograms_withExpected <- function(cenario_df, n, p, gen, main = '', cor, m = dim(cenario_df)[1]){ # Definindo uma funcao chamada PlotHistograms_withExpected para desehar graficos dos dados (barras) e grafico do esperado (linhas) em um mesmo plot. Esta funcao recebe 5 paramentro: cenario_df, n, p, gen, main
    esperado = adply(1:gen, 1, function(x) ExpectedPops(m, n, p, x))
    esperado$geracao <- as.factor(esperado$X1)
    esperado$X1 <- NULL
    m_cenario = melt(cenario_df, id.vars = c('Grupo', 'periodo'))
    names(m_cenario) <- c('Grupo', 'periodo', 'geracao', 'n.pretos')
    m_cenario$geracao <- as.factor(as.numeric(gsub('geracao.', '', m_cenario$geracao)))
    ggplot(m_cenario, aes(n.pretos, group = geracao)) + geom_histogram(fill=cor, binwidth = 0.5) + theme_bw()+ xlab("Número de Pretos")+ ylab("Contagem")+
    geom_line(data = esperado, aes(n.pretos, pops)) + geom_point(data = esperado, aes(n.pretos, pops)) + facet_wrap(~geracao, scales = "free_y")+ labs(title=main)
}

############################################
# Desenho dos histogramas com a linha esperada  #
############################################
PlotHistograms_withExpected(cenarios[[1]], 4, 0.50, 12, 'Cenário 1', 'lightskyblue2') # Desenha o histograma do cenario 1 e do esprado, a partir dos dados do cenario 1, do tamanho populacional =4, p inicial =0.5 e ao longo de 12 geracoes.
PlotHistograms_withExpected(cenarios[[2]], 4, 0.25, 12, 'Cenário 2', 'lightgoldenrod1') # similar ao de cima, mudando os parametros
PlotHistograms_withExpected(cenarios[[3]], 8, 0.50, 12, 'Cenário 3', 'lightpink3') # similar ao de cima, mudando os parametros
PlotHistograms_withExpected(cenarios[[4]],16, 0.25, 12, 'Cenário 4', 'palegreen3') # similar ao de cima, mudando os parametros

####################
# Testes estatísticos #
###################

# Desvio esperado ao final do processo:

ChiSqTest <- function(cenario_df, n, p, gen){ # Definindo uma funcao chamada ChiSqTest. Esta funcao recebe 4 paramentro: cenario_df, n, p, gen.
    m_cenario = melt(cenario_df, id.vars = c('Grupo', 'periodo'))
    M <- as.table(rbind(table(m_cenario$value, m_cenario$variable)[,gen], ExpectedPops(dim(cenario_df)[1], n, p, gen)$pops))
    rownames(M) <- c('observado', 'esperado')
    print(M)
    chisq.test(M)
}

ChiSqTest(cenarios[[1]], 4, 0.5, 12) # realiza teste te Chi quadrado para os dados do cenario 1, em que as populacoes são de tamanho 4, possuem p inicial igual a 0.5 ao longo de 12 geracoes
ChiSqTest(cenarios[[2]], 4,0.25, 12) # similar ao de cima, mudando os parametros
ChiSqTest(cenarios[[3]], 8, 0.5, 12) # similar ao de cima, mudando os parametros
ChiSqTest(cenarios[[4]],16,0.25, 12) # similar ao de cima, mudando os parametros

#############################################
# Evolução de frequencias, heterozigose and all that #
#############################################
PlotStats <- function(cenario_df, n, p, gen, main = '', m = dim(cenario_df)[1]){ Definindo uma funcao chamada PlotStats para desehar graficos dos dados do esperado em um mesmo plot. Esta funcao recebe 5 paramentro: cenario_df, n, p, gen, main

    m_cenario = melt(cenario_df, id.vars = c('Grupo', 'periodo'))
    names(m_cenario) <- c('Grupo', 'periodo', 'geracao', 'n.pretos')
    m_cenario$geracao <- as.factor(as.numeric(gsub('geracao.', '', m_cenario$geracao)))
    m_cenario <- mutate(m_cenario,
                        freq = n.pretos/n,
                        heter = 2 * freq * (1-freq))
    m_cenario <- melt(m_cenario, id.vars = c('Grupo', 'periodo', 'geracao'))
    var_freq <- data.frame(geracao = as.factor(0:gen),
                           var_freq = tapply(filter(m_cenario, variable == 'freq')$value,
                                             filter(m_cenario, variable == 'freq')$geracao,
                                             var),
                           variable = 'var freq')

    esperado = adply(1:gen, 1, function(x) ExpectedPops(m, n, p, x))
    esperado$geracao <- as.factor(esperado$X1)
    esperado$X1 <- NULL
    freq_esp <- tapply(esperado$n.pretos/n * esperado$pops,
                       esperado$geracao,
                       function(x) sum(x) / m)
    heter_esp <- tapply(2 * esperado$n.pretos/n * (1 - esperado$n.pretos/n) * esperado$pops,
                        esperado$geracao,
                        function(x) sum(x) / m)
    var_esp <- tapply((esperado$n.pretos/n - p)**2 * esperado$pops,
                      esperado$geracao,
                      function(x) sum(x) / m)
    stat_esperado <- data.frame(cbind(freq_esp, heter_esp, var_esp), geracao = as.factor(1:gen))
    m_esperado <- melt(stat_esperado, id.vars = 'geracao')

    ggplot(filter(m_cenario, variable != 'n.pretos'), aes(geracao, value, group = variable, color = variable)) +
    geom_smooth() + theme_bw() + geom_line(data = var_freq, aes(geracao, var_freq, group = 1)) + geom_line(data = m_esperado, aes(geracao, value, group = variable))+ labs(title=main)
}

######################################
# Desenho dos dados reais e do esperado   #
#####################################
PlotStats(cenarios[[1]], 4, 0.50, 12, 'Cenário 1') # Desenha o histograma do cenario 1 e do esprado, a partir dos dados do cenario 1, do tamanho populacional =4, p inicial =0.5 e ao longo de 12 geracoes.
PlotStats(cenarios[[2]], 4, 0.25, 12, 'Cenário 2') # similar ao de cima, mudando os parametros
PlotStats(cenarios[[3]], 8, 0.50, 12, 'Cenário 3') # similar ao de cima, mudando os parametros
PlotStats(cenarios[[4]],16, 0.25, 12, 'Cenário 4') # similar ao de cima, mudando os parametros