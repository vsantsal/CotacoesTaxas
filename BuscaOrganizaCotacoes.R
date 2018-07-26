library(tidyverse)
library(lubridate)
library(jsonlite)

#recebe datas do período (ver como integrar isso com usuário no shiny) e as transforma para obter dados da web

# data inicial, data final e transformações para utilizar posteriormente
dataInicial <- "01/01/2016"
dataFinal <- "30/06/2018"
dataInicial <- lubridate::dmy(dataInicial)
dataFinal <- lubridate::dmy(dataFinal)
strDataInicial <- format(dataInicial, "%m-%d-%Y")
strDataFinal <- format(dataFinal, "%m-%d-%Y")

# construção de vetor de moedas
vetorMoedas <- c("USD", "EUR", "GBP", "JPY", "CHF")

#exemplo url
#https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoMoedaPeriodo(moeda=@moeda,dataInicial=@dataInicial,dataFinalCotacao=@dataFinalCotacao)?@moeda='EUR'&@dataInicial='06-30-2018'&@dataFinalCotacao='07-18-2018'&$format=json&$select=paridadeCompra,paridadeVenda,cotacaoCompra,cotacaoVenda,dataHoraCotacao,tipoBoletim

# template de string url com o 'curinga' '@siglaMoeda@' para substituição por cada moeda do vetor
templateUrl <- paste0("https://olinda.bcb.gov.br/olinda/servico/PTAX/versao/v1/odata/CotacaoMoedaPeriodo(moeda=@moeda,dataInicial=@dataInicial,dataFinalCotacao=@dataFinalCotacao)?@moeda='@siglaMoeda@'&@dataInicial='",strDataInicial,"'&@dataFinalCotacao='",strDataFinal,"'&$format=json&$select=paridadeCompra,paridadeVenda,cotacaoCompra,cotacaoVenda,dataHoraCotacao,tipoBoletim")

# substituição por cada moeda do vetor
listaUrl <- purrr::pmap_chr(list(replacement = vetorMoedas,
                                 pattern = "@siglaMoeda@",
                                 x = rep(templateUrl,
                                         length(vetorMoedas))),
                            gsub)

#composição de maps para utilizar funções readLines e fromJSON sobre lista URL
listaJSON <- purrr::map(purrr::map(listaUrl, 
                                   readLines),
                        jsonlite::fromJSON)

bancoDados <- data.frame(paridadeCompra = numeric(0),
                         paridadeVenda = numeric(0),
                         cotacaoCompra = numeric(0),
                         cotacaoVenda = numeric(0),
                         dataHoraCotacao = numeric(0),
                         tipoBoletim = character(0),
                         siglaMoeda = character(0))

#criando dataset
for (i in seq_along(vetorMoedas)) {
  bancoDados <- rbind(bancoDados, 
                      cbind(as.data.frame(listaJSON[[i]][["value"]]),
                            moeda = vetorMoedas[i]))
}


## PARTE 2 - ORGANIZANDO OS DADOS (DEIXANDO-OS EM FORMATO TIDY)

#verificando estrutura
tibble::glimpse(bancoDados)

#o glimpse nos revela pontos a tratar no data set, como: i) h? uma coluna desnecess?ria ($X), ii) a dataHoraCotacao est? como fator - temos de transform?-la em data, iii) substituir o tipoBoletim de "Intermediário" por "Intermedi?rio", iv) transformar as duas colunas cotacaoCompra, cotacaoVenda nas colunas tipoCotacao, cotacao


#transformando a coluna factor dataHoraCotacao no formato datetime

bancoDados$dataHoraCotacao <- lubridate::ymd_hms(bancoDados$dataHoraCotacao)

#removendo coluna X, se existir
bancoDados <- bancoDados %>%
  dplyr::select(-one_of("X"))

#substituindo "Intermediário" por "Intermedi?rio"

bancoDados$tipoBoletim <- stringr::str_replace(bancoDados$tipoBoletim,
                                               "Intermediário",
                                               "Intermedi?rio")

#transformando as colunas cotacaoCompra, cotacaoVenda, paridadeCompra, paridadeVenda nas colunas paridadeCotacao, tipo

bancoDados <- bancoDados %>%
  tidyr::gather(tipo, 
                valor, 
                -c(moeda, 
                   tipoBoletim, 
                   dataHoraCotacao))

bancoDados$tipo <- bancoDados$tipo %>%
  stringr::str_replace_all(c("paridade" = "paridade_",
                             "cotacao" = "cotacao_")
  )

bancoDados <- bancoDados %>%
  tidyr::separate(tipo,
                  c("tipoParidadeCompra",
                    "tipoCompraVenda"),
                  sep = "_")

#salvando em arquivo csv para posterior leitura
write.csv(file = paste0("cotacoesmoeda_",
                        gsub("-", "",strDataInicial), 
                        "_", 
                        gsub("-", "", strDataFinal),
                        ".csv"), 
          x = bancoDados)
