#Bibliotecas:
library(dplyr)
library(readxl)
library(kableExtra)

#Função:
como_data <- function(x) {
  
  stopifnot(require(dplyr))
  x <- gsub(" .*", "", x)
  y <- gsub(".*/", "", x)
  x <- if_else((nchar(y)==4), as.Date(x, format="%d/%m/%Y"),
               as.Date(x, format="%d/%m/%y"))
  
}

#Preparando os bancos:

#Banco com os pedidos já analisados pelo COLAB:

cgu_emp <- read_excel("cgu_emp.xlsx")

load("base_cgu_completa.Rdata")

# Relação dos institutos e universidades cujos pedidos queremos analisar:

uni <- c("	FUNRei - Fundação Universidade Federal de São João Del Rei	"	,
         "	FURG – Fundação Universidade Federal do Rio Grande	"	,
         "	IF BAIANO – Instituto Federal de Educação, Ciência e Tecnologia Baiano	"	,
         "	IF FLUMINENSE – Instituto Federal de Educação, Ciência e Tecnologia Fluminense	"	,
         "	IF GOIANO – Instituto Federal de Educação, Ciência e Tecnologia Goiano	"	,
         "	IFAC – Instituto Federal de Educação, Ciência e Tecnologia do Acre	"	,
         "	IFAL – Instituto Federal de Educação, Ciência e Tecnologia de Alagoas	"	,
         "	IFAM – Instituto Federal de Educação, Ciência e Tecnologia do Amazonas	"	,
         "	IFAP – Instituto Federal de Educação, Ciência e Tecnologia do Amapá	"	,
         "	IFB – Instituto Federal de Educação, Ciência e Tecnologia de Brasília	"	,
         "	IFBA – Instituto Federal de Educação, Ciência e Tecnologia da Bahia	"	,
         "	IFC – Instituto Federal de Educação, Ciência e Tecnologia Catarinense	"	,
         "	IFCE – Instituto Federal de Educação, Ciência e Tecnologia do Ceará	"	,
         "	IFES – Instituto Federal de Educação, Ciência e Tecnologia do Espírito Santo	"	,
         "	IFFAR – Instituto Federal de Educação, Ciência e Tecnologia Farroupilha	"	,
         "	IFG – Instituto Federal de Educação, Ciência e Tecnologia de Goiás	"	,
         "	IFMA – Instituto Federal de Educação, Ciência e Tecnologia do Maranhão	"	,
         "	IFMG – Instituto Federal de Educação, Ciência e Tecnologia de Minas Gerais	"	,
         "	IFMGSE – Instituto Federal de Educação, Ciência e Tecnologia do Sudeste de Minas Gerais	"	,
         "	IFMS – Instituto Federal de Educação, Ciência e Tecnologia do Mato Grosso do Sul	"	,
         "	IFMT – Instituto Federal de Educação, Ciência e Tecnologia do Mato Grosso	"	,
         "	IFNMG – Instituto Federal de Educação, Ciência e Tecnologia do Norte de Minas Gerais	"	,
         "	IFPA – Instituto Federal de Educação, Ciência e Tecnologia do Pará	"	,
         "	IFPB – Instituto Federal de Educação, Ciência e Tecnologia da Paraíba	"	,
         "	IFPE – Instituto Federal de Educação, Ciência e Tecnologia de Pernambuco	"	,
         "	IFPI – Instituto Federal de Educação, Ciência e Tecnologia do Piauí	"	,
         "	IFPR – Instituto Federal de Educação, Ciência e Tecnologia do Paraná	"	,
         "	IFRJ – Instituto Federal de Educação, Ciência e Tecnologia do Rio de Janeiro	"	,
         "	IFRN – Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Norte	"	,
         "	IFRO – Instituto Federal de Educação, Ciência e Tecnologia de Rondônia	"	,
         "	IFRR – Instituto Federal de Educação, Ciência e Tecnologia de Roraima	"	,
         "	IFRS – Instituto Federal de Educação, Ciência e Tecnologia do Rio Grande do Sul	"	,
         "	IFSC – Instituto Federal de Educação, Ciência e Tecnologia de Santa Catarina	"	,
         "	IFSE – Instituto Federal de Educação, Ciência e Tecnologia de Sergipe	"	,
         "	IFSP – Instituto Federal de Educação, Ciência e Tecnologia de São Paulo	"	,
         "	IFSPE – Instituto Federal de Educação, Ciência e Tecnologia do Sertão Pernambucano	"	,
         "	IFSul – Instituto Federal de Educação, Ciência e Tecnologia Sul-Rio-Grandense	"	,
         "	IFSULDEMINAS – Instituto Federal de Educação, Ciência e Tecnologia do Sul de Minas Gerais	"	,
         "	IFTM – Instituto Federal de Educação, Ciência e Tecnologia do Triângulo Mineiro	"	,
         "	IFTO – Instituto Federal de Educação, Ciência e Tecnologia do Tocantins	"	,
         "	INPA – Instituto Nacional de Pesquisas da Amazônia	"	,
         "	INPE-MCT – Instituto Nacional de Pesquisas Espaciais	"	,
         "	IPEA – Fundação Instituto de Pesquisa Econômica Aplicada	"	,
         "	UFABC – Fundação Universidade Federal do ABC	"	,
         "	UFAC – Fundação Universidade Federal do Acre	"	,
         "	UFAL – Universidade Federal de Alagoas	"	,
         "	UFAM – Fundação Universidade do Amazonas	"	,
         "	UFBA – Universidade Federal da Bahia	"	,
         "	UFC – Universidade Federal do Ceará	"	,
         "	UFCA - Universidade Federal do Cariri	"	,
         "	UFCG – Universidade Federal de Campina Grande	"	,
         "	UFCSPA – Fundação Universidade Federal de Ciências da Saúde de Porto Alegre	"	,
         "	UFERSA-RN – Universidade Federal Rural do Semi-Árido	"	,
         "	UFES – Universidade Federal do Espírito Santo	"	,
         "	UFESBA - Universidade Federal do Sul da Bahia	"	,
         "	UFF – Universidade Federal Fluminense	"	,
         "	UFFS – Universidade Federal da Fronteira Sul	"	,
         "	UFG – Universidade Federal de Goiás	"	,
         "	UFGD – Fundação Universidade Federal da Grande Dourados	"	,
         "	UFJF – Universidade Federal de Juiz de Fora	"	,
         "	UFLA – Universidade Federal de Lavras	"	,
         "	UFMA – Fundação Universidade Federal do Maranhão	"	,
         "	UFMG – Universidade Federal de Minas Gerais	"	,
         "	UFMS – Fundação Universidade Federal de Mato Grosso do Sul	"	,
         "	UFMT – Fundação Universidade Federal de Mato Grosso	"	,
         "	UFOB - Universidade Federal do Oeste da Bahia	"	,
         "	UFOP – Fundação Universidade Federal de Ouro Preto	"	,
         "	UFOPA – Universidade Federal do Oeste do Pará	"	,
         "	UFPA – Universidade Federal do Pará	"	,
         "	UFPB – Universidade Federal da Paraíba	"	,
         "	UFPE – Universidade Federal de Pernambuco	"	,
         "	UFPel – Fundação Universidade Federal de Pelotas	"	,
         "	UFPI – Fundação Universidade Federal do Piauí	"	,
         "	UFPR – Universidade Federal do Paraná	"	,
         "	UFRA – Universidade Federal Rural da Amazônia	"	,
         "	UFRB – Universidade Federal do Recôncavo da Bahia	"	,
         "	UFRGS – Universidade Federal do Rio Grande do Sul	"	,
         "	UFRJ – Universidade Federal do Rio de Janeiro	"	,
         "	UFRN – Universidade Federal do Rio Grande do Norte	"	,
         "	UFRPE – Universidade Federal Rural de Pernambuco	"	,
         "	UFRR – Fundação Universidade Federal de Roraima	"	,
         "	UFRRJ – Universidade Federal Rural do Rio de Janeiro	"	,
         "	UFS – Fundação Universidade Federal de Sergipe	"	,
         "	UFSC – Universidade Federal de Santa Catarina	"	,
         "	UFSCar – Fundação Universidade Federal de São Carlos	"	,
         "	UFSM – Universidade Federal de Santa Maria	"	,
         "	UFT – Fundação Universidade Federal do Tocantins	"	,
         "	UFTM – Universidade Federal do Triângulo Mineiro	"	,
         "	UFU – Universidade Federal de Uberlândia	"	,
         "	UFV – Fundação Universidade Federal de Viçosa	"	,
         "	UFVJM – Universidade Federal dos Vales do Jequitinhonha e Mucuri	"	,
         "	UNB – Fundação Universidade de Brasília	"	,
         "	UNIFAL-MG – Universidade Federal de Alfenas	"	,
         "	UNIFAP – Fundação Universidade Federal do Amapá	"	,
         "	UNIFEI – Universidade Federal de Itajubá	"	,
         "	UNIFESP – Universidade Federal de São Paulo	"	,
         "	UNIFESSPA – Universidade Federal do Sul e Sudeste do Pará	"	,
         "	UNILA – Universidade Federal da Integração Latino-Americana	"	,
         "	UNILAB – Universidade da Integração Internacional da Lusofonia Afro-Brasileira	"	,
         "	UNIPAMPA – Fundação Universidade Federal do Pampa	"	,
         "	UNIR – Fundação Universidade Federal de Rondônia	"	,
         "	UNIRIO – Universidade Federal do Estado do Rio de Janeiro	"	,
         "	UNIVASF – Fundação Universidade Federal do Vale do São Francisco	"	,
         "	UTFPR – Universidade Tecnológica Federal do Paraná	"	)

#Retirando whitespaces antes e depois do nome dos institutos/universidades
uni <- trimws(uni, which = c("both"))

#total pedidos para universidades:
uni_total <- base_cgu_completa %>%
  filter(destino %in% uni) %>%
  mutate(data_do_pedido = como_data(data_do_pedido),
         data_da_resposta = como_data(data_da_resposta),
         estado = gsub(".*do|.*da|.*de", "", destino),
         estado = ifelse(grepl("Fluminense",destino), "Rio de Janeiro", estado),
         estado = ifelse(grepl("UFLA", destino), "Minas gerais", estado),
         estado = ifelse(grepl("Janeiro",estado), "Rio de Janeiro", estado),
         estado = ifelse(grepl("UFRGS", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("IFRN", destino), "Rio Grande do Norte", estado),
         estado = ifelse(grepl("UFJF", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UNIVASF", destino), "Pernambuco", estado),
         estado = ifelse(grepl("UFCG", destino), "Paraíba", estado),
         estado = ifelse(grepl("UFSM", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("UFSCar", destino), "São Paulo", estado),
         estado = ifelse(grepl("UFMS", destino), "Mato Grosso do Sul", estado),
         estado = ifelse(grepl("Uberlândia", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("IF BAIANO", destino), "Bahia", estado),
         estado = ifelse(grepl("Viçosa", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Mineiro", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UFERSA-RN", destino), "Rio Grande do Norte", estado),
         estado = ifelse(grepl("UFABC", destino), "São Paulo", estado),
         estado = ifelse(grepl("Ouro Preto", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Goiano", destino), "Goiás", estado),
         estado = ifelse(grepl("Catarinense", destino), "Santa Catarina", estado),
         estado = ifelse(grepl("Pernambucano", destino), "Pernambuco", estado),
         estado = ifelse(grepl("Cariri", destino), "Ceará", estado),
         estado = ifelse(grepl("UFVJM", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UNILAB", destino), "NA", estado),
         estado = ifelse(grepl("São João Del Rei", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("UFGD", destino), "Mato Grosso do Sul", estado),
         estado = ifelse(grepl("Pelotas", destino), "Rio Grande do Sul", estado),
         estado = ifelse(destino == "UFFS – Universidade Federal da Fronteira Sul", "NA", estado),
         estado = ifelse(grepl("Farroupilha", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("FURG", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("UNIPAMPA", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("IFRS", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("Alfenas", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("Itajubá", destino), "Minas Gerais", estado),
         estado = ifelse(grepl("IFSul", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("Porto Alegre", destino), "Rio Grande do Sul", estado),
         estado = ifelse(grepl("UNILA", destino), "Paraná", estado),
         estado = ifelse(grepl("IPEA", destino), "Distrito Federal", estado),
         estado = ifelse(grepl("Brasília", destino), "Distrito Federal", estado),
         estado = ifelse(grepl("INPE-MCT", destino), "São Paulo", estado),
         estado = ifelse(grepl("IFMS", destino), "Mato Grosso do Sul", estado))


min(uni_total$data_do_pedido)
max(uni_total$data_da_resposta)
  
nrow(uni_total) #21020

#Banco de análise
pedidos_uni <- cgu_emp %>%
  filter(destino %in% uni) %>%
  mutate(data_do_pedido = como_data(data_do_pedido),
         data_da_resposta = como_data(data_da_resposta),
         data_recurso_1 = as.Date(data_recurso_1, format="%Y-%m-%d"),
         data_recurso_2 = as.Date(data_recurso_2, format="%Y-%m-%d"),
         data_recurso_3 = as.Date(data_recurso_3, format="%Y-%m-%d"),
         data_recurso_4 = as.Date(data_recurso_4, format="%Y-%m-%d"),
         data_resposta_recurso_1 = as.Date(data_resposta_recurso_1, format="%Y-%m-%d"),
         data_resposta_recurso_2 = as.Date(data_resposta_recurso_2, format="%Y-%m-%d"),
         data_resposta_recurso_3 = as.Date(data_resposta_recurso_3, format="%Y-%m-%d"),
         data_resposta_recurso_4 = as.Date(data_resposta_recurso_4, format="%Y-%m-%d")) %>%
  select(-c(pasta_do_anexo_pedido, anexo_com_extensao_pedido, pasta_do_anexo_resposta, 
            anexo_com_extensao_resposta, pasta_do_anexo_recurso_1, anexo_com_extensao_recurso_1,
            pasta_do_anexo_recurso_2, anexo_com_extensao_recurso_2,
            pasta_do_anexo_recurso_3, anexo_com_extensao_recurso_3,
            pasta_do_anexo_recurso_4, anexo_com_extensao_recurso_4,
            pasta_do_anexo_resposta_recurso_1, anexo_com_extensao_resposta_recurso_1,
            pasta_do_anexo_resposta_recurso_2, anexo_com_extensao_resposta_recurso_2,
            pasta_do_anexo_resposta_recurso_3, anexo_com_extensao_resposta_recurso_3,
            pasta_do_anexo_resposta_recurso_4, anexo_com_extensao_resposta_recurso_4, responsavel)) %>%
  mutate(resposta = gsub("&lt", "", resposta),    #retirando um pouco do HTML
         resposta =  gsub("p&gt", "", resposta),
         resposta =  gsub("p class=&quot", "", resposta),
         resposta =  gsub("MsoNormal&quot", "", resposta),
         resposta =  gsub("&gt", "", resposta),
         resposta = gsub(",,", "", resposta),
         resposta = gsub("p style=&quot,margin-bottom: 0cm", "", resposta))
         

######################################################################################################################
#Panorama Geral:


#Percentual dos pedidos feitos à universidades em comparação a CGU
nrow(uni_total) / nrow(base_cgu_completa)   #23% de todos os pedidos feitos à CGU no período.

#Distribuição entre os institutos
por_destino <- uni_total %>%
  group_by(destino) %>%
  summarise(total_pedidos = n()) %>%
  arrange(desc(total_pedidos))
  
por_uf <- uni_total %>%
  group_by(estado) %>%
  summarise(total_pedidos = n()) %>%
  arrange(desc(total_pedidos)) #nada muito interessante aqui


#Tamanho da amostra
(nrow(pedidos_uni)/nrow(uni_total) ) * 100


########################################################################################################
# Analises:

pedidos_uni %>%
  group_by(assunto) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
