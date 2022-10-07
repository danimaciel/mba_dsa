# mba_dsa

#### PACOTES #######

pacotes <- c("XML", "readxl", "topicmodels", "caret", "tidyr", "ggplot2", "quanteda", "extractr","pdftools","stringr","NLP","curl", "tidytext", "wordcloud", "dplyr", "SnowballC", "stopwords", "pdftools", "tm", "RColorBrewer", "magrittr")

install.packages("textplot")
library("textplot")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##### CHAMANDO A BASE #####

base_mba <- readxl::read_excel(path = "Scopus _ Base com registros para análise MBA(rotulada).xlsx")
base_mba <- data.frame(base_mba) 

base_mba$Abstract <- tolower(base_mba$Abstract) 


###### Eliminação dos artigos relacionados à saúde #######

base_mba_remove <- grep("health*", base_mba$Abstract, invert = TRUE)
base_mba_remove_title <- grep("health*", base_mba$Title)
base_mba_remove_title

base_mba

base_mba <- base_mba[c(base_mba_remove), ]

##### VISUALIZAÇÃO DOS TRABALHOS POR ANO #####

base_mba %>% filter(Year < 2021) %>%  
  ggplot(aes(x = Year))+
  geom_bar(show.legend = TRUE) +
  labs(title = "Avaliação de Impactos relacionadas a agricultura e políticas públicas",
       subtitle = "Quantidade de Trabalhos por Ano",
       caption = "Gráfico do quantitativo de trabalhos analisados",
       x = "Ano",
       y = "Quantidade")


dim(base_mba)

##### LIMPANDO A BASE: REGEX, STOPWORDS #####

# Regex - tirar números da base
base_mba <- base_mba %>% 
  mutate(Abstract = gsub(pattern = "\\d",
                         replacement = "",
                         x = Abstract)) %>% 
  mutate(Abstract = gsub(pattern = "%|,|;|\\?|\\!|\\-|\\.|\\:|\\(|\\)|~",
                         replacement = "",
                         x = Abstract))
# Stopword
stopword_en <- c(stopwords("en"), "springer", "uk", "no", "abstract", "available", "taylor", "francis", "group", "ltd", "rights", "reserved", "this", "we", "old", "one", "an", "on", "of", "the", "in", "is", "of", "for the", "to the", "of the", "in the", "of a", "in this", "of this", "on the", "et", "al", "elsevier", "all","rights", "reserved")


# Removendo elementos entre duas colunas (vetores) 
teste_string <- str_remove(string = base_mba$Abstract,
                           pattern = "elsevier")

grep(pattern = "elsevier",
     x = teste_string)

# %in% - função/atalho para cruzar verdadeiro e falso
c(1:10)[!c(1:10) %in% c(3,4)]

remove_elements <- function(x, lixo){
  return(x[! x %in% lixo])
}

a = lapply(X = base_mba$Abstract,
           FUN = function(x) {
             strsplit(x = x,
                      split = ' ')})
View(a)
lista2 <- lapply(X = a,
                 FUN = function(elemento_de_lista){
                   remove_elements(x = elemento_de_lista[[1]],
                                   lixo = stopword_en)
                 })

paste(lista2[[1]], collapse = " ")

lista3 <- lapply(X = lista2,
                 FUN = function(x){ 
                   paste(x, collapse = " ")
                 })

# Transformação em vetor
base_mba$Abstract <- unlist(lista3)

##### TOKENIZÃO: NGRAMS, BIGRAMS E TRIGRAMS ######

# Tokenização | Separando em N-grams 
base_mba_tokens_1 <- base_mba %>%
  unnest_tokens(output = palavra_resumo,
                input = Abstract,
                token = "ngrams",
                n = 1 )

# Separando em N-gram de 2
base_mba_tokens_2 <- base_mba %>%
  unnest_tokens(output = palavra_resumo,
                input = Abstract,
                token = "ngrams",
                n = 2)

# Separando em N-gram de 3
base_mba_tokens_3 <- base_mba %>%
  unnest_tokens(output = palavra_resumo,
                input = Abstract,
                token = "ngrams",
                n = 3)

NFILTER <- 3

contagem_one_gramm <- base_mba_tokens_1 %>% 
  count(palavra_resumo,
        sort = TRUE) %>% 
  filter(n>=NFILTER)

contagem_two_gramm <- base_mba_tokens_2 %>%
  count(palavra_resumo,
        sort = TRUE) %>%
  filter(n>=NFILTER)

contagem_three_gramm <- base_mba_tokens_3 %>%
  #filter(str_detect(string = palavra_resumo,
  # pattern = "(model)|(method)|(fuzzy)|(interview)|(survey)|(payback)|(rif)|(siampi)|(asirpa)|(ambitec)")) %>%
  count(palavra_resumo,
        sort = TRUE) %>%
  filter(n>=NFILTER)


##### CRIANDO O CORPUS, RODANDO FREQUÊNCIA DE TERMOS E MATRIZ #####

corpus <- Corpus(VectorSource(base_mba$Abstract))

# Criando a matrix

JSS_dtm <- DocumentTermMatrix(corpus,
                              control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3,
                                             removeNumbers = TRUE, removePunctuation = TRUE))
dim(JSS_dtm)
nrow(JSS_dtm)

# Frequência de termos

term_tfidf <-
  tapply(JSS_dtm$v/slam::row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean) *
  log2(nDocs(JSS_dtm)/slam::col_sums(JSS_dtm > 0))

SS_dtm <- JSS_dtm[, term_tfidf >= 0.1]
JSS_dtm <- JSS_dtm[slam::row_sums(JSS_dtm) > 0,]
summary(slam::col_sums(JSS_dtm))
summary(term_tfidf)

##### AGRUPAMENTOS DE TÓPICOS #####

k <- 30
SEED <- 2010

jss_TM <- list(
  VEM = LDA(JSS_dtm, k = k, control = list(seed = SEED)))
  
sapply(jss_TM[1:2], slot, "alpha")

sapply(jss_TM, function(x)
  mean(apply(posterior(x)$topics,
             1, function(z) - sum(z * log(z)))))

Topic <- topics(jss_TM[["VEM"]], 1)  #agrupamento dos artigos
table(Topic)

Terms <- terms(x = jss_TM[["VEM"]], 5) #Os cinco termos mais frequentes para cada tópico são obtidos por
Terms[,1:30]

length(Topic)

base_mba <- base_mba %>%   
  mutate(VEM = topics(jss_TM[["VEM"]], 1))
         
base_mba <- data.frame(base_mba)

Topic <- topics(jss_TM[["VEM"]], 1)
Terms <- terms(jss_TM[["VEM"]], 50)


##### VISUALIZAÇÃO #####

# Gráfico revista x tópico 

extracao_fonte <- base_mba %>% select(Source.title, 
                                      "VEM")
extracao_fonte <- extracao_fonte[!is.na(extracao_fonte$Source.title), ]

cont_extr_fnt <- extracao_fonte %>% group_by(VEM) %>% count("VEM")

cont_extr_fnt %>% count("VEM")

soma_extr <- as.data.frame(matrix(nrow = 0, ncol = 2))

for (i in unique(extracao_fonte)) {
  counts <- extracao_fonte[extracao_fonte$Source.title == i, ]
  table(counts$VEM)
}

View(as.data.frame(table(extracao_fonte)))


# Nuvem de Palavras - por conjunto de termos
wordcloud(c(Terms[,1:10]),
          min.freq = 20,
          max.words = 50)

# Relação dos termos por trabalho 
base_mba_termos <- terms(jss_TM[["VEM_fixed"]], 10)

ggplot(data = base_mba,mapping = aes(x = Year, y = Document.Type))+
  geom_boxplot()

Terms %>% ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

topico <- as.data.frame(Terms)


### Exportando arquivos
writexl::write_xlsx(x = topico, path = "topic.xlsx")
writexl::write_xlsx(x = base_mba, path = "base_atual.xlsx")
writexl::write_xlsx(x = contagem_three_gramm, path = "trigram.xlsx")
writexl::write_xlsx(x = contagem_two_gramm, path = "bigram.xlsx")
writexl::write_xlsx(x = contagem_one_gramm, path = "ngram.xlsx")

######### Visualização ############

#Gráfico de Trigrams
trigram <- read_excel(path = "trigram.xlsx")
trigram <- as.data.frame(trigram)

trigram %>% filter(n >= 20) %>% 
  ggplot(mapping = aes(x = n, y = palavra_resumo, fill = n))+
  geom_col(show.legend = FALSE)+
  labs(title = "Palavras | Trigram",
       caption = "Gráfico do quantitativo de palavras de NGRAMS = 3 > que 20",
       x = "Quantidade",
       y = "Palavras - Trigram")

#Gráfico de Bigrams
bigram <- read_excel(path = "bigram.xlsx")
bigram <- as.data.frame(x = bigram)

bigram %>% filter(n > 100) %>% 
  ggplot(mapping = aes(x = n, y = palavra_resumo, fill= n))+
  geom_col(show.legend = FALSE)+
  labs(title = "Palavras | Bigram",
       caption = "Gráfico do quantitativo de palavras de NGRAMS = 2 > que 100",
       x = "Quantidade",
       y = "Palavras - Bigram")

#Gráfico de Ngrams
ngram <- read_excel(path = "ngram.xlsx")  
ngram <- as.data.frame(ngram)

ngram %>% filter(n > 600) %>% 
  ggplot(mapping = aes(x = n, y = palavra_resumo, fill= n))+
  geom_col(show.legend = FALSE)+
  labs(title = "Palavras | Ngram",
       caption = "Gráfico do quantitativo de palavras de NGRAMS > que 600",
       x = "Quantidade",
       y = "Palavras - Ngram")

#Gráfico de Metodologias | Abstract
metodologia <- read_excel(path = "metodologias_trigram.xlsx")  
metodologia <- as.data.frame(x = metodologia)  

metodologia %>% filter(qtd >= 6) %>% 
  ggplot(mapping = aes(x = qtd, y = metodologia, fill=qtd))+
  geom_col(show.legend = FALSE)+
  labs(title = "Metodologias Identificadas - Abstract",
       caption = "Gráfico do quantitativo de metodologias com ocorrência > que 6",
       x = "Quantidade",
       y = "Metodologia")

#Gráfico de Metodologias | Keyword author
mtd_key_au <- read_excel(path = "metodologias_keyword_au.xlsx")
mtd_key_au <- as.data.frame(x = mtd_key_au)  

mtd_key_au %>% filter(qtd >= 2) %>% 
  ggplot(mapping = aes(x = qtd, y = metodologia, fill=qtd))+
  geom_col(show.legend = FALSE)+
  labs(title = "Metodologias Identificadas - Keyword Author",
       caption = "Gráfico do quantitativo de metodologias com ocorrência > que 3",
       x = "Quantidade",
       y = "Metodologia")

#Wordclouds
set.seed(1234)

wordcloud(words = trigram$palavra_resumo,
          freq = trigram$n, min.freq = 1, 
          random.order = TRUE, 
          rot.per=0.53,
          colors = brewer.pal(8, "Dark2"), 
          max.words = 150)
          
wordcloud(words = ngram$palavra_resumo,freq = ngram$n, min.freq = 100, random.order = TRUE, rot.per=0.35, colors = brewer.pal(8, "Dark2"),
          max.words = 200)

wordcloud(words = bigram$palavra_resumo,freq = bigram$n, min.freq = 20, random.order = TRUE, colors = brewer.pal(8, "Dark2"),
          max.words = 100)
          
      
  # Gráfico de calor 
 
base_graf <- readxl::read_xlsx("base_atual.xlsx") 
base_df <- base_graf[, c("Source.title", "VEM")] 

df <- table(base_df)
df <- as.data.frame.matrix(df) 
df_sum <- apply(df, 1, sum) 
df_sum <- order(df_sum, decreasing = TRUE) 
df_sorted <- df[df_sum,] 
df_sorted <- data.matrix(df_sorted) 
heatmap(df_sorted[1:30, ], cexRow=0.8)     
 
#### METODOLOGIA NA VARIÁVEL KEYWORD AUTHOR
# Busca entre tabelas
base_mba <- read_excel(path = "Scopus _ Base com registros para análise MBA.xlsx")
base_mba <- data.frame(base_mba)
metodologias <- read_excel(path = "Metodologias.xlsx")
metodologias$Metodologia

i_metodologia <- 18
padrao_metodologia <- metodologias$Metodologia[i_metodologia]
is_sel_base <- grep(pattern = padrao_metodologia,
                    x = base_mba$Author.Keywords,
                    ignore.case = TRUE,
                    value = F)
i_sel_base <- is_sel_base[3]
class(base_mba[i_sel_base,"Author.Keywords"])

keywords <- strsplit(x = base_mba[i_sel_base,"Author.Keywords"],
                     split = ";")[[1]]

termos_selecionados <- grep(pattern = padrao_metodologia,
                            x = keywords,
                            ignore.case = T,
                            value = T)


scopus[i_sel_base, "keyword_automatico"] <- paste(... = padrao_metodologia, ";", termos_selecionados)

# uso do For para rodar a base toda

for (i_metodologia in 1:nrow(metodologias)) {
  # print(i_metodologia)
  
  padrao_metodologia <- metodologias$Metodologia[i_metodologia]
  is_sel_base <- grep(pattern = padrao_metodologia,
                      x = base_mba$Author.Keywords,
                      ignore.case = TRUE,
                      value = F)
  if(length(is_sel_base) > 0) {
    # print(is_sel_base)
    # identificando apenas as ocorrências (traz as linhas de metodologia que ocorrem em scopus)
    for (i_sel_base in is_sel_base) {
      print(i_metodologia)
      print(i_sel_base)
      
      keywords <- strsplit(x = base_mba[i_sel_base,"Author.Keywords"],
                           split = ";")[[1]]
      
      termos_selecionados <- grep(pattern = padrao_metodologia,
                                  x = keywords,
                                  ignore.case = T,
                                  value = T)
      termos_selecionados <- paste(termos_selecionados,
                                   collapse = "; ")
      
      base_mba[i_sel_base, "keyword_author"] <- paste(... = padrao_metodologia, ";", termos_selecionados)
  
      
    }
  }
}
