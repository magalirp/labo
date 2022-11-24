require(data.table)
require(dplyr)

setwd('~/buckets/b1/exp/competencia4/hibridacion')

lis1 <- fread('lis_sep_1.csv')
lis3 <- fread('lis_sep_3.csv')
mag1 <- fread('mag_sep_1.csv')
mag3 <- fread('mag_sep_3.csv')

lis1 <- lis1 %>% 
  filter(numero_de_cliente != 'numero_de_cliente') %>% 
  mutate(rank = as.numeric(rank))

lis3 <- lis3 %>% 
  filter(numero_de_cliente != 'numero_de_cliente') %>% 
  mutate(rank = as.numeric(rank))

mag1 <- mag1 %>% 
  filter(numero_de_cliente != 'numero_de_cliente') %>% 
  mutate(rank = as.numeric(rank))

mag3 <- mag3 %>% 
  filter(numero_de_cliente != 'numero_de_cliente') %>% 
  mutate(rank = as.numeric(rank))

lis1 <- lis1[, list(rank_lis1 = mean(rank)), numero_de_cliente]
lis3 <- lis3[, list(rank_lis3 = mean(rank)), numero_de_cliente]
mag1 <- mag1[, list(rank_mag1 = mean(rank)), numero_de_cliente]
mag3 <- mag3[, list(rank_mag3 = mean(rank)), numero_de_cliente]


data <- lis1 %>% 
  left_join(
    lis3, 
    by = c("numero_de_cliente" = "numero_de_cliente")
  ) %>% 
  left_join(
    mag1,
    by = c("numero_de_cliente" = "numero_de_cliente")
  ) %>% 
  left_join(
    mag3,
    by = c("numero_de_cliente" = "numero_de_cliente")
  )

data <- data %>% 
  mutate(rank_prom = (rank_lis1 + rank_lis3 + rank_mag1 + rank_mag3) / 4 ) %>% 
  arrange(rank_prom)

data <- data %>% 
  mutate(rank_de_rank = frank(rank_prom))

data <- data %>% 
  mutate(Predicted = case_when(
    rank_de_rank < 10201 ~ 1,
    TRUE ~ 0
  ))

send <- data %>% select(numero_de_cliente, Predicted)


fwrite(send, 'kaggle.csv', sep = ",")
