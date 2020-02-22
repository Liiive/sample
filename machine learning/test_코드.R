library(tidyverse)

#Train
data_15 <- read_csv('15_data.csv')
data_16 <- read_csv('16_data.csv')
data_17 <- read_csv('17_data.csv')

wage_15 <- read_csv('15_wage.csv')
wage_16 <- read_csv('16_wage.csv')
wage_17 <- read_csv('17_wage.csv')


total_15 <- left_join(data_15, wage_16, by = c('이름' = '선수'))
total_16 <- left_join(data_16, wage_17, by = c('이름' = '선수'))

total <- bind_rows(total_15, total_16)
total_final <- total[rowSums(is.na(total[ , 32])) == 0, ]
total_f <- data.frame(total_final[,32])
colnames(total_f) = c('target')
total_final <- data.frame(total_final[,1:29])
train_data <- bind_cols(total_final, total_f)
train_data <- train_data[,-28]
train_data <- train_data %>% 
  filter(G>=10)

write.csv(train_data,"train_data.csv", row.names = FALSE)

#Test
wage_18 <- read_csv('18_wage.csv')
data_18 <- read_csv('18_data.csv')
data_18 <- data_18[1:29]

total_17 <- left_join(data_17, wage_18, by = c('이름' = '선수'))
test_data <- total_17[rowSums(is.na(total_17[ , 32])) == 0, ]
test_d <- data.frame(test_data[,32])
colnames(test_d) = c('target')
test_data2 <- data.frame(test_data[,1:29])
test_data <- bind_cols(test_data2, test_d)
test_data <- test_data[,-28]
test_data <- test_data %>% 
  filter(G>=10)

write.csv(test_data,"test_data.csv", row.names = FALSE)

a <- read.csv('test_data_곽주원.csv')
b<- read.csv('train_data_곽주원.csv')

str(b)
b <- b %>% 
  mutate(target = `연봉.만원.`) 
b <- b %>% 
  mutate('이름' = Name)
b <- b[,c(31,1:30)]
b <- b[,-30]
b <- b[,-2]

b$target <- b$target %>% str_remove_all("[[:punct:]]")
b$target <- as.numeric(as.character(b$target))
final_train <- bind_rows(train_data, b)

write.csv(final_train,"real_train_data.csv", row.names = FALSE)

a <- a %>% 
  mutate('이름' = Name)
a <- a[,c(30,1:29)]
a <- a[,-2]

a$target <- a$target %>% str_remove_all("[[:punct:]]")
a$target <- as.numeric(as.character(a$target))
final_test <- bind_rows(test_data, a)

write.csv(final_test,"real_test_data.csv", row.names = FALSE)


add <- read.csv('train_2014.csv')
add$target <- add$target %>% str_remove_all("[[:punct:]]")
add$target <- as.numeric(as.character(add$target))
hontoni_final <- bind_rows(add, final_train)
write.csv(hontoni_final,"real_final_train_data.csv", row.names = FALSE)

print("hello")