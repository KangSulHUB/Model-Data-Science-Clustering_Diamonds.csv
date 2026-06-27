library(tidyverse)
library(modelr)

#cek rata rata harga percut
diamonds %>%
  group_by(cut) %>%
  summarise(avg_price = mean(price)) %>%
  arrange(desc(avg_price))

library(broom)
glimpse(diamonds)

#visualisasi distribusi
ggplot(diamonds,aes(x = price)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribusi Harga Berlian")
#statistik deskriptip harga(price)
summary(diamonds$price)

#hubungan antara karat vs harga
#scatterplot
ggplot(diamonds,aes(x = carat, y = price)) +
  geom_point(alpha = 0.1) +
  labs(title = "Hubungan antar karat dan harga")
#Gunakan informasi log untuk melihat hubungan non - linear
ggplot(diamonds, aes(x = log(carat), y = log(price))) +
  geom_bin2d()+
  labs(title = "log(harga) vs log(price)")

#Filter data fokus pada berlian yang lebih umum
diamonds2 <- diamonds %>%
  filter(carat <= 2.5)

#Transformasi logaritmik

diamonds2 <- diamonds %>%
  mutate(lprice = log2(price),
         lcarat = log2(carat))

# periksa otlier pada dimensi fisik
# periksa nilai x, y , z yang tidak masuk akall
diamonds2 %>%
  filter(y < 3 | y > 20 | x < 3 | z < 3) %>%
  select(carat, x, y, z, price)
# ganti outlier
#ganti outlier dengan NA (opsional, tapi disarankan)
diamonds2 <-diamonds %>%
  mutate(
    x = ifelse(x < 3 | x > 30, NA, x),
    y = ifelse(y < 3 | y > 30, NA, x),
    z = ifelse(z < 3 | z > 30, NA, x)
  )
# model 1 hanya karat
mod1 <- lm(price ~ carat, data = diamonds2)
# model 2 tambahkan kualitas potongan(cut)
mod2 <- lm(price ~ carat + cut, data = diamonds2)
#model 3 tambah color dan clarity (kejernihan)
mod3 <- lm(price ~ carat + cut + color + clarity, data = diamonds2)

#ringkasan model
#lihat koefisien dari mod3
tidy(mod3) %>%
  arrange(desc(abs(estimate))) %>%
  print(n = 15)

#ringkasan model secara keseluruhan
glance(mod3)

#visualisasi prediksi
#lihat model memprediksi harga rata rata
#buat grid data baru
grid <- diamonds2 %>%
  data_grid(cut, .model = mod3) %>%
  add_predictions(mod3, "pred_price") %>%
  mutate(pred_price = 2^pred_price)

#plot prediksi 
ggplot(grid, aes(x = cut, y = pred_price)) +
  geom_point(size = 3, color = "red") +
  labs(title = "Harga prediksi rata rata perjenis potongan")

#analisis residual
diamonds2 <- diamonds2 %>%
  add_residuals(mod3, "resid_price")

#plot1 residual vs nilai prediksi
diamonds <- diamonds %>%
  add_predictions(mod3,"pred_price")

ggplot(diamonds, aes(x = pred_price, y = resid_price)) +
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual vs Prediksi(log scale)",
       y = "Prediksi log(harga)",
       y = "Residual (log scale)")