setwd("C:/Users/ASUS/Documents/GitHub/R_Alexander_Stozharov/lesson_2")
df <- read.csv("3_superstore_data.csv")
cat("\nПервые строки:")
head(df)
library(dplyr)
cat("\nIncome больше 30 000:")
df <- filter(df, Income > 30000)
print(df)

cat("\nТолько следующие столбцы: Id, Year_Birth, Education, Marital_Status, Income, Response:")
df <- select(df, Id, Year_Birth, Education, Marital_Status, Income, Response)
print(df)

cat("\nСоздайте новые столбцы: Age (возраст на момент 2023 года) и Rich_flag (который принимает True, если Income больше 80 000):")
df <- mutate(df, Age = 2023 - Year_Birth, Rich_flag = Income > 80000)
print(df)

cat("\nВ отдельный датасет запишите средние значения по Income по каждому типу Education — используйте group_by и summarize.")
summary_data <- df %>%
  group_by(Education) %>%
  summarize(Average_Income = mean(Income))
print(summary_data)

cat("\nПрисоедините созданный датасет к основному по полю Education — используйте join.")
df <- df %>%
  left_join(summary_data, by = "Education")
print(df)




View(df)
#Плоская таблица полезна для базового анализа данных, фильтрации, сортировки

# Установка и загрузка библиотеки pivottabler
#install.packages("pivottabler")
library(pivottabler)

# Создание экземпляра PivotTable
pt <- PivotTable$new()

# Добавление данных в PivotTable
pt$addData(df)

# Определение столбцов и строк
pt$addColumnDataGroups("Marital_Status")
pt$addRowDataGroups("Education")
pt$defineCalculation(calculationName = "Total count", summariseExpression = "n()")
pt$evaluatePivot()

# Вывод сводной таблицы
pt


library(ggplot2)

# Создание столбчатой диаграммы
ggplot(df, aes(x = Education, fill = Rich_flag)) +
  geom_bar() +
  labs(title = "Количество наблюдений в разрезе образования",
       x = "Education",
       y = "Количество наблюдений") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"))

#видимость зависимости степени материального состояния от степени обучения


ggplot(df, aes(x = Year_Birth)) +
  geom_line(stat = "count") +
  labs(title = "Распределение по годам рождения",
       x = "Год рождения",
       y = "Количество наблюдений")

#больше всего людей, рождённых в районе 1975 года



