#HELPER

vector <- c(1, 2, 3) # Создание вектора
x = 1:5 # Создание вектора с элементами от 1 до 5
seq(число1, число2, шаг)
sort(vector) # расставляет элементы в порядке возрастания
unique(vector) # удаляет повторяющиеся элементы
rev(vector) # отзеркаливает вектор
sum(vector) # - возвращает сумму всех элементов
prod(vector) # - возвращает произведение всех элементов вектора
x <- x[! x %in% c(11, 24, 3)] # - удаление элементов из вектора
names(vector) # - именование элементов
as.integer(vector) # - приведение к конкретному типу
is.integer(vector) # - проверка на тип
runif(num) # возврат случайного значения от 0 до 1
round() # округление по мат. законам
library(pckg_name) # подключение пакета
require(pckg_name) # подключение пакета - разницы почти нет
install.packages("pckg_name", dependencies = TRUE) # установка пакета
update.packages() # обновление пакетов
sessionInfo() # небольшая справка 
vector[-(1:3)] # вывод без каких-либо элементов
attr(x, 'author x') <- 'Slava' # добавление атрибута
attributes(x) <- NULL # удаление атрибута
sample(1:100, 50) # создание 50 случайных чисел от 0 до 100


x[x > 77 & x < 99] # выведет все элементы x, соотв. условию
all(x < 200) # проверяет на верность ВСЕ элементы x
any(x < 200) # вернет TRUE если хоть 1 элемент соотв. условию
which(x >= 50) # вернет номера элементов, соотв. условию
which.min(x) # вернет номер элемента с наименьшим значением
which.max(x) # вернет номер элемента с наибольшим значением


matrix(1:6, nrow = 2) # создание матрицы
matrix(1:6, nrow = 2, byrow = TRUE) # упаковка данных по строчкам
matrix(1:2, nrow = 2, ncol = 5) # спец размера
dim(matrix) # вернуть размерность
m1 %*% m2 # умножение по правилами линейной алгебры
m1[1, 3] # доступ к элементу
m1[2, ] # доступ к целой строке
m1[, 2] # доступ к целому столбцу
m1[1, ] <- 0 # замена элементов целой строки
m[, ind, drop = FALSE] # вывод столбца в матричном виде, а не вектором
rownames(m) <- c('row1', 'row2')
colnames(m) <- paste0('column', 1:5)
rbind(m1, m2) # присоединений матриц по строчкам
cbind(m1, m2) # присоединение матриц по столбцам
apply(m, 1, f) # функция apply, f = function()
apply(m, 1:2, function(i) if (i < 2) i else 13) # example
rowSums(m) # сумма рядов
colMeans(m) # среднее по колонкам
diag() # полезная функция


list(1:5, 'Slava', matrix(0, 2, 2)) # создание списка
list(a = 1, b = 1:3, '1to5' = 1:5, 42) # именованный список
list(a = list(1, 2, 3), b = list(list(4), 5, 6)) # список списков
с(l1, l2) # объединение списков
unlist(l) # перевод списка в вектор
l[3:2]; l[-(1:2)] # возвращает подсписок, векторные правила индексирования
l[[1]] # возврат первого элемента
l$Slava # доступ к элементу с именем Slava
lapply(list, func) # функция lapply
sapply(list, func) # вернёт результат вектором


df <- data.frame(x = 1:4, y = LETTERS[1:4], z = c(T, F),
                 row.names = c('odin', 'dva', 'tri', 'chetire')) # Создание data frame
str(df) # краткая сводка об объекте
dim(df) # вернёт размер
length(df) # вернёт количество СТОЛБЦОВ
names(df) # возвращает имена СТОЛБЦОВ
df[3:4, -1] # матричная индексация
df[c(F, T), c('z', 'x')] # логическая и по именам индексация
df$z # обращение к столбцу
df[df$x > 2, ] # выборка по условиям
subset(df, x > 2) # тоже самое
subset(df, x > 2, select = c(x, z)) # тоже самое, но вернет только столбцы x и z
rbind(); cbind() # работают так же, как и для матрицы
df1 <- data.frame(x = c(3, 2, 6, 1), salary = c(100, 1000, 300, 500))
merge(df, df1, by = 'x') # аналог sql inner join; by - установка ключа


read.table(file, header, sep, quote, na.strings, colClasses, comment.char, skip) # Чтение табличных данных, все 
                                                                                 # элементы определены по умолчанию,
                                                                                 # но можно задать свои
read.csv();read.csv2();read.delim(); read.delim2(); # оболочки read.table() со своими параметрами
str(); summary(); head(); tail() # позволяют проверить, что все импортировалось нормально
df[complete.cases(df), ]; na.omit(df) # удаление наблюдений с пропущенными значениями
df$new_var <- 5 # добавление новых переменных
df$old_var <- f(df$old_var) # изменение переменных
df$old_var <- NULL # удаление переменных
within() # преобразует много переменных
write.table(); write.csv(); write.csv2() # запись в файлы, аналогична чтению


paste(c("углекислый", "веселящий"), "газ") # объединение строк
paste(c("углекислый", "веселящий"), "газ", sep = "_") # объединение строк
paste(c("углекислый", "веселящий"), "газ", collapse = ", а также ") # объединение строк
strsplit(name, "разделитель", fixed = TRUE) # разбиение строки
grep("что ищем", name) # поиск, где находится что-то
grepl("что ищем", name) # вернёт TRUE FALSE 
gsub("\\b[[:alpha{4,5}:]]\\b", "на что заменим", name) # замена найденного, 1 элемент - шаблон
#library(stringr) - пакет для работы со строками
str_extract(name, "y.") # вернёт все элементы по типу ye yr yt yi и т.д.
str_replace(name, "[xy]", "?") # заменит все первые x или y на ?
# если добавить _all к имени функции - будет искать все вхождения
tolower(name); toupper(name) # меняет регистр всех элементов
str_length(name) # длина строки
formatC(vector, digits = 3) # оставить в векторы три значимых знака
cat() # напечатать объект "как есть", действуют \n \t


f <- factor(sample(LETTERS, 30, replace = TRUE)) # создание фактора - гибрид int и char
as.numeric(f); as.character(f)
levels(f) # уникальные уровни фактора
nlevels(f) # количество уникальных уровней
f[f == 'A'] <- 'Z'; f # замена букв A на Z
(f <- droplevels(f)) # удаление пустых уровней 
#Если заключить в круглые скобки - объект будет напечатан
levels(f) <- tolower(levels(f)) # перевод всех уровней в нижний регистр
levels(f)[1] <- 'bbb'; # Первый уровень заменяется на bbb
f <- ordered(sample(LETTERS, 30, replace = TRUE)) # создание упорядоченного фактора
cut(rnorm(10), -5:5) # разбивает на интервалы
table(cut(rnorm(1000), -5:5)) # проводит подсчёт кол-ва эл-ов для каждого уровня
tapply(warpbreaks$breaks, warpbreaks$wool, max) # ф-ия t из семейства apply



#OPTIONS
digits # кол-во знаков при печати чисел
error # поведение при ошибке
width # длина строки при печати векторов и матриц
options(stringsAsFactors = FALSE) # отмена правила, что все строковые переменные
# при вызове из файла становятся факторами

getwd() # вернуть путь к рабочей директории
head(list.files()) # аналог ls -l (первые 6)
list.dirs("..", recursive = FALSE) # Директории на уровне с моей директорией
setwd() # смена рабочей директории
list.files(pattern = ".*\\.csv$") # поиск всех csv файлов
readLines("avianHabitat.csv", 5) # чтение 5 строк с файла, проверка


replicate(kolichestvo, func) # вызывает функцию несколько раз
mapply(seq, from = 1:4, to = 2:5, by = 1/ (1 + 1:4)) # пример m из apply
m <- outer(letters, LETTERS, paste0) # перебор всевозможных комбинаций аргументов
dim(m)
#### VECTORIZE
lp_norm <- function(x, p = 2) {
  if (p >= 1) sum(abs(x)^p)^(1/p)
  else NaN
}
lp_norm(1:10, -1:4)

lp_norm <- Vectorize(lp_norm, "p")
lp_norm(1:10, -1:4)
####
df1 <- data.frame(id = 1:2, value = rnorm(2))
df2 <- data.frame(id = 3:4, value = runif(2))
df3 <- data.frame(id = 222, value = 2)
do.call(rbind, list(df1, df2, df3)) # вызывает функцию на списке аргументво


func0 <- function(x, y){
  return (x)
} #- пример создания функции

if (x > y) {
  print("x > y")
} else {
  print ("y >= x")
} # синтаксис if-else
ifelse(runif(8) > 0.5, "Orel", "Reshka")

switch("factorial",
       sum = 5 + 5,
       product = 5 * 5,
       factorial = factorial(5),
       0) #синтаксис switch

i <- 0
repeat {
  i <- i + runif(1)
  print(i)
  if (i > 5) break
} #синтаксис repeat

i <- 2**14
while(i > 1000){
  i <- i/2
  print(i)
} #синтаксис while

for (i in 1:8){
  if (i %% 2 == 0) print(i)
}
for (i in letters){
  if (i == 'b') next
  if (i == 'd') break
  print(i)
} # синтаксис for (а также next)

# Пример плохого цикла for
y <- 1:1e6
system.time({
  x <- 0
  for (i in y) x[i] <- sqrt(y[i])
})
# Пример замены
system.time({
  y <- sqrt(y)
})


df<-data.frame(first_name = c("John", "Peter", "Mary", "Caroline"),
               
               last_name = c("White", "Black", "Red", "Blue"),
               
               email_address = c("White@mail.ru", "Black@mail.ru ", "Red@mail.ru ", "Blue@mail.ru "),
               
               postal_address = c("Washington", "Birmingem", "Rostov", "Bratsk"),
               
               date_added = c("01.01.2022", "02.02.2022", "03.03.2022", "04.04.2022"))

select(df, first_name, last_name, date_added)
df %>% select(c(1:2, 5))
select(df, contains("name"), date_added)
select(df, matches("_.{4,5}$"))
