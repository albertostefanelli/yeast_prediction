
options(scipen=999)

rm(list=ls(all=TRUE))
library(devtools)
# install and load defined list of packages
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

list_of_required_pkg <- c(
  "RSelenium", 
  "wdman",
  "ggplot2"
)

ipak(list_of_required_pkg)

url <- "https://laconfraternitadellapizza.net/calcolapizza/"
server <- phantomjs(port=5000L)

browser <- remoteDriver(browserName = "phantomjs", port=5000L)
browser$open()

browser$navigate(url)

src <- browser$getPageSource()


substr(src, 1, 1000)
browser$screenshot(display=TRUE)
panielli <- browser$findElement(using = 'id', value="panielli")
panielli$clearElement()
panielli$sendKeysToElement(list("1"))

total_weight <- browser$findElement(using = 'id', value="peso")
total_weight$clearElement()
total_weight$sendKeysToElement(list("6000"))

total_hours_max <- 24
total_hours <- browser$findElement(using = 'id', value="liev")
total_hours$clearElement()
total_hours$sendKeysToElement(list(total_hours_max))
 
room_temp_range <- 15:35
fridge_hours_range <- total_hours_max-1


yest_temp <- c()
fridge_h <- c()
for (r in room_temp_range){
  room_temp <- as.character(r)
  print(room_temp)
  room_t <- browser$findElement(using = 'id', value="gradi")
  room_t$clearElement()
  room_t$sendKeysToElement(list(r))
  for (f in seq(fridge_hours_range)){
    fridge_hours <- f
    hours_fridge <- browser$findElement(using = 'id', value="frigo")
    hours_fridge$clearElement()
    hours_fridge$sendKeysToElement(list(f))
    yeast <- browser$findElement(using = 'id', value="lievito")
    yest_raw <- yeast$getElementText()[[1]]
    yest_comma <- gsub("[,-]", ".", yest_raw)
    print(yest_comma)
    yest_comma_final <- as.numeric(gsub("[^0-9.-]", "", yest_comma))
    fridge_h[[fridge_hours]] <- yest_comma_final
  }
  yest_temp[[room_temp]] <- fridge_h

}

names(yest_temp) <- as.numeric(names(yest_temp))


apply(expand.grid(data.frame(yest_temp)), 1L, relist, skeleton = rapply(yest_temp, head, n = 1L, how = "list")) 

grid <- expand.grid(room_temp_range,seq(fridge_hours_range))
grid$yeast <- NA

for (g in 1:nrow(grid)){
  room_temp_g <- grid[g,"Var1"]
  firdge_temp_g <- grid[g,"Var2"]
  grid[g, "yeast"] <- yest_temp[[as.character(room_temp_g)]][firdge_temp_g]
}


names(grid)[1:2] <- c("room_temp", "fridge_hours")

temp_pol <- lm(log(yeast)~poly(room_temp,5,raw = TRUE)+poly(fridge_hours,8,raw = TRUE),grid)
predition <- exp(predict(temp_pol,data.frame(room_temp = 24,fridge_hours=23)))

## ALL GOOD TO HERE 
# implement temperature environmnet 
temp_environment <- browser$findElement(using = 'id', value="gradi")
temp_environment$clearElement()
temp_environment$sendKeysToElement(list("15"))




yest_fridge <- c()
for (i in 0:23){
temp <- as.character(i)
total_hours <- browser$findElement(using = 'id', value="liev")
total_hours$clearElement()
total_hours$sendKeysToElement(list("24"))
hours_fridge <- browser$findElement(using = 'id', value="frigo")
hours_fridge$clearElement()
hours_fridge$sendKeysToElement(list(i))
yeast <- browser$findElement(using = 'id', value="lievito")
yest_raw <- yeast$getElementText()[[1]]
yest_comma <- gsub("[,-]", ".", yest_raw)
print(yest_comma)
yest_comma_final <- as.numeric(gsub("[^0-9.-]", "", yest_comma))
yest_fridge[[temp]] <- yest_comma_final
}

library(ggplot2)

df <- data.frame(cbind(x,y))

y_temp <- array(yest_temp)
x_temp <- as.numeric(names(yest_temp))

temp_pol <- lm(log(y_temp)~poly(x_temp,5,raw = TRUE))
predition_5_15_temp <- exp(predict(temp_pol,data.frame(x_temp = c(5:15))))
names(predition_5_15_temp) <- c(5:15)
15 degree  4.169995 for 23 hours 

y_fridge <- array(yest_fridge)
x_fridge <- as.numeric(names(yest_fridge))

hourse_fridge <- lm(log(y_fridge)~poly(x_fridge,7,raw = TRUE))
summary(hourse_fridge)
round(exp(fitted(hourse_fridge)),2)/yest_fridge
predition_hours <- exp(predict(hourse_fridge,data.frame(x_fridge = c(23))))
X degree 5.148519  for 23 + 1



log_base <- lm(log(y)~x)
quadratic <- lm(y~x+ I(x^2))
quadratic_log <- lm(log(y)~x+ I(x^2))
polinomial <- lm(y~x+ I(x^2) + I(x^3))
polinomial_log <- lm(log(y)~x+ I(x^2) + I(x^3))

polinomial_log <- lm(log(y)~poly(x,8,raw = TRUE))
summary(polinomial_log)
round(exp(fitted(polinomial_log)),2)/yest_fridge
names(predition_5_15_temp) <- c(5:15)





library(mgcv)
# Build the model
model <- gam(log(y) ~ s(x))
summary(model)
round(exp(fitted(model)),2)/yest_temp
predition_5_15 <- exp(predict(model,data.frame(x = c(24))))
names(predition_5_15) <- c(5:15)










