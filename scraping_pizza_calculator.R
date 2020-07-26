
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

browser$screenshot(display=TRUE)
panielli <- browser$findElement(using = 'id', value="panielli")
panielli$clearElement()
panielli$sendKeysToElement(list("1"))

total_weight <- browser$findElement(using = 'id', value="peso")
total_weight$clearElement()
total_weight$sendKeysToElement(list("6000"))

# total_hours_max <- 24
# total_hours <- browser$findElement(using = 'id', value="liev")
# total_hours$clearElement()
# total_hours$sendKeysToElement(list(total_hours_max))

## NOTES:
# 1. Oils do not affect yeast quantities. They are counted as half flour and half water
# 2. Flour does not affet yeast quantities
# 3. Everything else affect yeast
# 4. Salt affect yeast activity but I am not sure it has been implemented in the app.

total_hours_rage <- 3:96
total_hours_rage <- 3:5
room_temp_range <- 15:35
room_temp_range <- 15:18
teglia_range <- c(0, 1)
hidro_range <- (50:100)
hidro_range <- 50:53
fridge_range <- 0:(max(total_hours_rage)-1)
salt_range <- 0:70
salt_range <- 50:51

grid <- expand.grid(total_hours_rage,room_temp_range,fridge_range, teglia_range,hidro_range,salt_range)
names(grid)[1:6] <- c("Total Hours", "Room Temp", "Hours Fridge", "Teglia", "Hidro","Salt")
grid <- grid[!grid$"Hours Fridge">grid$"Total Hours"-1, ]

grid$yeast <- NA

done <- 0
total_combinations <- nrow(grid)
teglia_list <- c()
hours_list <- c()
fridge_list <- c()
room_list <- c()
hidro_list <- c()
salt_list <- c()
for (t in teglia_range){
  teglia_name <- ifelse(t==0,"No", "Yes")
  #print(teglia_name)
  teglia_t <- browser$findElement(using = 'id', value="teglia")
  teglia_t$clickElement()
  for (tot in total_hours_rage){
    hours_name <- as.character(tot)
    #print(hours_name)
    total_hours <- browser$findElement(using = 'id', value="liev")
    total_hours$clearElement()
    total_hours$sendKeysToElement(list(tot))
  for (f in 0:(tot-1)){
    fridge_name <- as.character(f)
    #print(fridge_name)
    hours_fridge <- browser$findElement(using = 'id', value="frigo")
    hours_fridge$clearElement()
    hours_fridge$sendKeysToElement(list(f))
  for (r in room_temp_range){
    room_name <- as.character(r)
    #print(room_name)
    room_t <- browser$findElement(using = 'id', value="gradi")
    room_t$clearElement()
    room_t$sendKeysToElement(list(r))
  for (s in salt_range){
    salt_name <- as.character(s)
    salt <-  browser$findElement(using = 'id', value="salepl")
    salt$clearElement()
    salt$sendKeysToElement(list(s))
  for (h in hidro_range){
    hidro_name <- as.character(h)
    hidro_t <- browser$findElement(using = 'id', value="idro")
    hidro_t$clearElement()
    hidro_t$sendKeysToElement(list(h))
    yeast <- browser$findElement(using = 'id', value="lievito")
    yest_raw <- yeast$getElementText()[[1]]
    yest_comma <- gsub("[,-]", ".", yest_raw)
    print(yest_comma)
    yest_comma_final <- as.numeric(gsub("[^0-9.-]", "", yest_comma))
    grid[grid$"Teglia"==t & grid$"Total Hours"==tot & grid$"Hours Fridge"==f & grid$"Room Temp"==r & grid$"Salt"==s & grid$"Hidro"==h ,"yeast"] <- yest_comma_final
    done <- done+1
    #tot_minus_1 <- total_combinations-1)
    print(paste("Yeast for", "Teglia:", teglia_name, "Total h ferment:", hours_name, "Time in Fridge:", fridge_name, "Room Temp:", room_name, "Hydration:", hidro_name, "Salt:",salt_name, sep=" "))
    print(paste("----% Complete:",done/total_combinations*100, "----"))
  }}}}}}



### MODELING

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










