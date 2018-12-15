library(tidyverse)
library(RMySQL)
library(countrycode)

mydb <- dbConnect(MySQL(),
                  user=Sys.getenv("DB_USER"),
                  password=Sys.getenv("DB_PWD"),
                  dbname=Sys.getenv("DB_NAME"),
                  host=Sys.getenv("DB_HOST"),
                  port=as.integer(Sys.getenv("DB_PORT")),
                  encoding = "UTF-8")

users <- dbGetQuery(mydb,
                    "SELECT *
                    FROM user")

payment <- dbGetQuery(mydb,
                        "SELECT *
                        FROM payment_order
                        WHERE status = 99")

lapply(dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

payments <- select(payment,order_no,project_idx,reward_idx,amount,amount_order,exchange,amount_delivery,delivery_exchange,paid_date,order_country_code,order_longitude,order_latitude,user_agent)


# payment record by country,date
paymentCountry <- payments %>% group_by(order_country_code,date=as.Date(paid_date)) %>% 
  summarize(paying=n(),volume=sum(amount),
                   orderAmount=sum(amount_order*exchange),
                   orderTotal=sum((amount_order*exchange)+(amount_delivery*delivery_exchange)) )%>% 
  mutate(amountPer=orderAmount/paying,amountTotalPer=orderTotal/paying,countryname=countrycode(order_country_code,origin="iso2c",destination="country.name"))

paymentCountry$countryname <- countrycode(paymentCountry$order_country_code,origin="iso2c",destination="country.name")

saveRDS(paymentCountry,file="paymentCountry.RDS")

# payment record by geoip
paymentGeo <- select(payments,order_no,amount,amount_order,exchange,amount_delivery,delivery_exchange,paid_date,order_longitude,order_latitude) %>%
  # group_by(order_no,lon=order_longitude,lat=order_latitude,date=as.Date(paid_date)) %>%
  mutate(paid=amount_order*exchange,delivery=amount_delivery*delivery_exchange)

saveRDS(paymentGeo,file="paymentGeo.RDS")

usersC <- select(users,idx,email,name,grade,locale,activated_date,login_date,write_date,email_subscribe,last_country_code,
                 birthday,gender,country_code)

usersC$countryname <- countrycode(ifelse(is.na(usersC$last_country_code),usersC$country_code,usersC$last_country_code),
                                  origin="iso2c",destination="country.name")

fCols <- c(4,5,9,10,12,13,14)
dCols <- c(6,7,8,11)

usersC[,fCols] %<>% lapply(function(x) as.factor(x))
usersC[,dCols] %<>% lapply(function(x) as.Date(x))

saveRDS(usersC,file="usersC.RDS")
