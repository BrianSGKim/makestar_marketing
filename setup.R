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

payments <- select(payment,order_no,project_idx,reward_idx,amount,amount_order,exchange,amount_delivery,delivery_exchange,paid_date,order_country_code,order_longitude,order_latitude,user_agent)


# payment record by country,date
paymentCountry <- payments %>% group_by(order_country_code,date=as.Date(paid_date)) %>% 
  summarize(paying=n(),volume=sum(amount),
                   orderAmount=sum(amount_order*exchange),
                   orderTotal=sum((amount_order*exchange)+(amount_delivery*delivery_exchange)) )%>% 
  mutate(amountPer=orderAmount/paying,amountTotalPer=orderTotal/paying,countryname=countrycode(order_country_code,origin="iso2c",destination="country.name"))

paymentCountry$countryname <- countrycode(paymentc$order_country_code,origin="iso2c",destination="country.name")

# payment record by geoip
paymentGeo <- select(payments,order_no,amount,amount_order,exchange,amount_delivery,delivery_exchange,paid_date,order_longitude,order_latitude) %>%
  # group_by(order_no,lon=order_longitude,lat=order_latitude,date=as.Date(paid_date)) %>%
  mutate(paid=amount_order*exchange,delivery=amount_delivery*delivery_exchange)


