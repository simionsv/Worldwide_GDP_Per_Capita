


URL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(URL, destfile = "./data1.csv")

URL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(URL, destfile = "./data2.csv")

URL<- "http://data.okfn.org/data/core/population/r/population.csv"
download.file(URL, destfile = "./data3.csv")

library(dplyr)
abs <- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/getting_data/data1.csv", colClasses="character")
abs<- select(abs, "country"=X, "gdp"=Gross.domestic.product.2012,"name"=X.2, "value"=X.3  )
abs<- abs[-(1:4),]
abs<- transform( abs, gdp=as.numeric(gdp))
abs<- mutate(abs, title=gsub(",","",value))
abs$title<- as.numeric(abs$title)
abs<- arrange(abs, gdp)
abs<- abs[-(191:326), ]
str(abs)

abc <- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/getting_data/data2.csv", colClasses="character")
abc<- select(abc, "country"=CountryCode,"Income"=Income.Group, "region"=Region  )


abd<- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/getting_data/data3.csv", colClasses="character")
abd<- select(abd, "country"=Country.Code,"population"=Value, "year"=Year)
abd<- filter(abd, year=="2010")
head(abd)

country_gdp<- merge(abs, abc, by="country")
country_gdp<- merge(country_gdp, abd, by="country")
country_gdp<- arrange( country_gdp, gdp)
country_gdp$population<- as.numeric(country_gdp$population)
country_gdp<- mutate(country_gdp, "GDP_Per_Capita"=(title*1000)/population)
country_gdp<- select(country_gdp, -year)
country_gdp<- arrange(country_gdp,desc(GDP_Per_Capita) )
country_gdp<- mutate(country_gdp, position = seq(1:186))
country_gdp<- select(country_gdp,country, "GDP_Rank"=gdp, "Country_Name"=name, "Gross_Product"=title, region, "Population"=population, GDP_Per_Capita, position )

Gross_Product_Sum_by_region <- tapply(country_gdp$Gross_Product, country_gdp$region, sum)

dim(country_gdp)
str(country_gdp)
head(country_gdp,10)
tail(country_gdp,10)
Gross_Product_Sum_by_region
