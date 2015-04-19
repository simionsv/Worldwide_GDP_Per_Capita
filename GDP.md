---
title: "GDP"
output: html_document
---

```r
library(dplyr)
abs <- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/getting_data/data1.csv", colClasses="character")
abs<- select(abs, "country"=X, "gdp"=Gross.domestic.product.2012,"name"=X.2, "value"=X.3  )
abs<- abs[-(1:4),]
abs<- transform( abs, gdp=as.numeric(gdp))
```

```
## Warning: NAs introduced by coercion
```

```r
abs<- mutate(abs, title=gsub(",","",value))
abs$title<- as.numeric(abs$title)
```

```
## Warning: NAs introduced by coercion
```

```r
abs<- arrange(abs, gdp)
abs<- abs[-(191:326), ]
str(abs)
```

```
## 'data.frame':	190 obs. of  5 variables:
##  $ country: chr  "USA" "CHN" "JPN" "DEU" ...
##  $ gdp    : num  1 2 3 4 5 6 7 8 9 10 ...
##  $ name   : chr  "United States" "China" "Japan" "Germany" ...
##  $ value  : chr  " 16,244,600 " " 8,227,103 " " 5,959,718 " " 3,428,131 " ...
##  $ title  : num  16244600 8227103 5959718 3428131 2612878 ...
```

```r
abc <- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/getting_data/data2.csv", colClasses="character")
abc<- select(abc, "country"=CountryCode,"Income"=Income.Group, "region"=Region  )


abd<- read.csv("C:/Users/Sergio Simioni/Desktop/Data_Science/getting_data/data3.csv", colClasses="character")
abd<- select(abd, "country"=Country.Code,"population"=Value, "year"=Year)
abd<- filter(abd, year=="2010")
head(abd)
```

```
##   country population year
## 1     ARB  357868000 2010
## 2     CSS    6880000 2010
## 3     EAS 2201536674 2010
## 4     EAP 1961558757 2010
## 5     EMU  331766000 2010
## 6     ECS  890424544 2010
```

```r
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
```

```
## [1] 186   8
```

```r
str(country_gdp)
```

```
## 'data.frame':	186 obs. of  8 variables:
##  $ country       : chr  "MCO" "LUX" "NOR" "QAT" ...
##  $ GDP_Rank      : num  147 74 23 54 149 20 82 12 56 33 ...
##  $ Country_Name  : chr  "Monaco" "Luxembourg" "Norway" "Qatar" ...
##  $ Gross_Product : num  6075 55178 499667 171476 5474 ...
##  $ region        : chr  "Europe & Central Asia" "Europe & Central Asia" "Europe & Central Asia" "Middle East & North Africa" ...
##  $ Population    : num  35407 507000 4889000 1759000 64600 ...
##  $ GDP_Per_Capita: num  171.6 108.8 102.2 97.5 84.7 ...
##  $ position      : int  1 2 3 4 5 6 7 8 9 10 ...
```

```r
head(country_gdp,10)
```

```
##    country GDP_Rank     Country_Name Gross_Product
## 1      MCO      147           Monaco          6075
## 2      LUX       74       Luxembourg         55178
## 3      NOR       23           Norway        499667
## 4      QAT       54            Qatar        171476
## 5      BMU      149          Bermuda          5474
## 6      CHE       20      Switzerland        631173
## 7      MAC       82 Macao SAR, China         43582
## 8      AUS       12        Australia       1532408
## 9      KWT       56           Kuwait        160913
## 10     DNK       33          Denmark        314887
##                        region Population GDP_Per_Capita position
## 1       Europe & Central Asia      35407      171.57624        1
## 2       Europe & Central Asia     507000      108.83235        2
## 3       Europe & Central Asia    4889000      102.20229        3
## 4  Middle East & North Africa    1759000       97.48493        4
## 5               North America      64600       84.73684        5
## 6       Europe & Central Asia    7826000       80.65078        6
## 7         East Asia & Pacific     544000       80.11397        7
## 8         East Asia & Pacific   22299000       68.72093        8
## 9  Middle East & North Africa    2736000       58.81323        9
## 10      Europe & Central Asia    5547000       56.76708       10
```

```r
tail(country_gdp,10)
```

```
##     country GDP_Rank             Country_Name Gross_Product
## 177     GIN      148                   Guinea          5632
## 178     GNB      176            Guinea-Bissau           822
## 179     GMB      175              Gambia, The           917
## 180     ETH       85                 Ethiopia         41605
## 181     CAF      165 Central African Republic          2184
## 182     MDG      132               Madagascar          9975
## 183     NER      144                    Niger          6773
## 184     LBR      168                  Liberia          1734
## 185     BDI      162                  Burundi          2472
## 186     MWI      152                   Malawi          4264
##                 region Population GDP_Per_Capita position
## 177 Sub-Saharan Africa    9982000      0.5642156      177
## 178 Sub-Saharan Africa    1515000      0.5425743      178
## 179 Sub-Saharan Africa    1729000      0.5303644      179
## 180 Sub-Saharan Africa   82950000      0.5015672      180
## 181 Sub-Saharan Africa    4401000      0.4962509      181
## 182 Sub-Saharan Africa   20714000      0.4815584      182
## 183 Sub-Saharan Africa   15512000      0.4366297      183
## 184 Sub-Saharan Africa    3994000      0.4341512      184
## 185 Sub-Saharan Africa    8382000      0.2949177      185
## 186 Sub-Saharan Africa   14901000      0.2861553      186
```

```r
Gross_Product_Sum_by_region
```

```
##        East Asia & Pacific      Europe & Central Asia 
##                   19626905                   21170794 
##  Latin America & Caribbean Middle East & North Africa 
##                    5805845                    3275872 
##              North America                 South Asia 
##                   18071498                    2286093 
##         Sub-Saharan Africa 
##                    1295525
```

