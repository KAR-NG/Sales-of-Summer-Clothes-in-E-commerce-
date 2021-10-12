Data Analysis plus model development for product success
================
Kar Ng
2021

-   [1 R Libraries](#1-r-libraries)
-   [2 Introduction](#2-introduction)
-   [3 Data Preparation](#3-data-preparation)
    -   [3.1 Data Importation](#31-data-importation)
    -   [3.2 Data Description](#32-data-description)
    -   [3.3 Data Exploration](#33-data-exploration)
-   [4 Data Cleaning](#4-data-cleaning)
    -   [4.1 Remove variables](#41-remove-variables)
    -   [4.2 Text Mining](#42-text-mining)
    -   [4.3 Create Metrics](#43-create-metrics)
-   [5 Exploratory Data Analysis
    (EDA)](#5-exploratory-data-analysis-eda)
-   [6 Statistical Analysis](#6-statistical-analysis)
-   [Reference](#reference)

------------------------------------------------------------------------

------------------------------------------------------------------------

## 1 R Libraries

``` r
library(tidyverse)
library(kableExtra)
```

## 2 Introduction

This project will analyse a public dataset on *Kaggle* website, named
“Sales of Summer clothes in E-commerce Wish”. As the name suggests, this
dataset will include information related to the sales of summer clothes
on Wish. There are 43 columns of variables in the dataset including
price, units\_sold, rating, tags, colour, countries shipped to and etc.

A series of tasks that this project will answer include:

-   How about trying to validate the established idea of human
    sensitiveness to price drops ?

-   You may look for top categories of products so that you know what
    sells best

-   Do bad products sell ? How about the relationship between the
    quality of a product (ratings) and its success ? Does the price
    factor into this ?

-   Do seller’s fame factor into top products ?

-   Do the number of tags (making a product more discoverable) factor
    into the success of a product ?

Two popular data analysis techniques will be applied - exploratory data
analysis and machine learning. The machine learning technique is to
build models and choose the best one to predict *how well a product is
going to sell*.

## 3 Data Preparation

The dataset is downloaded from kaggle website, visit this
[Link](https://www.kaggle.com/jmmvutu/summer-products-and-sales-in-ecommerce-wish/tasks?taskId=1617)
and uploaded to R to complete the analysis.

### 3.1 Data Importation

``` r
cloth <- read_csv("summer.csv")
```

    ## Rows: 1573 Columns: 43

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (19): title, title_orig, currency_buyer, tags, product_color, product_va...
    ## dbl (24): price, retail_price, units_sold, uses_ad_boosts, rating, rating_co...

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
sample_n(cloth, 10) %>% 
  kbl() %>% 
  kable_styling(bootstrap_options = c("border", "stripped"))
```

<table class="table table-bordered" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
title
</th>
<th style="text-align:left;">
title\_orig
</th>
<th style="text-align:right;">
price
</th>
<th style="text-align:right;">
retail\_price
</th>
<th style="text-align:left;">
currency\_buyer
</th>
<th style="text-align:right;">
units\_sold
</th>
<th style="text-align:right;">
uses\_ad\_boosts
</th>
<th style="text-align:right;">
rating
</th>
<th style="text-align:right;">
rating\_count
</th>
<th style="text-align:right;">
rating\_five\_count
</th>
<th style="text-align:right;">
rating\_four\_count
</th>
<th style="text-align:right;">
rating\_three\_count
</th>
<th style="text-align:right;">
rating\_two\_count
</th>
<th style="text-align:right;">
rating\_one\_count
</th>
<th style="text-align:right;">
badges\_count
</th>
<th style="text-align:right;">
badge\_local\_product
</th>
<th style="text-align:right;">
badge\_product\_quality
</th>
<th style="text-align:right;">
badge\_fast\_shipping
</th>
<th style="text-align:left;">
tags
</th>
<th style="text-align:left;">
product\_color
</th>
<th style="text-align:left;">
product\_variation\_size\_id
</th>
<th style="text-align:right;">
product\_variation\_inventory
</th>
<th style="text-align:left;">
shipping\_option\_name
</th>
<th style="text-align:right;">
shipping\_option\_price
</th>
<th style="text-align:right;">
shipping\_is\_express
</th>
<th style="text-align:right;">
countries\_shipped\_to
</th>
<th style="text-align:right;">
inventory\_total
</th>
<th style="text-align:right;">
has\_urgency\_banner
</th>
<th style="text-align:left;">
urgency\_text
</th>
<th style="text-align:left;">
origin\_country
</th>
<th style="text-align:left;">
merchant\_title
</th>
<th style="text-align:left;">
merchant\_name
</th>
<th style="text-align:left;">
merchant\_info\_subtitle
</th>
<th style="text-align:right;">
merchant\_rating\_count
</th>
<th style="text-align:right;">
merchant\_rating
</th>
<th style="text-align:left;">
merchant\_id
</th>
<th style="text-align:right;">
merchant\_has\_profile\_picture
</th>
<th style="text-align:left;">
merchant\_profile\_picture
</th>
<th style="text-align:left;">
product\_url
</th>
<th style="text-align:left;">
product\_picture
</th>
<th style="text-align:left;">
product\_id
</th>
<th style="text-align:left;">
theme
</th>
<th style="text-align:left;">
crawl\_month
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mode féminine été décontracté à manches courtes col rond imprimé pas
aujourd’hui lettre T-shirt mignon drôle impression coton chemise hauts
</td>
<td style="text-align:left;">
Women’s Fashion Summer Casual Short Sleeve Round Neck Printed Not Today
Letter T-shirt Cute Funny Printing Cotton Shirt Tops
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3.13
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Summer,Fashion,Necks,Printed Tee,Funny,printed shirts,summer t-shirts,T
Shirts,Casual,Round neck,Women’s Fashion,women shirt,printed,loose
t-shirt,Women,Tops,Cotton
</td>
<td style="text-align:left;">
pink
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
paoyeye
</td>
<td style="text-align:left;">
paoyeye
</td>
<td style="text-align:left;">
(18,877 notes)
</td>
<td style="text-align:right;">
18877
</td>
<td style="text-align:right;">
4.079886
</td>
<td style="text-align:left;">
58fdedf645f9b210f58c00cc
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5e69cad1c01db73c10b938e7>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5e69cad1c01db73c10b938e7-medium.jpg>
</td>
<td style="text-align:left;">
5e69cad1c01db73c10b938e7
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
Fashion Back Lace Up Flower Print Tops Short Short Off Shoulder Loose
Loose Licol plissé deux pièces Set deux pièces
</td>
<td style="text-align:left;">
Fashion Back Lace Up Flower Print Tops Shorts Off Shoulder Loose Pleated
Halter Two Piece Set
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
1000
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3.72
</td>
<td style="text-align:right;">
489
</td>
<td style="text-align:right;">
204
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
44
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Shorts,Lace,Halter,Pleated,Loose,Backs,Outfits,Fashion,Lace
Up,Flowers,Suits,Print,off shoulder,Tops,Women
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
XXS
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Quantité limitée !
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
Jinqianbao888
</td>
<td style="text-align:left;">
jinqianbao888
</td>
<td style="text-align:left;">
86 % avis positifs (40,245 notes)
</td>
<td style="text-align:right;">
40245
</td>
<td style="text-align:right;">
3.997590
</td>
<td style="text-align:left;">
55769de4fa0be7249554bce2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5ad96a001c6def05447630a0>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5ad96a001c6def05447630a0-medium.jpg>
</td>
<td style="text-align:left;">
5ad96a001c6def05447630a0
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
New Summer Women’s New Fashion Sleeveless Polka Dot V-neck Strap Mini
Backless Casual Top Dresses
</td>
<td style="text-align:left;">
New Summer Women’s New Fashion Sleeveless Polka Dot V-neck Strap Mini
Backless Casual Top Dresses
</td>
<td style="text-align:right;">
3.68
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.71
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Summer,Fashion,Mini,Tops,Polkas,V-neck,Casual,black,polka
dot,sleeveless,backless,Dress,Women Fashion
</td>
<td style="text-align:left;">
navy
</td>
<td style="text-align:left;">
XS
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
Maxnina
</td>
<td style="text-align:left;">
maxnina
</td>
<td style="text-align:left;">
80 % avis positifs (105,015 notes)
</td>
<td style="text-align:right;">
105015
</td>
<td style="text-align:right;">
3.789601
</td>
<td style="text-align:left;">
5177b0b63feb620dd802a197
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5d82f19340367911da1b9ee7>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5d82f19340367911da1b9ee7-medium.jpg>
</td>
<td style="text-align:left;">
5d82f19340367911da1b9ee7
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
Sexy femmes mode Shorts lâches lacent taille élastique Summer Beach
pantalons courts Shorts
</td>
<td style="text-align:left;">
Sexy Women Fashion Loose Shorts Lace Up Elastic Waist Summer Beach Short
Pants Shorts
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
5000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.60
</td>
<td style="text-align:right;">
985
</td>
<td style="text-align:right;">
388
</td>
<td style="text-align:right;">
198
</td>
<td style="text-align:right;">
168
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
154
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
elastic waist,Lace,Elastic,pants,casualshort,Short pants,summer
shorts,Women’s Fashion,women shorts,women fashion shorts,High Waist
Pants,Shorts,Lace Up,solid color,sexy,Beach,Waist
</td>
<td style="text-align:left;">
black
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
sangery
</td>
<td style="text-align:left;">
sangery
</td>
<td style="text-align:left;">
(7,889 notes)
</td>
<td style="text-align:right;">
7889
</td>
<td style="text-align:right;">
3.995056
</td>
<td style="text-align:left;">
56d6cf588bd26e03ee663123
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5afa87619987a56b4ae7405c>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5afa87619987a56b4ae7405c-medium.jpg>
</td>
<td style="text-align:left;">
5afa87619987a56b4ae7405c
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
8 Couleurs Streetwear Femme LOVE Lettres de l’amour Streetwear Femmes
Imprimé sans manches coton douillet Débardeur Débardeur Lady Fashion
Chemisier ample Chemisier Femme T-shirts décontractés T-shirts
décontractés
</td>
<td style="text-align:left;">
8 Colors Streetwear Women LOVE Letters Printed Sleeveless Cotton Cozy
Tank Top Lady Fashion Loose Shirt Blouse Women Casual T-shirts
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
10000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4.11
</td>
<td style="text-align:right;">
2483
</td>
<td style="text-align:right;">
1325
</td>
<td style="text-align:right;">
521
</td>
<td style="text-align:right;">
360
</td>
<td style="text-align:right;">
134
</td>
<td style="text-align:right;">
143
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
blouse,Vest,Fashion,Tank,Shirt,Love,graphic
tee,Cotton,Tops,topsamptshirt,T Shirts,tank top,Women’s
Fashion,printed,Summer Fashion,sleeveless,Printed T Shirts,Casual,Women
</td>
<td style="text-align:left;">
black
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
galowtew
</td>
<td style="text-align:left;">
galowtew
</td>
<td style="text-align:left;">
87 % avis positifs (34,311 notes)
</td>
<td style="text-align:right;">
34311
</td>
<td style="text-align:right;">
4.066218
</td>
<td style="text-align:left;">
5a9cfdb4ccf0c85918542fdd
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5c1c89326432f859634aa737>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5c1c89326432f859634aa737-medium.jpg>
</td>
<td style="text-align:left;">
5c1c89326432f859634aa737
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
5 couleurs mode femmes push up bikini ensemble sexy couleur unie été
maillot de bain chaud maillot de bain casual beachwear soutien-gorge +
bas de bikini
</td>
<td style="text-align:left;">
5 Colors Fashion Women Push Up Bikini Set Sexy Solid Color Summer Hot
Bathing Suit Swimsuit Casual Beachwear Bra + Bikini Bottom
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
1000
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4.22
</td>
<td style="text-align:right;">
237
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Summer,Fashion,Women’s
Fashion,Swimsuit,Bikini,Suits,sexy,Women,Casual,Bras,Swimming
</td>
<td style="text-align:left;">
black
</td>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
Pure Fashion
</td>
<td style="text-align:left;">
purefashionltd
</td>
<td style="text-align:left;">
(96,838 notes)
</td>
<td style="text-align:right;">
96838
</td>
<td style="text-align:right;">
4.264659
</td>
<td style="text-align:left;">
5cb636dda7173671aedef051
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5d0c8e125adf55596cf111bd>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5d0c8e125adf55596cf111bd-medium.jpg>
</td>
<td style="text-align:left;">
5d0c8e125adf55596cf111bd
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
2018 Femmes Été décontracté Couleur unie Décontractée Sans manches Robe
de plage Débardeur de plage Top A-line Pocket Pocket Robe Sexy Col V
profond Short Club Party Mini Halter Robe Midi Robe Ete Femme Genou
Longueur Jupes plissées Jupes Femmes Mode Swing Coton Robe T-Shirt
Grande taille S-6XL
</td>
<td style="text-align:left;">
2018 Women Summer Casual Solid Color Loose Sleeveless Beach Tank Top
A-line Pocket Dress Sexy Deep V-neck Short Club Party Mini Halter Midi
Dresses Robe Ete Femme Knee Length Pleated Skirts Ladies Fashion Swing
Cotton T-Shirt Dress Plus Size S-6XL
</td>
<td style="text-align:right;">
5.87
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
20000
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3.98
</td>
<td style="text-align:right;">
4137
</td>
<td style="text-align:right;">
2090
</td>
<td style="text-align:right;">
852
</td>
<td style="text-align:right;">
576
</td>
<td style="text-align:right;">
278
</td>
<td style="text-align:right;">
341
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Mini,Fashion,halter dress,Summer,plus size dress,Pleated,V
Neckdress,beach dress,Party Dress,sexy,Skirts,Casual,Cotton,Deep
V-Neck,party,Ladies Fashion,Tops,summer dress,Ladies,women
dress,Robes,Women,Beach,Plus Size,Halter,Dress,V-neck,Pocket,Women’s
Fashion,sleeveless,knee,casual dress,Shorts,Tank,T
Shirts,sleevelessdresse,loose dress
</td>
<td style="text-align:left;">
red
</td>
<td style="text-align:left;">
XS
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
yshsbjj
</td>
<td style="text-align:left;">
yshsbjj
</td>
<td style="text-align:left;">
88 % avis positifs (4,646 notes)
</td>
<td style="text-align:right;">
4646
</td>
<td style="text-align:right;">
4.118812
</td>
<td style="text-align:left;">
580dd6425250a21944205a36
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5b0bbe32c1082f71c9ad9209>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5b0bbe32c1082f71c9ad9209-medium.jpg>
</td>
<td style="text-align:left;">
5b0bbe32c1082f71c9ad9209
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
XS-8XL Femmes Faashion D’été Col En V Sans Manches Tops Lâche Plissée
Débardeurs Dames De Mode Swing Flowy Hem Shirs Gilet D’été Shirs Tunique
Coton Blouses
</td>
<td style="text-align:left;">
XS-8XL Women Faashion Summer V-neck Sleeveless Tops Loose Pleated Tank
Tops Ladies Fashion Swing Flowy Hem Shirs Summer Vest Shirs Tunic Cotton
Blouses
</td>
<td style="text-align:right;">
1.91
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
10000
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
4.05
</td>
<td style="text-align:right;">
697
</td>
<td style="text-align:right;">
385
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
81
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:right;">
58
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Summer,Vest,Fashion,camisole,tunic top,Pleated,womens
top,V-neck,Ladies,Women’s Fashion,sleeveless tops,loose
t-shirt,tunic,summer tops,Women,blouse,Tank,Ladies Fashion,Tops,Tops &
T-Shirts,women tank top,pleatedtop,Casual Tops,Cotton,sleeveless
</td>
<td style="text-align:left;">
blue
</td>
<td style="text-align:left;">
XS
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
chenhai1028
</td>
<td style="text-align:left;">
chenhai1028
</td>
<td style="text-align:left;">
87 % avis positifs (2,399 notes)
</td>
<td style="text-align:right;">
2399
</td>
<td style="text-align:right;">
4.105461
</td>
<td style="text-align:left;">
5939085087117919c6f12a77
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5e3e69f009a91d0893974dab>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5e3e69f009a91d0893974dab-medium.jpg>
</td>
<td style="text-align:left;">
5e3e69f009a91d0893974dab
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
Pnma Hommes Été À Séchage Rapide Shorts Hommes Swim Beach Shorts Shim
Shorts Beach Wear Sports Gym
</td>
<td style="text-align:left;">
Pnma Men Summer Quick Dry Shorts Mens Swim Beach Shorts Swim Shorts
Beach Wear Sports Gym
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
65
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3.97
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
runningshort,Beach
Shorts,beachpant,Bottom,sailboatshort,Shorts,Sport,beach swimwear,Men’s
Fashion,Puma,Summer,Men Shorts,men’s shorts,surfshort,Beach,Men
</td>
<td style="text-align:left;">
white
</td>
<td style="text-align:left;">
S
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
DSVIE FASHION
</td>
<td style="text-align:left;">
dsviefashion
</td>
<td style="text-align:left;">
(239 notes)
</td>
<td style="text-align:right;">
239
</td>
<td style="text-align:right;">
3.882845
</td>
<td style="text-align:left;">
5d9f24a451a4ed72d95bac9c
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5e1bd924d7be4445acfa8c6d>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5e1bd924d7be4445acfa8c6d-medium.jpg>
</td>
<td style="text-align:left;">
5e1bd924d7be4445acfa8c6d
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
<tr>
<td style="text-align:left;">
Femmes été tricolore couture manches courtes robe longue dames casual
col en V coton, plus la taille robes longues XS-5XL
</td>
<td style="text-align:left;">
Women Summer Three-color Stitching Short Sleeve Long Dress Ladies Casual
V Collar Cotton Plus Size Long Dresses XS-5XL
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
EUR
</td>
<td style="text-align:right;">
100
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.71
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
Summer,V Collar,Plus Size,Stitching,Sleeve,Dresses,Dress,summer
dress,long dress,short sleeves,Casual,ladies dress,Women’s
Fashion,Shorts,Women,Ladies,women dress,Short
Sleeved,Cotton,Collar,casual dress
</td>
<td style="text-align:left;">
red
</td>
<td style="text-align:left;">
XS
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Livraison standard
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
50
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
CN
</td>
<td style="text-align:left;">
lianlie
</td>
<td style="text-align:left;">
lianlie
</td>
<td style="text-align:left;">
84 % avis positifs (224 notes)
</td>
<td style="text-align:right;">
224
</td>
<td style="text-align:right;">
3.892857
</td>
<td style="text-align:left;">
5eb8b5ebac21070efaf91c99
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
<https://www.wish.com/c/5ec5cf37cd3da70de19c3f18>
</td>
<td style="text-align:left;">
<https://contestimg.wish.com/api/webimage/5ec5cf37cd3da70de19c3f18-medium.jpg>
</td>
<td style="text-align:left;">
5ec5cf37cd3da70de19c3f18
</td>
<td style="text-align:left;">
summer
</td>
<td style="text-align:left;">
2020-08
</td>
</tr>
</tbody>
</table>

### 3.2 Data Description

### 3.3 Data Exploration

## 4 Data Cleaning

### 4.1 Remove variables

### 4.2 Text Mining

### 4.3 Create Metrics

## 5 Exploratory Data Analysis (EDA)

## 6 Statistical Analysis

## Reference

<https://www.kaggle.com/jmmvutu/summer-products-and-sales-in-ecommerce-wish>
