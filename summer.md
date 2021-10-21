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
    -   [4.2 Remove missing values](#42-remove-missing-values)
    -   [4.3 Factor conversion](#43-factor-conversion)
    -   [4.4 Typos in the factor
        variables](#44-typos-in-the-factor-variables)
    -   [4.5 Examining the Rating](#45-examining-the-rating)
    -   [4.6 New Metric: price\_drop](#46-new-metric-price_drop)
    -   [4.6 New Metric: discount\_per](#46-new-metric-discount_per)
    -   [4.7 New Metric: price\_class](#47-new-metric-price_class)
-   [5 Visualisation](#5-visualisation)
    -   [5.1 Validated! Human sensitive to price
        drops](#51-validated-human-sensitive-to-price-drops)
    -   [5.2 Typical top product
        categories](#52-typical-top-product-categories)
    -   [5.3 The Effect of Rating on
        Sales](#53-the-effect-of-rating-on-sales)
    -   [5.4 Logarithmically graphing the
        Fame](#54-logarithmically-graphing-the-fame)
    -   [5.5 The Number of Tags in produce
        success](#55-the-number-of-tags-in-produce-success)
    -   [5.6 EXTRA: Which text has the most
        sales?](#56-extra-which-text-has-the-most-sales)
    -   [5.7 EXTRA: Text Mining](#57-extra-text-mining)
    -   [5.8 EXTRA: Badges and Product
        Success](#58-extra-badges-and-product-success)
    -   [5.9 EXTRA: Advertisement Boost and Product
        Success](#59-extra-advertisement-boost-and-product-success)
-   [6 Staitistcal Analysis](#6-staitistcal-analysis)
    -   [6.1 Feature Selection](#61-feature-selection)
    -   [6.3 Feature cleaning](#63-feature-cleaning)
    -   [6.2 EDA](#62-eda)
        -   [6.2.1 Histogram](#621-histogram)
        -   [6.2.2 Boxplot](#622-boxplot)
        -   [6.3.3 Relationship Curve](#633-relationship-curve)
        -   [6.3.3 Correlogram](#633-correlogram)
    -   [6.4 Multiple Linear Regression](#64-multiple-linear-regression)
    -   [6.5 Random Forest’s Important
        Plot](#65-random-forests-important-plot)
-   [7 CONCLUSION](#7-conclusion)
-   [8 Legality](#8-legality)
-   [9 Reference](#9-reference)

------------------------------------------------------------------------

------------------------------------------------------------------------

## 1 R Libraries

``` r
library(tidyverse)
library(kableExtra)
library(skimr)
library(lubridate)
library(hrbrthemes)
library(tidytext)
library(ggExtra)
library(patchwork)
library(tidytext)
library(corrplot)
library(caret)
library(MASS)
library(car)
library(widyr)
library(igraph)
library(ggraph)
library(plotly)

# Format setting

options(scipen = 999)
```

## 2 Introduction

This project will analyse a public dataset on *Kaggle* website, named
“Sales of Summer clothes in E-commerce Wish”. As the name suggests, this
dataset have information related to the sales of summer clothes on
Wish.com. There are 43 columns of variables in the dataset including
price, units\_sold, rating, tags, colour, countries shipped to and etc.

A series of tasks this project will answer include:

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

This project will also study the statistical relationship between
variables with the success of a product in the term of units sold.

## 3 Data Preparation

The dataset is downloaded from kaggle website, visit this
[Link](https://www.kaggle.com/jmmvutu/summer-products-and-sales-in-ecommerce-wish/tasks?taskId=1617)

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

### 3.2 Data Description

``` r
Variable <- names(cloth)

Description <- c("Title for localized for european countries. May be the same as title_orig if the seller did not offer a translation.", 
                 "Original english title of the product.",
                 "Price you would pay to get the product.",
                 "Reference price for similar articles on the market, or in other stores/places. Used by the seller to indicate a regular value or the price before discount.",
                 "Currency of the prices.",
                 "Number of units sold. Lower bound approximation by steps.",
                 "Whether the seller paid to boost his product within the platform (highlighting, better placement or whatever).",
                 "Mean product rating.",
                 "Total number of ratings of the product.",
                 "Number of 5-star ratings.",
                 "Number of 4-star ratings.",
                 "Number of 3-star ratings.",
                 "Number of 2-star ratings.",                 
                 "Number of 1-star ratings.",
                 "Number of badges the product or the seller have.",
                 "A badge that denotes the product is a local product. Conditions may vary (being produced locally, or something else). Some people may prefer buying local products rather than. 1 means Yes, has the badge.",
                 "Badge awarded when many buyers consistently gave good evaluations. 1 means Yes, has the badge.",
                 "Badge awarded when this product's order is consistently shipped rapidly.",
                 "tags set by the seller.",
                 "Product's main color.",
                 "One of the available size variation for this product.",
                 "Inventory the seller has. Max allowed quantity is 50.",
                 "Shipping_option_name.",
                 "Shipping price.",
                 "Whether the shipping is express or not. 1 for True.",
                 "Number of countries this product is shipped to. Sellers may choose to limit where they ship a product to.",
                 "Total inventory for all the product's variations (size/color variations for instance).",
                 "Whether there was an urgency banner with an urgency.",
                 "A text banner that appear over some products in the search results.",
                 "Origin_country.",
                 "Merchant's displayed name (show in the UI as the seller's shop name).",
                 "Merchant's canonical name. A name not shown publicly. Used by the website under the hood as a canonical name. Easier to process since all lowercase without white space.",
                 "The subtitle text as shown on a seller's info section to the user. (raw, not preprocessed). The website shows this to the user to give an overview of the seller's stats to the user. Mostly consists of `% <positive_feedbacks> (<rating_count> reviews)` written in french.",
                 "Number of ratings of this seller.",
                 "Merchant's rating.",
                 "Merchant unique id.",
                 "Convenience boolean that says whether there is a `merchant_profile_picture` url.",
                 "Custom profile picture of the seller (if the seller has one). Empty otherwise.",
                 "Url to the product page. You may need to login to access it.",
                 "Product_picture.",
                 "Product identifier. You can use this key to remove duplicate entries if you're not interested in studying them.",
                 "The search term used in the search bar of the website to get these search results.",
                 "Meta: for info only.")


data.frame(Variable, Description) %>% 
  kbl(caption = "Adapated from the Kaggle Website.") %>% 
  kable_styling(bootstrap_options = c("striped", "bordered"))
```

<table class="table table-striped table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>
Adapated from the Kaggle Website.
</caption>
<thead>
<tr>
<th style="text-align:left;">
Variable
</th>
<th style="text-align:left;">
Description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
title
</td>
<td style="text-align:left;">
Title for localized for european countries. May be the same as
title\_orig if the seller did not offer a translation.
</td>
</tr>
<tr>
<td style="text-align:left;">
title\_orig
</td>
<td style="text-align:left;">
Original english title of the product.
</td>
</tr>
<tr>
<td style="text-align:left;">
price
</td>
<td style="text-align:left;">
Price you would pay to get the product.
</td>
</tr>
<tr>
<td style="text-align:left;">
retail\_price
</td>
<td style="text-align:left;">
Reference price for similar articles on the market, or in other
stores/places. Used by the seller to indicate a regular value or the
price before discount.
</td>
</tr>
<tr>
<td style="text-align:left;">
currency\_buyer
</td>
<td style="text-align:left;">
Currency of the prices.
</td>
</tr>
<tr>
<td style="text-align:left;">
units\_sold
</td>
<td style="text-align:left;">
Number of units sold. Lower bound approximation by steps.
</td>
</tr>
<tr>
<td style="text-align:left;">
uses\_ad\_boosts
</td>
<td style="text-align:left;">
Whether the seller paid to boost his product within the platform
(highlighting, better placement or whatever).
</td>
</tr>
<tr>
<td style="text-align:left;">
rating
</td>
<td style="text-align:left;">
Mean product rating.
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_count
</td>
<td style="text-align:left;">
Total number of ratings of the product.
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_five\_count
</td>
<td style="text-align:left;">
Number of 5-star ratings.
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_four\_count
</td>
<td style="text-align:left;">
Number of 4-star ratings.
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_three\_count
</td>
<td style="text-align:left;">
Number of 3-star ratings.
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_two\_count
</td>
<td style="text-align:left;">
Number of 2-star ratings.
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_one\_count
</td>
<td style="text-align:left;">
Number of 1-star ratings.
</td>
</tr>
<tr>
<td style="text-align:left;">
badges\_count
</td>
<td style="text-align:left;">
Number of badges the product or the seller have.
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_local\_product
</td>
<td style="text-align:left;">
A badge that denotes the product is a local product. Conditions may vary
(being produced locally, or something else). Some people may prefer
buying local products rather than. 1 means Yes, has the badge.
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_product\_quality
</td>
<td style="text-align:left;">
Badge awarded when many buyers consistently gave good evaluations. 1
means Yes, has the badge.
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_fast\_shipping
</td>
<td style="text-align:left;">
Badge awarded when this product’s order is consistently shipped rapidly.
</td>
</tr>
<tr>
<td style="text-align:left;">
tags
</td>
<td style="text-align:left;">
tags set by the seller.
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_color
</td>
<td style="text-align:left;">
Product’s main color.
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_variation\_size\_id
</td>
<td style="text-align:left;">
One of the available size variation for this product.
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_variation\_inventory
</td>
<td style="text-align:left;">
Inventory the seller has. Max allowed quantity is 50.
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_option\_name
</td>
<td style="text-align:left;">
Shipping\_option\_name.
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_option\_price
</td>
<td style="text-align:left;">
Shipping price.
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_is\_express
</td>
<td style="text-align:left;">
Whether the shipping is express or not. 1 for True.
</td>
</tr>
<tr>
<td style="text-align:left;">
countries\_shipped\_to
</td>
<td style="text-align:left;">
Number of countries this product is shipped to. Sellers may choose to
limit where they ship a product to.
</td>
</tr>
<tr>
<td style="text-align:left;">
inventory\_total
</td>
<td style="text-align:left;">
Total inventory for all the product’s variations (size/color variations
for instance).
</td>
</tr>
<tr>
<td style="text-align:left;">
has\_urgency\_banner
</td>
<td style="text-align:left;">
Whether there was an urgency banner with an urgency.
</td>
</tr>
<tr>
<td style="text-align:left;">
urgency\_text
</td>
<td style="text-align:left;">
A text banner that appear over some products in the search results.
</td>
</tr>
<tr>
<td style="text-align:left;">
origin\_country
</td>
<td style="text-align:left;">
Origin\_country.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_title
</td>
<td style="text-align:left;">
Merchant’s displayed name (show in the UI as the seller’s shop name).
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_name
</td>
<td style="text-align:left;">
Merchant’s canonical name. A name not shown publicly. Used by the
website under the hood as a canonical name. Easier to process since all
lowercase without white space.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_info\_subtitle
</td>
<td style="text-align:left;">
The subtitle text as shown on a seller’s info section to the user. (raw,
not preprocessed). The website shows this to the user to give an
overview of the seller’s stats to the user. Mostly consists of
`% &lt;positive_feedbacks&gt; (&lt;rating_count&gt; reviews)` written in
french.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_rating\_count
</td>
<td style="text-align:left;">
Number of ratings of this seller.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_rating
</td>
<td style="text-align:left;">
Merchant’s rating.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_id
</td>
<td style="text-align:left;">
Merchant unique id.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_has\_profile\_picture
</td>
<td style="text-align:left;">
Convenience boolean that says whether there is a
`merchant_profile_picture` url.
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_profile\_picture
</td>
<td style="text-align:left;">
Custom profile picture of the seller (if the seller has one). Empty
otherwise.
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_url
</td>
<td style="text-align:left;">
Url to the product page. You may need to login to access it.
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_picture
</td>
<td style="text-align:left;">
Product\_picture.
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_id
</td>
<td style="text-align:left;">
Product identifier. You can use this key to remove duplicate entries if
you’re not interested in studying them.
</td>
</tr>
<tr>
<td style="text-align:left;">
theme
</td>
<td style="text-align:left;">
The search term used in the search bar of the website to get these
search results.
</td>
</tr>
<tr>
<td style="text-align:left;">
crawl\_month
</td>
<td style="text-align:left;">
Meta: for info only.
</td>
</tr>
</tbody>
</table>

### 3.3 Data Exploration

The dataset has 1,573 rows of observation and 43 columns of variables.
Variables are currently categorised into 2 types, which are character
and numeric.

``` r
skim_without_charts(cloth)
```

<table style="width: auto;" class="table table-condensed">
<caption>
Data summary
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Name
</td>
<td style="text-align:left;">
cloth
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of rows
</td>
<td style="text-align:left;">
1573
</td>
</tr>
<tr>
<td style="text-align:left;">
Number of columns
</td>
<td style="text-align:left;">
43
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Column type frequency:
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
character
</td>
<td style="text-align:left;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
numeric
</td>
<td style="text-align:left;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Group variables
</td>
<td style="text-align:left;">
None
</td>
</tr>
</tbody>
</table>

**Variable type: character**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim\_variable
</th>
<th style="text-align:right;">
n\_missing
</th>
<th style="text-align:right;">
complete\_rate
</th>
<th style="text-align:right;">
min
</th>
<th style="text-align:right;">
max
</th>
<th style="text-align:right;">
empty
</th>
<th style="text-align:right;">
n\_unique
</th>
<th style="text-align:right;">
whitespace
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
title
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
327
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1201
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
title\_orig
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
272
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1201
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
currency\_buyer
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
tags
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
61
</td>
<td style="text-align:right;">
448
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1230
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_color
</td>
<td style="text-align:right;">
41
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
101
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_variation\_size\_id
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_option\_name
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
urgency\_text
</td>
<td style="text-align:right;">
1100
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
origin\_country
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
0.99
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_title
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
51
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
958
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_name
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
52
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
957
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_info\_subtitle
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
54
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1058
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_id
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
958
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_profile\_picture
</td>
<td style="text-align:right;">
1347
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
99
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
125
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_url
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1341
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_picture
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
76
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1341
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_id
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1341
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
theme
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
crawl\_month
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

**Variable type: numeric**

<table>
<thead>
<tr>
<th style="text-align:left;">
skim\_variable
</th>
<th style="text-align:right;">
n\_missing
</th>
<th style="text-align:right;">
complete\_rate
</th>
<th style="text-align:right;">
mean
</th>
<th style="text-align:right;">
sd
</th>
<th style="text-align:right;">
p0
</th>
<th style="text-align:right;">
p25
</th>
<th style="text-align:right;">
p50
</th>
<th style="text-align:right;">
p75
</th>
<th style="text-align:right;">
p100
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
price
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
8.33
</td>
<td style="text-align:right;">
3.93
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
5.81
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
49
</td>
</tr>
<tr>
<td style="text-align:left;">
retail\_price
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
23.29
</td>
<td style="text-align:right;">
30.36
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
26.00
</td>
<td style="text-align:right;">
252
</td>
</tr>
<tr>
<td style="text-align:left;">
units\_sold
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4339.01
</td>
<td style="text-align:right;">
9356.54
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
100.00
</td>
<td style="text-align:right;">
1000.00
</td>
<td style="text-align:right;">
5000.00
</td>
<td style="text-align:right;">
100000
</td>
</tr>
<tr>
<td style="text-align:left;">
uses\_ad\_boosts
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.43
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
rating
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.82
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
3.85
</td>
<td style="text-align:right;">
4.11
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_count
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
889.66
</td>
<td style="text-align:right;">
1983.93
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
24.00
</td>
<td style="text-align:right;">
150.00
</td>
<td style="text-align:right;">
855.00
</td>
<td style="text-align:right;">
20744
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_five\_count
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
442.26
</td>
<td style="text-align:right;">
980.20
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
79.00
</td>
<td style="text-align:right;">
413.50
</td>
<td style="text-align:right;">
11548
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_four\_count
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
179.60
</td>
<td style="text-align:right;">
400.52
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
31.50
</td>
<td style="text-align:right;">
168.25
</td>
<td style="text-align:right;">
4152
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_three\_count
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
134.55
</td>
<td style="text-align:right;">
311.69
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
24.00
</td>
<td style="text-align:right;">
129.25
</td>
<td style="text-align:right;">
3658
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_two\_count
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
63.71
</td>
<td style="text-align:right;">
151.34
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
62.00
</td>
<td style="text-align:right;">
2003
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_one\_count
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
95.74
</td>
<td style="text-align:right;">
214.08
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
20.00
</td>
<td style="text-align:right;">
94.00
</td>
<td style="text-align:right;">
2789
</td>
</tr>
<tr>
<td style="text-align:left;">
badges\_count
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.34
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_local\_product
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.02
</td>
<td style="text-align:right;">
0.13
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_product\_quality
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.07
</td>
<td style="text-align:right;">
0.26
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_fast\_shipping
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.01
</td>
<td style="text-align:right;">
0.11
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_variation\_inventory
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
33.08
</td>
<td style="text-align:right;">
21.35
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
50
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_option\_price
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.35
</td>
<td style="text-align:right;">
1.02
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_is\_express
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.05
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
countries\_shipped\_to
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
40.46
</td>
<td style="text-align:right;">
20.30
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
31.00
</td>
<td style="text-align:right;">
40.00
</td>
<td style="text-align:right;">
43.00
</td>
<td style="text-align:right;">
140
</td>
</tr>
<tr>
<td style="text-align:left;">
inventory\_total
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
49.82
</td>
<td style="text-align:right;">
2.56
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
50.00
</td>
<td style="text-align:right;">
50
</td>
</tr>
<tr>
<td style="text-align:left;">
has\_urgency\_banner
</td>
<td style="text-align:right;">
1100
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_rating\_count
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
26495.83
</td>
<td style="text-align:right;">
78474.46
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1987.00
</td>
<td style="text-align:right;">
7936.00
</td>
<td style="text-align:right;">
24564.00
</td>
<td style="text-align:right;">
2174765
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_rating
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
4.03
</td>
<td style="text-align:right;">
0.20
</td>
<td style="text-align:right;">
2.33
</td>
<td style="text-align:right;">
3.92
</td>
<td style="text-align:right;">
4.04
</td>
<td style="text-align:right;">
4.16
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_has\_profile\_picture
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.14
</td>
<td style="text-align:right;">
0.35
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
0.00
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>

Looking at the “complete\_rate”, I see that *urgency\_text*,
*merchant\_profile\_picture*, and *has\_urgency\_banner* have too many
missing data with a very low complete rate of less than 30%. They will
be removed during data cleaning.

Here provides another way of looking at missing values in the dataset.

``` r
colSums(is.na(cloth))
```

    ##                        title                   title_orig 
    ##                            0                            0 
    ##                        price                 retail_price 
    ##                            0                            0 
    ##               currency_buyer                   units_sold 
    ##                            0                            0 
    ##               uses_ad_boosts                       rating 
    ##                            0                            0 
    ##                 rating_count            rating_five_count 
    ##                            0                           45 
    ##            rating_four_count           rating_three_count 
    ##                           45                           45 
    ##             rating_two_count             rating_one_count 
    ##                           45                           45 
    ##                 badges_count          badge_local_product 
    ##                            0                            0 
    ##        badge_product_quality          badge_fast_shipping 
    ##                            0                            0 
    ##                         tags                product_color 
    ##                            0                           41 
    ##    product_variation_size_id  product_variation_inventory 
    ##                           14                            0 
    ##         shipping_option_name        shipping_option_price 
    ##                            0                            0 
    ##          shipping_is_express         countries_shipped_to 
    ##                            0                            0 
    ##              inventory_total           has_urgency_banner 
    ##                            0                         1100 
    ##                 urgency_text               origin_country 
    ##                         1100                           17 
    ##               merchant_title                merchant_name 
    ##                            0                            4 
    ##       merchant_info_subtitle        merchant_rating_count 
    ##                            1                            0 
    ##              merchant_rating                  merchant_id 
    ##                            0                            0 
    ## merchant_has_profile_picture     merchant_profile_picture 
    ##                            0                         1347 
    ##                  product_url              product_picture 
    ##                            0                            0 
    ##                   product_id                        theme 
    ##                            0                            0 
    ##                  crawl_month 
    ##                            0

Following provide a way of looking at the dataset vertically with the
listing of some values within each variables and their classified type
in R. This will helps to see which variables are irrelevant to this
project and should be removed.

``` r
glimpse(cloth)
```

    ## Rows: 1,573
    ## Columns: 43
    ## $ title                        <chr> "2020 Summer Vintage Flamingo Print  Paja~
    ## $ title_orig                   <chr> "2020 Summer Vintage Flamingo Print  Paja~
    ## $ price                        <dbl> 16.00, 8.00, 8.00, 8.00, 2.72, 3.92, 7.00~
    ## $ retail_price                 <dbl> 14, 22, 43, 8, 3, 9, 6, 11, 84, 22, 5, 8,~
    ## $ currency_buyer               <chr> "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",~
    ## $ units_sold                   <dbl> 100, 20000, 100, 5000, 100, 10, 50000, 10~
    ## $ uses_ad_boosts               <dbl> 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0,~
    ## $ rating                       <dbl> 3.76, 3.45, 3.57, 4.03, 3.10, 5.00, 3.84,~
    ## $ rating_count                 <dbl> 54, 6135, 14, 579, 20, 1, 6742, 286, 15, ~
    ## $ rating_five_count            <dbl> 26, 2269, 5, 295, 6, 1, 3172, 120, 6, 287~
    ## $ rating_four_count            <dbl> 8, 1027, 4, 119, 4, 0, 1352, 56, 2, 128, ~
    ## $ rating_three_count           <dbl> 10, 1118, 2, 87, 2, 0, 971, 61, 3, 92, 81~
    ## $ rating_two_count             <dbl> 1, 644, 0, 42, 2, 0, 490, 18, 1, 68, 61, ~
    ## $ rating_one_count             <dbl> 9, 1077, 3, 36, 6, 0, 757, 31, 3, 112, 12~
    ## $ badges_count                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ badge_local_product          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ badge_product_quality        <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ badge_fast_shipping          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ tags                         <chr> "Summer,Fashion,womenunderwearsuit,printe~
    ## $ product_color                <chr> "white", "green", "leopardprint", "black"~
    ## $ product_variation_size_id    <chr> "M", "XS", "XS", "M", "S", "Size-XS", "XS~
    ## $ product_variation_inventory  <dbl> 50, 50, 1, 50, 1, 1, 50, 50, 50, 50, 2, 2~
    ## $ shipping_option_name         <chr> "Livraison standard", "Livraison standard~
    ## $ shipping_option_price        <dbl> 4, 2, 3, 2, 1, 1, 2, 3, 2, 2, 2, 2, 1, 2,~
    ## $ shipping_is_express          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,~
    ## $ countries_shipped_to         <dbl> 34, 41, 36, 41, 35, 40, 31, 139, 36, 33, ~
    ## $ inventory_total              <dbl> 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 5~
    ## $ has_urgency_banner           <dbl> 1, 1, 1, NA, 1, NA, NA, NA, 1, NA, 1, 1, ~
    ## $ urgency_text                 <chr> "Quantité limitée !", "Quantité limitée !~
    ## $ origin_country               <chr> "CN", "CN", "CN", "CN", "CN", "CN", "CN",~
    ## $ merchant_title               <chr> "zgrdejia", "SaraHouse", "hxt520", "allen~
    ## $ merchant_name                <chr> "zgrdejia", "sarahouse", "hxt520", "allen~
    ## $ merchant_info_subtitle       <chr> "(568 notes)", "83 % avis positifs (17,75~
    ## $ merchant_rating_count        <dbl> 568, 17752, 295, 23832, 14482, 65, 10194,~
    ## $ merchant_rating              <dbl> 4.128521, 3.899673, 3.989831, 4.020435, 4~
    ## $ merchant_id                  <chr> "595097d6a26f6e070cb878d1", "56458aa03a69~
    ## $ merchant_has_profile_picture <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,~
    ## $ merchant_profile_picture     <chr> NA, NA, NA, NA, NA, NA, "https://s3-us-we~
    ## $ product_url                  <chr> "https://www.wish.com/c/5e9ae51d43d6a96e3~
    ## $ product_picture              <chr> "https://contestimg.wish.com/api/webimage~
    ## $ product_id                   <chr> "5e9ae51d43d6a96e303acdb0", "58940d436a0d~
    ## $ theme                        <chr> "summer", "summer", "summer", "summer", "~
    ## $ crawl_month                  <chr> "2020-08", "2020-08", "2020-08", "2020-08~

I identified that following variables can be removed for various
reasons.

-   *title*: Redundant. We have already the translated title in the
    second column.  
-   *currency\_buyer*: Only one currency “EUR”, it doesn’t provide
    analysis insight.  
-   *merchant\_profile\_picture*: Contain too many missing values,
    complete rate was only 14%. This column is also redundant. There is
    a relevant column already that also indicating the same.  
-   *has\_urgency\_banner*: Too many missing values in this column,
    complete rate was only 30%.  
-   *urgency\_text*: Contain too many missing values, complete rate was
    only 30%.  
-   *merchant\_id*: Redundant and irrelevant for relationship
    analysis.  
-   *product\_url*: I do not need this column for this analysis.  
-   *product\_picture*: I do not need this column for this analysis.  
-   *product\_id*: I do not need this column for this analysis.  
-   *theme*: Only “summer” in the entire dataset, this column wouldn’t
    contribute much to the analysis of this project.

``` r
c <- cloth %>% 
  mutate(theme = as.factor(theme))

levels(c$theme)
```

    ## [1] "summer"

-   *crawl\_month*: Only shows “2020-08-01” in the entire dataset, this
    column wouldn’t contribute much to the analysis of this project.

``` r
c <- cloth %>% 
  mutate(crawl_month = ym(crawl_month))

summary(c$crawl_month)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2020-08-01" "2020-08-01" "2020-08-01" "2020-08-01" "2020-08-01" "2020-08-01"

## 4 Data Cleaning

### 4.1 Remove variables

This section removes variables that have been previously identified to
be redundant or irrelevant to this project.

-   *title*  
-   *currency\_buyer*  
-   *merchant\_profile\_picture*  
-   *has\_urgency\_banner*  
-   *urgency\_text*  
-   *merchant\_id*  
-   *product\_url*  
-   *product\_picture*  
-   *product\_id*  
-   *theme*  
-   *crawl\_month*

``` r
# Preserving the original data "cloth", and create a new variable "cloth2" 

cloth2 <- cloth %>% 
  dplyr::select(-title, -currency_buyer, -merchant_profile_picture, -has_urgency_banner, 
                -urgency_text, -merchant_id, -product_url, -product_picture, -product_id, 
                -theme, -crawl_month)
```

I will assess the remaining variables in the dataset in later stage and
would remove them if required.

### 4.2 Remove missing values

This section removes 116 rows of data, which is 7.37% of the overall
dataset. The dataset drops from 1573 rows of data to 1457.

``` r
cloth2 <- na.omit(cloth2)

(count(cloth) - count(cloth2))/count(cloth) * 100
```

    ##          n
    ## 1 7.374444

There are ways to manage missing values such as imputation using mean,
median, or using imputation models from the R’s caret package. However,
I removed the missing values instead of imputating them just to make
this project simpler. Additionally, only 7.37% of data is removed, and I
still have 92.63% (1457 rows) of data for this analysis.

### 4.3 Factor conversion

This section converts character variables and several numerical
variables into factor.

Converting data into factor helps (1) the overall R processing speed,
(2) initiate the role of these variables in data categorisation, (3)
regression analysis, (4) Enable some functions of R that require vectors
to be in factor format.

``` r
cloth2 <- cloth2 %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(uses_ad_boosts = as.factor(uses_ad_boosts),       # A binary data for yes or not 
         shipping_is_express = as.factor(shipping_is_express),  # A binary data for yes or not 
         merchant_has_profile_picture = as.factor(merchant_has_profile_picture)  # A binary data for yes or not 
         )
```

### 4.4 Typos in the factor variables

This section checks typos in the factor variables. I have identified
many typos in following variables.

**1. Cleaning typos in the “Product\_color”**

I will convert -

-   *army green* and *Army green* into *armygreen*  
-   *Black* into *black*  
-   *Blue* into *blue*  
-   *gray* into *grey*  
-   *light green* into *lightgreen*  
-   *navy blue* into *navyblue*  
-   *Pink* into *pink*  
-   *RED* into *red*  
-   *Rose red* into *rosered*  
-   *White* into *white*  
-   *wine red* into *winered*

``` r
summary(cloth2$product_color)
```

    ##          applegreen             apricot                army          army green 
    ##                   2                   2                   1                   1 
    ##          Army green           armygreen               beige               black 
    ##                   2                  30                  13                 285 
    ##               Black        black & blue       black & green      black & stripe 
    ##                   3                   2                   4                   1 
    ##       black & white      black & yellow                blue                Blue 
    ##                   3                   2                  96                   1 
    ##         blue & pink               brown      brown & yellow            burgundy 
    ##                   1                   7                   1                   2 
    ##               camel          camouflage              claret              coffee 
    ##                   2                   3                   1                   7 
    ##           coolblack            coralred            darkblue           denimblue 
    ##                   2                   2                   6                   1 
    ##           dustypink              floral    fluorescentgreen                gold 
    ##                   2                   5                   4                   1 
    ##                gray        gray & white               green                grey 
    ##                  10                   1                  86                  69 
    ##  greysnakeskinprint               ivory              jasper               khaki 
    ##                   1                   1                   1                  11 
    ##            lakeblue             leopard        leopardprint         light green 
    ##                   2                   4                   1                   1 
    ##           lightblue           lightgray          lightgreen           lightgrey 
    ##                  12                   1                   3                   1 
    ##          lightkhaki           lightpink         lightpurple            lightred 
    ##                   1                   3                   1                   2 
    ##         lightyellow           mintgreen          multicolor                navy 
    ##                   2                   2                  20                   2 
    ##           navy blue            navyblue    navyblue & white                nude 
    ##                   2                  28                   1                   1 
    ##            offblack            offwhite              orange          orange-red 
    ##                   1                   1                  25                   3 
    ## orange & camouflage                pink                Pink        pink & black 
    ##                   1                  99                   2                   2 
    ##         pink & blue         pink & grey        pink & white        prussianblue 
    ##                   2                   2                   2                   1 
    ##              purple             rainbow                 red                 RED 
    ##                  50                   1                  91                   1 
    ##          red & blue                rose            Rose red            rosegold 
    ##                   1                   5                   1                   1 
    ##             rosered              silver             skyblue                star 
    ##                   8                   1                   6                   1 
    ##              violet       watermelonred               white               White 
    ##                   1                   2                 233                   3 
    ##       white & black       white & green         white & red         whitefloral 
    ##                   2                  10                   1                   2 
    ##         whitestripe                wine            wine red             winered 
    ##                   1                   2                   1                  28 
    ##    winered & yellow              yellow 
    ##                   1                  97

``` r
# Rectification

cloth2 <- cloth2 %>% 
  mutate(product_color = as.character(product_color),
         product_color = case_when(product_color == "army green" ~ "armygreen",
                                   product_color == "Army green" ~ "armygreen",
                                   product_color == "Black" ~ "black",
                                   product_color == "Blue" ~ "blue",
                                   product_color == "gray" ~ "grey",
                                   product_color == "light green" ~ "lightgreen",
                                   product_color == "*navy blue" ~ "navyblue",
                                   product_color == "Pink" ~ "pink",
                                   product_color == "RED" ~ "red",
                                   product_color == "Rose red" ~ "rosered",
                                   product_color == "White" ~ "white",
                                   product_color == "wine red" ~ "winered",
                                   TRUE ~ product_color),
         product_color = as.factor(product_color))
```

**2. product\_variation\_size\_id**

The data in this variable is too messy. I will do some buik computation
to aid the cleaning a little, including trimming leading and trailing
white spaces, remove punctuation, and set all levels to upper case.

``` r
cloth2 <- cloth2 %>% 
  mutate(product_variation_size_id = str_to_upper(product_variation_size_id),
         product_variation_size_id = str_replace_all(product_variation_size_id, "[[:punct:]]", " "),
         product_variation_size_id = trimws(product_variation_size_id),
         product_variation_size_id = as.factor(product_variation_size_id))

summary(cloth2$product_variation_size_id) %>% kbl()
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
x
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
04 3XL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1 PC XL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
100 CM
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
100 X 100CM 39 3 X 39 3INCH
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
10PCS
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1M BY 3M
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
20PCS
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
20PCS 10PAIRS
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
25
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
25 S
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
26 WAIST 72CM 28INCH
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
29
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
2PCS
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
2XL
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
30 CM
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
32 L
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
33
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
34
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
35
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
36
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
3XL
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
4
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
4 5 YEARS
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
40 CM
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
4XL
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
5
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
5PAIRS
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
5XL
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
60
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
6XL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
80 X 200 CM
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
AU PLUG LOW QUALITY
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
BABY FLOAT BOAT
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
CHOOSE A SIZE
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
DAUGHTER 24M
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
EU 35
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
EU39 US8
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
FIRST GENERATION
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
FLOATING CHAIR FOR KID
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
L
</td>
<td style="text-align:right;">
49
</td>
</tr>
<tr>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
196
</td>
</tr>
<tr>
<td style="text-align:left;">
ONE SIZE
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
PACK OF 1
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
PANTS S
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
S
</td>
<td style="text-align:right;">
633
</td>
</tr>
<tr>
<td style="text-align:left;">
S WAIST58 62CM
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
S BUST 88CM
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
S DIAMETER 30CM
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
S M CHILD
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE S
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE XXS
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE 4XL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE 5XL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE M
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE S
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE XS
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE XXS
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZE4XL
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
SIZEL
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
SUIT S
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
US 6 5 EU 37
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
US S
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
US5 5 EU35
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
WOMEN SIZE 36
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
WOMEN SIZE 37
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
X L
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
XL
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
XS
</td>
<td style="text-align:right;">
333
</td>
</tr>
<tr>
<td style="text-align:left;">
XXL
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
XXS
</td>
<td style="text-align:right;">
92
</td>
</tr>
<tr>
<td style="text-align:left;">
XXXL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
XXXS
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
XXXXL
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
XXXXXL
</td>
<td style="text-align:right;">
2
</td>
</tr>
</tbody>
</table>

Grouping some of the identifiable size categories.

``` r
cloth2 <- cloth2 %>% 
  mutate(product_variation_size_id = as.character(product_variation_size_id),
         product_variation_size_id = case_when(product_variation_size_id == "2XL" ~ "XXL", 
                                               product_variation_size_id == "3XL" ~ "XXXL", 
                                               product_variation_size_id == "4XL" ~ "XXXXL", 
                                               product_variation_size_id == "5XL" ~ "XXXXXL", 
                                               product_variation_size_id == "SIZE S" ~ "S", 
                                               product_variation_size_id == "SIZE XXS" ~ "XXS", 
                                               product_variation_size_id == "SIZE 4XL" ~ "XXXL", 
                                               product_variation_size_id == "SIZE 5XL" ~ "XXXXXL", 
                                               product_variation_size_id == "SIZE M" ~ "M", 
                                               product_variation_size_id == "SIZE S" ~ "S", 
                                               product_variation_size_id == "SIZE XS" ~ "XS", 
                                               product_variation_size_id == "SIZE XXS" ~ "XXS", 
                                               product_variation_size_id == "SIZE4XL" ~ "XXXXL", 
                                               product_variation_size_id == "SIZEL" ~ "L",
                                               product_variation_size_id == "SIZEL" ~ "L",
                                               TRUE ~ product_variation_size_id),
         product_variation_size_id = as.factor(product_variation_size_id))
```

**3. origin\_country**

Nothing to rectify in this column.

``` r
summary(cloth2$origin_country)
```

    ##   CN   GB   SG   US   VE 
    ## 1419    1    2   30    5

### 4.5 Examining the Rating

There are 5 columns for different counts of rating from rating 1 to
rating 5. There is also a “rating\_count” representing the total number
of rates received. In the aim of analysis of this project, I do not need
these columns because I only need the “rating” column which indicates
the overall rating.

``` r
rate <- cloth2 %>% dplyr::select(rating, rating_count, rating_five_count, rating_four_count, rating_three_count,
                                 rating_two_count, rating_one_count)

rate
```

    ## # A tibble: 1,457 x 7
    ##    rating rating_count rating_five_count rating_four_count rating_three_count
    ##     <dbl>        <dbl>             <dbl>             <dbl>              <dbl>
    ##  1   3.76           54                26                 8                 10
    ##  2   3.45         6135              2269              1027               1118
    ##  3   3.57           14                 5                 4                  2
    ##  4   4.03          579               295               119                 87
    ##  5   3.1            20                 6                 4                  2
    ##  6   5               1                 1                 0                  0
    ##  7   3.84         6742              3172              1352                971
    ##  8   3.76          286               120                56                 61
    ##  9   3.47           15                 6                 2                  3
    ## 10   3.6           687               287               128                 92
    ## # ... with 1,447 more rows, and 2 more variables: rating_two_count <dbl>,
    ## #   rating_one_count <dbl>

Therefore, I will remove these columns.

``` r
cloth2 <- cloth2 %>% dplyr::select(-rating_count, -rating_five_count, -rating_four_count, 
                         -rating_three_count, -rating_two_count, -rating_one_count)
```

### 4.6 New Metric: price\_drop

The “price” in the dataset is the price that an item will be sold at,
whereas “retail\_price” is a reference price or regular price and is
generally higher than the “price” column. Both will be shown on the
product listing page for marketing purposes.

It will be interesting to see how is a product sold based on price
dropped. This drop of prices will be calculated here and visualized in
the next stage.

``` r
cloth2 <- cloth2 %>% 
  mutate(price_drop = retail_price - price) %>% 
  relocate(price_drop, .after = retail_price)
```

Basic statistics of this “price\_drop” column are:

``` r
summary(cloth2$price_drop)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -7.00   -1.00    0.17   15.06   18.00  244.00

### 4.6 New Metric: discount\_per

Based on the newly created “price\_drop”, a discount percentage
“discount\_per” is synthesised to help to study the effect of price
dropped on sales.

``` r
cloth2 <- cloth2 %>% 
  mutate(discount_per = round(price_drop/retail_price*100), 2) %>% 
  relocate(discount_per, .after = price_drop)
```

Basic statistics of this “discount\_per” column are:

``` r
summary(cloth2$discount_per)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -18.00  -12.00    4.00   25.56   72.00   97.00

Why don’t we use price\_drop instead? Because produces are sold at
different prices, the sample sizes of expensive products are different
than the products at cheaper prices. Creating a discount percentage
columne (discount\_per) will aid the scale down the value to make our
observation easier (Hopefully).

### 4.7 New Metric: price\_class

It can be useful to create classes for different prices. Based on the
dataset, the price ranges between 0 to 50.

``` r
summary(cloth2$price)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   5.860   8.000   8.367  11.000  49.000

The result of the classes.

``` r
cloth2 <- cloth2 %>% 
  mutate(price_class = case_when(price < 10 ~ "EUR<10",
                                 price > 10 & price < 20 ~ "EUR10-20",
                                 price > 20 & price < 30 ~ "EUR20-30",
                                 price > 30 & price < 40 ~ "EUR30-40",
                                 TRUE ~ "EUR40-50"),
         price_class = as.factor(price_class)) %>% 
  relocate(price_class, .after = price)

levels(cloth2$price_class)
```

    ## [1] "EUR<10"   "EUR10-20" "EUR20-30" "EUR40-50"

## 5 Visualisation

This section will analyse the 5 main tasks listed in the introduction.

### 5.1 Validated! Human sensitive to price drops

This section answers the first task of this project - **How about trying
to validate the established idea of human sensitiveness to price drops
?**

It will be in relevant to the sales of a product in relation to the
magnitude of it’s price drops. I will use discount in percentage (price
drop/retail price \* 100) to represent price drops for each of these
1457 items in the dataset.

Following is the first graph, it appears that there is no obvious
relation between discounts and unit sold.

``` r
p1 <- ggplot(cloth2, aes(x = discount_per, y = units_sold)) +
  geom_jitter(size = 4, alpha = 0.2, colour = "green") +
  labs(x = "Discount (%)",
       y = "Unit Sold (Quantity)",
       title = "Unit Sold versus Discounts ($)") + 
  theme_modern_rc() +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  scale_y_continuous(labels = function(x)paste0((x/1000), "k"))


p1
```

![](summer_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

However, following bar chart shows that in term of the total number of
products sold from different discount classes, it is definitely that a
discount rate between 75% to near 100% will outcompete other discount
classes.

``` r
df1 <- cloth2 %>% 
  dplyr::select(discount_per, units_sold) %>% 
  mutate(class = case_when(discount_per < 0 ~ "0%",
                           discount_per > 0 & discount_per < 25 ~ "0-25%",
                           discount_per > 25 & discount_per < 50 ~ "25-50%",
                           discount_per > 50 & discount_per < 75 ~ "50-75%",
                           TRUE ~ "75-100%"),
         class = factor(class, levels = c("0%", "0-25%", "25-50%", "50-75%", "75-100%"))) %>% 
  group_by(class) %>% 
  summarise(total = sum(units_sold)) 



p2 <- ggplot(df1, aes(x = class, y = total, fill = class)) +
  geom_bar(stat = "identity", colour = "black") +
  geom_label(aes(label = prettyNum(total, big.mark = ",")), vjust = -1, fill = "grey") +
  labs(x = "Discounts",
       y = "Total Sold (Count)",
       title = "More Items Sold at Higher Discount Rate (%)") +
  scale_y_continuous(labels = function(x)paste0((x/1000000), " Mil"),
                     lim = c(0, 3000000)) +
  theme_modern_rc() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))

p2
```

![](summer_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Please be aware. This result shows a tend but is not a well-designed
proper experiment. It is a trend based on collected observation.
However, it do show a trend that the higher the discount rate, the more
the total number of items sold. It may help to conclude that human is
sensitive towards price.

To explain why discount rate at “0%” has the highest items sold, it is
because that human sensitivity to price drops is a complex mechanism.
For examples, there are much more items that are sold at 0% have their
prices already much cheaper than the discounted items, regardless of
price-drop magnitude. Though these items are having far cheap prices but
also having a value that is enough to build trust from consumer and made
their purchases succeed.

### 5.2 Typical top product categories

Second analysis task of this project: **Look for top categories of
products so that you know what sells best.**

Identify that following variables can help to answer this task.

-   product\_color  
-   product\_variation\_size\_id

Setting up data frame with relevant variables.

``` r
df2 <- cloth2 %>% dplyr::select(units_sold, product_color, product_variation_size_id)
```

**1. product\_color**

There are 87 colour categories and a few colours dominating the majority
of the sales. It is quite *pareto*. The top 5 colours are black, white,
grey, purple, and blue.

``` r
# df

color_df <- df2 %>% 
  group_by(product_color) %>% 
  summarise(total = sum(units_sold))

# plot

p3 <- ggplot(color_df, aes(y = fct_reorder(product_color, total), x = total, group = 1)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  theme_modern_rc() +
  labs(x = "Total Sold per Item Category",
       y = "Colour Category",
       title = "Top 5 Best-Selling Colours are Black, White, Grey, Purple, and Blue") +
  theme(plot.title = element_text(size = 17)) +
  scale_x_continuous(labels = function(x)(prettyNum(x, big.mark = ",")))


p3 
```

![](summer_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

**2. product\_variation\_size\_id**

Another *pareto* trend. There are 63 different level of size “classes”
in the dataset. The top 5 best-selling sizes are S, M, XS, L and XXS.

``` r
# df

size_df <- df2 %>% 
  group_by(product_variation_size_id) %>% 
  summarise(total = sum(units_sold))

# plot

p4 <- ggplot(size_df, aes(y = fct_reorder(product_variation_size_id, total), x = total, group = 1)) +
  geom_point(size = 3, color = "yellow") +
  geom_line(size = 1, color = "yellow") +
  theme_modern_rc() +
  labs(x = "Total Sold per Item Category",
       y = "Size Category",
       title = "Top 5 Best-Selling Sizes are S, M, XS, L and XXS") +
  theme(plot.title = element_text(size = 17)) +
  scale_x_continuous(labels = function(x)(prettyNum(x, big.mark = ",")))

p4
```

![](summer_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

### 5.3 The Effect of Rating on Sales

The third analysis task: **Do bad products sell ? How about the
relationship between the quality of a product (ratings) and its success
? Does the price factor into this ?**

Yes, the better the rating, the better the sales of the product. From
following plot, I can see that as long as the product has a rating of
above 3, it will sell.

The price does not have obvious relationship with the rating and sales.

``` r
p5 <- ggplot(cloth2, aes(x = rating, y = units_sold, colour = price_class)) +
  geom_point(size = 3, alpha = 0.4) +
  theme_modern_rc() +
  labs(x = "Product Rating (1 - 5)",
       y = "Units Sold (Count)",
       title = "Best Performing Rating Falls Between 3 - 5") +
  theme(plot.title = element_text(vjust = 2)) +
  facet_wrap(~price_class) +
  scale_y_continuous(labels = function(x)(prettyNum(x, big.mark = ",")))


p5
```

![](summer_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### 5.4 Logarithmically graphing the Fame

The fourth analysis task of this project asked: **Do seller’s fame
factor into top products?**

There are two options here, whether I should use
“merchant\_rating\_count” or “merchant\_rating” for this analysis. The
“merchant\_rating\_count” indicates total number of rating, which would
indirectly tell how popular a seller is. On the other hand,
“merchant\_rating” will only give the overall rating of a seller, and it
won’t tell how many buyers are voting for the seller.

Therefore, I will use “merchant\_rating\_count” to be a better
indication of “fame factor” specified by the task.

``` r
library(patchwork)


df5 <- cloth2 %>% dplyr::select(units_sold, product_color, product_variation_size_id, 
                                  merchant_rating_count, merchant_rating_count, merchant_rating)


p1 <- ggplot(df5, aes(x = log(merchant_rating_count), y = units_sold)) +
  geom_point(colour = "orange", shape = 21, size = 4) +
  geom_smooth(colour = "white") +
  theme_modern_rc() +
  labs(x = "log(Merchant Rating Count)",
       y = "Unit Sold (Count)",
       title = "logarithmic") 
  

df5 <- cloth2 %>% dplyr::select(units_sold, product_color, product_variation_size_id, 
                                  merchant_rating_count, merchant_rating_count, merchant_rating)


p2 <- ggplot(df5, aes(x = merchant_rating_count, y = units_sold)) +
  geom_point(colour = "pink", shape = 21, size = 4) +
  geom_smooth(colour = "white") +
  theme_modern_rc() +
  labs(x = "Merchant Rating Count",
       y = "Unit Sold (Count)",
       title = "Arithmetic") +
  scale_x_continuous(labels = function(x)(prettyNum(x, big.mark = ",")))

mypatch <- p2 + p1 & theme_modern_rc() 

mypatch + 
  plot_annotation(title = "Gentle Relationship between Fame and Sales") +
  theme(text = element_text(size = 40))
```

![](summer_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

*Insights*

-   The fame of a merchant is important, thought there is a outlier
    showing a famous merchant has a drop in unit sold, but more data
    points are needed to prove this theory.

-   Sales will increase with the popularity of a merchant.

-   However, the relationship is not absolute proved by the evidence of
    arithmetic graph.

### 5.5 The Number of Tags in produce success

The fifth question: *Do the number of tags (making a product more
discoverable) factor into the success of a product ?*

``` r
# df 

df5 <- cloth2 %>% 
  dplyr::select(merchant_name, price_class, price, units_sold, tags) %>% 
  mutate(tags = as.character(tags))

seller_tags <- df5 %>% 
  unnest_tokens(input = tags, output = word) %>%        # Tokenise tags 
  group_by(merchant_name) %>% 
  summarise(tags_count = n()) %>% 
  arrange(desc(tags_count))

# join tables

df5.2 <- df5 %>% 
  left_join(seller_tags, by = "merchant_name")

# plot

ggplot(df5.2, aes(x = tags_count, y = units_sold)) +
  geom_hex(bins = 40, colour = "grey") +
  theme_modern_rc() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 14, vjust = 2)) +
  labs(title = "Tags are Required and Impact on Sales",
       x = "Number of Tags",
       y = "Units Sold") +
  scale_y_continuous(labels = function(x)(prettyNum(x, big.mark = ","))) 
```

![](summer_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

### 5.6 EXTRA: Which text has the most sales?

In term of total number of products sold, top 50 words out of 1,838 that
used as tags are:

``` r
# set up df

df5.6 <- df5 %>% 
  unnest_tokens(input = tags, output = word) %>% 
  anti_join(stop_words, by = "word") %>%        # remove meaningless words such as "a", "and", "the"
  filter(str_detect(word, "[:alpha:]")) %>%     # only include alphabetical word
  distinct()                                    # to remove duplicate word by a merchant 
 
# slice the top 50 best words for units_sold

df5.6.2 <- df5.6 %>% 
  group_by(word) %>% 
  summarise(total_sold = sum(units_sold)) %>% 
  arrange(desc(total_sold)) %>% 
  mutate(id = paste0("id", row_number())) %>% 
  slice(c(1:50))



# plot

ggplot(df5.6.2, aes(y = fct_reorder(word, total_sold), x = total_sold, group = 1)) +
  geom_point(size = 4, color = "green") +
  geom_line(size = 1, color = "green") +
  theme_modern_rc() +
  labs(x = "Total Sold ",
       y = "Texts",
       title = "The Top 50 Best Performing Texts that Used As Tags") +
  theme(plot.title = element_text(size = 17)) +
  scale_x_continuous(labels = function(x)(prettyNum(x, big.mark = ",")))
```

![](summer_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

### 5.7 EXTRA: Text Mining

Following table shows the frequency of words that has been used for at
least 100 times in the entire dataset.

For example, in the entire 1457 rows of dataset, the word “fashion” has
been used 1174 times for different product, 1074 times for “summer”, and
1043 times of “women’s”.

``` r
top_words <- df5.6 %>% 
  count(word, name = "users_count") %>% 
  arrange(desc(users_count)) %>% 
  filter(users_count > 100)      

top_words  
```

    ## # A tibble: 39 x 2
    ##    word       users_count
    ##    <chr>            <int>
    ##  1 fashion           1174
    ##  2 summer            1074
    ##  3 women's           1043
    ##  4 women              963
    ##  5 casual             792
    ##  6 size               530
    ##  7 sleeveless         491
    ##  8 dress              448
    ##  9 tops               442
    ## 10 shorts             437
    ## # ... with 29 more rows

Applying pairwise correlation between words across all products. This
correlation to find how often words are found together in the dataset.
The default algorithm is pearson correlation. Following shows the first
6 rows out of the 228 rows from the data frame.

``` r
word_cor <- df5.6 %>% 
  semi_join(top_words, by = "word") %>%     # df5.6 has all words, semi join to get top 100 words
  pairwise_cor(item = word, feature = merchant_name) %>% 
  filter(correlation >= 0.2)       # Don't include correlation that is too small.

head(word_cor)
```

    ## # A tibble: 6 x 3
    ##   item1   item2   correlation
    ##   <chr>   <chr>         <dbl>
    ## 1 casual  summer        0.223
    ## 2 women's fashion       0.498
    ## 3 casual  fashion       0.200
    ## 4 women   fashion       0.316
    ## 5 fashion women's       0.498
    ## 6 casual  women's       0.229

I have created 2 tables, one is the “frequency table” and the other one
is the “correlation data frame”. The relationships between these two
data frames are that,

1.  The “correlation table” finds the correlation between the words that
    found in the “frequency table” only.

2.  However, not all the words found the “frequency table” will be
    correlated to each other in the “correlation table”, while majority
    are.

This can be visualised from following “word network plot”.

``` r
set.seed(123)

graph_from_data_frame(d = word_cor,                # the correlation between word
                      vertices = top_words) %>%    
  ggraph(layout=  "fr") +                          # specify an algorithm to spread out the words
  geom_edge_link(colour = "white") +
  geom_node_point() +
  geom_node_text(aes(label = name), colour = "lightblue", repel = T) +
  theme_modern_rc() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))
```

![](summer_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

-   Three words “womens”, “printed”, and “lace” tend to often appear by
    themselves. These words only appear in the “frequency table” but not
    in the “correlation table”.

-   Words that linked by the white line in the cluster means that they
    appear frequently together in tags. For examples:

    -   The combination “shorts” and “pants” appear together very
        frequently.  
    -   The combination “beach” and “party” appear together very
        frequently.  
    -   The combination “summer” and “laddis” appear together very
        frequently.

*Clean up the plot*

Remove words are not connected to other words, which are the “lace”,
“women” and “printed”.

``` r
set.seed(123)

graph_from_data_frame(d = word_cor,    
                      vertices = top_words %>%   
                        semi_join(word_cor, by = c("word" = "item1"))) %>%     # remove words that are not connected in cor df.
  ggraph(layout=  "fr") +   
  geom_edge_link(aes(alpha = correlation), colour = "white") +         # The higher the correlation, the lighter the line will be
  geom_node_point() +
  geom_node_text(aes(label = name, color = users_count, size = users_count), repel = TRUE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "grey20"))
```

![](summer_files/figure-gfm/unnamed-chunk-37-1.png)<!-- --> The basic of
the plot is still the same, there was just a minor change of its
positioning on the graph panel. I added two more items into the graph to
help visualisation.

1.  The larger and brighter the words, the more merchant have used the
    words.

2.  The brighter the line, the higher the correlation between the two
    associated words, or the more often the words appear together.

Functionisation of workflow above to allow customisable adjustments for
different min\_user\_count and min\_correlation.

``` r
set.seed(123)

my_word_graph <- function(dataframe,
                          min_users_count = 100,
                          min_correlation = 0.2){
  
  # frequency data frame
    top_words <- dataframe %>% 
    count(word, name = "users_count") %>% 
    arrange(desc(users_count)) %>% 
    filter(users_count > min_users_count)
  
  # correlation data frame
  word_cor <- dataframe %>% 
    semi_join(top_words, by = "word") %>% 
    pairwise_cor(item = word, feature = merchant_name) %>% 
    filter(correlation >= min_correlation)  
  
  # plot
    graph_from_data_frame(d = word_cor,    
                        vertices = top_words %>%  
                          semi_join(word_cor, by = c("word" = "item1"))) %>% 
      ggraph(layout=  "fr") +   
      geom_edge_link(aes(alpha = correlation), colour = "white") +         
      geom_node_point() +
      geom_node_text(aes(label = name, color = users_count, size = users_count), repel = TRUE) +
      theme_void() +
      theme(legend.position = "left",
            panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.background = element_rect(fill = "grey20"))
  
}

# Set correlation at 0.4
  
my_word_graph(dataframe = df5.6, 
              min_users_count = 100, 
              min_correlation = 0.4) 
```

![](summer_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

When I set the minimum correlation to 0.4, more words are removed and
left with 6 clusters. I can see that most merchant use the words
“fashion”, “women’s”, and “women” together.

**Text Plots for Sales**

Plot word graph. Following graph shows that fashion, womens, and summers
are the best words that can drive sales and related adjacent words that
can be used together when creating a tag.

``` r
set.seed(123)

word_sales <- df5.6 %>% 
  group_by(word) %>% 
  summarise(total_sold = sum(units_sold)) %>% 
  arrange(desc(total_sold)) %>% 
  filter(total_sold > 50000)

word_cor <- df5.6 %>% 
  semi_join(word_sales, by = "word") %>%     
  pairwise_cor(item = word, feature = merchant_name) %>% 
  filter(correlation >= 0.2)


graph_from_data_frame(d = word_cor,    
                      vertices = word_sales %>%   
                        semi_join(word_cor, by = c("word" = "item1"))) %>%     
  ggraph(layout=  "fr") +   
  geom_edge_link(aes(alpha = correlation), colour = "white") +         
  geom_node_point() +
  geom_node_text(aes(label = name, color = total_sold, size = total_sold), repel = TRUE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.background = element_rect(fill = "grey20"))
```

    ## Warning: ggrepel: 10 unlabeled data points (too many overlaps). Consider
    ## increasing max.overlaps

![](summer_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

### 5.8 EXTRA: Badges and Product Success

There are 3 interesting badges in the dataset that could be useful in
explanining the popularity of a product.

-   Badge\_fast\_shipping: Badge awarded when this product’s order is
    consistently shipped rapidly. 1 means Yes.

-   Badge\_local\_product: A badge that denotes the product is a local
    product. 1 means Yes.

-   Badge\_product\_quality: Badge awarded when many buyers consistently
    gave good evaluations. 1 means Yes.

``` r
# df

df5.7 <- cloth2 %>% 
  dplyr::select(units_sold, badge_local_product, badge_product_quality, badge_fast_shipping) %>% 
  mutate(badge_local_product = as.factor(badge_local_product),
         badge_product_quality = as.factor(badge_product_quality),
         badge_fast_shipping = as.factor(badge_fast_shipping)) %>% 
  pivot_longer(c(2:4), names_to = "badge_type", values_to = "result")


# plot

ggplot(df5.7, aes(x = badge_type, y = units_sold, fill = result)) +
  geom_boxplot(alpha = 0.5, aes(colour = result)) +
  stat_summary(aes(fun = "mean"), size = 5, geom = "point", shape = 4, 
               colour = "white", position = position_dodge(0.75)) +
  theme_modern_rc() +
  theme(plot.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  labs(title = "Impact of Various Badges on Units Sold",
       y = "Units Sold (Count)",
       x = "Badge Type") +
  scale_y_continuous(labels = function(x)(prettyNum(x, big.mark = ","))) 
```

    ## No summary function supplied, defaulting to `mean_se()`

![](summer_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

-   Product with Badge of fast shipping has higher median and mean of
    the number of unit soil per product.

-   There is no much different between whether has a product with a
    local-produce badge.

-   Good quality produce has higher units sold.

### 5.9 EXTRA: Advertisement Boost and Product Success

Following plot shows that advertisement boost does not affect the
success of a product.

-   The median between two boxplots (with and without advertisement
    boost) are nearly similar.  
-   The mean between two boxplots (with and without advertisement boost)
    are nearly similar.  
-   The distributions of points in two boxplots are nearly similar.

``` r
# df

df5.8 <- cloth2 %>% 
  dplyr::select(units_sold, uses_ad_boosts) 

# plot

ggplot(df5.8, aes(x = uses_ad_boosts, y = units_sold, colour = uses_ad_boosts)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  geom_boxplot(outlier.shape = NA, alpha = 0, width = 0.8, colour = "grey") +
  stat_summary(fun = "mean", size = 6, geom = "point", shape = 4, colour = "white") +
  theme_modern_rc() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  labs(title = "There is no Significant Different Between Sales and Ad.Boosts",
       y = "Units Sold (Count)") +
  scale_y_continuous(labels = function(x)(prettyNum(x, big.mark = ",")))
```

![](summer_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->
Surprisingly, more units were sold without having the advertisement
boost function from *Wish*.

``` r
# df

df5.8.2 <- cloth2 %>% 
  dplyr::select(units_sold, uses_ad_boosts) %>% 
  group_by(uses_ad_boosts) %>% 
  summarise(total_unit_sold = sum(units_sold)) %>% 
  ungroup() %>% 
  mutate(total = sum(total_unit_sold),
         percentage = sum(total_unit_sold/total * 100))

df5.8.2
```

    ## # A tibble: 2 x 4
    ##   uses_ad_boosts total_unit_sold   total percentage
    ##   <fct>                    <dbl>   <dbl>      <dbl>
    ## 1 0                      3692909 6322239        100
    ## 2 1                      2629330 6322239        100

``` r
# plot

ggplot(df5.8.2, aes(x = uses_ad_boosts, y = total_unit_sold, colour = uses_ad_boosts, fill = uses_ad_boosts)) +
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_text(aes(label = prettyNum(total_unit_sold, big.mark = ",")), vjust = -1) +
  theme_modern_rc() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0))) +
  labs(title = "More Units Sold without Add Boosts",
       y = "Total Sold (Count)") +
  scale_y_continuous(labels = function(x)(prettyNum(x, big.mark = ",")), 
                     lim = c(0, 4000000))
```

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, : font
    ## family not found in Windows font database

![](summer_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

## 6 Staitistcal Analysis

This section will start to evaluate the statistical relationship between
*how well a product is sold* with all other relevant variables. This
information is stored within the feature “units\_sold”.

### 6.1 Feature Selection

**Primary Addition**

The number of tags in relation to the number of each product sold will
be very interesting to include. This value can be attracted from the
“tags” column that has a massive amount of text describing relevant
tags. This extraction *has been done* in section 5.5. Here, I will just
add the extracted column into the current dataset.

``` r
cloth2 <- cloth2 %>% 
  left_join(seller_tags, by = "merchant_name")
```

**Primary Removal**

Removing variables that are irrelevant to this analysis based on my
domain knowledge. I will based on the characteristics of features, for
examples (1) if there are too many missing values in that column (I have
done it during data cleaning), (2) The data in the column do not help in
categorising the data, for example if a variable is having non-repeated
unique values in the entire column like a “ID” or “Name”, (3) The
columns that are redundant or basically irrelevant to the predictive
objective.

Variables I am removing include:

-   title\_orig  
-   price\_drop
-   discount\_per
-   tags  
-   merchant\_title  
-   merchant\_name  
-   merchant\_info\_subtitle  
-   the accidentally induced column - “2”

``` r
cloth3 <- cloth2 %>% dplyr::select(-title_orig, -price_drop, -discount_per, -tags, -merchant_title, -merchant_name, -merchant_info_subtitle, -'2')
```

### 6.3 Feature cleaning

There are too many levels for important factor variables such as
“shipping\_option\_name”, “product\_variation\_size\_id”, and “colour”
and which may make the statistical analysis too long and complex to
interpret. Therefore, I will try to group the level with less data into
a variable named “other” to help analysis.

**1. shipping\_option\_name**

Examining the distribution of sample sizes:

``` r
cloth3 %>% 
  dplyr::select(shipping_option_name) %>% 
  group_by(shipping_option_name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         proportion = count/total) %>% 
  kable(align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
shipping\_option\_name
</th>
<th style="text-align:center;">
count
</th>
<th style="text-align:center;">
total
</th>
<th style="text-align:center;">
proportion
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Livraison standard
</td>
<td style="text-align:center;">
1397
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.9588195
</td>
</tr>
<tr>
<td style="text-align:center;">
Standard Shipping
</td>
<td style="text-align:center;">
20
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0137268
</td>
</tr>
<tr>
<td style="text-align:center;">
Envio Padrão
</td>
<td style="text-align:center;">
7
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0048044
</td>
</tr>
<tr>
<td style="text-align:center;">
Expediere Standard
</td>
<td style="text-align:center;">
6
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0041181
</td>
</tr>
<tr>
<td style="text-align:center;">
Envío normal
</td>
<td style="text-align:center;">
5
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0034317
</td>
</tr>
<tr>
<td style="text-align:center;">
&lt;U+0627&gt;&lt;U+0644&gt;&lt;U+0634&gt;&lt;U+062D&gt;&lt;U+0646&gt;
&lt;U+0627&gt;&lt;U+0644&gt;&lt;U+0642&gt;&lt;U+064A&gt;&lt;U+0627&gt;&lt;U+0633&gt;&lt;U+064A&gt;
</td>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0027454
</td>
</tr>
<tr>
<td style="text-align:center;">
&lt;U+0421&gt;&lt;U+0442&gt;&lt;U+0430&gt;&lt;U+043D&gt;&lt;U+0434&gt;&lt;U+0430&gt;&lt;U+0440&gt;&lt;U+0442&gt;&lt;U+043D&gt;&lt;U+0430&gt;&lt;U+044F&gt;
&lt;U+0434&gt;&lt;U+043E&gt;&lt;U+0441&gt;&lt;U+0442&gt;&lt;U+0430&gt;&lt;U+0432&gt;&lt;U+043A&gt;&lt;U+0430&gt;
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0020590
</td>
</tr>
<tr>
<td style="text-align:center;">
Standardowa wysylka
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0020590
</td>
</tr>
<tr>
<td style="text-align:center;">
Standardversand
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0020590
</td>
</tr>
<tr>
<td style="text-align:center;">
&lt;U+0E01&gt;&lt;U+0E32&gt;&lt;U+0E23&gt;&lt;U+0E2A&gt;&lt;U+0E48&gt;&lt;U+0E07&gt;&lt;U+0E2A&gt;&lt;U+0E34&gt;&lt;U+0E19&gt;&lt;U+0E04&gt;&lt;U+0E49&gt;&lt;U+0E32&gt;&lt;U+0E21&gt;&lt;U+0E32&gt;&lt;U+0E15&gt;&lt;U+0E23&gt;&lt;U+0E10&gt;&lt;U+0E32&gt;&lt;U+0E19&gt;
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0013727
</td>
</tr>
<tr>
<td style="text-align:center;">
Livraison Express
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0013727
</td>
</tr>
<tr>
<td style="text-align:center;">
Spedizione standard
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0013727
</td>
</tr>
<tr>
<td style="text-align:center;">
Standart Gönderi
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0013727
</td>
</tr>
<tr>
<td style="text-align:center;">
&lt;U+1780&gt;&lt;U+17B6&gt;&lt;U+179A&gt;&lt;U+178A&gt;&lt;U+17B9&gt;&lt;U+1780&gt;&lt;U+1787&gt;&lt;U+1789&gt;&lt;U+17D2&gt;&lt;U+1787&gt;&lt;U+17BC&gt;&lt;U+1793&gt;&lt;U+178F&gt;&lt;U+17B6&gt;&lt;U+1798&gt;&lt;U+179F&gt;&lt;U+17D2&gt;&lt;U+178F&gt;&lt;U+1784&gt;&lt;U+17CB&gt;&lt;U+178A&gt;&lt;U+17B6&gt;&lt;U+179A&gt;
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0006863
</td>
</tr>
</tbody>
</table>

Keep only Livraison standard and group all other shipping option in
“other\_shipping”. I will also remove the original
shipping\_option\_name column.

``` r
# The codes

cloth3 <- cloth3 %>% 
  mutate(shipping_option_name = as.character(shipping_option_name),
         shipping_name = case_when(shipping_option_name != "Livraison standard" ~ "Other_shipping",
                                           TRUE ~ shipping_option_name),
         shipping_name = as.factor(shipping_name)) %>% 
  dplyr::select(-shipping_option_name)

# The result

cloth3 %>% 
  dplyr::select(shipping_name) %>% 
  group_by(shipping_name) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         proportion = count/total) %>% 
  kable(align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
shipping\_name
</th>
<th style="text-align:center;">
count
</th>
<th style="text-align:center;">
total
</th>
<th style="text-align:center;">
proportion
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Livraison standard
</td>
<td style="text-align:center;">
1397
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.9588195
</td>
</tr>
<tr>
<td style="text-align:center;">
Other\_shipping
</td>
<td style="text-align:center;">
60
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0411805
</td>
</tr>
</tbody>
</table>

**2. product\_variation\_size\_id**

Examine the sample size of each level:

``` r
cloth3 %>% 
  dplyr::select(product_variation_size_id ) %>% 
  group_by(product_variation_size_id ) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         proportion_percentage = count/total * 100) %>% 
  kable(align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
product\_variation\_size\_id
</th>
<th style="text-align:center;">
count
</th>
<th style="text-align:center;">
total
</th>
<th style="text-align:center;">
proportion\_percentage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
S
</td>
<td style="text-align:center;">
647
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
44.4063143
</td>
</tr>
<tr>
<td style="text-align:center;">
XS
</td>
<td style="text-align:center;">
340
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
23.3356211
</td>
</tr>
<tr>
<td style="text-align:center;">
M
</td>
<td style="text-align:center;">
197
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
13.5209334
</td>
</tr>
<tr>
<td style="text-align:center;">
XXS
</td>
<td style="text-align:center;">
97
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.6575154
</td>
</tr>
<tr>
<td style="text-align:center;">
L
</td>
<td style="text-align:center;">
51
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
3.5003432
</td>
</tr>
<tr>
<td style="text-align:center;">
XXL
</td>
<td style="text-align:center;">
19
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.3040494
</td>
</tr>
<tr>
<td style="text-align:center;">
XL
</td>
<td style="text-align:center;">
17
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.1667811
</td>
</tr>
<tr>
<td style="text-align:center;">
XXXXL
</td>
<td style="text-align:center;">
8
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.5490734
</td>
</tr>
<tr>
<td style="text-align:center;">
XXXS
</td>
<td style="text-align:center;">
6
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.4118051
</td>
</tr>
<tr>
<td style="text-align:center;">
XXXXXL
</td>
<td style="text-align:center;">
5
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.3431709
</td>
</tr>
<tr>
<td style="text-align:center;">
XXXL
</td>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2745367
</td>
</tr>
<tr>
<td style="text-align:center;">
2PCS
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
33
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
34
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
25
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
29
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
35
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
EU 35
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
ONE SIZE
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
S BUST 88CM
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
SIZE XXS
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
SUIT S
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
04 3XL
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
1 PC XL
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
100 CM
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
100 X 100CM 39 3 X 39 3INCH
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
10PCS
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
1M BY 3M
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
20PCS
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
20PCS 10PAIRS
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
25 S
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
26 WAIST 72CM 28INCH
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
30 CM
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
32 L
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
36
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
4 5 YEARS
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
40 CM
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
5
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
5PAIRS
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
60
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
6XL
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
80 X 200 CM
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
AU PLUG LOW QUALITY
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
BABY FLOAT BOAT
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
CHOOSE A SIZE
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
DAUGHTER 24M
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
EU39 US8
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
FIRST GENERATION
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
FLOATING CHAIR FOR KID
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
PACK OF 1
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
PANTS S
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
S WAIST58 62CM
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
S DIAMETER 30CM
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
S M CHILD
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
SIZE S
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
US 6 5 EU 37
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
US S
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
US5 5 EU35
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
WOMEN SIZE 36
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
WOMEN SIZE 37
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
X L
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
</tbody>
</table>

-   I will keep only S, XS, M, XXS, L, XXL, and XL, they contribute
    roughly 90% of to the dataset, and grouping the rest of the levels
    into “other size”

``` r
# The codes

cloth3 <- cloth3 %>% 
  mutate(product_variation_size_id = as.character(product_variation_size_id),
         product_sizes = case_when(product_variation_size_id == "S" ~ "S",
                                   product_variation_size_id == "XS" ~ "XS",
                                   product_variation_size_id == "M" ~ "M",
                                   product_variation_size_id == "XXS" ~ "XXS",
                                   product_variation_size_id == "L" ~ "L",
                                   product_variation_size_id == "XXL" ~ "XXL",
                                   product_variation_size_id == "X" ~ "X",
                                           TRUE ~ "Other_sizes"),
         product_sizes = as.factor(product_sizes)) %>% 
  dplyr::select(-product_variation_size_id)

# Checking

cloth3 %>% 
  dplyr::select(product_sizes) %>% 
  group_by(product_sizes) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         proportion_percentage = count/total * 100) %>% 
  kable(align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
product\_sizes
</th>
<th style="text-align:center;">
count
</th>
<th style="text-align:center;">
total
</th>
<th style="text-align:center;">
proportion\_percentage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
S
</td>
<td style="text-align:center;">
647
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
44.406314
</td>
</tr>
<tr>
<td style="text-align:center;">
XS
</td>
<td style="text-align:center;">
340
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
23.335621
</td>
</tr>
<tr>
<td style="text-align:center;">
M
</td>
<td style="text-align:center;">
197
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
13.520933
</td>
</tr>
<tr>
<td style="text-align:center;">
Other\_sizes
</td>
<td style="text-align:center;">
106
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
7.275223
</td>
</tr>
<tr>
<td style="text-align:center;">
XXS
</td>
<td style="text-align:center;">
97
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.657515
</td>
</tr>
<tr>
<td style="text-align:center;">
L
</td>
<td style="text-align:center;">
51
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
3.500343
</td>
</tr>
<tr>
<td style="text-align:center;">
XXL
</td>
<td style="text-align:center;">
19
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.304049
</td>
</tr>
</tbody>
</table>

**3. product\_color**

Lastly, I apply the same technique to “product\_color”. Examine the
sample sizes of its levels:

``` r
cloth3 %>% 
  dplyr::select(product_color) %>% 
  group_by(product_color) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         proportion_percentage = count/total * 100) %>% 
  kable(align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
product\_color
</th>
<th style="text-align:center;">
count
</th>
<th style="text-align:center;">
total
</th>
<th style="text-align:center;">
proportion\_percentage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
black
</td>
<td style="text-align:center;">
288
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
19.7666438
</td>
</tr>
<tr>
<td style="text-align:center;">
white
</td>
<td style="text-align:center;">
236
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
16.1976664
</td>
</tr>
<tr>
<td style="text-align:center;">
pink
</td>
<td style="text-align:center;">
101
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.9320522
</td>
</tr>
<tr>
<td style="text-align:center;">
blue
</td>
<td style="text-align:center;">
97
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.6575154
</td>
</tr>
<tr>
<td style="text-align:center;">
yellow
</td>
<td style="text-align:center;">
97
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.6575154
</td>
</tr>
<tr>
<td style="text-align:center;">
red
</td>
<td style="text-align:center;">
92
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.3143445
</td>
</tr>
<tr>
<td style="text-align:center;">
green
</td>
<td style="text-align:center;">
86
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
5.9025395
</td>
</tr>
<tr>
<td style="text-align:center;">
grey
</td>
<td style="text-align:center;">
79
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
5.4221002
</td>
</tr>
<tr>
<td style="text-align:center;">
purple
</td>
<td style="text-align:center;">
50
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
3.4317090
</td>
</tr>
<tr>
<td style="text-align:center;">
armygreen
</td>
<td style="text-align:center;">
33
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
2.2649279
</td>
</tr>
<tr>
<td style="text-align:center;">
winered
</td>
<td style="text-align:center;">
29
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.9903912
</td>
</tr>
<tr>
<td style="text-align:center;">
navyblue
</td>
<td style="text-align:center;">
28
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.9217570
</td>
</tr>
<tr>
<td style="text-align:center;">
orange
</td>
<td style="text-align:center;">
25
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.7158545
</td>
</tr>
<tr>
<td style="text-align:center;">
multicolor
</td>
<td style="text-align:center;">
20
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
1.3726836
</td>
</tr>
<tr>
<td style="text-align:center;">
beige
</td>
<td style="text-align:center;">
13
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.8922443
</td>
</tr>
<tr>
<td style="text-align:center;">
lightblue
</td>
<td style="text-align:center;">
12
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.8236102
</td>
</tr>
<tr>
<td style="text-align:center;">
khaki
</td>
<td style="text-align:center;">
11
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.7549760
</td>
</tr>
<tr>
<td style="text-align:center;">
white & green
</td>
<td style="text-align:center;">
10
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.6863418
</td>
</tr>
<tr>
<td style="text-align:center;">
rosered
</td>
<td style="text-align:center;">
9
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.6177076
</td>
</tr>
<tr>
<td style="text-align:center;">
brown
</td>
<td style="text-align:center;">
7
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.4804393
</td>
</tr>
<tr>
<td style="text-align:center;">
coffee
</td>
<td style="text-align:center;">
7
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.4804393
</td>
</tr>
<tr>
<td style="text-align:center;">
darkblue
</td>
<td style="text-align:center;">
6
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.4118051
</td>
</tr>
<tr>
<td style="text-align:center;">
skyblue
</td>
<td style="text-align:center;">
6
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.4118051
</td>
</tr>
<tr>
<td style="text-align:center;">
floral
</td>
<td style="text-align:center;">
5
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.3431709
</td>
</tr>
<tr>
<td style="text-align:center;">
rose
</td>
<td style="text-align:center;">
5
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.3431709
</td>
</tr>
<tr>
<td style="text-align:center;">
black & green
</td>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2745367
</td>
</tr>
<tr>
<td style="text-align:center;">
fluorescentgreen
</td>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2745367
</td>
</tr>
<tr>
<td style="text-align:center;">
leopard
</td>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2745367
</td>
</tr>
<tr>
<td style="text-align:center;">
lightgreen
</td>
<td style="text-align:center;">
4
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2745367
</td>
</tr>
<tr>
<td style="text-align:center;">
black & white
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
camouflage
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
lightpink
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
orange-red
</td>
<td style="text-align:center;">
3
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.2059025
</td>
</tr>
<tr>
<td style="text-align:center;">
applegreen
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
apricot
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
black & blue
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
black & yellow
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
burgundy
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
camel
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
coolblack
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
coralred
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
dustypink
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
lakeblue
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
lightred
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
lightyellow
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
mintgreen
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
navy
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
navy blue
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
pink & black
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
pink & blue
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
pink & grey
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
pink & white
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
watermelonred
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
white & black
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
whitefloral
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
wine
</td>
<td style="text-align:center;">
2
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.1372684
</td>
</tr>
<tr>
<td style="text-align:center;">
army
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
black & stripe
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
blue & pink
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
brown & yellow
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
claret
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
denimblue
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
gold
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
gray & white
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
greysnakeskinprint
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
ivory
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
jasper
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
leopardprint
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
lightgray
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
lightgrey
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
lightkhaki
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
lightpurple
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
navyblue & white
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
nude
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
offblack
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
offwhite
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
orange & camouflage
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
prussianblue
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
rainbow
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
red & blue
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
rosegold
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
silver
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
star
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
violet
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
white & red
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
whitestripe
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
<tr>
<td style="text-align:center;">
winered & yellow
</td>
<td style="text-align:center;">
1
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
0.0686342
</td>
</tr>
</tbody>
</table>

In this color feature, I am keeping to top 10 colours that have
dominating samples size, and grouping the rest of the colour into “other
colors”.

``` r
 cloth3 <- cloth3 %>% 
  mutate(product_color = as.character(product_color),
         product_colors = case_when(product_color == "black" ~ "black",
                                   product_color == "white" ~ "white",
                                   product_color == "pink" ~ "pink",
                                   product_color == "blue" ~ "blue",
                                   product_color == "yellow" ~ "yellow",
                                   product_color == "red" ~ "red",
                                   product_color == "green" ~ "green",
                                   product_color == "grey" ~ "grey",
                                   product_color == "purple" ~ "purple",
                                   product_color == "armygreen" ~ "armygreen",
                                           TRUE ~ "Other_colors"),
         product_colors = as.factor(product_colors)) %>% 
  dplyr::select(-product_color)


cloth3 %>% 
  dplyr::select(product_colors) %>% 
  group_by(product_colors) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         proportion_percentage = count/total * 100) %>% 
  kable(align = "c")
```

<table>
<thead>
<tr>
<th style="text-align:center;">
product\_colors
</th>
<th style="text-align:center;">
count
</th>
<th style="text-align:center;">
total
</th>
<th style="text-align:center;">
proportion\_percentage
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:center;">
Other\_colors
</td>
<td style="text-align:center;">
298
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
20.452986
</td>
</tr>
<tr>
<td style="text-align:center;">
black
</td>
<td style="text-align:center;">
288
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
19.766644
</td>
</tr>
<tr>
<td style="text-align:center;">
white
</td>
<td style="text-align:center;">
236
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
16.197666
</td>
</tr>
<tr>
<td style="text-align:center;">
pink
</td>
<td style="text-align:center;">
101
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.932052
</td>
</tr>
<tr>
<td style="text-align:center;">
blue
</td>
<td style="text-align:center;">
97
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.657515
</td>
</tr>
<tr>
<td style="text-align:center;">
yellow
</td>
<td style="text-align:center;">
97
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.657515
</td>
</tr>
<tr>
<td style="text-align:center;">
red
</td>
<td style="text-align:center;">
92
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
6.314344
</td>
</tr>
<tr>
<td style="text-align:center;">
green
</td>
<td style="text-align:center;">
86
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
5.902539
</td>
</tr>
<tr>
<td style="text-align:center;">
grey
</td>
<td style="text-align:center;">
79
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
5.422100
</td>
</tr>
<tr>
<td style="text-align:center;">
purple
</td>
<td style="text-align:center;">
50
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
3.431709
</td>
</tr>
<tr>
<td style="text-align:center;">
armygreen
</td>
<td style="text-align:center;">
33
</td>
<td style="text-align:center;">
1457
</td>
<td style="text-align:center;">
2.264928
</td>
</tr>
</tbody>
</table>

### 6.2 EDA

#### 6.2.1 Histogram

Following histogram looks for the trends of all numerical variables.

``` r
# df

df.his <- cloth3 %>% 
  gather(key = "key", value = "value") %>% 
  mutate(value = as.numeric(value))

# plot

ggplot(df.his, aes(x = value, fill = key)) + 
  geom_histogram(colour = "black") +
  facet_wrap(~key, scale = "free") +
  theme_modern_rc() +
  theme(legend.position = "none",
        strip.text = element_text(colour = "white", size = 10)) 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](summer_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

*Insights*

-   Identify that *badge\_fast\_shipping*, *badge\_local\_product*,
    *badge\_product\_quality*, *merchant\_has\_profile\_picture*,
    *uses\_ad\_boosts*, and *shipping\_is\_express* are binary variable
    and should be converted to factor.

``` r
cloth3 <- cloth3 %>% 
  mutate(badge_fast_shipping = as.factor(badge_fast_shipping),
         badge_local_product = as.factor(badge_local_product),
         badge_product_quality = as.factor(badge_product_quality),
         merchant_has_profile_picture = as.factor(merchant_has_profile_picture),
         uses_ad_boosts = as.factor(uses_ad_boosts),
         shipping_is_express = as.factor(shipping_is_express))
```

-   *inventory\_total* is a numerical variable with only single value of
    50, information gaining from this variable in relation to the number
    of product soil will be limited. Therefore, this variable should be
    removed.

``` r
cloth3 <- cloth3 %>% dplyr::select(-inventory_total)
```

-   Most variables have skewed distribution, variables that has high
    potential predictive power (Gaussian distributed) are
    country\_shipped\_to, merchant rating, price, rating, and perhaps
    tags\_count.

``` r
cloth3
```

    ## # A tibble: 1,457 x 22
    ##    price price_class retail_price units_sold uses_ad_boosts rating badges_count
    ##    <dbl> <fct>              <dbl>      <dbl> <fct>           <dbl>        <dbl>
    ##  1 16    EUR10-20              14        100 0                3.76            0
    ##  2  8    EUR<10                22      20000 1                3.45            0
    ##  3  8    EUR<10                43        100 0                3.57            0
    ##  4  8    EUR<10                 8       5000 1                4.03            0
    ##  5  2.72 EUR<10                 3        100 1                3.1             0
    ##  6  3.92 EUR<10                 9         10 0                5               0
    ##  7  7    EUR<10                 6      50000 0                3.84            0
    ##  8 12    EUR10-20              11       1000 0                3.76            0
    ##  9 11    EUR10-20              84        100 1                3.47            0
    ## 10  5.78 EUR<10                22       5000 0                3.6             0
    ## # ... with 1,447 more rows, and 15 more variables: badge_local_product <fct>,
    ## #   badge_product_quality <fct>, badge_fast_shipping <fct>,
    ## #   product_variation_inventory <dbl>, shipping_option_price <dbl>,
    ## #   shipping_is_express <fct>, countries_shipped_to <dbl>,
    ## #   origin_country <fct>, merchant_rating_count <dbl>, merchant_rating <dbl>,
    ## #   merchant_has_profile_picture <fct>, tags_count <int>, shipping_name <fct>,
    ## #   product_sizes <fct>, product_colors <fct>

**Categorical variables**

Following histogram looks for the trends of all categorical variables.

``` r
his_cat <- cloth3 %>% 
  dplyr::select(origin_country, price_class, product_colors, shipping_name) 

his_cat <- his_cat %>% 
  gather(key = "key", value = "value") %>% 
  group_by(key, value) 
  

ggplot(his_cat, aes(y = reorder(value, table(value)[value]), colour = key)) +
  geom_bar(fill = "black") +
  facet_wrap(~key, scale = "free", ncol = 1, nrow = 4) +
  theme_modern_rc() +
  theme(legend.position = "none",
        strip.text = element_text(colour = "white"))
```

![](summer_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

It is important to note that “Other\_colors” in the graph of
“product\_color” is a combination of numerous colour, and therefore the
best color is black.

#### 6.2.2 Boxplot

Applying boxplot to visualise the existence of outliers and how are data
distributed in the form of box plot in each feature.

``` r
# df

df.box <- cloth3 %>% 
  dplyr::select(is.numeric) %>% 
  gather(key = "key", value = "value") %>% 
  mutate(value = as.numeric(value))

# plot

ggplot(df.box, aes(x = value, fill = key)) +
  geom_boxplot(colour = "white") +
  facet_wrap(~key, scale = "free", ncol = 5, nrow = 3) +
  theme_modern_rc() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, colour = "white"))
```

![](summer_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

There are many outliers in each features. It may affect the assumption
of regression models and a non-parametric machine learning model that
immune to outlier should be applied. I am adopting a careful strategy. I
will remove “outliers among outliers”, which means the outliers that are
too far away from the main group of outliers in respective variable.

I am removing 24 rows of outliers from the dataset, which is only 1.6%
of the overall dataset.

``` r
cloth3 <- cloth3 %>% 
  dplyr::filter(merchant_rating_count < 2000000,
                price < 40,
                retail_price < 250,
                shipping_option_price < 10, 
                units_sold < 25000) 
```

#### 6.3.3 Relationship Curve

Following plots try to visualise the relationship between each
explaining variables with the responding variable, “units\_sold”.

``` r
df_rela <- cloth3 %>% 
  dplyr::select(is.numeric) %>% 
  gather(key = "key", value = "value", -units_sold) 

 
ggplot(df_rela, aes(x = value, y = units_sold, colour = key)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~key, scales = "free") +
  theme_modern_rc() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 2),
        strip.text = element_text(size = 10)) +
  geom_smooth(se = F) +
  labs(x = "Variables",
       y = "Units Sold",
       title = "Relationship between Numeric Predictors with Units Sold")
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](summer_files/figure-gfm/unnamed-chunk-58-1.png)<!-- -->

The relationship between each predictors with the “units sold” (It can
be understood as how well a product is sold) is complex. I will apply
inferential model later to help interpretation.

#### 6.3.3 Correlogram

This section is to check multicollinearity between numerical predictors.
As a rule of thumb, the pair of predictors with correlation above 0.8
should have one removed among the both to avoid multicollinearity
problem. If it is not done, the standard errors of coefficients
estimates during regression analysis will be inflated, and may affect
the accuracy of respective P-values. I will also apply VIF to support
the decisions made in this correlogram.

**Insights:**

-   “Shipping\_option\_price” and “price” has correlation higher than
    0.80

``` r
df_cor <- cloth3 %>% 
  dplyr::select(is.numeric)

corrplot(cor(df_cor), method = "number", type = "upper")
```

![](summer_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

### 6.4 Multiple Linear Regression

Before starting the very first preliminary model, I have found that
there are several cleaning tasks that I missed, which are:

-   Converting *shipping\_option\_price* from double into factor. It has
    discrete numeric ranking from 1 to 7, instead of floating prices.

-   *“merchant\_rating\_count”* is not required in this analysis. There
    is a relevant column *”merchant\_rating“* already which better
    describes the rating of each merchant.

-   Removing *shipping\_is\_express*, there is only 1 item was shipped
    in expressed, and remaining 1432 (99.9%) are not shipped in
    expressed. There is not enough samples to effect of studying the
    effect of expressed shipping on the number of units sold for each
    item.

-   Removing *price\_class* as it seems not directly related to product
    sold. I have more interested in the relation between column of
    “price” and the outcome variable.

-   Removing *origin\_country*, there is no information in the
    description table stating further detail of this column.
    Furthermore, 96% of data in this column is dominated by China, and
    the rest of the countries have not enough sample size to study their
    effects on the number of product sold for each product type.

-   Removing *shipping\_option\_price* because it is correlated with the
    column “price” at a level that multicollinearity can be a issue.

``` r
cloth3 <- cloth3 %>% 
  mutate(shipping_option_price = as.factor(shipping_option_price)) %>% 
  dplyr::select(-price_class, -merchant_rating_count, -shipping_is_express, -origin_country, -shipping_option_price)
```

Create data partition.

``` r
set.seed(123)

# create data partition

training.set <- cloth3$units_sold %>% createDataPartition(p = 0.8, list = F)


# Create train and test set


train.data <- cloth3[training.set, ]
test.data <- cloth3[-training.set, ]
```

``` r
summary(train.data)
```

    ##      price         retail_price      units_sold    uses_ad_boosts
    ##  Min.   : 1.000   Min.   :  1.00   Min.   :    2   0:643         
    ##  1st Qu.: 5.840   1st Qu.:  7.00   1st Qu.:  100   1:505         
    ##  Median : 8.000   Median : 10.00   Median : 1000                 
    ##  Mean   : 8.304   Mean   : 22.85   Mean   : 3531                 
    ##  3rd Qu.:11.000   3rd Qu.: 26.00   3rd Qu.: 5000                 
    ##  Max.   :27.000   Max.   :169.00   Max.   :20000                 
    ##                                                                  
    ##      rating       badges_count    badge_local_product badge_product_quality
    ##  Min.   :1.000   Min.   :0.0000   0:1131              0:1064               
    ##  1st Qu.:3.500   1st Qu.:0.0000   1:  17              1:  84               
    ##  Median :3.820   Median :0.0000                                            
    ##  Mean   :3.769   Mean   :0.0993                                            
    ##  3rd Qu.:4.090   3rd Qu.:0.0000                                            
    ##  Max.   :5.000   Max.   :2.0000                                            
    ##                                                                            
    ##  badge_fast_shipping product_variation_inventory countries_shipped_to
    ##  0:1135              Min.   : 1.00               Min.   :  6.00      
    ##  1:  13              1st Qu.: 6.00               1st Qu.: 31.00      
    ##                      Median :50.00               Median : 40.00      
    ##                      Mean   :32.76               Mean   : 39.83      
    ##                      3rd Qu.:50.00               3rd Qu.: 43.00      
    ##                      Max.   :50.00               Max.   :139.00      
    ##                                                                      
    ##  merchant_rating merchant_has_profile_picture   tags_count    
    ##  Min.   :2.941   0:986                        Min.   :  9.00  
    ##  1st Qu.:3.914   1:162                        1st Qu.: 26.00  
    ##  Median :4.039                                Median : 47.50  
    ##  Mean   :4.026                                Mean   : 62.72  
    ##  3rd Qu.:4.151                                3rd Qu.: 80.00  
    ##  Max.   :4.578                                Max.   :326.00  
    ##                                                               
    ##             shipping_name      product_sizes      product_colors
    ##  Livraison standard:1099   L          : 44   Other_colors:240   
    ##  Other_shipping    :  49   M          :145   black       :219   
    ##                            Other_sizes: 83   white       :182   
    ##                            S          :518   yellow      : 83   
    ##                            XS         :264   blue        : 79   
    ##                            XXL        : 16   red         : 78   
    ##                            XXS        : 78   (Other)     :267

``` r
model_mlr <- lm(units_sold ~., data = train.data)

summary(model_mlr)
```

    ## 
    ## Call:
    ## lm(formula = units_sold ~ ., data = train.data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -7549  -3252  -1554   1321  18219 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   -10605.180   3701.431  -2.865 0.004246 ** 
    ## price                            -83.150     47.539  -1.749 0.080553 .  
    ## retail_price                      -8.358      5.736  -1.457 0.145378    
    ## uses_ad_boosts1                   69.076    319.082   0.216 0.828650    
    ## rating                           758.829    339.020   2.238 0.025398 *  
    ## badges_count                   -1260.470   1617.823  -0.779 0.436077    
    ## badge_local_product1            1990.445   2523.824   0.789 0.430477    
    ## badge_product_quality1          1738.382   1708.669   1.017 0.309188    
    ## badge_fast_shipping1                  NA         NA      NA       NA    
    ## product_variation_inventory       30.316      8.854   3.424 0.000640 ***
    ## countries_shipped_to             -26.242      8.401  -3.124 0.001831 ** 
    ## merchant_rating                 3127.455    856.735   3.650 0.000274 ***
    ## merchant_has_profile_picture1   1013.988    454.868   2.229 0.025999 *  
    ## tags_count                         5.042      3.057   1.649 0.099440 .  
    ## shipping_nameOther_shipping     -487.049    770.487  -0.632 0.527430    
    ## product_sizesM                  -387.008    912.297  -0.424 0.671492    
    ## product_sizesOther_sizes       -1535.034    994.608  -1.543 0.123027    
    ## product_sizesS                  -903.272    830.603  -1.087 0.277055    
    ## product_sizesXS                -2291.626    883.878  -2.593 0.009647 ** 
    ## product_sizesXXL               -2361.689   1547.529  -1.526 0.127267    
    ## product_sizesXXS               -2479.638   1041.209  -2.381 0.017409 *  
    ## product_colorsblack              355.146   1106.190   0.321 0.748230    
    ## product_colorsblue               439.192   1202.072   0.365 0.714910    
    ## product_colorsgreen               37.565   1242.462   0.030 0.975885    
    ## product_colorsgrey              1214.007   1247.782   0.973 0.330797    
    ## product_colorsOther_colors       919.614   1103.259   0.834 0.404717    
    ## product_colorspink              -147.612   1207.958  -0.122 0.902763    
    ## product_colorspurple            1879.636   1355.101   1.387 0.165693    
    ## product_colorsred                 57.656   1212.916   0.048 0.962096    
    ## product_colorswhite              506.196   1110.208   0.456 0.648517    
    ## product_colorsyellow           -1099.811   1208.222  -0.910 0.362875    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5195 on 1118 degrees of freedom
    ## Multiple R-squared:  0.108,  Adjusted R-squared:  0.08488 
    ## F-statistic: 4.669 on 29 and 1118 DF,  p-value: 0.00000000000001251

Based on statistical summary from the multiple linear regression model,
the p-value of the F-statistics is less than 0.05. which indicates that
at least 1 predictor variable is statistical significant. The adjusted
R-squared is extremely low at only 8.7% of the variability of the y
variable can be explained by this model, it indicates that it is a bad
model if one wants to use it for prediction. The variability of
predictions (95% Prediction interval) that this model can supply is very
large and therefore affecting the precision of these predictions.

Despite the low adjusted R-squared, there are significant trends. These
associated predictors still provide information about the responding
variable (unit\_sold). It is important to know that even though
R-squared is low, low P values will still indicate the true relationship
between associated predictors and the responding variable.

Extracting the data frame that contains the coefficient estimates and
p-values from the model. Statistically related features (variables) are:

``` r
# df

df6.4 <- data.frame(summary(model_mlr)$coef) %>% 
  rename(P_value = Pr...t..) %>% 
  filter(P_value < 0.05) %>% 
  rownames_to_column() %>% 
  slice(-1) %>% 
  rename(feature = rowname) %>% 
  mutate(sig = case_when(P_value < 0.05 & P_value > 0.01 ~ "*",
                         P_value < 0.01 & P_value > 0.001 ~ "**",
                         P_value < 0.001 ~ "***",
                         TRUE ~ " "),
         feature = as.factor(feature)) %>% 
  arrange(P_value) 

df6.4
```

    ##                         feature    Estimate  Std..Error   t.value      P_value
    ## 1               merchant_rating  3127.45451  856.735200  3.650433 0.0002738984
    ## 2   product_variation_inventory    30.31638    8.854208  3.423951 0.0006395477
    ## 3          countries_shipped_to   -26.24196    8.400655 -3.123799 0.0018312427
    ## 4               product_sizesXS -2291.62567  883.877720 -2.592695 0.0096469909
    ## 5              product_sizesXXS -2479.63796 1041.209345 -2.381498 0.0174089792
    ## 6                        rating   758.82899  339.020214  2.238300 0.0253977312
    ## 7 merchant_has_profile_picture1  1013.98818  454.867548  2.229194 0.0259990780
    ##   sig
    ## 1 ***
    ## 2 ***
    ## 3  **
    ## 4  **
    ## 5   *
    ## 6   *
    ## 7   *

Visualise the statistics that only shows variables that are
significantly related to units\_sold.

``` r
# plot

 ggplot(df6.4, aes(y = Estimate, x = fct_reorder(feature, -Estimate), colour = feature)) +
  geom_bar(stat = "identity", alpha = 0) +
  theme_modern_rc() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 10, size = 10, hjust = 0.7),
        plot.margin = unit(c(1,1,1,1), "cm")) +
  geom_text(aes(label = paste0("(", round(Estimate, 2), ")")), vjust = 1, colour = "white") +
  geom_text(aes(label = sig), size = 8, colour = "white") +
  labs(x = "Variables", 
       y = "Coefficient Estimate",
       subtitle = "*: P<0.05, **: P<0.01, ***: P<0.001",
       title = "Coefficient of Variables in Relation to Unit_Sold") +
  scale_y_continuous(lim = c(-3000, 3500), breaks = seq(-3000, 3500, 1000))
```

![](summer_files/figure-gfm/unnamed-chunk-65-1.png)<!-- -->

*Insights*

Surprisingly, price did not affect the number of sales significantly. I
can see that the rating of merchant, product, and if product has profile
picture significant affect the number of units sold for a type of
product positively. The more inventory the seller has will also affect
the number of unit sold by the seller positively, while keeping other
variables constant.

Negatively related variables are countries shipped to, product size XS
and XXS. The higher the amount of these products, while keeping other
variables constant.

In the next two section, I will compare this result with other model.

### 6.5 Random Forest’s Important Plot

Applying a random forest important plot and shown similar results. Many
top important variables indicated by random forest model are also
indicated by the previous multiple linear regression model.

``` r
model_rf <- train(units_sold ~., data = train.data,
                  trControl = trainControl("cv", number = 10),
                  importance = TRUE)
```

Code the plot.

``` r
plot(varImp(model_rf))
```

![](summer_files/figure-gfm/unnamed-chunk-67-1.png)<!-- -->

## 7 CONCLUSION

## 8 Legality

This project is created for skills demonstration and learning Only.

## 9 Reference

<https://www.kaggle.com/jmmvutu/summer-products-and-sales-in-ecommerce-wish>

Minitab Blog Editor 2014, *How to Interpret a Regression Model with Low
R-squared and Low P values*, viewed 16 October 2021,
<https://blog.minitab.com/en/adventures-in-statistics-2/how-to-interpret-a-regression-model-with-low-r-squared-and-low-p-values>
