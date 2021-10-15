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
    -   [5.5 Tags (Text) Analysis](#55-tags-text-analysis)
-   [6 Staitistcal Analysis](#6-staitistcal-analysis)
    -   [6.1 Feature Selection](#61-feature-selection)
    -   [6.2 EDA](#62-eda)
        -   [6.2.1 Histogram](#621-histogram)
        -   [6.2.2 Boxplot](#622-boxplot)
        -   [6.3.3 Relationship Curve](#633-relationship-curve)
        -   [6.3.3 Correlogram](#633-correlogram)
    -   [6.3 Feature cleaning](#63-feature-cleaning)
    -   [6.4 Multiple Linear Regression](#64-multiple-linear-regression)
-   [7 Predictive Aanalysis](#7-predictive-aanalysis)
-   [Legality](#legality)
-   [Reference](#reference)

------------------------------------------------------------------------

------------------------------------------------------------------------

## 1 R Libraries

``` r
library(tidyverse)
library(kableExtra)
library(skimr)
library(lubridate)
library(hrbrthemes)
library(hrbrthemes)
library(tidytext)
library(ggExtra)
library(patchwork)
library(tidytext)
library(corrplot)
library(caret)

# Format setting

options(scipen = 999)
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

### 3.2 Data Description

``` r
Variable <- names(cloth)

Description <- c("Title for localized for european countries. May be the same as title_orig if the seller did not offer a translation.", 
                 "Original english title of the product.",
                 "Price you would pay to get the product.",
                 "Reference price for similar articles on the market, or in other stores/places. Used by the seller to indicate a regular value or the price before discount.",
                 "Currency of the prices.",
                 "Number of units sold. Lower bound approximation by steps",
                 "Whether the seller paid to boost his product within the platform (highlighting, better placement or whatever)",
                 "Mean product rating.",
                 "Total number of ratings of the product",
                 "Number of 5-star ratings",
                 "Number of 4-star ratings",
                 "Number of 3-star ratings",
                 "Number of 2-star ratings",                 
                 "Number of 1-star ratings",
                 "Number of badges the product or the seller have",
                 "A badge that denotes the product is a local product. Conditions may vary (being produced locally, or something else). Some people may prefer buying local products rather than. 1 means Yes, has the badge",
                 "Badge awarded when many buyers consistently gave good evaluations. 1 means Yes, has the badge",
                 "Badge awarded when this product's order is consistently shipped rapidly",
                 "tags set by the seller",
                 "Product's main color",
                 "One of the available size variation for this product",
                 "Inventory the seller has. Max allowed quantity is 50",
                 "shipping_option_name",
                 "shipping price",
                 "whether the shipping is express or not. 1 for True",
                 "Number of countries this product is shipped to. Sellers may choose to limit where they ship a product to",
                 "Total inventory for all the product's variations (size/color variations for instance)",
                 "Whether there was an urgency banner with an urgency",
                 "A text banner that appear over some products in the search results.",
                 "origin_country",
                 "Merchant's displayed name (show in the UI as the seller's shop name)",
                 "Merchant's canonical name. A name not shown publicly. Used by the website under the hood as a canonical name. Easier to process since all lowercase without white space",
                 "The subtitle text as shown on a seller's info section to the user. (raw, not preprocessed). The website shows this to the user to give an overview of the seller's stats to the user. Mostly consists of `% <positive_feedbacks> (<rating_count> reviews)` written in french",
                 "Number of ratings of this seller",
                 "merchant's rating",
                 "merchant unique id",
                 "Convenience boolean that says whether there is a `merchant_profile_picture` url",
                 "Custom profile picture of the seller (if the seller has one). Empty otherwise",
                 "url to the product page. You may need to login to access it",
                 "Product_picture",
                 "product identifier. You can use this key to remove duplicate entries if you're not interested in studying them.",
                 "the search term used in the search bar of the website to get these search results.",
                 "meta: for info only.")


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
Number of units sold. Lower bound approximation by steps
</td>
</tr>
<tr>
<td style="text-align:left;">
uses\_ad\_boosts
</td>
<td style="text-align:left;">
Whether the seller paid to boost his product within the platform
(highlighting, better placement or whatever)
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
Total number of ratings of the product
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_five\_count
</td>
<td style="text-align:left;">
Number of 5-star ratings
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_four\_count
</td>
<td style="text-align:left;">
Number of 4-star ratings
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_three\_count
</td>
<td style="text-align:left;">
Number of 3-star ratings
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_two\_count
</td>
<td style="text-align:left;">
Number of 2-star ratings
</td>
</tr>
<tr>
<td style="text-align:left;">
rating\_one\_count
</td>
<td style="text-align:left;">
Number of 1-star ratings
</td>
</tr>
<tr>
<td style="text-align:left;">
badges\_count
</td>
<td style="text-align:left;">
Number of badges the product or the seller have
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_local\_product
</td>
<td style="text-align:left;">
A badge that denotes the product is a local product. Conditions may vary
(being produced locally, or something else). Some people may prefer
buying local products rather than. 1 means Yes, has the badge
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_product\_quality
</td>
<td style="text-align:left;">
Badge awarded when many buyers consistently gave good evaluations. 1
means Yes, has the badge
</td>
</tr>
<tr>
<td style="text-align:left;">
badge\_fast\_shipping
</td>
<td style="text-align:left;">
Badge awarded when this product’s order is consistently shipped rapidly
</td>
</tr>
<tr>
<td style="text-align:left;">
tags
</td>
<td style="text-align:left;">
tags set by the seller
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_color
</td>
<td style="text-align:left;">
Product’s main color
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_variation\_size\_id
</td>
<td style="text-align:left;">
One of the available size variation for this product
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_variation\_inventory
</td>
<td style="text-align:left;">
Inventory the seller has. Max allowed quantity is 50
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_option\_name
</td>
<td style="text-align:left;">
shipping\_option\_name
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_option\_price
</td>
<td style="text-align:left;">
shipping price
</td>
</tr>
<tr>
<td style="text-align:left;">
shipping\_is\_express
</td>
<td style="text-align:left;">
whether the shipping is express or not. 1 for True
</td>
</tr>
<tr>
<td style="text-align:left;">
countries\_shipped\_to
</td>
<td style="text-align:left;">
Number of countries this product is shipped to. Sellers may choose to
limit where they ship a product to
</td>
</tr>
<tr>
<td style="text-align:left;">
inventory\_total
</td>
<td style="text-align:left;">
Total inventory for all the product’s variations (size/color variations
for instance)
</td>
</tr>
<tr>
<td style="text-align:left;">
has\_urgency\_banner
</td>
<td style="text-align:left;">
Whether there was an urgency banner with an urgency
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
origin\_country
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_title
</td>
<td style="text-align:left;">
Merchant’s displayed name (show in the UI as the seller’s shop name)
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_name
</td>
<td style="text-align:left;">
Merchant’s canonical name. A name not shown publicly. Used by the
website under the hood as a canonical name. Easier to process since all
lowercase without white space
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
french
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_rating\_count
</td>
<td style="text-align:left;">
Number of ratings of this seller
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_rating
</td>
<td style="text-align:left;">
merchant’s rating
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_id
</td>
<td style="text-align:left;">
merchant unique id
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_has\_profile\_picture
</td>
<td style="text-align:left;">
Convenience boolean that says whether there is a
`merchant_profile_picture` url
</td>
</tr>
<tr>
<td style="text-align:left;">
merchant\_profile\_picture
</td>
<td style="text-align:left;">
Custom profile picture of the seller (if the seller has one). Empty
otherwise
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_url
</td>
<td style="text-align:left;">
url to the product page. You may need to login to access it
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_picture
</td>
<td style="text-align:left;">
Product\_picture
</td>
</tr>
<tr>
<td style="text-align:left;">
product\_id
</td>
<td style="text-align:left;">
product identifier. You can use this key to remove duplicate entries if
you’re not interested in studying them.
</td>
</tr>
<tr>
<td style="text-align:left;">
theme
</td>
<td style="text-align:left;">
the search term used in the search bar of the website to get these
search results.
</td>
</tr>
<tr>
<td style="text-align:left;">
crawl\_month
</td>
<td style="text-align:left;">
meta: for info only.
</td>
</tr>
</tbody>
</table>

### 3.3 Data Exploration

The dataset has 1573 rows of observation and 43 columns of variables.
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

Looking at the dataset horizontally with the listing of some values
within each variables and their classified type in R. This will helps to
see which variables are irrelevant to this project and should be
removed.

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

I identify that following variables can be removed for various reasons.

-   *title*: Redundant. We have already the translated title in the
    second column.  
-   *currency\_buyer*: Only one currency “EUR”, it doesn’t provide
    analysis insight.  
-   *merchant\_profile\_picture*: Contain too many missing values,
    complete rate was only 14%. This column is also redundant. relevant
    column indicating the existence of profile picture already.  
-   *has\_urgency\_banner*, complete rate was only 30%.  
-   *urgency\_text*: Contain too many missing values, complete rate was
    only 30%.  
-   *merchant\_id*: Redundant. I am not interested in individual
    merchant, it is an overall analysis.  
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

-   *crawl\_month*: Only “2020-08-01” in the entire dataset, this column
    wouldn’t contribute much to the analysis of this project.

``` r
c <- cloth %>% 
  mutate(crawl_month = ym(crawl_month))

summary(c$crawl_month)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2020-08-01" "2020-08-01" "2020-08-01" "2020-08-01" "2020-08-01" "2020-08-01"

## 4 Data Cleaning

Major tasks in this section:

-   Remove unrelated variables  
-   Manage missing values  
-   Convert character and some numerical variables into factor

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

I will assess the remaining variables again in later stage and would
remove them if I found that they don’t provide value to this analysis.

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
median, or machine learning models. However, I remove the missing values
for simplicity of this project. Only 7.37% of data is removed, and I
still have 92.63% (1457 rows) of data for this analysis.

Why missing values need to be managed? It would affect the result of any
metrics, for example, the average, as well as affecting the performance
of machine learning models.

### 4.3 Factor conversion

In order for effective analysis, convert character variables into factor
is essential. Additionally, some numeric variables will be converted
into factor type such as binary vector or vector that uses numbers for
grouping purposes.

Converting data into factor helps (1) the overall R processing speed,
(2) initial the role of these numbers in data categorisation, (3) Enable
some functions of R that require vectors to be in factor format.

``` r
cloth2 <- cloth2 %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(uses_ad_boosts = as.factor(uses_ad_boosts),       # A binary data for yes or not 
         shipping_is_express = as.factor(shipping_is_express),  # A binary data for yes or not 
         merchant_has_profile_picture = as.factor(merchant_has_profile_picture)  # A binary data for yes or not 
         )
```

### 4.4 Typos in the factor variables

I will check typos in the factor variables (character type) that are
useful for data categorisation during analysis. An important criteria I
will check on is the number of repetitation within these factors. The
factor with high repetition of levels will be assessed in this section.

I have identified them, which are:

-   product\_color  
-   product\_variation\_size\_id  
-   origin\_country

**1. Product\_color**

A lot of typos need to be cleaned, which are:

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

The data is too messy in this column. I am doing some buik computation
to aid the cleaning a little. Trim leading and trailing white space,
remove punctuation, and set all levels to Upper case.

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

There are 5 columns for different counts of rating and a “rating\_count”
representing the total number of rates received. In the aim of analysis
of this project, I do not need these columns because I only need the
“rating” column which indicates the overall rating.

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
  select(discount_per, units_sold) %>% 
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

-   The fame of a merchant is important but not critical.  
-   Sales will increase with the popularity of a merchant.  
-   However, the relationship is not absolute proved by the evidence of
    arithmetic graph.

### 5.5 Tags (Text) Analysis

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
  labs(title = "Tags are Required But No Direct Impacts On Product Success",
       x = "Number of Tags",
       y = "Units Sold") +
  scale_y_continuous(labels = function(x)(prettyNum(x, big.mark = ","))) 
```

![](summer_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

## 6 Staitistcal Analysis

This section will start to evaluate the statistical relationship between
*how well a product is sold* with all other relevant variables. The
responding variable will be “units\_sold”.

### 6.1 Feature Selection

**Primary Addition**

The number of tags in relation to the number of each product sold will
be very interesting to include. This value can be attracted from the
“tags” column that has a massive amount of text describing relevant
tags. This extraction has been done in section 5.5. Here, I will just
add the extracted column into the current dataset.

``` r
cloth2 <- cloth2 %>% 
  left_join(seller_tags, by = "merchant_name")
```

**Primary Removing**

This section will remove variables that is irrelevant to this analysis
based on my domain knowledge. I will based on the characteristics of a
feature, for examples (1) if there are too many missing values in that
column (I have done it during data cleaning), (2) The data in the column
do not help in categorising the data such as having non-repeated unique
values in the entire column, (3) The column is basically irrelevant to
the predictive objective.

Variables I am removing include:

-   title\_orig  
-   tags  
-   merchant\_title  
-   merchant\_name  
-   merchant\_info\_subtitle  
-   the accidentally induced column - “2”

``` r
cloth3 <- cloth2 %>% dplyr::select(-title_orig, -tags, -merchant_title, -merchant_name, -merchant_info_subtitle, -'2')
```

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

![](summer_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

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
    of product soil will be limited. Therefore, this variable will be
    removed.

``` r
cloth3 <- cloth3 %>% dplyr::select(-inventory_total)
```

-   Most variables have skewed distribution, variables that has high
    potential predictive power (Gaussian distributed) are
    country\_shiped to, merchant rating, price, rating, and perhaps
    tags\_count.

-   A non-parametric machine learning algorithm should applied for this
    dataset.

Following histogram looks for the trends of all categorical variables.

``` r
his_cat <- cloth3 %>% 
  dplyr::select(origin_country, price_class, product_color, shipping_option_name) 

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

![](summer_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

From above graph, we are able to see which categorical level are the
dominant ones. Product color might be harder to see and so I provide an
alternative zoomed version before with count higher than 50.

``` r
his_cat_color <- his_cat %>% 
  filter(key == "product_color") %>% 
  group_by(value) %>% 
  summarise(count = n()) %>% 
  filter(count > 20)
  
ggplot(his_cat_color, aes(y = fct_reorder(value, count), x = count, colour = value)) +
  geom_bar(fill = "black", stat = "identity") +
  theme_modern_rc() +
  theme(legend.position = "none") +
  labs(x = "Count",
       y = "Product Colour",
       title = "Zoomed in to Dominant Product Colour (Count > 50)")
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

![](summer_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

#### 6.2.2 Boxplot

Applying boxplot to visualise the existence of outliers and how are data
distributed in the form of box plot in each feature.

``` r
# df

df.box <- cloth3 %>% 
  select(is.numeric) %>% 
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

![](summer_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

There are many outliers in each features. It may affect the assumption
of regression models and a non-parametric machine learning model that
immune to outlier should be applied.

#### 6.3.3 Relationship Curve

-   Relationship between

``` r
df_rela <- cloth3 %>% 
  select(is.numeric) %>% 
  gather(key = "key", value = "value", -units_sold) 

 
ggplot(df_rela, aes(x = value, y = units_sold, colour = key)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~key, scales = "free") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 2),
        strip.text = element_text(size = 10)) +
  geom_smooth(se = F) +
  labs(x = "Variables",
       y = "Units Sold",
       title = "Relationship between Numeric Predictors with Units Sold")
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Computation failed in `stat_smooth()`:
    ## x has insufficient unique values to support 10 knots: reduce k.

    ## Warning: Computation failed in `stat_smooth()`:
    ## x has insufficient unique values to support 10 knots: reduce k.

![](summer_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

The relationship between each predictors with the “units sold” (It can
be understood as how well a product is sold) is complex. It is a rare
situation where graphical transformation of data doesn’t help much. I
will apply inferential model later to help interpretation.

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
-   “retail\_price” and “retail\_price” has correlation higher than 0.80

``` r
df_cor <- cloth3 %>% 
  select(is.numeric)

corrplot(cor(df_cor), method = "number", type = "upper")
```

![](summer_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

This feature selection will be handled in next section.

### 6.3 Feature cleaning

In this real dataset, pareto principle (80:20) is applied heavily, where
in the categorical variables, most of the data are concentrated in
certain levels and in a result, the remaining levels have insufficient
for efficient modeling.

This is happening to the “shipping\_option\_name”,
“product\_variation\_size\_id”, and “colour”. Therefore I will try to
group the level with less data into “other” category to help analysis.

**1. shipping\_option\_name**

Examining the distribution of sample sizes:

``` r
cloth3 %>% 
  select(shipping_option_name) %>% 
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
  select(shipping_name) %>% 
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
  select(product_variation_size_id ) %>% 
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
  select(product_sizes) %>% 
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
  select(product_color) %>% 
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
  select(product_colors) %>% 
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

### 6.4 Multiple Linear Regression

``` r
model_mlr <- lm(units_sold ~., data = cloth3)

summary(model_mlr) 
```

    ## 
    ## Call:
    ## lm(formula = units_sold ~ ., data = cloth3)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -47299  -3894  -1697   1541  91342 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                   Estimate   Std. Error t value
    ## (Intercept)                   -4364.154833  5539.612292  -0.788
    ## price                           424.470210   202.957749   2.091
    ## price_classEUR10-20           -3061.096075   967.948314  -3.162
    ## price_classEUR20-30           -3804.285717  3887.952084  -0.978
    ## price_classEUR40-50           -2873.497012  4589.429980  -0.626
    ## retail_price                      9.961737    12.301814   0.810
    ## price_drop                              NA           NA      NA
    ## discount_per                    -11.083384     8.850122  -1.252
    ## uses_ad_boosts1                -197.743647   469.002323  -0.422
    ## rating                          705.071212   514.837504   1.370
    ## badges_count                   -978.165763  2403.824394  -0.407
    ## badge_local_product1           1346.540921  3490.127397   0.386
    ## badge_product_quality1         1365.207989  2542.793198   0.537
    ## badge_fast_shipping1                    NA           NA      NA
    ## product_variation_inventory      26.438182    12.730838   2.077
    ## shipping_option_price         -1702.331765   547.077550  -3.112
    ## shipping_is_express1             13.208879  7097.433145   0.002
    ## countries_shipped_to            -37.258735    12.053776  -3.091
    ## origin_countryGB                -96.724952  8616.915994  -0.011
    ## origin_countrySG               6421.963881  6094.095583   1.054
    ## origin_countryUS              -1955.479615  1616.253109  -1.210
    ## origin_countryVE              -2341.202552  3913.867462  -0.598
    ## merchant_rating_count             0.026523     0.003403   7.794
    ## merchant_rating                1933.391968  1272.837165   1.519
    ## merchant_has_profile_picture1  2404.112382   656.783804   3.660
    ## tags_count                       -5.860852     4.529592  -1.294
    ## shipping_nameOther_shipping    -468.504983  1160.455734  -0.404
    ## product_sizesM                  188.516993  1365.554661   0.138
    ## product_sizesOther_sizes      -2736.281977  1500.866662  -1.823
    ## product_sizesS                -1667.576537  1264.113010  -1.319
    ## product_sizesXS               -3638.332940  1340.874020  -2.713
    ## product_sizesXXL              -7262.854054  2370.351133  -3.064
    ## product_sizesXXS              -3844.822099  1577.948855  -2.437
    ## product_colorsblack            2683.573811  1597.267578   1.680
    ## product_colorsblue             1666.234684  1748.317613   0.953
    ## product_colorsgreen            1219.059024  1794.976714   0.679
    ## product_colorsgrey             2627.962823  1817.350620   1.446
    ## product_colorsOther_colors     2579.078354  1603.590424   1.608
    ## product_colorspink              488.208191  1744.342325   0.280
    ## product_colorspurple           3847.917670  1953.784568   1.969
    ## product_colorsred               892.911971  1773.925633   0.503
    ## product_colorswhite            2086.343268  1610.046831   1.296
    ## product_colorsyellow            179.176536  1769.507653   0.101
    ##                                         Pr(>|t|)    
    ## (Intercept)                             0.430940    
    ## price                                   0.036668 *  
    ## price_classEUR10-20                     0.001598 ** 
    ## price_classEUR20-30                     0.328004    
    ## price_classEUR40-50                     0.531342    
    ## retail_price                            0.418204    
    ## price_drop                                    NA    
    ## discount_per                            0.210652    
    ## uses_ad_boosts1                         0.673362    
    ## rating                                  0.171059    
    ## badges_count                            0.684128    
    ## badge_local_product1                    0.699692    
    ## badge_product_quality1                  0.591426    
    ## badge_fast_shipping1                          NA    
    ## product_variation_inventory             0.038009 *  
    ## shipping_option_price                   0.001897 ** 
    ## shipping_is_express1                    0.998515    
    ## countries_shipped_to                    0.002033 ** 
    ## origin_countryGB                        0.991046    
    ## origin_countrySG                        0.292154    
    ## origin_countryUS                        0.226525    
    ## origin_countryVE                        0.549815    
    ## merchant_rating_count         0.0000000000000125 ***
    ## merchant_rating                         0.128995    
    ## merchant_has_profile_picture1           0.000261 ***
    ## tags_count                              0.195910    
    ## shipping_nameOther_shipping             0.686476    
    ## product_sizesM                          0.890219    
    ## product_sizesOther_sizes                0.068494 .  
    ## product_sizesS                          0.187326    
    ## product_sizesXS                         0.006740 ** 
    ## product_sizesXXL                        0.002225 ** 
    ## product_sizesXXS                        0.014949 *  
    ## product_colorsblack                     0.093158 .  
    ## product_colorsblue                      0.340727    
    ## product_colorsgreen                     0.497154    
    ## product_colorsgrey                      0.148387    
    ## product_colorsOther_colors              0.107989    
    ## product_colorspink                      0.779610    
    ## product_colorspurple                    0.049094 *  
    ## product_colorsred                       0.614794    
    ## product_colorswhite                     0.195246    
    ## product_colorsyellow                    0.919360    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8538 on 1416 degrees of freedom
    ## Multiple R-squared:  0.1321, Adjusted R-squared:  0.1076 
    ## F-statistic: 5.388 on 40 and 1416 DF,  p-value: < 0.00000000000000022

``` r
library(MASS)
```

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:patchwork':
    ## 
    ##     area

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
model_step <- train(units_sold ~., data = cloth3,
                    method = "lmStepAIC",
                    trControl = trainControl(method = "cv",
                                             number = 10)) 
```

    ## Start:  AIC=23833.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23833.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23833.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23833.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsyellow           1      74745  96920984888 23832
    ## - uses_ad_boosts1                1     671198  96921581341 23832
    ## - product_colorspink             1    1006643  96921916786 23832
    ## - product_sizesM                 1    1808964  96922719107 23832
    ## - product_colorsred              1    1885578  96922795721 23832
    ## - badge_local_product1           1    4977124  96925887267 23832
    ## - shipping_is_express1           1    7067498  96927977641 23832
    ## - badges_count                   1    8290880  96929201023 23832
    ## - `price_classEUR40-50`          1   12110188  96933020331 23832
    ## - shipping_nameOther_shipping    1   13220867  96934131010 23832
    ## - badge_product_quality1         1   14297452  96935207596 23832
    ## - product_colorsgreen            1   18724083  96939634226 23832
    ## - retail_price                   1   35560910  96956471053 23832
    ## - origin_countryVE               1   35924345  96956834489 23832
    ## - product_colorsblue             1   42607447  96963517591 23832
    ## - origin_countrySG               1   71732999  96992643142 23833
    ## - discount_per                   1   75760940  96996671084 23833
    ## - `price_classEUR20-30`          1   77181938  96998092081 23833
    ## - product_colorswhite            1  118079836  97038989979 23833
    ## - origin_countryUS               1  126819400  97047729544 23833
    ## - product_colorsgrey             1  135362780  97056272923 23833
    ## - merchant_rating                1  147458746  97068368890 23834
    ## <none>                                         96920910143 23834
    ## - rating                         1  150990501  97071900645 23834
    ## - product_colorsblack            1  154911572  97075821715 23834
    ## - tags_count                     1  162034483  97082944626 23834
    ## - product_sizesS                 1  163590986  97084501129 23834
    ## - product_colorsOther_colors     1  164357865  97085268008 23834
    ## - product_colorspurple           1  187535391  97108445534 23834
    ## - product_variation_inventory    1  204193815  97125103958 23834
    ## - price                          1  272934095  97193844238 23835
    ## - product_sizesOther_sizes       1  403676575  97324586718 23837
    ## - product_sizesXS                1  561394055  97482304198 23839
    ## - product_sizesXXS               1  571829625  97492739769 23839
    ## - shipping_option_price          1  580764866  97501675009 23839
    ## - `price_classEUR10-20`          1  599259282  97520169425 23840
    ## - product_sizesXXL               1  645916929  97566827072 23840
    ## - countries_shipped_to           1  681616733  97602526876 23841
    ## - merchant_has_profile_picture1  1  787250311  97708160454 23842
    ## - merchant_rating_count          1 3994771739 100915681883 23885
    ## 
    ## Step:  AIC=23831.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - uses_ad_boosts1                1     681808  96921666696 23830
    ## - product_sizesM                 1    1869226  96922854115 23830
    ## - product_colorspink             1    3426319  96924411207 23830
    ## - badge_local_product1           1    5033614  96926018502 23830
    ## - product_colorsred              1    5622311  96926607199 23830
    ## - shipping_is_express1           1    7058550  96928043438 23830
    ## - badges_count                   1    8353665  96929338554 23830
    ## - `price_classEUR40-50`          1   12102317  96933087205 23830
    ## - shipping_nameOther_shipping    1   13153338  96934138226 23830
    ## - badge_product_quality1         1   14376718  96935361606 23830
    ## - origin_countryVE               1   35898945  96956883834 23830
    ## - retail_price                   1   35957844  96956942732 23830
    ## - product_colorsgreen            1   44443763  96965428651 23830
    ## - origin_countrySG               1   71796754  96992781642 23831
    ## - discount_per                   1   75872749  96996857637 23831
    ## - `price_classEUR20-30`          1   77195561  96998180450 23831
    ## - product_colorsblue             1   98826158  97019811046 23831
    ## - origin_countryUS               1  127911765  97048896653 23831
    ## - merchant_rating                1  147721342  97068706230 23832
    ## <none>                                         96920984888 23832
    ## - rating                         1  150919945  97071904833 23832
    ## - tags_count                     1  162992622  97083977510 23832
    ## - product_sizesS                 1  163639325  97084624213 23832
    ## - product_variation_inventory    1  205603719  97126588607 23832
    ## - price                          1  272859608  97193844497 23833
    ## - product_colorsgrey             1  290085777  97211070666 23833
    ## - product_colorspurple           1  347305314  97268290202 23834
    ## - product_colorswhite            1  348361865  97269346753 23834
    ## - product_sizesOther_sizes       1  406008369  97326993257 23835
    ## - product_colorsblack            1  469307336  97390292224 23836
    ## - product_colorsOther_colors     1  527670499  97448655388 23837
    ## - product_sizesXS                1  561321495  97482306384 23837
    ## - product_sizesXXS               1  575439655  97496424543 23837
    ## - shipping_option_price          1  581134788  97502119676 23837
    ## - `price_classEUR10-20`          1  599549964  97520534852 23838
    ## - product_sizesXXL               1  646051031  97567035919 23838
    ## - countries_shipped_to           1  684580897  97605565785 23839
    ## - merchant_has_profile_picture1  1  787317385  97708302274 23840
    ## - merchant_rating_count          1 3998276676 100919261565 23883
    ## 
    ## Step:  AIC=23829.52
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_sizesM                 1    1919681  96923586377 23828
    ## - product_colorspink             1    3409836  96925076532 23828
    ## - badge_local_product1           1    4901115  96926567812 23828
    ## - product_colorsred              1    5653059  96927319755 23828
    ## - shipping_is_express1           1    7277211  96928943907 23828
    ## - badges_count                   1    8365821  96930032518 23828
    ## - `price_classEUR40-50`          1   12238576  96933905272 23828
    ## - shipping_nameOther_shipping    1   13013843  96934680540 23828
    ## - badge_product_quality1         1   14343256  96936009952 23828
    ## - retail_price                   1   35822306  96957489003 23828
    ## - origin_countryVE               1   36837342  96958504038 23828
    ## - product_colorsgreen            1   44218534  96965885230 23828
    ## - origin_countrySG               1   71357740  96993024436 23829
    ## - discount_per                   1   75576131  96997242827 23829
    ## - `price_classEUR20-30`          1   78686448  97000353144 23829
    ## - product_colorsblue             1   99055733  97020722429 23829
    ## - origin_countryUS               1  128247440  97049914136 23829
    ## <none>                                         96921666696 23830
    ## - merchant_rating                1  148169020  97069835716 23830
    ## - rating                         1  151764216  97073430912 23830
    ## - product_sizesS                 1  163206834  97084873530 23830
    ## - tags_count                     1  163686803  97085353499 23830
    ## - product_variation_inventory    1  210475001  97132141697 23830
    ## - price                          1  276453988  97198120685 23831
    ## - product_colorsgrey             1  289648933  97211315629 23831
    ## - product_colorspurple           1  348191921  97269858617 23832
    ## - product_colorswhite            1  348674291  97270340987 23832
    ## - product_sizesOther_sizes       1  408913911  97330580607 23833
    ## - product_colorsblack            1  468860772  97390527469 23834
    ## - product_colorsOther_colors     1  527286154  97448952851 23835
    ## - product_sizesXS                1  560666680  97482333376 23835
    ## - product_sizesXXS               1  576309201  97497975897 23835
    ## - shipping_option_price          1  584581387  97506248084 23835
    ## - `price_classEUR10-20`          1  604021990  97525688686 23836
    ## - product_sizesXXL               1  645451119  97567117816 23836
    ## - countries_shipped_to           1  683907874  97605574570 23837
    ## - merchant_has_profile_picture1  1  786637963  97708304659 23838
    ## - merchant_rating_count          1 4005535342 100927202039 23881
    ## 
    ## Step:  AIC=23827.54
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorspink             1    3534320  96927120697 23826
    ## - badge_local_product1           1    4655588  96928241965 23826
    ## - product_colorsred              1    5443344  96929029721 23826
    ## - shipping_is_express1           1    7556683  96931143060 23826
    ## - badges_count                   1    8342064  96931928441 23826
    ## - `price_classEUR40-50`          1   12219216  96935805593 23826
    ## - shipping_nameOther_shipping    1   13178856  96936765233 23826
    ## - badge_product_quality1         1   14363405  96937949782 23826
    ## - retail_price                   1   35945120  96959531497 23826
    ## - origin_countryVE               1   36721590  96960307967 23826
    ## - product_colorsgreen            1   43918552  96967504930 23826
    ## - origin_countrySG               1   71323206  96994909584 23827
    ## - discount_per                   1   75779596  96999365973 23827
    ## - `price_classEUR20-30`          1   78329340  97001915717 23827
    ## - product_colorsblue             1  100974605  97024560982 23827
    ## - origin_countryUS               1  126930864  97050517241 23827
    ## <none>                                         96923586377 23828
    ## - merchant_rating                1  148422260  97072008637 23828
    ## - rating                         1  151629297  97075215674 23828
    ## - tags_count                     1  162963317  97086549694 23828
    ## - product_variation_inventory    1  210023074  97133609451 23828
    ## - price                          1  280147295  97203733672 23829
    ## - product_colorsgrey             1  291560734  97215147111 23830
    ## - product_colorspurple           1  348964163  97272550540 23830
    ## - product_colorswhite            1  351634904  97275221281 23830
    ## - product_colorsblack            1  473121115  97396707493 23832
    ## - product_colorsOther_colors     1  527134116  97450720493 23833
    ## - shipping_option_price          1  590793637  97514380014 23834
    ## - `price_classEUR10-20`          1  610869997  97534456374 23834
    ## - countries_shipped_to           1  685918889  97609505266 23835
    ## - product_sizesS                 1  688535491  97612121868 23835
    ## - merchant_has_profile_picture1  1  789384233  97712970610 23836
    ## - product_sizesXXL               1  848345083  97771931460 23837
    ## - product_sizesOther_sizes       1  897800428  97821386805 23838
    ## - product_sizesXXS               1 1161812875  98085399252 23841
    ## - product_sizesXS                1 1674227689  98597814066 23848
    ## - merchant_rating_count          1 4006946123 100930532500 23879
    ## 
    ## Step:  AIC=23825.59
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsred              1    2878741  96929999437 23824
    ## - badge_local_product1           1    4636083  96931756780 23824
    ## - shipping_is_express1           1    7639863  96934760560 23824
    ## - badges_count                   1    8381261  96935501957 23824
    ## - `price_classEUR40-50`          1   12241199  96939361895 23824
    ## - shipping_nameOther_shipping    1   13529770  96940650467 23824
    ## - badge_product_quality1         1   14787509  96941908206 23824
    ## - retail_price                   1   35122273  96962242970 23824
    ## - origin_countryVE               1   36832420  96963953117 23824
    ## - product_colorsgreen            1   41220982  96968341679 23824
    ## - origin_countrySG               1   71407084  96998527781 23825
    ## - discount_per                   1   74742932  97001863628 23825
    ## - `price_classEUR20-30`          1   78366753  97005487449 23825
    ## - product_colorsblue             1  104674154  97031794851 23825
    ## - origin_countryUS               1  129429048  97056549745 23825
    ## - merchant_rating                1  147860417  97074981114 23826
    ## <none>                                         96927120697 23826
    ## - rating                         1  150969323  97078090020 23826
    ## - tags_count                     1  163281762  97090402459 23826
    ## - product_variation_inventory    1  209105143  97136225840 23826
    ## - price                          1  281417292  97208537989 23827
    ## - product_colorsgrey             1  317480293  97244600990 23828
    ## - product_colorspurple           1  366909375  97294030072 23829
    ## - product_colorswhite            1  434658012  97361778709 23830
    ## - shipping_option_price          1  592239862  97519360558 23832
    ## - product_colorsblack            1  600781492  97527902189 23832
    ## - `price_classEUR10-20`          1  612042812  97539163509 23832
    ## - countries_shipped_to           1  682968682  97610089378 23833
    ## - product_sizesS                 1  685039695  97612160392 23833
    ## - product_colorsOther_colors     1  685898857  97613019554 23833
    ## - merchant_has_profile_picture1  1  787133658  97714254355 23834
    ## - product_sizesXXL               1  848011377  97775132073 23835
    ## - product_sizesOther_sizes       1  899154163  97826274860 23836
    ## - product_sizesXXS               1 1160933071  98088053768 23839
    ## - product_sizesXS                1 1671568503  98598689199 23846
    ## - merchant_rating_count          1 4006356663 100933477360 23877
    ## 
    ## Step:  AIC=23823.63
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - badge_local_product1           1    4880517  96934879955 23822
    ## - shipping_is_express1           1    7637871  96937637309 23822
    ## - badges_count                   1    8624504  96938623942 23822
    ## - `price_classEUR40-50`          1   11302132  96941301569 23822
    ## - shipping_nameOther_shipping    1   14167216  96944166654 23822
    ## - badge_product_quality1         1   14987744  96944987181 23822
    ## - retail_price                   1   35257249  96965256687 23822
    ## - origin_countryVE               1   36696384  96966695821 23822
    ## - product_colorsgreen            1   38348823  96968348260 23822
    ## - origin_countrySG               1   71333621  97001333059 23823
    ## - discount_per                   1   75092258  97005091696 23823
    ## - `price_classEUR20-30`          1   78508798  97008508236 23823
    ## - product_colorsblue             1  103135291  97033134729 23823
    ## - origin_countryUS               1  131111832  97061111269 23823
    ## <none>                                         96929999437 23824
    ## - merchant_rating                1  150596611  97080596048 23824
    ## - rating                         1  152574812  97082574250 23824
    ## - tags_count                     1  165825868  97095825305 23824
    ## - product_variation_inventory    1  207654773  97137654211 23824
    ## - price                          1  280889331  97210888769 23825
    ## - product_colorsgrey             1  324365553  97254364990 23826
    ## - product_colorspurple           1  370956526  97300955964 23827
    ## - product_colorswhite            1  474770371  97404769808 23828
    ## - shipping_option_price          1  591132105  97521131543 23830
    ## - `price_classEUR10-20`          1  611360751  97541360189 23830
    ## - product_colorsblack            1  668745442  97598744879 23831
    ## - product_sizesS                 1  687654574  97617654011 23831
    ## - countries_shipped_to           1  689210486  97619209923 23831
    ## - product_colorsOther_colors     1  771527059  97701526497 23832
    ## - merchant_has_profile_picture1  1  784366757  97714366194 23832
    ## - product_sizesXXL               1  847848147  97777847584 23833
    ## - product_sizesOther_sizes       1  901096982  97831096419 23834
    ## - product_sizesXXS               1 1170320903  98100320340 23837
    ## - product_sizesXS                1 1676534242  98606533679 23844
    ## - merchant_rating_count          1 4006206666 100936206104 23875
    ## 
    ## Step:  AIC=23821.69
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - badges_count                   1    4041715  96938921669 23820
    ## - shipping_is_express1           1    7037864  96941917819 23820
    ## - `price_classEUR40-50`          1   11143812  96946023767 23820
    ## - badge_product_quality1         1   11810490  96946690445 23820
    ## - shipping_nameOther_shipping    1   14313801  96949193755 23820
    ## - retail_price                   1   35753698  96970633653 23820
    ## - origin_countryVE               1   36657534  96971537489 23820
    ## - product_colorsgreen            1   36879196  96971759151 23820
    ## - origin_countrySG               1   71282658  97006162612 23821
    ## - discount_per                   1   76210744  97011090699 23821
    ## - `price_classEUR20-30`          1   78621694  97013501649 23821
    ## - product_colorsblue             1  102125452  97037005407 23821
    ## - origin_countryUS               1  133887004  97068766959 23822
    ## <none>                                         96934879955 23822
    ## - merchant_rating                1  152582961  97087462916 23822
    ## - rating                         1  153544235  97088424190 23822
    ## - tags_count                     1  166167734  97101047689 23822
    ## - product_variation_inventory    1  215286060  97150166015 23823
    ## - price                          1  282269997  97217149952 23824
    ## - product_colorsgrey             1  322064176  97256944131 23824
    ## - product_colorspurple           1  371297396  97306177350 23825
    ## - product_colorswhite            1  473175130  97408055084 23826
    ## - shipping_option_price          1  595029320  97529909275 23828
    ## - `price_classEUR10-20`          1  612000730  97546880685 23828
    ## - product_colorsblack            1  664691824  97599571779 23829
    ## - product_sizesS                 1  686875935  97621755889 23829
    ## - countries_shipped_to           1  689330514  97624210469 23829
    ## - product_colorsOther_colors     1  768347838  97703227793 23830
    ## - merchant_has_profile_picture1  1  779770381  97714650335 23830
    ## - product_sizesXXL               1  849632609  97784512564 23831
    ## - product_sizesOther_sizes       1  910144670  97845024624 23832
    ## - product_sizesXXS               1 1168999457  98103879412 23835
    ## - product_sizesXS                1 1671686849  98606566804 23842
    ## - merchant_rating_count          1 4010684916 100945564871 23873
    ## 
    ## Step:  AIC=23819.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - shipping_is_express1           1    4847340  96943769009 23818
    ## - badge_product_quality1         1    9422488  96948344158 23818
    ## - `price_classEUR40-50`          1   10999976  96949921645 23818
    ## - shipping_nameOther_shipping    1   13790723  96952712393 23818
    ## - retail_price                   1   36069433  96974991102 23818
    ## - origin_countryVE               1   36469450  96975391119 23818
    ## - product_colorsgreen            1   36610638  96975532307 23818
    ## - origin_countrySG               1   71285736  97010207405 23819
    ## - discount_per                   1   77383122  97016304791 23819
    ## - `price_classEUR20-30`          1   78031610  97016953279 23819
    ## - product_colorsblue             1  104028282  97042949951 23819
    ## - origin_countryUS               1  133629273  97072550942 23820
    ## <none>                                         96938921669 23820
    ## - merchant_rating                1  148881306  97087802975 23820
    ## - rating                         1  155519571  97094441240 23820
    ## - tags_count                     1  163979269  97102900939 23820
    ## - product_variation_inventory    1  223862434  97162784104 23821
    ## - price                          1  281979125  97220900795 23822
    ## - product_colorsgrey             1  322226354  97261148023 23822
    ## - product_colorspurple           1  371384254  97310305923 23823
    ## - product_colorswhite            1  473858343  97412780013 23824
    ## - shipping_option_price          1  597083462  97536005131 23826
    ## - `price_classEUR10-20`          1  614961810  97553883479 23826
    ## - product_colorsblack            1  664532221  97603453890 23827
    ## - product_sizesS                 1  686144828  97625066498 23827
    ## - countries_shipped_to           1  691033663  97629955332 23827
    ## - product_colorsOther_colors     1  769468032  97708389702 23828
    ## - merchant_has_profile_picture1  1  780730209  97719651879 23828
    ## - product_sizesXXL               1  861833385  97800755055 23829
    ## - product_sizesOther_sizes       1  932673873  97871595542 23830
    ## - product_sizesXXS               1 1168968243  98107889912 23834
    ## - product_sizesXS                1 1670441711  98609363380 23840
    ## - merchant_rating_count          1 4025693810 100964615479 23871
    ## 
    ## Step:  AIC=23817.81
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - badge_product_quality1         1    9440229  96953209239 23816
    ## - `price_classEUR40-50`          1   10844641  96954613650 23816
    ## - shipping_nameOther_shipping    1   12121392  96955890401 23816
    ## - origin_countryVE               1   35983631  96979752640 23816
    ## - retail_price                   1   36125483  96979894492 23816
    ## - product_colorsgreen            1   37029933  96980798942 23816
    ## - origin_countrySG               1   71346502  97015115512 23817
    ## - `price_classEUR20-30`          1   76293911  97020062920 23817
    ## - discount_per                   1   78274423  97022043432 23817
    ## - product_colorsblue             1  104598725  97048367734 23817
    ## - origin_countryUS               1  133353877  97077122886 23818
    ## <none>                                         96943769009 23818
    ## - merchant_rating                1  151834143  97095603152 23818
    ## - rating                         1  152746667  97096515677 23818
    ## - tags_count                     1  164951402  97108720412 23818
    ## - product_variation_inventory    1  224756450  97168525459 23819
    ## - price                          1  280573316  97224342325 23820
    ## - product_colorsgrey             1  323085941  97266854951 23820
    ## - product_colorspurple           1  371196970  97314965980 23821
    ## - product_colorswhite            1  474562716  97418331726 23822
    ## - `price_classEUR10-20`          1  611202978  97554971987 23824
    ## - shipping_option_price          1  619729421  97563498431 23824
    ## - product_colorsblack            1  664682626  97608451635 23825
    ## - product_sizesS                 1  684060062  97627829071 23825
    ## - countries_shipped_to           1  688908889  97632677898 23825
    ## - product_colorsOther_colors     1  778058153  97721827162 23826
    ## - merchant_has_profile_picture1  1  779024611  97722793620 23826
    ## - product_sizesXXL               1  860759755  97804528765 23827
    ## - product_sizesOther_sizes       1  933130157  97876899166 23828
    ## - product_sizesXXS               1 1168817211  98112586220 23832
    ## - product_sizesXS                1 1667948813  98611717822 23838
    ## - merchant_rating_count          1 4026216242 100969985251 23869
    ## 
    ## Step:  AIC=23815.94
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - `price_classEUR40-50`          1   10772409  96963981648 23814
    ## - shipping_nameOther_shipping    1   11594330  96964803569 23814
    ## - origin_countryVE               1   34882315  96988091554 23814
    ## - retail_price                   1   36497068  96989706307 23814
    ## - product_colorsgreen            1   37701375  96990910613 23815
    ## - origin_countrySG               1   70382845  97023592084 23815
    ## - `price_classEUR20-30`          1   77286327  97030495565 23815
    ## - discount_per                   1   79164928  97032374166 23815
    ## - product_colorsblue             1  104871308  97058080547 23815
    ## - origin_countryUS               1  130210837  97083420076 23816
    ## <none>                                         96953209239 23816
    ## - merchant_rating                1  160932933  97114142171 23816
    ## - tags_count                     1  162994491  97116203730 23816
    ## - rating                         1  185158562  97138367800 23816
    ## - product_variation_inventory    1  229546404  97182755643 23817
    ## - price                          1  281284620  97234493859 23818
    ## - product_colorsgrey             1  330454493  97283663732 23818
    ## - product_colorspurple           1  371532881  97324742119 23819
    ## - product_colorswhite            1  474586671  97427795909 23820
    ## - `price_classEUR10-20`          1  611660018  97564869257 23822
    ## - shipping_option_price          1  620639492  97573848730 23822
    ## - product_colorsblack            1  669157583  97622366822 23823
    ## - product_sizesS                 1  683181617  97636390856 23823
    ## - countries_shipped_to           1  695914131  97649123370 23823
    ## - merchant_has_profile_picture1  1  778650689  97731859928 23824
    ## - product_colorsOther_colors     1  779203106  97732412344 23824
    ## - product_sizesXXL               1  858048438  97811257677 23826
    ## - product_sizesOther_sizes       1  940686580  97893895819 23827
    ## - product_sizesXXS               1 1173371055  98126580294 23830
    ## - product_sizesXS                1 1663628734  98616837973 23836
    ## - merchant_rating_count          1 4079488985 101032698224 23868
    ## 
    ## Step:  AIC=23814.09
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - shipping_nameOther_shipping    1   10980346  96974961994 23812
    ## - origin_countryVE               1   34463192  96998444839 23813
    ## - product_colorsgreen            1   34546097  96998527744 23813
    ## - retail_price                   1   37790983  97001772630 23813
    ## - `price_classEUR20-30`          1   69334534  97033316181 23813
    ## - origin_countrySG               1   69380446  97033362093 23813
    ## - discount_per                   1   79900154  97043881802 23813
    ## - product_colorsblue             1  107602693  97071584340 23814
    ## - origin_countryUS               1  128664525  97092646173 23814
    ## <none>                                         96963981648 23814
    ## - tags_count                     1  163265406  97127247053 23814
    ## - merchant_rating                1  173101410  97137083057 23814
    ## - rating                         1  187813888  97151795536 23815
    ## - product_variation_inventory    1  228492236  97192473883 23815
    ## - price                          1  271017258  97234998906 23816
    ## - product_colorsgrey             1  332097991  97296079639 23817
    ## - product_colorspurple           1  372626869  97336608517 23817
    ## - product_colorswhite            1  476212928  97440194576 23819
    ## - shipping_option_price          1  615251030  97579232677 23820
    ## - `price_classEUR10-20`          1  618620372  97582602020 23820
    ## - product_colorsblack            1  672090845  97636072493 23821
    ## - product_sizesS                 1  689595166  97653576813 23821
    ## - countries_shipped_to           1  698092391  97662074039 23822
    ## - merchant_has_profile_picture1  1  783918056  97747899704 23823
    ## - product_colorsOther_colors     1  784379494  97748361142 23823
    ## - product_sizesXXL               1  856874092  97820855740 23824
    ## - product_sizesOther_sizes       1  981274483  97945256131 23825
    ## - product_sizesXXS               1 1188187297  98152168945 23828
    ## - product_sizesXS                1 1676233906  98640215553 23835
    ## - merchant_rating_count          1 4073596355 101037578002 23866
    ## 
    ## Step:  AIC=23812.24
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryVE               1   34240688  97009202682 23811
    ## - product_colorsgreen            1   34775412  97009737405 23811
    ## - retail_price                   1   39240974  97014202967 23811
    ## - `price_classEUR20-30`          1   68916293  97043878286 23811
    ## - origin_countrySG               1   69792942  97044754935 23811
    ## - discount_per                   1   80788398  97055750392 23811
    ## - product_colorsblue             1  111754094  97086716088 23812
    ## - origin_countryUS               1  126429100  97101391094 23812
    ## <none>                                         96974961994 23812
    ## - tags_count                     1  162952600  97137914593 23812
    ## - merchant_rating                1  172098630  97147060623 23813
    ## - rating                         1  187101589  97162063583 23813
    ## - product_variation_inventory    1  227750860  97202712854 23813
    ## - price                          1  275748808  97250710801 23814
    ## - product_colorsgrey             1  336833450  97311795444 23815
    ## - product_colorspurple           1  380975209  97355937203 23815
    ## - product_colorswhite            1  482720669  97457682663 23817
    ## - `price_classEUR10-20`          1  622893519  97597855513 23819
    ## - shipping_option_price          1  629468356  97604430349 23819
    ## - product_colorsblack            1  681015859  97655977853 23819
    ## - product_sizesS                 1  690640013  97665602007 23820
    ## - countries_shipped_to           1  695765252  97670727246 23820
    ## - product_colorsOther_colors     1  788945240  97763907233 23821
    ## - merchant_has_profile_picture1  1  793352286  97768314280 23821
    ## - product_sizesXXL               1  860347961  97835309955 23822
    ## - product_sizesOther_sizes       1  982275522  97957237515 23823
    ## - product_sizesXXS               1 1181180638  98156142631 23826
    ## - product_sizesXS                1 1678503458  98653465452 23833
    ## - merchant_rating_count          1 4077821810 101052783804 23864
    ## 
    ## Step:  AIC=23810.7
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsgreen            1   34428606  97043631288 23809
    ## - retail_price                   1   39647661  97048850343 23809
    ## - `price_classEUR20-30`          1   66269404  97075472085 23810
    ## - origin_countrySG               1   70039030  97079241712 23810
    ## - discount_per                   1   84285685  97093488367 23810
    ## - product_colorsblue             1  111742972  97120945653 23810
    ## - origin_countryUS               1  124914712  97134117394 23810
    ## <none>                                         97009202682 23811
    ## - tags_count                     1  161982560  97171185242 23811
    ## - merchant_rating                1  179492611  97188695293 23811
    ## - rating                         1  212808124  97222010805 23812
    ## - product_variation_inventory    1  231941773  97241144455 23812
    ## - price                          1  264363449  97273566131 23812
    ## - product_colorsgrey             1  334006460  97343209142 23813
    ## - product_colorspurple           1  377502975  97386705657 23814
    ## - product_colorswhite            1  464219134  97473421815 23815
    ## - shipping_option_price          1  612596776  97621799458 23817
    ## - `price_classEUR10-20`          1  613362536  97622565218 23817
    ## - product_colorsblack            1  662936626  97672139308 23818
    ## - product_sizesS                 1  682605562  97691808244 23818
    ## - countries_shipped_to           1  695790439  97704993120 23818
    ## - product_colorsOther_colors     1  783480318  97792682999 23819
    ## - merchant_has_profile_picture1  1  798733079  97807935761 23819
    ## - product_sizesXXL               1  852249852  97861452534 23820
    ## - product_sizesOther_sizes       1  975409816  97984612498 23822
    ## - product_sizesXXS               1 1168934891  98178137573 23824
    ## - product_sizesXS                1 1686988792  98696191473 23831
    ## - merchant_rating_count          1 4078563222 101087765904 23863
    ## 
    ## Step:  AIC=23809.16
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - retail_price                   1   37732076  97081363364 23808
    ## - `price_classEUR20-30`          1   66685773  97110317061 23808
    ## - origin_countrySG               1   69973534  97113604822 23808
    ## - discount_per                   1   79463754  97123095042 23808
    ## - product_colorsblue             1   91235496  97134866784 23808
    ## - origin_countryUS               1  126380360  97170011648 23809
    ## <none>                                         97043631288 23809
    ## - tags_count                     1  160172944  97203804232 23809
    ## - merchant_rating                1  180269582  97223900870 23810
    ## - product_variation_inventory    1  213261177  97256892465 23810
    ## - rating                         1  224130265  97267761553 23810
    ## - price                          1  270130231  97313761519 23811
    ## - product_colorsgrey             1  304899463  97348530751 23811
    ## - product_colorspurple           1  351039854  97394671142 23812
    ## - product_colorswhite            1  429871920  97473503208 23813
    ## - `price_classEUR10-20`          1  617276083  97660907371 23816
    ## - shipping_option_price          1  618817651  97662448939 23816
    ## - product_colorsblack            1  631516441  97675147729 23816
    ## - product_sizesS                 1  681505742  97725137030 23816
    ## - countries_shipped_to           1  685923147  97729554435 23816
    ## - product_colorsOther_colors     1  757294639  97800925927 23817
    ## - merchant_has_profile_picture1  1  819652232  97863283520 23818
    ## - product_sizesXXL               1  851282247  97894913535 23819
    ## - product_sizesOther_sizes       1  963478246  98007109534 23820
    ## - product_sizesXXS               1 1169556441  98213187729 23823
    ## - product_sizesXS                1 1695949010  98739580298 23830
    ## - merchant_rating_count          1 4075440631 101119071919 23861
    ## 
    ## Step:  AIC=23807.67
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - discount_per                   1   42946097  97124309460 23806
    ## - `price_classEUR20-30`          1   57929717  97139293081 23807
    ## - origin_countrySG               1   71506821  97152870184 23807
    ## - product_colorsblue             1   92090965  97173454328 23807
    ## - origin_countryUS               1  130255543  97211618907 23807
    ## <none>                                         97081363364 23808
    ## - tags_count                     1  158341366  97239704729 23808
    ## - merchant_rating                1  186391806  97267755170 23808
    ## - product_variation_inventory    1  207264594  97288627958 23809
    ## - rating                         1  222509537  97303872901 23809
    ## - product_colorsgrey             1  297185899  97378549263 23810
    ## - price                          1  301193377  97382556741 23810
    ## - product_colorspurple           1  353943194  97435306558 23810
    ## - product_colorswhite            1  433022825  97514386189 23812
    ## - shipping_option_price          1  607490003  97688853367 23814
    ## - `price_classEUR10-20`          1  610341046  97691704410 23814
    ## - product_colorsblack            1  651108492  97732471856 23814
    ## - product_sizesS                 1  673376878  97754740242 23815
    ## - countries_shipped_to           1  684041631  97765404995 23815
    ## - product_colorsOther_colors     1  758965308  97840328672 23816
    ## - merchant_has_profile_picture1  1  838490544  97919853907 23817
    ## - product_sizesXXL               1  874056799  97955420163 23817
    ## - product_sizesOther_sizes       1  961795345  98043158709 23819
    ## - product_sizesXXS               1 1192905288  98274268652 23822
    ## - product_sizesXS                1 1679592846  98760956209 23828
    ## - merchant_rating_count          1 4157878846 101239242210 23861
    ## 
    ## Step:  AIC=23806.25
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - `price_classEUR20-30`          1   70399504  97194708964 23805
    ## - origin_countrySG               1   73710440  97198019900 23805
    ## - product_colorsblue             1   87904909  97212214369 23805
    ## - origin_countryUS               1  137127206  97261436666 23806
    ## - tags_count                     1  138782934  97263092395 23806
    ## <none>                                         97124309460 23806
    ## - merchant_rating                1  187639485  97311948945 23807
    ## - product_variation_inventory    1  208103385  97332412846 23807
    ## - rating                         1  219737878  97344047339 23807
    ## - product_colorsgrey             1  294168356  97418477816 23808
    ## - price                          1  322461572  97446771032 23809
    ## - product_colorspurple           1  355618652  97479928112 23809
    ## - product_colorswhite            1  428166806  97552476266 23810
    ## - shipping_option_price          1  610411481  97734720941 23813
    ## - `price_classEUR10-20`          1  646018000  97770327460 23813
    ## - product_colorsblack            1  665126152  97789435613 23813
    ## - product_sizesS                 1  675363057  97799672517 23813
    ## - countries_shipped_to           1  692302980  97816612440 23814
    ## - product_colorsOther_colors     1  765678960  97889988420 23815
    ## - merchant_has_profile_picture1  1  844400637  97968710097 23816
    ## - product_sizesXXL               1  870858337  97995167797 23816
    ## - product_sizesOther_sizes       1  949193210  98073502671 23817
    ## - product_sizesXXS               1 1177473028  98301782488 23820
    ## - product_sizesXS                1 1646657919  98770967379 23826
    ## - merchant_rating_count          1 4149078560 101273388020 23859
    ## 
    ## Step:  AIC=23805.2
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countrySG               1   70964846  97265673810 23804
    ## - product_colorsblue             1   88259957  97282968921 23804
    ## - origin_countryUS               1  131304023  97326012987 23805
    ## - tags_count                     1  139336073  97334045037 23805
    ## <none>                                         97194708964 23805
    ## - merchant_rating                1  181115351  97375824315 23806
    ## - product_variation_inventory    1  230634060  97425343024 23806
    ## - rating                         1  236475061  97431184025 23806
    ## - price                          1  252910502  97447619466 23807
    ## - product_colorsgrey             1  289630536  97484339500 23807
    ## - product_colorspurple           1  348603793  97543312757 23808
    ## - product_colorswhite            1  412947185  97607656149 23809
    ## - shipping_option_price          1  553116053  97747825017 23811
    ## - `price_classEUR10-20`          1  587244888  97781953852 23811
    ## - product_colorsblack            1  640561842  97835270806 23812
    ## - product_sizesS                 1  669062964  97863771928 23812
    ## - countries_shipped_to           1  701666373  97896375337 23813
    ## - product_colorsOther_colors     1  737994203  97932703167 23813
    ## - merchant_has_profile_picture1  1  858756254  98053465218 23815
    ## - product_sizesXXL               1  860103575  98054812539 23815
    ## - product_sizesOther_sizes       1  947008863  98141717827 23816
    ## - product_sizesXXS               1 1187437703  98382146667 23819
    ## - product_sizesXS                1 1642445637  98837154601 23825
    ## - merchant_rating_count          1 4158910548 101353619513 23858
    ## 
    ## Step:  AIC=23804.16
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsblue             1   89384024  97355057834 23803
    ## - origin_countryUS               1  131948357  97397622167 23804
    ## - tags_count                     1  138203850  97403877660 23804
    ## <none>                                         97265673810 23804
    ## - merchant_rating                1  177931416  97443605226 23805
    ## - product_variation_inventory    1  227978886  97493652696 23805
    ## - rating                         1  241867745  97507541555 23805
    ## - price                          1  248423615  97514097425 23806
    ## - product_colorsgrey             1  313162728  97578836538 23806
    ## - product_colorspurple           1  347493384  97613167194 23807
    ## - product_colorswhite            1  415264203  97680938013 23808
    ## - shipping_option_price          1  554954867  97820628677 23810
    ## - `price_classEUR10-20`          1  580484983  97846158793 23810
    ## - product_colorsblack            1  645850834  97911524644 23811
    ## - product_sizesS                 1  673221456  97938895267 23811
    ## - countries_shipped_to           1  714723519  97980397329 23812
    ## - product_colorsOther_colors     1  756285267  98021959077 23812
    ## - merchant_has_profile_picture1  1  855491959  98121165769 23814
    ## - product_sizesXXL               1  860646228  98126320038 23814
    ## - product_sizesOther_sizes       1  950014235  98215688045 23815
    ## - product_sizesXXS               1 1197126985  98462800795 23818
    ## - product_sizesXS                1 1626016248  98891690059 23824
    ## - merchant_rating_count          1 4149256268 101414930078 23857
    ## 
    ## Step:  AIC=23803.36
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - tags_count                     1  136944739  97492002573 23803
    ## - origin_countryUS               1  142749152  97497806986 23803
    ## <none>                                         97355057834 23803
    ## - merchant_rating                1  176066271  97531124105 23804
    ## - rating                         1  237076262  97592134096 23805
    ## - price                          1  250519389  97605577224 23805
    ## - product_variation_inventory    1  252186586  97607244420 23805
    ## - product_colorsgrey             1  268194078  97623251913 23805
    ## - product_colorspurple           1  308045241  97663103075 23806
    ## - product_colorswhite            1  344299757  97699357592 23806
    ## - shipping_option_price          1  559676898  97914734732 23809
    ## - product_colorsblack            1  561151208  97916209042 23809
    ## - `price_classEUR10-20`          1  579649973  97934707807 23809
    ## - countries_shipped_to           1  653930841  98008988676 23810
    ## - product_colorsOther_colors     1  670984657  98026042491 23810
    ## - product_sizesS                 1  680497675  98035555509 23811
    ## - merchant_has_profile_picture1  1  838236568  98193294402 23813
    ## - product_sizesXXL               1  842683561  98197741395 23813
    ## - product_sizesOther_sizes       1  930274113  98285331947 23814
    ## - product_sizesXXS               1 1175129292  98530187127 23817
    ## - product_sizesXS                1 1614283565  98969341400 23823
    ## - merchant_rating_count          1 4131309749 101486367583 23856
    ## 
    ## Step:  AIC=23803.21
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryUS               1  127710384  97619712957 23803
    ## <none>                                         97492002573 23803
    ## - merchant_rating                1  211739563  97703742136 23804
    ## - rating                         1  231340356  97723342929 23804
    ## - price                          1  264568294  97756570867 23805
    ## - product_variation_inventory    1  267544123  97759546696 23805
    ## - product_colorsgrey             1  290525467  97782528040 23805
    ## - product_colorspurple           1  294867719  97786870292 23805
    ## - product_colorswhite            1  373351684  97865354257 23806
    ## - shipping_option_price          1  543339711  98035342284 23809
    ## - `price_classEUR10-20`          1  577852547  98069855120 23809
    ## - product_colorsblack            1  591889970  98083892543 23809
    ## - countries_shipped_to           1  617335840  98109338413 23810
    ## - product_sizesS                 1  641240255  98133242828 23810
    ## - product_colorsOther_colors     1  655237445  98147240018 23810
    ## - product_sizesXXL               1  806941516  98298944089 23812
    ## - merchant_has_profile_picture1  1  812561183  98304563756 23812
    ## - product_sizesOther_sizes       1  844452560  98336455132 23813
    ## - product_sizesXXS               1 1111719904  98603722477 23816
    ## - product_sizesXS                1 1519744080  99011746653 23822
    ## - merchant_rating_count          1 4034758044 101526760617 23854
    ## 
    ## Step:  AIC=23802.92
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## <none>                                         97619712957 23803
    ## - merchant_rating                1  215504628  97835217585 23804
    ## - rating                         1  231636517  97851349474 23804
    ## - price                          1  255560408  97875273365 23804
    ## - product_variation_inventory    1  260514414  97880227371 23804
    ## - product_colorsgrey             1  279687974  97899400932 23805
    ## - product_colorspurple           1  299305226  97919018183 23805
    ## - product_colorswhite            1  381799021  98001511978 23806
    ## - shipping_option_price          1  532704238  98152417195 23808
    ## - `price_classEUR10-20`          1  579369765  98199082722 23809
    ## - product_colorsblack            1  581131925  98200844882 23809
    ## - product_sizesS                 1  626679976  98246392933 23809
    ## - product_colorsOther_colors     1  626875293  98246588250 23809
    ## - countries_shipped_to           1  660009113  98279722071 23810
    ## - product_sizesXXL               1  792263198  98411976155 23812
    ## - product_sizesOther_sizes       1  833804651  98453517608 23812
    ## - merchant_has_profile_picture1  1  847280562  98466993519 23812
    ## - product_sizesXXS               1 1150990579  98770703536 23816
    ## - product_sizesXS                1 1494920378  99114633335 23821
    ## - merchant_rating_count          1 4104744279 101724457236 23855
    ## Start:  AIC=23742.17
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23742.17
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23742.17
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1      59358 91443145306 23740
    ## - origin_countryGB               1     680590 91443766538 23740
    ## - product_sizesM                 1    1079598 91444165546 23740
    ## - retail_price                   1    1194935 91444280883 23740
    ## - product_colorsyellow           1    1409682 91444495630 23740
    ## - badge_local_product1           1    1808127 91444894075 23740
    ## - product_colorspink             1    2177861 91445263810 23740
    ## - badges_count                   1    3639431 91446725379 23740
    ## - badge_product_quality1         1    9935096 91453021044 23740
    ## - `price_classEUR40-50`          1   10449204 91453535152 23740
    ## - product_colorsred              1   14224049 91457309998 23740
    ## - discount_per                   1   16160064 91459246012 23740
    ## - product_colorsgreen            1   20011226 91463097175 23741
    ## - origin_countryVE               1   24221275 91467307223 23741
    ## - product_colorsblue             1   24299491 91467385439 23741
    ## - shipping_nameOther_shipping    1   35637061 91478723010 23741
    ## - `price_classEUR20-30`          1   36725789 91479811737 23741
    ## - tags_count                     1   37696500 91480782448 23741
    ## - uses_ad_boosts1                1   45630653 91488716601 23741
    ## - origin_countrySG               1   85957766 91529043714 23741
    ## - product_colorswhite            1  102388804 91545474753 23742
    ## - rating                         1  118278022 91561363970 23742
    ## - product_sizesS                 1  119479379 91562565327 23742
    ## - product_colorsgrey             1  121387018 91564472966 23742
    ## - product_colorsOther_colors     1  124253636 91567339585 23742
    ## <none>                                        91443085948 23742
    ## - origin_countryUS               1  152276968 91595362916 23742
    ## - product_colorsblack            1  162013889 91605099837 23743
    ## - product_sizesOther_sizes       1  219782556 91662868505 23743
    ## - merchant_rating                1  220786630 91663872579 23743
    ## - price                          1  238687565 91681773513 23744
    ## - product_colorspurple           1  276687907 91719773856 23744
    ## - product_sizesXXS               1  354800713 91797886661 23745
    ## - product_sizesXS                1  435199051 91878284999 23746
    ## - `price_classEUR10-20`          1  480302848 91923388796 23747
    ## - product_variation_inventory    1  532042844 91975128792 23748
    ## - shipping_option_price          1  557776834 92000862783 23748
    ## - countries_shipped_to           1  625899091 92068985040 23749
    ## - product_sizesXXL               1  630951149 92074037097 23749
    ## - merchant_has_profile_picture1  1  676561183 92119647131 23750
    ## - merchant_rating_count          1 4000332040 95443417988 23796
    ## 
    ## Step:  AIC=23740.17
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1     677152 91443822458 23738
    ## - product_sizesM                 1    1072799 91444218105 23738
    ## - retail_price                   1    1178016 91444323322 23738
    ## - product_colorsyellow           1    1408573 91444553879 23738
    ## - badge_local_product1           1    1749735 91444895041 23738
    ## - product_colorspink             1    2178497 91445323803 23738
    ## - badges_count                   1    3673919 91446819225 23738
    ## - badge_product_quality1         1   10267426 91453412732 23738
    ## - `price_classEUR40-50`          1   10635000 91453780306 23738
    ## - product_colorsred              1   14213039 91457358345 23738
    ## - discount_per                   1   16122863 91459268169 23738
    ## - product_colorsgreen            1   19969757 91463115063 23739
    ## - origin_countryVE               1   24192790 91467338096 23739
    ## - product_colorsblue             1   24315993 91467461299 23739
    ## - shipping_nameOther_shipping    1   36019127 91479164433 23739
    ## - `price_classEUR20-30`          1   37385775 91480531081 23739
    ## - tags_count                     1   37643288 91480788594 23739
    ## - uses_ad_boosts1                1   45826845 91488972151 23739
    ## - origin_countrySG               1   86028115 91529173421 23739
    ## - product_colorswhite            1  102410294 91545555600 23740
    ## - rating                         1  118236713 91561382019 23740
    ## - product_sizesS                 1  119427277 91562572583 23740
    ## - product_colorsgrey             1  121390654 91564535960 23740
    ## - product_colorsOther_colors     1  124452321 91567597627 23740
    ## <none>                                        91443145306 23740
    ## - origin_countryUS               1  152388303 91595533609 23740
    ## - product_colorsblack            1  162035400 91605180706 23741
    ## - product_sizesOther_sizes       1  219739534 91662884840 23741
    ## - merchant_rating                1  222166458 91665311764 23741
    ## - price                          1  239674471 91682819777 23742
    ## - product_colorspurple           1  276734923 91719880229 23742
    ## - product_sizesXXS               1  354819960 91797965266 23743
    ## - product_sizesXS                1  435380748 91878526054 23744
    ## - `price_classEUR10-20`          1  488952890 91932098196 23745
    ## - product_variation_inventory    1  532515798 91975661104 23746
    ## - shipping_option_price          1  561398614 92004543920 23746
    ## - countries_shipped_to           1  625899011 92069044317 23747
    ## - product_sizesXXL               1  631031144 92074176450 23747
    ## - merchant_has_profile_picture1  1  676545981 92119691287 23748
    ## - merchant_rating_count          1 4002671435 95445816741 23794
    ## 
    ## Step:  AIC=23738.18
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1    1081913 91444904371 23736
    ## - retail_price                   1    1186849 91445009307 23736
    ## - product_colorsyellow           1    1390010 91445212468 23736
    ## - badge_local_product1           1    1752016 91445574475 23736
    ## - product_colorspink             1    2160736 91445983195 23736
    ## - badges_count                   1    3687681 91447510140 23736
    ## - badge_product_quality1         1   10273401 91454095859 23736
    ## - `price_classEUR40-50`          1   10722437 91454544895 23736
    ## - product_colorsred              1   14156414 91457978872 23736
    ## - discount_per                   1   16248842 91460071300 23736
    ## - product_colorsgreen            1   19903874 91463726333 23737
    ## - origin_countryVE               1   24212717 91468035176 23737
    ## - product_colorsblue             1   24762213 91468584672 23737
    ## - shipping_nameOther_shipping    1   36062344 91479884802 23737
    ## - `price_classEUR20-30`          1   37584156 91481406614 23737
    ## - tags_count                     1   37944191 91481766650 23737
    ## - uses_ad_boosts1                1   45488917 91489311376 23737
    ## - origin_countrySG               1   85957274 91529779732 23737
    ## - product_colorswhite            1  102327729 91546150187 23738
    ## - rating                         1  118730659 91562553117 23738
    ## - product_sizesS                 1  119463159 91563285617 23738
    ## - product_colorsgrey             1  121226850 91565049309 23738
    ## - product_colorsOther_colors     1  124318341 91568140799 23738
    ## <none>                                        91443822458 23738
    ## - origin_countryUS               1  152299132 91596121591 23738
    ## - product_colorsblack            1  161930372 91605752830 23739
    ## - product_sizesOther_sizes       1  220050992 91663873451 23739
    ## - merchant_rating                1  221931180 91665753639 23739
    ## - price                          1  239857092 91683679550 23740
    ## - product_colorspurple           1  276546406 91720368864 23740
    ## - product_sizesXXS               1  355314968 91799137426 23741
    ## - product_sizesXS                1  435083968 91878906427 23742
    ## - `price_classEUR10-20`          1  490145089 91933967547 23743
    ## - product_variation_inventory    1  531843423 91975665882 23744
    ## - shipping_option_price          1  560905867 92004728325 23744
    ## - countries_shipped_to           1  625812108 92069634567 23745
    ## - product_sizesXXL               1  631237499 92075059957 23745
    ## - merchant_has_profile_picture1  1  676272704 92120095162 23746
    ## - merchant_rating_count          1 4002805530 95446627988 23792
    ## 
    ## Step:  AIC=23736.2
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1    1170863 91446075234 23734
    ## - product_colorsyellow           1    1554803 91446459174 23734
    ## - badge_local_product1           1    1888977 91446793348 23734
    ## - product_colorspink             1    2285417 91447189789 23734
    ## - badges_count                   1    3719648 91448624019 23734
    ## - badge_product_quality1         1   10286591 91455190962 23734
    ## - `price_classEUR40-50`          1   10703367 91455607738 23734
    ## - product_colorsred              1   14752894 91459657266 23734
    ## - discount_per                   1   16298290 91461202661 23734
    ## - product_colorsgreen            1   20631117 91465535488 23735
    ## - origin_countryVE               1   24281736 91469186108 23735
    ## - product_colorsblue             1   24887983 91469792354 23735
    ## - shipping_nameOther_shipping    1   35864076 91480768447 23735
    ## - `price_classEUR20-30`          1   37854195 91482758566 23735
    ## - tags_count                     1   38420998 91483325369 23735
    ## - uses_ad_boosts1                1   45328547 91490232918 23735
    ## - origin_countrySG               1   85961194 91530865565 23735
    ## - product_colorswhite            1  103009594 91547913966 23736
    ## - rating                         1  119235365 91564139736 23736
    ## - product_colorsgrey             1  121962780 91566867151 23736
    ## - product_colorsOther_colors     1  126242067 91571146438 23736
    ## <none>                                        91444904371 23736
    ## - origin_countryUS               1  154345720 91599250091 23736
    ## - product_colorsblack            1  162856741 91607761113 23737
    ## - merchant_rating                1  221637731 91666542102 23737
    ## - price                          1  239020576 91683924948 23738
    ## - product_colorspurple           1  278454941 91723359313 23738
    ## - product_sizesS                 1  361276939 91806181310 23739
    ## - product_sizesOther_sizes       1  392090170 91836994541 23740
    ## - `price_classEUR10-20`          1  489065184 91933969556 23741
    ## - product_variation_inventory    1  531914624 91976818995 23742
    ## - shipping_option_price          1  559947044 92004851416 23742
    ## - product_sizesXXS               1  618189498 92063093870 23743
    ## - countries_shipped_to           1  624733727 92069638098 23743
    ## - merchant_has_profile_picture1  1  675232478 92120136849 23744
    ## - product_sizesXXL               1  767896010 92212800381 23745
    ## - product_sizesXS                1 1083723448 92528627819 23750
    ## - merchant_rating_count          1 4001762610 95446666981 23790
    ## 
    ## Step:  AIC=23734.22
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1    1465374 91447540608 23732
    ## - badge_local_product1           1    1962259 91448037493 23732
    ## - product_colorspink             1    2147819 91448223053 23732
    ## - badges_count                   1    3805119 91449880353 23732
    ## - badge_product_quality1         1   10444018 91456519252 23732
    ## - `price_classEUR40-50`          1   10991730 91457066964 23732
    ## - product_colorsred              1   14467240 91460542474 23732
    ## - product_colorsgreen            1   20203836 91466279069 23733
    ## - origin_countryVE               1   24343359 91470418592 23733
    ## - product_colorsblue             1   24506419 91470581653 23733
    ## - discount_per                   1   25713652 91471788886 23733
    ## - shipping_nameOther_shipping    1   36746137 91482821370 23733
    ## - `price_classEUR20-30`          1   36917662 91482992896 23733
    ## - tags_count                     1   38238353 91484313587 23733
    ## - uses_ad_boosts1                1   45003837 91491079071 23733
    ## - origin_countrySG               1   86216948 91532292182 23733
    ## - product_colorswhite            1  102378962 91548454196 23734
    ## - rating                         1  118960614 91565035848 23734
    ## - product_colorsgrey             1  121007392 91567082626 23734
    ## - product_colorsOther_colors     1  125499649 91571574883 23734
    ## <none>                                        91446075234 23734
    ## - origin_countryUS               1  154997914 91601073147 23734
    ## - product_colorsblack            1  162430288 91608505522 23735
    ## - merchant_rating                1  222684063 91668759297 23735
    ## - price                          1  245764513 91691839747 23736
    ## - product_colorspurple           1  277764872 91723840106 23736
    ## - product_sizesS                 1  360303494 91806378727 23737
    ## - product_sizesOther_sizes       1  393008799 91839084032 23738
    ## - `price_classEUR10-20`          1  487919478 91933994711 23739
    ## - product_variation_inventory    1  530956060 91977031293 23740
    ## - shipping_option_price          1  558848582 92004923816 23740
    ## - product_sizesXXS               1  623286865 92069362099 23741
    ## - countries_shipped_to           1  624526291 92070601525 23741
    ## - merchant_has_profile_picture1  1  678370159 92124445393 23742
    ## - product_sizesXXL               1  772718823 92218794057 23743
    ## - product_sizesXS                1 1082552991 92528628225 23748
    ## - merchant_rating_count          1 4048289494 95494364728 23789
    ## 
    ## Step:  AIC=23732.24
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1     706718 91448247326 23730
    ## - badge_local_product1           1    1840903 91449381511 23730
    ## - badges_count                   1    3654638 91451195247 23730
    ## - badge_product_quality1         1   10247675 91457788283 23730
    ## - `price_classEUR40-50`          1   11039332 91458579940 23730
    ## - product_colorsred              1   19127972 91466668580 23731
    ## - origin_countryVE               1   24464241 91472004849 23731
    ## - discount_per                   1   26143872 91473684480 23731
    ## - product_colorsgreen            1   28652877 91476193485 23731
    ## - product_colorsblue             1   36778150 91484318758 23731
    ## - `price_classEUR20-30`          1   36957312 91484497920 23731
    ## - tags_count                     1   37541436 91485082044 23731
    ## - shipping_nameOther_shipping    1   37707330 91485247938 23731
    ## - uses_ad_boosts1                1   44518048 91492058656 23731
    ## - origin_countrySG               1   85979996 91533520604 23732
    ## - rating                         1  119148344 91566688952 23732
    ## <none>                                        91447540608 23732
    ## - origin_countryUS               1  153615059 91601155668 23732
    ## - merchant_rating                1  221672435 91669213043 23733
    ## - product_colorsgrey             1  222372103 91669912712 23733
    ## - product_colorswhite            1  242437731 91689978339 23734
    ## - price                          1  245341876 91692882484 23734
    ## - product_colorsOther_colors     1  322726892 91770267500 23735
    ## - product_sizesS                 1  359783087 91807323696 23735
    ## - product_sizesOther_sizes       1  394913837 91842454445 23736
    ## - product_colorsblack            1  411885041 91859425649 23736
    ## - product_colorspurple           1  472096770 91919637379 23737
    ## - `price_classEUR10-20`          1  487332484 91934873092 23737
    ## - product_variation_inventory    1  529734880 91977275488 23738
    ## - shipping_option_price          1  557538698 92005079306 23738
    ## - product_sizesXXS               1  628117655 92075658263 23739
    ## - countries_shipped_to           1  631221004 92078761612 23739
    ## - merchant_has_profile_picture1  1  678189389 92125729997 23740
    ## - product_sizesXXL               1  772311095 92219851703 23741
    ## - product_sizesXS                1 1081780682 92529321290 23746
    ## - merchant_rating_count          1 4055918657 95503459265 23787
    ## 
    ## Step:  AIC=23730.25
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1    1835414 91450082740 23728
    ## - badges_count                   1    3670649 91451917975 23728
    ## - badge_product_quality1         1   10430084 91458677410 23728
    ## - `price_classEUR40-50`          1   11017851 91459265177 23728
    ## - product_colorsred              1   19882822 91468130148 23729
    ## - origin_countryVE               1   24506280 91472753606 23729
    ## - discount_per                   1   26195215 91474442541 23729
    ## - product_colorsgreen            1   30578896 91478826222 23729
    ## - `price_classEUR20-30`          1   36939769 91485187095 23729
    ## - tags_count                     1   37630847 91485878173 23729
    ## - shipping_nameOther_shipping    1   37794677 91486042004 23729
    ## - product_colorsblue             1   40436909 91488684236 23729
    ## - uses_ad_boosts1                1   44506561 91492753887 23729
    ## - origin_countrySG               1   86018343 91534265669 23730
    ## - rating                         1  118754179 91567001505 23730
    ## <none>                                        91448247326 23730
    ## - origin_countryUS               1  155143916 91603391242 23731
    ## - merchant_rating                1  221323070 91669570396 23731
    ## - price                          1  245232795 91693480121 23732
    ## - product_colorsgrey             1  259181517 91707428843 23732
    ## - product_colorswhite            1  324339070 91772586396 23733
    ## - product_sizesS                 1  359089102 91807336428 23733
    ## - product_sizesOther_sizes       1  395397282 91843644608 23734
    ## - product_colorsOther_colors     1  447228689 91895476015 23735
    ## - `price_classEUR10-20`          1  487177048 91935424374 23735
    ## - product_colorspurple           1  528136728 91976384054 23736
    ## - product_variation_inventory    1  529165216 91977412542 23736
    ## - shipping_option_price          1  557363082 92005610408 23736
    ## - product_colorsblack            1  567271161 92015518487 23736
    ## - product_sizesXXS               1  628508407 92076755733 23737
    ## - countries_shipped_to           1  630917713 92079165039 23737
    ## - merchant_has_profile_picture1  1  677512077 92125759403 23738
    ## - product_sizesXXL               1  772267759 92220515085 23739
    ## - product_sizesXS                1 1081194455 92529441781 23744
    ## - merchant_rating_count          1 4055400219 95503647545 23785
    ## 
    ## Step:  AIC=23728.27
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1    2056968 91452139708 23726
    ## - `price_classEUR40-50`          1   11577369 91461660109 23726
    ## - badge_product_quality1         1   11768790 91461851530 23726
    ## - product_colorsred              1   20155549 91470238288 23727
    ## - origin_countryVE               1   24556848 91474639588 23727
    ## - discount_per                   1   26303288 91476386028 23727
    ## - product_colorsgreen            1   30025853 91480108593 23727
    ## - `price_classEUR20-30`          1   36536029 91486618768 23727
    ## - tags_count                     1   37824636 91487907376 23727
    ## - shipping_nameOther_shipping    1   38388353 91488471093 23727
    ## - product_colorsblue             1   40168936 91490251676 23727
    ## - uses_ad_boosts1                1   43746322 91493829061 23727
    ## - origin_countrySG               1   85902461 91535985201 23728
    ## - rating                         1  119032034 91569114774 23728
    ## <none>                                        91450082740 23728
    ## - origin_countryUS               1  157225103 91607307843 23729
    ## - merchant_rating                1  222296793 91672379533 23730
    ## - price                          1  244184511 91694267251 23730
    ## - product_colorsgrey             1  258252370 91708335110 23730
    ## - product_colorswhite            1  324293957 91774376697 23731
    ## - product_sizesS                 1  359150372 91809233112 23731
    ## - product_sizesOther_sizes       1  399463472 91849546212 23732
    ## - product_colorsOther_colors     1  446063032 91896145772 23733
    ## - `price_classEUR10-20`          1  485500698 91935583438 23733
    ## - product_colorspurple           1  528571081 91978653820 23734
    ## - product_variation_inventory    1  545110494 91995193233 23734
    ## - shipping_option_price          1  557682915 92007765655 23734
    ## - product_colorsblack            1  565545225 92015627964 23734
    ## - product_sizesXXS               1  628440221 92078522961 23735
    ## - countries_shipped_to           1  631617922 92081700662 23735
    ## - merchant_has_profile_picture1  1  675677813 92125760553 23736
    ## - product_sizesXXL               1  773155827 92223238567 23737
    ## - product_sizesXS                1 1079497443 92529580183 23742
    ## - merchant_rating_count          1 4058016036 95508098775 23783
    ## 
    ## Step:  AIC=23726.3
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   11863084 91464002792 23725
    ## - badge_product_quality1         1   14462556 91466602264 23725
    ## - product_colorsred              1   20283963 91472423671 23725
    ## - origin_countryVE               1   24512811 91476652520 23725
    ## - discount_per                   1   26818129 91478957837 23725
    ## - product_colorsgreen            1   29851903 91481991611 23725
    ## - `price_classEUR20-30`          1   36005851 91488145559 23725
    ## - tags_count                     1   37020504 91489160212 23725
    ## - shipping_nameOther_shipping    1   38597764 91490737472 23725
    ## - product_colorsblue             1   40818007 91492957715 23725
    ## - uses_ad_boosts1                1   45029175 91497168883 23725
    ## - origin_countrySG               1   85900756 91538040465 23726
    ## - rating                         1  120303601 91572443309 23726
    ## <none>                                        91452139708 23726
    ## - origin_countryUS               1  157272883 91609412591 23727
    ## - merchant_rating                1  220556198 91672695906 23728
    ## - price                          1  246154757 91698294465 23728
    ## - product_colorsgrey             1  258154711 91710294419 23728
    ## - product_colorswhite            1  324079394 91776219102 23729
    ## - product_sizesS                 1  359145841 91811285549 23729
    ## - product_sizesOther_sizes       1  406713966 91858853674 23730
    ## - product_colorsOther_colors     1  445665597 91897805305 23731
    ## - `price_classEUR10-20`          1  487082592 91939222300 23731
    ## - product_colorspurple           1  528701766 91980841474 23732
    ## - product_variation_inventory    1  555422828 92007562536 23732
    ## - product_colorsblack            1  567093092 92019232800 23732
    ## - shipping_option_price          1  569750088 92021889796 23732
    ## - product_sizesXXS               1  628251349 92080391057 23733
    ## - countries_shipped_to           1  633782855 92085922563 23733
    ## - merchant_has_profile_picture1  1  676348153 92128487861 23734
    ## - product_sizesXXL               1  783304079 92235443788 23736
    ## - product_sizesXS                1 1079088628 92531228336 23740
    ## - merchant_rating_count          1 4070019153 95522158861 23781
    ## 
    ## Step:  AIC=23724.47
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1   14095159 91478097951 23723
    ## - product_colorsred              1   18637971 91482640763 23723
    ## - origin_countryVE               1   23895852 91487898644 23723
    ## - discount_per                   1   26484863 91490487655 23723
    ## - product_colorsgreen            1   26562496 91490565288 23723
    ## - `price_classEUR20-30`          1   27689110 91491691901 23723
    ## - tags_count                     1   38020867 91502023658 23723
    ## - shipping_nameOther_shipping    1   39275422 91503278213 23723
    ## - product_colorsblue             1   41887479 91505890270 23723
    ## - uses_ad_boosts1                1   46153220 91510156011 23723
    ## - origin_countrySG               1   84569897 91548572689 23724
    ## - rating                         1  124154338 91588157130 23724
    ## <none>                                        91464002792 23725
    ## - origin_countryUS               1  156212273 91620215064 23725
    ## - merchant_rating                1  233840111 91697842902 23726
    ## - price                          1  239993593 91703996385 23726
    ## - product_colorsgrey             1  257379376 91721382168 23726
    ## - product_colorswhite            1  321109125 91785111916 23727
    ## - product_sizesS                 1  363433881 91827436673 23728
    ## - product_sizesOther_sizes       1  440534132 91904536924 23729
    ## - product_colorsOther_colors     1  442799094 91906801885 23729
    ## - `price_classEUR10-20`          1  512852309 91976855100 23730
    ## - product_colorspurple           1  526548392 91990551184 23730
    ## - shipping_option_price          1  558496896 92022499688 23730
    ## - product_variation_inventory    1  560828390 92024831182 23731
    ## - product_colorsblack            1  563954384 92027957175 23731
    ## - countries_shipped_to           1  637878653 92101881445 23732
    ## - product_sizesXXS               1  642399012 92106401804 23732
    ## - merchant_has_profile_picture1  1  680737435 92144740226 23732
    ## - product_sizesXXL               1  781839559 92245842351 23734
    ## - product_sizesXS                1 1091365485 92555368277 23738
    ## - merchant_rating_count          1 4063254725 95527257517 23779
    ## 
    ## Step:  AIC=23722.67
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1   17600063 91495698014 23721
    ## - origin_countryVE               1   22677560 91500775511 23721
    ## - discount_per                   1   27301995 91505399946 23721
    ## - product_colorsgreen            1   27429331 91505527282 23721
    ## - `price_classEUR20-30`          1   27841611 91505939562 23721
    ## - tags_count                     1   36988040 91515085991 23721
    ## - shipping_nameOther_shipping    1   37553655 91515651606 23721
    ## - product_colorsblue             1   41960354 91520058305 23721
    ## - uses_ad_boosts1                1   46752176 91524850127 23721
    ## - origin_countrySG               1   83174722 91561272673 23722
    ## <none>                                        91478097951 23723
    ## - origin_countryUS               1  152226931 91630324882 23723
    ## - rating                         1  157414553 91635512504 23723
    ## - price                          1  238335987 91716433938 23724
    ## - merchant_rating                1  248557313 91726655264 23724
    ## - product_colorsgrey             1  264995035 91743092986 23725
    ## - product_colorswhite            1  319370083 91797468034 23725
    ## - product_sizesS                 1  359814950 91837912901 23726
    ## - product_sizesOther_sizes       1  441225563 91919323514 23727
    ## - product_colorsOther_colors     1  441696547 91919794498 23727
    ## - `price_classEUR10-20`          1  511654314 91989752265 23728
    ## - product_colorspurple           1  524941556 92003039507 23728
    ## - shipping_option_price          1  558696777 92036794728 23729
    ## - product_colorsblack            1  567121426 92045219377 23729
    ## - product_variation_inventory    1  568319637 92046417588 23729
    ## - countries_shipped_to           1  641378931 92119476882 23730
    ## - product_sizesXXS               1  646894702 92124992653 23730
    ## - merchant_has_profile_picture1  1  680592035 92158689986 23730
    ## - product_sizesXXL               1  777221226 92255319177 23732
    ## - product_sizesXS                1 1084967991 92563065942 23736
    ## - merchant_rating_count          1 4119159528 95597257479 23778
    ## 
    ## Step:  AIC=23720.93
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   17725299 91513423313 23719
    ## - origin_countryVE               1   22524363 91518222377 23719
    ## - discount_per                   1   27234828 91522932842 23719
    ## - `price_classEUR20-30`          1   27828187 91523526201 23719
    ## - product_colorsblue             1   30205620 91525903634 23719
    ## - tags_count                     1   39886907 91535584921 23720
    ## - shipping_nameOther_shipping    1   40632875 91536330890 23720
    ## - uses_ad_boosts1                1   47673575 91543371589 23720
    ## - origin_countrySG               1   83014487 91578712502 23720
    ## <none>                                        91495698014 23721
    ## - origin_countryUS               1  157024948 91652722962 23721
    ## - rating                         1  159076174 91654774188 23721
    ## - price                          1  235329546 91731027560 23722
    ## - product_colorsgrey             1  247653092 91743351106 23723
    ## - merchant_rating                1  253237797 91748935812 23723
    ## - product_colorswhite            1  311066272 91806764286 23723
    ## - product_sizesS                 1  361475810 91857173824 23724
    ## - product_sizesOther_sizes       1  439919853 91935617867 23725
    ## - product_colorsOther_colors     1  447365219 91943063233 23725
    ## - product_colorspurple           1  509213214 92004911228 23726
    ## - `price_classEUR10-20`          1  509865618 92005563632 23726
    ## - shipping_option_price          1  552854716 92048552730 23727
    ## - product_variation_inventory    1  563995653 92059693667 23727
    ## - product_colorsblack            1  584745415 92080443429 23727
    ## - countries_shipped_to           1  653549601 92149247615 23728
    ## - product_sizesXXS               1  658187151 92153885165 23728
    ## - merchant_has_profile_picture1  1  672408890 92168106904 23729
    ## - product_sizesXXL               1  776529578 92272227592 23730
    ## - product_sizesXS                1 1086851277 92582549291 23734
    ## - merchant_rating_count          1 4119964705 95615662719 23777
    ## 
    ## Step:  AIC=23719.18
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsblue             1   21857713 91535281026 23718
    ## - origin_countryVE               1   22502116 91535925429 23718
    ## - discount_per                   1   24695055 91538118369 23718
    ## - `price_classEUR20-30`          1   28400578 91541823892 23718
    ## - tags_count                     1   39570792 91552994105 23718
    ## - shipping_nameOther_shipping    1   42173868 91555597181 23718
    ## - uses_ad_boosts1                1   46167941 91559591254 23718
    ## - origin_countrySG               1   82913619 91596336932 23718
    ## <none>                                        91513423313 23719
    ## - origin_countryUS               1  160766699 91674190012 23720
    ## - rating                         1  165790481 91679213795 23720
    ## - product_colorsgrey             1  230793956 91744217269 23721
    ## - price                          1  238736853 91752160166 23721
    ## - merchant_rating                1  250621571 91764044884 23721
    ## - product_colorswhite            1  294693984 91808117298 23721
    ## - product_sizesS                 1  360842911 91874266224 23722
    ## - product_sizesOther_sizes       1  434245201 91947668514 23723
    ## - product_colorsOther_colors     1  437213563 91950636876 23723
    ## - product_colorspurple           1  491569146 92004992459 23724
    ## - `price_classEUR10-20`          1  511887803 92025311116 23725
    ## - product_variation_inventory    1  550311969 92063735282 23725
    ## - shipping_option_price          1  554515908 92067939221 23725
    ## - product_colorsblack            1  578587236 92092010549 23725
    ## - countries_shipped_to           1  647287503 92160710816 23726
    ## - product_sizesXXS               1  655866907 92169290220 23727
    ## - merchant_has_profile_picture1  1  690540156 92203963469 23727
    ## - product_sizesXXL               1  775550365 92288973678 23728
    ## - product_sizesXS                1 1086691916 92600115229 23733
    ## - merchant_rating_count          1 4122122555 95635545869 23775
    ## 
    ## Step:  AIC=23717.49
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   22585639 91557866665 23716
    ## - discount_per                   1   24203410 91559484436 23716
    ## - `price_classEUR20-30`          1   28913840 91564194866 23716
    ## - tags_count                     1   38575882 91573856908 23716
    ## - shipping_nameOther_shipping    1   45943922 91581224948 23716
    ## - uses_ad_boosts1                1   48116747 91583397773 23716
    ## - origin_countrySG               1   83659138 91618940164 23717
    ## <none>                                        91535281026 23718
    ## - rating                         1  162280659 91697561685 23718
    ## - origin_countryUS               1  166784257 91702065283 23718
    ## - product_colorsgrey             1  213361950 91748642975 23719
    ## - price                          1  241117726 91776398752 23719
    ## - merchant_rating                1  247998954 91783279980 23719
    ## - product_colorswhite            1  272838994 91808120020 23719
    ## - product_sizesS                 1  363400742 91898681768 23721
    ## - product_colorsOther_colors     1  416918763 91952199789 23721
    ## - product_sizesOther_sizes       1  426504671 91961785697 23722
    ## - product_colorspurple           1  471980531 92007261557 23722
    ## - `price_classEUR10-20`          1  512613921 92047894947 23723
    ## - shipping_option_price          1  555104526 92090385552 23723
    ## - product_colorsblack            1  562879544 92098160570 23724
    ## - product_variation_inventory    1  571767033 92107048059 23724
    ## - countries_shipped_to           1  626119741 92161400767 23724
    ## - product_sizesXXS               1  646289014 92181570040 23725
    ## - merchant_has_profile_picture1  1  684090597 92219371623 23725
    ## - product_sizesXXL               1  767874285 92303155311 23726
    ## - product_sizesXS                1 1077429536 92612710562 23731
    ## - merchant_rating_count          1 4115610439 95650891465 23773
    ## 
    ## Step:  AIC=23715.82
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - discount_per                   1   26195515 91584062180 23714
    ## - `price_classEUR20-30`          1   27448733 91585315398 23714
    ## - tags_count                     1   37863344 91595730009 23714
    ## - shipping_nameOther_shipping    1   45776584 91603643248 23715
    ## - uses_ad_boosts1                1   53163966 91611030631 23715
    ## - origin_countrySG               1   84190135 91642056800 23715
    ## <none>                                        91557866665 23716
    ## - origin_countryUS               1  165924874 91723791539 23716
    ## - rating                         1  181186397 91739053062 23716
    ## - product_colorsgrey             1  211653898 91769520562 23717
    ## - price                          1  232069116 91789935781 23717
    ## - merchant_rating                1  255308422 91813175087 23718
    ## - product_colorswhite            1  260923946 91818790611 23718
    ## - product_sizesS                 1  358303953 91916170618 23719
    ## - product_colorsOther_colors     1  413953905 91971820569 23720
    ## - product_sizesOther_sizes       1  422273194 91980139859 23720
    ## - product_colorspurple           1  467940648 92025807313 23721
    ## - `price_classEUR10-20`          1  505748581 92063615246 23721
    ## - shipping_option_price          1  542055675 92099922340 23722
    ## - product_colorsblack            1  549634699 92107501364 23722
    ## - product_variation_inventory    1  576004511 92133871176 23722
    ## - countries_shipped_to           1  625920727 92183787391 23723
    ## - product_sizesXXS               1  637909153 92195775818 23723
    ## - merchant_has_profile_picture1  1  688118852 92245985517 23724
    ## - product_sizesXXL               1  761093215 92318959880 23725
    ## - product_sizesXS                1 1082939145 92640805810 23729
    ## - merchant_rating_count          1 4116046310 95673912975 23771
    ## 
    ## Step:  AIC=23714.19
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - tags_count                     1   30516696 91614578876 23713
    ## - `price_classEUR20-30`          1   32668394 91616730574 23713
    ## - shipping_nameOther_shipping    1   45026004 91629088184 23713
    ## - uses_ad_boosts1                1   52352753 91636414933 23713
    ## - origin_countrySG               1   85783974 91669846154 23713
    ## <none>                                        91584062180 23714
    ## - origin_countryUS               1  172826959 91756889139 23715
    ## - rating                         1  178537005 91762599185 23715
    ## - product_colorsgrey             1  213814018 91797876198 23715
    ## - price                          1  242278475 91826340655 23716
    ## - merchant_rating                1  255925071 91839987251 23716
    ## - product_colorswhite            1  256454104 91840516284 23716
    ## - product_sizesS                 1  361530708 91945592888 23717
    ## - product_colorsOther_colors     1  416143520 92000205700 23718
    ## - product_sizesOther_sizes       1  422488942 92006551122 23718
    ## - product_colorspurple           1  473091808 92057153988 23719
    ## - `price_classEUR10-20`          1  521785775 92105847955 23720
    ## - shipping_option_price          1  539781010 92123843190 23720
    ## - product_colorsblack            1  553557332 92137619513 23720
    ## - product_variation_inventory    1  575701416 92159763596 23720
    ## - product_sizesXXS               1  630252152 92214314332 23721
    ## - countries_shipped_to           1  639853226 92223915406 23721
    ## - merchant_has_profile_picture1  1  693684251 92277746431 23722
    ## - product_sizesXXL               1  755337802 92339399982 23723
    ## - product_sizesXS                1 1065897770 92649959950 23727
    ## - merchant_rating_count          1 4111059116 95695121297 23770
    ## 
    ## Step:  AIC=23712.63
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR20-30`          1   32266521 91646845397 23711
    ## - shipping_nameOther_shipping    1   45694728 91660273604 23711
    ## - uses_ad_boosts1                1   54727102 91669305977 23711
    ## - origin_countrySG               1   85408666 91699987542 23712
    ## <none>                                        91614578876 23713
    ## - origin_countryUS               1  165368988 91779947863 23713
    ## - rating                         1  175789652 91790368527 23713
    ## - product_colorsgrey             1  221573542 91836152417 23714
    ## - price                          1  244841559 91859420435 23714
    ## - product_colorswhite            1  263922851 91878501727 23714
    ## - merchant_rating                1  274878707 91889457583 23715
    ## - product_sizesS                 1  349961916 91964540791 23716
    ## - product_sizesOther_sizes       1  397652955 92012231831 23716
    ## - product_colorsOther_colors     1  408891554 92023470430 23717
    ## - product_colorspurple           1  466492860 92081071735 23717
    ## - `price_classEUR10-20`          1  519106712 92133685588 23718
    ## - shipping_option_price          1  528258338 92142837214 23718
    ## - product_colorsblack            1  568341724 92182920600 23719
    ## - product_variation_inventory    1  593866438 92208445314 23719
    ## - product_sizesXXS               1  607357648 92221936524 23719
    ## - countries_shipped_to           1  627943055 92242521931 23720
    ## - merchant_has_profile_picture1  1  680045251 92294624126 23720
    ## - product_sizesXXL               1  736539097 92351117972 23721
    ## - product_sizesXS                1 1037637897 92652216773 23725
    ## - merchant_rating_count          1 4080644351 95695223227 23768
    ## 
    ## Step:  AIC=23711.09
    ## .outcome ~ price + `price_classEUR10-20` + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1   45597457 91692442854 23710
    ## - uses_ad_boosts1                1   61263081 91708108478 23710
    ## - origin_countrySG               1   84236951 91731082348 23710
    ## <none>                                        91646845397 23711
    ## - origin_countryUS               1  161599312 91808444709 23711
    ## - rating                         1  183222394 91830067791 23712
    ## - price                          1  212674755 91859520152 23712
    ## - product_colorsgrey             1  219371655 91866217052 23712
    ## - product_colorswhite            1  256555903 91903401300 23713
    ## - merchant_rating                1  271363211 91918208608 23713
    ## - product_sizesS                 1  346172743 91993018140 23714
    ## - product_sizesOther_sizes       1  389718069 92036563466 23715
    ## - product_colorsOther_colors     1  394972626 92041818023 23715
    ## - product_colorspurple           1  462570262 92109415659 23716
    ## - shipping_option_price          1  499478634 92146324031 23716
    ## - `price_classEUR10-20`          1  503367014 92150212411 23716
    ## - product_colorsblack            1  554816699 92201662096 23717
    ## - product_variation_inventory    1  606542262 92253387659 23718
    ## - product_sizesXXS               1  610415172 92257260569 23718
    ## - countries_shipped_to           1  632915928 92279761325 23718
    ## - merchant_has_profile_picture1  1  684417111 92331262508 23719
    ## - product_sizesXXL               1  729366997 92376212394 23720
    ## - product_sizesXS                1 1039461785 92686307182 23724
    ## - merchant_rating_count          1 4088162484 95735007881 23766
    ## 
    ## Step:  AIC=23709.74
    ## .outcome ~ price + `price_classEUR10-20` + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1   56355371 91748798225 23709
    ## - origin_countrySG               1   84783498 91777226352 23709
    ## <none>                                        91692442854 23710
    ## - origin_countryUS               1  156847692 91849290546 23710
    ## - rating                         1  181695482 91874138335 23710
    ## - price                          1  212714930 91905157783 23711
    ## - product_colorsgrey             1  224773059 91917215913 23711
    ## - product_colorswhite            1  262831801 91955274655 23712
    ## - merchant_rating                1  268956411 91961399265 23712
    ## - product_sizesS                 1  346999985 92039442839 23713
    ## - product_colorsOther_colors     1  392023462 92084466316 23713
    ## - product_sizesOther_sizes       1  398323147 92090766001 23713
    ## - product_colorspurple           1  478235409 92170678263 23715
    ## - `price_classEUR10-20`          1  494900844 92187343698 23715
    ## - shipping_option_price          1  516625332 92209068186 23715
    ## - product_colorsblack            1  563381759 92255824613 23716
    ## - product_sizesXXS               1  602242620 92294685474 23716
    ## - product_variation_inventory    1  607099069 92299541923 23716
    ## - countries_shipped_to           1  624263338 92316706191 23717
    ## - merchant_has_profile_picture1  1  696896061 92389338915 23718
    ## - product_sizesXXL               1  734955333 92427398187 23718
    ## - product_sizesXS                1 1047635227 92740078081 23723
    ## - merchant_rating_count          1 4092025910 95784468764 23765
    ## 
    ## Step:  AIC=23708.54
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countrySG               1   79543130 91828341355 23708
    ## <none>                                        91748798225 23709
    ## - origin_countryUS               1  155388689 91904186914 23709
    ## - rating                         1  190634691 91939432916 23709
    ## - price                          1  218275453 91967073678 23710
    ## - product_colorsgrey             1  222254105 91971052330 23710
    ## - product_colorswhite            1  266251050 92015049276 23710
    ## - merchant_rating                1  271744624 92020542850 23710
    ## - product_sizesS                 1  341954814 92090753039 23711
    ## - product_colorsOther_colors     1  385665240 92134463465 23712
    ## - product_sizesOther_sizes       1  427481518 92176279744 23713
    ## - product_colorspurple           1  481869097 92230667322 23713
    ## - `price_classEUR10-20`          1  499315295 92248113520 23714
    ## - shipping_option_price          1  521852538 92270650764 23714
    ## - product_colorsblack            1  553046027 92301844252 23714
    ## - countries_shipped_to           1  616094526 92364892752 23715
    ## - product_sizesXXS               1  629595738 92378393964 23716
    ## - product_variation_inventory    1  659438919 92408237144 23716
    ## - merchant_has_profile_picture1  1  696579284 92445377510 23717
    ## - product_sizesXXL               1  728487547 92477285772 23717
    ## - product_sizesXS                1 1039583071 92788381296 23721
    ## - merchant_rating_count          1 4054740786 95803539011 23763
    ## 
    ## Step:  AIC=23707.68
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## <none>                                        91828341355 23708
    ## - origin_countryUS               1  156006115 91984347470 23708
    ## - rating                         1  196107680 92024449035 23709
    ## - price                          1  215665949 92044007304 23709
    ## - product_colorsgrey             1  243000887 92071342243 23709
    ## - product_colorswhite            1  267711808 92096053163 23710
    ## - merchant_rating                1  267716660 92096058016 23710
    ## - product_sizesS                 1  345252837 92173594193 23711
    ## - product_colorsOther_colors     1  399391409 92227732765 23711
    ## - product_sizesOther_sizes       1  429641637 92257982993 23712
    ## - product_colorspurple           1  480957507 92309298862 23713
    ## - `price_classEUR10-20`          1  494380242 92322721597 23713
    ## - shipping_option_price          1  526291131 92354632486 23713
    ## - product_colorsblack            1  557708113 92386049468 23714
    ## - countries_shipped_to           1  629398405 92457739760 23715
    ## - product_sizesXXS               1  637193831 92465535186 23715
    ## - product_variation_inventory    1  654037290 92482378645 23715
    ## - merchant_has_profile_picture1  1  692273811 92520615166 23716
    ## - product_sizesXXL               1  728900809 92557242164 23716
    ## - product_sizesXS                1 1024731732 92853073088 23720
    ## - merchant_rating_count          1 4045505358 95873846713 23762
    ## Start:  AIC=23822.56
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23822.56
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23822.56
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23822.56
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1      52715 94869865081 23821
    ## - origin_countryGB               1     282137 94870094504 23821
    ## - product_colorspink             1     615246 94870427613 23821
    ## - product_sizesM                 1     708791 94870521158 23821
    ## - shipping_is_express1           1     996630 94870808996 23821
    ## - shipping_nameOther_shipping    1    2759173 94872571540 23821
    ## - product_colorsred              1   15642633 94885454999 23821
    ## - retail_price                   1   17814731 94887627098 23821
    ## - origin_countryVE               1   18799726 94888612093 23821
    ## - `price_classEUR40-50`          1   23921492 94893733859 23821
    ## - `price_classEUR20-30`          1   27269023 94897081389 23821
    ## - product_colorsblue             1   27360441 94897172807 23821
    ## - uses_ad_boosts1                1   28543189 94898355556 23821
    ## - product_colorsgreen            1   31812677 94901625043 23821
    ## - badge_local_product1           1   39058923 94908871290 23821
    ## - badges_count                   1   52700863 94922513229 23821
    ## - product_colorswhite            1   65884930 94935697296 23822
    ## - discount_per                   1   72727188 94942539555 23822
    ## - rating                         1   85400036 94955212403 23822
    ## - badge_product_quality1         1   87431274 94957243641 23822
    ## - origin_countryUS               1  103651429 94973463795 23822
    ## - product_sizesS                 1  129909897 94999722263 23822
    ## - product_colorsgrey             1  131138549 95000950915 23822
    ## - tags_count                     1  144052014 95013864381 23823
    ## <none>                                        94869812367 23823
    ## - merchant_rating                1  145650949 95015463316 23823
    ## - product_colorsblack            1  164676177 95034488544 23823
    ## - product_colorsOther_colors     1  169423963 95039236329 23823
    ## - price                          1  217426559 95087238926 23824
    ## - product_sizesOther_sizes       1  241348529 95111160896 23824
    ## - product_colorspurple           1  306356646 95176169013 23825
    ## - product_variation_inventory    1  359232591 95229044958 23826
    ## - product_sizesXXS               1  397134759 95266947126 23826
    ## - shipping_option_price          1  541773973 95411586339 23828
    ## - product_sizesXS                1  547898762 95417711129 23828
    ## - countries_shipped_to           1  579659954 95449472321 23829
    ## - `price_classEUR10-20`          1  657364928 95527177295 23830
    ## - product_sizesXXL               1  665780627 95535592994 23830
    ## - merchant_has_profile_picture1  1 1041615297 95911427663 23835
    ## - merchant_rating_count          1 4030984556 98900796922 23875
    ## 
    ## Step:  AIC=23820.56
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1     279476 94870144558 23819
    ## - product_sizesM                 1     723425 94870588506 23819
    ## - product_colorspink             1     875613 94870740695 23819
    ## - shipping_is_express1           1     993739 94870858820 23819
    ## - shipping_nameOther_shipping    1    2807085 94872672166 23819
    ## - retail_price                   1   17764859 94887629940 23819
    ## - origin_countryVE               1   18819395 94888684477 23819
    ## - `price_classEUR40-50`          1   23952145 94893817227 23819
    ## - `price_classEUR20-30`          1   27287072 94897152154 23819
    ## - uses_ad_boosts1                1   28496216 94898361297 23819
    ## - product_colorsred              1   32551307 94902416389 23819
    ## - badge_local_product1           1   39006364 94908871446 23819
    ## - badges_count                   1   52648579 94922513661 23819
    ## - product_colorsblue             1   56972451 94926837533 23819
    ## - product_colorsgreen            1   65681581 94935546662 23820
    ## - discount_per                   1   72675685 94942540766 23820
    ## - rating                         1   85569145 94955434226 23820
    ## - badge_product_quality1         1   87383137 94957248218 23820
    ## - origin_countryUS               1  103807790 94973672872 23820
    ## - product_sizesS                 1  130151360 95000016442 23820
    ## - tags_count                     1  144182292 95014047373 23821
    ## <none>                                        94869865081 23821
    ## - merchant_rating                1  145627918 95015492999 23821
    ## - product_colorswhite            1  182279406 95052144488 23821
    ## - price                          1  217612174 95087477256 23822
    ## - product_sizesOther_sizes       1  243046821 95112911902 23822
    ## - product_colorsgrey             1  274320606 95144185687 23822
    ## - product_variation_inventory    1  359644202 95229509283 23824
    ## - product_sizesXXS               1  399592595 95269457676 23824
    ## - product_colorsblack            1  474418754 95344283835 23825
    ## - product_colorsOther_colors     1  518692567 95388557648 23826
    ## - shipping_option_price          1  541781064 95411646145 23826
    ## - product_sizesXS                1  547849601 95417714683 23826
    ## - product_colorspurple           1  548299947 95418165029 23826
    ## - countries_shipped_to           1  582691860 95452556942 23827
    ## - `price_classEUR10-20`          1  657378332 95527243413 23828
    ## - product_sizesXXL               1  666226460 95536091541 23828
    ## - merchant_has_profile_picture1  1 1041567772 95911432853 23833
    ## - merchant_rating_count          1 4039235033 98909100115 23873
    ## 
    ## Step:  AIC=23818.56
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1     727390 94870871947 23817
    ## - product_colorspink             1     873937 94871018494 23817
    ## - shipping_is_express1           1     984179 94871128737 23817
    ## - shipping_nameOther_shipping    1    2812615 94872957172 23817
    ## - retail_price                   1   17785762 94887930320 23817
    ## - origin_countryVE               1   18829030 94888973587 23817
    ## - `price_classEUR40-50`          1   24032410 94894176968 23817
    ## - `price_classEUR20-30`          1   27405428 94897549985 23817
    ## - uses_ad_boosts1                1   28329920 94898474478 23817
    ## - product_colorsred              1   32523668 94902668226 23817
    ## - badge_local_product1           1   39000274 94909144832 23817
    ## - badges_count                   1   52654207 94922798765 23817
    ## - product_colorsblue             1   58006422 94928150979 23817
    ## - product_colorsgreen            1   65641221 94935785779 23818
    ## - discount_per                   1   72877891 94943022449 23818
    ## - rating                         1   85835853 94955980411 23818
    ## - badge_product_quality1         1   87362878 94957507436 23818
    ## - origin_countryUS               1  103789937 94973934494 23818
    ## - product_sizesS                 1  130164923 95000309481 23818
    ## - tags_count                     1  144571335 95014715892 23819
    ## <none>                                        94870144558 23819
    ## - merchant_rating                1  145537359 95015681917 23819
    ## - product_colorswhite            1  182336553 95052481111 23819
    ## - price                          1  217759061 95087903619 23820
    ## - product_sizesOther_sizes       1  243219739 95113364297 23820
    ## - product_colorsgrey             1  274265978 95144410536 23820
    ## - product_variation_inventory    1  359391837 95229536395 23822
    ## - product_sizesXXS               1  399906472 95270051030 23822
    ## - product_colorsblack            1  474513795 95344658352 23823
    ## - product_colorsOther_colors     1  518679536 95388824093 23824
    ## - shipping_option_price          1  541528015 95411672572 23824
    ## - product_sizesXS                1  547658077 95417802635 23824
    ## - product_colorspurple           1  548289244 95418433801 23824
    ## - countries_shipped_to           1  582627994 95452772551 23825
    ## - `price_classEUR10-20`          1  658650877 95528795434 23826
    ## - product_sizesXXL               1  666357803 95536502360 23826
    ## - merchant_has_profile_picture1  1 1041353316 95911497874 23831
    ## - merchant_rating_count          1 4039365580 98909510137 23871
    ## 
    ## Step:  AIC=23816.57
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1     869266 94871741213 23815
    ## - shipping_is_express1           1     960523 94871832471 23815
    ## - shipping_nameOther_shipping    1    2816864 94873688812 23815
    ## - retail_price                   1   17720443 94888592390 23815
    ## - origin_countryVE               1   18870965 94889742913 23815
    ## - `price_classEUR40-50`          1   24005480 94894877428 23815
    ## - `price_classEUR20-30`          1   27525355 94898397303 23815
    ## - uses_ad_boosts1                1   28368528 94899240475 23815
    ## - product_colorsred              1   32820249 94903692197 23815
    ## - badge_local_product1           1   39488470 94910360417 23815
    ## - badges_count                   1   52640104 94923512051 23815
    ## - product_colorsblue             1   57533500 94928405447 23815
    ## - product_colorsgreen            1   66057627 94936929574 23816
    ## - discount_per                   1   72828749 94943700696 23816
    ## - rating                         1   86321272 94957193219 23816
    ## - badge_product_quality1         1   87288264 94958160212 23816
    ## - origin_countryUS               1  105132515 94976004462 23816
    ## <none>                                        94870871947 23817
    ## - tags_count                     1  145079835 95015951782 23817
    ## - merchant_rating                1  145225211 95016097159 23817
    ## - product_colorswhite            1  181809383 95052681330 23817
    ## - price                          1  217193793 95088065740 23818
    ## - product_colorsgrey             1  273710870 95144582817 23818
    ## - product_variation_inventory    1  359192252 95230064200 23820
    ## - product_sizesS                 1  446812259 95317684206 23821
    ## - product_sizesOther_sizes       1  470438226 95341310173 23821
    ## - product_colorsblack            1  473867528 95344739475 23821
    ## - product_colorsOther_colors     1  520473720 95391345667 23822
    ## - shipping_option_price          1  540816005 95411687952 23822
    ## - product_colorspurple           1  548402608 95419274555 23822
    ## - countries_shipped_to           1  582194279 95453066226 23823
    ## - `price_classEUR10-20`          1  657987669 95528859617 23824
    ## - product_sizesXXS               1  746665833 95617537780 23825
    ## - product_sizesXXL               1  835673764 95706545711 23826
    ## - merchant_has_profile_picture1  1 1040858613 95911730560 23829
    ## - product_sizesXS                1 1522537366 96393409314 23836
    ## - merchant_rating_count          1 4038644684 98909516631 23869
    ## 
    ## Step:  AIC=23814.59
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1     968389 94872709602 23813
    ## - shipping_nameOther_shipping    1    2889133 94874630346 23813
    ## - retail_price                   1   17519917 94889261130 23813
    ## - origin_countryVE               1   18861916 94890603129 23813
    ## - `price_classEUR40-50`          1   24038492 94895779705 23813
    ## - `price_classEUR20-30`          1   27562998 94899304211 23813
    ## - uses_ad_boosts1                1   28225645 94899966858 23813
    ## - product_colorsred              1   34650805 94906392018 23813
    ## - badge_local_product1           1   39475076 94911216289 23813
    ## - badges_count                   1   52689864 94924431077 23813
    ## - product_colorsblue             1   63045102 94934786315 23814
    ## - product_colorsgreen            1   72106912 94943848124 23814
    ## - discount_per                   1   72624167 94944365380 23814
    ## - rating                         1   86567460 94958308673 23814
    ## - badge_product_quality1         1   87731374 94959472587 23814
    ## - origin_countryUS               1  106786652 94978527865 23814
    ## <none>                                        94871741213 23815
    ## - merchant_rating                1  144902287 95016643500 23815
    ## - tags_count                     1  145348061 95017089274 23815
    ## - price                          1  217473344 95089214557 23816
    ## - product_colorswhite            1  232692510 95104433723 23816
    ## - product_colorsgrey             1  313563073 95185304286 23817
    ## - product_variation_inventory    1  358732359 95230473572 23818
    ## - product_sizesS                 1  446015398 95317756611 23819
    ## - product_sizesOther_sizes       1  472340594 95344081807 23819
    ## - shipping_option_price          1  540947818 95412689031 23820
    ## - countries_shipped_to           1  581434716 95453175928 23821
    ## - product_colorspurple           1  602834030 95474575243 23821
    ## - product_colorsblack            1  632527086 95504268299 23821
    ## - `price_classEUR10-20`          1  659637915 95531379128 23822
    ## - product_colorsOther_colors     1  705589582 95577330795 23822
    ## - product_sizesXXS               1  747550879 95619292092 23823
    ## - product_sizesXXL               1  835947988 95707689201 23824
    ## - merchant_has_profile_picture1  1 1040098541 95911839754 23827
    ## - product_sizesXS                1 1523610578 96395351791 23834
    ## - merchant_rating_count          1 4038455764 98910196977 23867
    ## 
    ## Step:  AIC=23812.6
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1    2442187 94875151788 23811
    ## - retail_price                   1   17331351 94890040953 23811
    ## - origin_countryVE               1   18767285 94891476887 23811
    ## - `price_classEUR40-50`          1   23081992 94895791593 23811
    ## - uses_ad_boosts1                1   28553773 94901263375 23811
    ## - `price_classEUR20-30`          1   28893195 94901602797 23811
    ## - product_colorsred              1   34428842 94907138443 23811
    ## - badge_local_product1           1   38526651 94911236253 23811
    ## - badges_count                   1   53150243 94925859845 23811
    ## - product_colorsblue             1   63197115 94935906717 23812
    ## - product_colorsgreen            1   71609065 94944318667 23812
    ## - discount_per                   1   72359102 94945068703 23812
    ## - rating                         1   86038151 94958747753 23812
    ## - badge_product_quality1         1   89702273 94962411875 23812
    ## - origin_countryUS               1  107069169 94979778771 23812
    ## <none>                                        94872709602 23813
    ## - tags_count                     1  144835824 95017545425 23813
    ## - merchant_rating                1  146735065 95019444667 23813
    ## - price                          1  219601153 95092310755 23814
    ## - product_colorswhite            1  233471344 95106180946 23814
    ## - product_colorsgrey             1  313744428 95186454029 23815
    ## - product_variation_inventory    1  357817595 95230527197 23816
    ## - product_sizesS                 1  445116641 95317826242 23817
    ## - product_sizesOther_sizes       1  472036847 95344746449 23817
    ## - shipping_option_price          1  541363449 95414073051 23818
    ## - countries_shipped_to           1  580857796 95453567398 23819
    ## - product_colorspurple           1  603275601 95475985203 23819
    ## - product_colorsblack            1  632746763 95505456364 23819
    ## - `price_classEUR10-20`          1  677612256 95550321858 23820
    ## - product_colorsOther_colors     1  710204956 95582914557 23820
    ## - product_sizesXXS               1  746738260 95619447862 23821
    ## - product_sizesXXL               1  836954415 95709664017 23822
    ## - merchant_has_profile_picture1  1 1039346793 95912056394 23825
    ## - product_sizesXS                1 1523042864 96395752466 23832
    ## - merchant_rating_count          1 4043176319 98915885921 23865
    ## 
    ## Step:  AIC=23810.63
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1   18084449 94893236238 23809
    ## - origin_countryVE               1   18713864 94893865653 23809
    ## - `price_classEUR40-50`          1   23005514 94898157302 23809
    ## - uses_ad_boosts1                1   27984510 94903136298 23809
    ## - `price_classEUR20-30`          1   29040672 94904192460 23809
    ## - product_colorsred              1   35808651 94910960439 23809
    ## - badge_local_product1           1   39145608 94914297397 23809
    ## - badges_count                   1   53924913 94929076702 23809
    ## - product_colorsblue             1   65464302 94940616091 23810
    ## - product_colorsgreen            1   72365140 94947516928 23810
    ## - discount_per                   1   73155388 94948307177 23810
    ## - rating                         1   85923697 94961075486 23810
    ## - badge_product_quality1         1   90326716 94965478504 23810
    ## - origin_countryUS               1  106070107 94981221896 23810
    ## <none>                                        94875151788 23811
    ## - tags_count                     1  145267651 95020419439 23811
    ## - merchant_rating                1  146144095 95021295884 23811
    ## - price                          1  218726884 95093878672 23812
    ## - product_colorswhite            1  237045985 95112197773 23812
    ## - product_colorsgrey             1  317686067 95192837856 23813
    ## - product_variation_inventory    1  358191995 95233343783 23814
    ## - product_sizesS                 1  444782047 95319933836 23815
    ## - product_sizesOther_sizes       1  474627278 95349779066 23815
    ## - shipping_option_price          1  545172241 95420324030 23816
    ## - countries_shipped_to           1  580008210 95455159999 23817
    ## - product_colorspurple           1  611323231 95486475019 23817
    ## - product_colorsblack            1  640811709 95515963498 23818
    ## - `price_classEUR10-20`          1  675518829 95550670617 23818
    ## - product_colorsOther_colors     1  716324580 95591476369 23819
    ## - product_sizesXXS               1  744700215 95619852004 23819
    ## - product_sizesXXL               1  838333263 95713485052 23820
    ## - merchant_has_profile_picture1  1 1044213340 95919365128 23823
    ## - product_sizesXS                1 1524253124 96399404912 23830
    ## - merchant_rating_count          1 4045508643 98920660432 23863
    ## 
    ## Step:  AIC=23808.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   18991461 94912227699 23807
    ## - `price_classEUR40-50`          1   25105035 94918341272 23807
    ## - `price_classEUR20-30`          1   26138912 94919375149 23807
    ## - uses_ad_boosts1                1   27017588 94920253826 23807
    ## - product_colorsred              1   35683252 94928919490 23807
    ## - badge_local_product1           1   41546951 94934783189 23808
    ## - badges_count                   1   57319869 94950556107 23808
    ## - product_colorsblue             1   65159242 94958395480 23808
    ## - discount_per                   1   67896445 94961132682 23808
    ## - product_colorsgreen            1   71148074 94964384312 23808
    ## - rating                         1   84765682 94978001920 23808
    ## - badge_product_quality1         1   94553908 94987790146 23808
    ## - origin_countryUS               1  107961470 95001197708 23808
    ## - tags_count                     1  142738687 95035974925 23809
    ## <none>                                        94893236238 23809
    ## - merchant_rating                1  149932361 95043168599 23809
    ## - product_colorswhite            1  237482579 95130718817 23810
    ## - price                          1  238993123 95132229361 23810
    ## - product_colorsgrey             1  313829635 95207065873 23811
    ## - product_variation_inventory    1  355554945 95248791183 23812
    ## - product_sizesS                 1  439807207 95333043444 23813
    ## - product_sizesOther_sizes       1  475037531 95368273769 23813
    ## - shipping_option_price          1  537620490 95430856728 23814
    ## - countries_shipped_to           1  582103056 95475339294 23815
    ## - product_colorspurple           1  612389179 95505625416 23815
    ## - product_colorsblack            1  653327736 95546563974 23816
    ## - `price_classEUR10-20`          1  670993307 95564229545 23816
    ## - product_colorsOther_colors     1  712660454 95605896692 23817
    ## - product_sizesXXS               1  754241923 95647478161 23817
    ## - product_sizesXXL               1  848212507 95741448745 23819
    ## - merchant_has_profile_picture1  1 1056505746 95949741983 23821
    ## - product_sizesXS                1 1514174050 96407410288 23828
    ## - merchant_rating_count          1 4101304741 98994540979 23862
    ## 
    ## Step:  AIC=23807.15
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   24454102 94936681800 23806
    ## - `price_classEUR20-30`          1   24704700 94936932399 23806
    ## - uses_ad_boosts1                1   30270306 94942498005 23806
    ## - product_colorsred              1   35351607 94947579306 23806
    ## - badge_local_product1           1   41769932 94953997631 23806
    ## - badges_count                   1   57264530 94969492228 23806
    ## - product_colorsblue             1   65206609 94977434308 23806
    ## - product_colorsgreen            1   70762301 94982990000 23806
    ## - discount_per                   1   71087724 94983315423 23806
    ## - badge_product_quality1         1   93540620 95005768319 23806
    ## - rating                         1   97777334 95010005033 23807
    ## - origin_countryUS               1  107111657 95019339356 23807
    ## - tags_count                     1  141709053 95053936751 23807
    ## <none>                                        94912227699 23807
    ## - merchant_rating                1  154805343 95067033042 23807
    ## - product_colorswhite            1  228315116 95140542814 23808
    ## - price                          1  230786200 95143013899 23808
    ## - product_colorsgrey             1  312189257 95224416955 23810
    ## - product_variation_inventory    1  358847701 95271075400 23810
    ## - product_sizesS                 1  435429642 95347657340 23811
    ## - product_sizesOther_sizes       1  471520210 95383747909 23812
    ## - shipping_option_price          1  526290598 95438518296 23812
    ## - countries_shipped_to           1  582454161 95494681859 23813
    ## - product_colorspurple           1  608781520 95521009218 23814
    ## - product_colorsblack            1  641992408 95554220107 23814
    ## - `price_classEUR10-20`          1  663207095 95575434794 23814
    ## - product_colorsOther_colors     1  709460498 95621688197 23815
    ## - product_sizesXXS               1  747136239 95659363938 23815
    ## - product_sizesXXL               1  842858700 95755086399 23817
    ## - merchant_has_profile_picture1  1 1062278673 95974506371 23820
    ## - product_sizesXS                1 1521320258 96433547956 23826
    ## - merchant_rating_count          1 4104703982 99016931680 23861
    ## 
    ## Step:  AIC=23805.48
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + badges_count + 
    ##     badge_local_product1 + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR20-30`          1   14492312 94951174112 23804
    ## - product_colorsred              1   31060931 94967742731 23804
    ## - uses_ad_boosts1                1   31558049 94968239849 23804
    ## - badge_local_product1           1   44613347 94981295147 23804
    ## - badges_count                   1   60391127 94997072927 23804
    ## - product_colorsgreen            1   63669498 95000351299 23804
    ## - product_colorsblue             1   67104563 95003786363 23804
    ## - discount_per                   1   70466545 95007148345 23805
    ## - badge_product_quality1         1   97108943 95033790743 23805
    ## - rating                         1  101508413 95038190213 23805
    ## - origin_countryUS               1  104822664 95041504464 23805
    ## <none>                                        94936681800 23806
    ## - tags_count                     1  146253758 95082935558 23806
    ## - merchant_rating                1  167271502 95103953302 23806
    ## - price                          1  206601154 95143282954 23806
    ## - product_colorswhite            1  224853730 95161535530 23807
    ## - product_colorsgrey             1  311051393 95247733193 23808
    ## - product_variation_inventory    1  361350332 95298032132 23809
    ## - product_sizesS                 1  445108958 95381790759 23810
    ## - shipping_option_price          1  509616369 95446298169 23811
    ## - product_sizesOther_sizes       1  517532015 95454213815 23811
    ## - countries_shipped_to           1  586840309 95523522109 23812
    ## - product_colorspurple           1  607761786 95544443586 23812
    ## - product_colorsblack            1  638428781 95575110581 23812
    ## - `price_classEUR10-20`          1  677842437 95614524237 23813
    ## - product_colorsOther_colors     1  707190342 95643872142 23813
    ## - product_sizesXXS               1  770170786 95706852586 23814
    ## - product_sizesXXL               1  843778225 95780460025 23815
    ## - merchant_has_profile_picture1  1 1071668118 96008349918 23818
    ## - product_sizesXS                1 1547744594 96484426394 23825
    ## - merchant_rating_count          1 4096079973 99032761773 23859
    ## 
    ## Step:  AIC=23803.68
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1   31996914 94983171027 23802
    ## - uses_ad_boosts1                1   35751486 94986925599 23802
    ## - badge_local_product1           1   42327436 94993501549 23802
    ## - badges_count                   1   56722337 95007896449 23803
    ## - product_colorsgreen            1   63855305 95015029417 23803
    ## - product_colorsblue             1   67898727 95019072839 23803
    ## - discount_per                   1   76513203 95027687316 23803
    ## - badge_product_quality1         1   93073002 95044247114 23803
    ## - origin_countryUS               1  102901414 95054075526 23803
    ## - rating                         1  105704549 95056878661 23803
    ## <none>                                        94951174112 23804
    ## - tags_count                     1  146802353 95097976465 23804
    ## - merchant_rating                1  164326055 95115500167 23804
    ## - price                          1  194513985 95145688097 23804
    ## - product_colorswhite            1  222192193 95173366306 23805
    ## - product_colorsgrey             1  310455478 95261629591 23806
    ## - product_variation_inventory    1  368512956 95319687068 23807
    ## - product_sizesS                 1  445464508 95396638621 23808
    ## - shipping_option_price          1  495311504 95446485616 23809
    ## - product_sizesOther_sizes       1  515613073 95466787185 23809
    ## - countries_shipped_to           1  587420490 95538594603 23810
    ## - product_colorspurple           1  606738066 95557912178 23810
    ## - product_colorsblack            1  631233647 95582407760 23810
    ## - product_colorsOther_colors     1  700826626 95652000738 23811
    ## - `price_classEUR10-20`          1  713298816 95664472928 23812
    ## - product_sizesXXS               1  774419243 95725593355 23812
    ## - product_sizesXXL               1  842346087 95793520199 23813
    ## - merchant_has_profile_picture1  1 1076627988 96027802100 23817
    ## - product_sizesXS                1 1556004864 96507178977 23823
    ## - merchant_rating_count          1 4102585346 99053759458 23857
    ## 
    ## Step:  AIC=23802.13
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1   35648931 95018819958 23801
    ## - product_colorsgreen            1   44992740 95028163766 23801
    ## - badge_local_product1           1   45288954 95028459980 23801
    ## - product_colorsblue             1   48155540 95031326566 23801
    ## - badges_count                   1   59394493 95042565519 23801
    ## - discount_per                   1   75716016 95058887043 23801
    ## - badge_product_quality1         1   95597804 95078768830 23801
    ## - origin_countryUS               1  106866266 95090037293 23802
    ## - rating                         1  110407955 95093578981 23802
    ## <none>                                        94983171027 23802
    ## - tags_count                     1  154963259 95138134285 23802
    ## - merchant_rating                1  169035605 95152206631 23803
    ## - product_colorswhite            1  190198856 95173369882 23803
    ## - price                          1  195002723 95178173749 23803
    ## - product_colorsgrey             1  279244151 95262415177 23804
    ## - product_variation_inventory    1  362897792 95346068819 23805
    ## - product_sizesS                 1  449751040 95432922067 23806
    ## - shipping_option_price          1  491660052 95474831079 23807
    ## - product_sizesOther_sizes       1  516124025 95499295052 23807
    ## - product_colorspurple           1  574787058 95557958085 23808
    ## - countries_shipped_to           1  599714011 95582885038 23808
    ## - product_colorsblack            1  618768049 95601939076 23809
    ## - product_colorsOther_colors     1  696600812 95679771839 23810
    ## - `price_classEUR10-20`          1  719497556 95702668583 23810
    ## - product_sizesXXS               1  792395278 95775566304 23811
    ## - product_sizesXXL               1  842097446 95825268472 23812
    ## - merchant_has_profile_picture1  1 1062326539 96045497566 23815
    ## - product_sizesXS                1 1564662849 96547833876 23822
    ## - merchant_rating_count          1 4100479363 99083650390 23856
    ## 
    ## Step:  AIC=23800.62
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1   40714970 95059534928 23799
    ## - product_colorsgreen            1   43155093 95061975051 23799
    ## - product_colorsblue             1   49465684 95068285642 23799
    ## - badges_count                   1   57455564 95076275522 23799
    ## - discount_per                   1   75383056 95094203014 23800
    ## - badge_product_quality1         1   92809132 95111629090 23800
    ## - origin_countryUS               1  109812172 95128632130 23800
    ## - rating                         1  117338942 95136158899 23800
    ## <none>                                        95018819958 23801
    ## - tags_count                     1  159656872 95178476830 23801
    ## - merchant_rating                1  175415908 95194235866 23801
    ## - product_colorswhite            1  190164958 95208984916 23801
    ## - price                          1  198696708 95217516666 23801
    ## - product_colorsgrey             1  279252916 95298072874 23803
    ## - product_variation_inventory    1  390405337 95409225295 23804
    ## - product_sizesS                 1  440187291 95459007249 23805
    ## - shipping_option_price          1  493732508 95512552466 23805
    ## - product_sizesOther_sizes       1  539580224 95558400182 23806
    ## - product_colorspurple           1  581457760 95600277717 23807
    ## - countries_shipped_to           1  596357112 95615177070 23807
    ## - product_colorsblack            1  610989152 95629809109 23807
    ## - product_colorsOther_colors     1  687007296 95705827254 23808
    ## - `price_classEUR10-20`          1  726922023 95745741981 23809
    ## - product_sizesXXS               1  809567707 95828387665 23810
    ## - product_sizesXXL               1  834309171 95853129129 23810
    ## - merchant_has_profile_picture1  1 1053377707 96072197665 23813
    ## - product_sizesXS                1 1551547689 96570367647 23820
    ## - merchant_rating_count          1 4074699520 99093519478 23854
    ## 
    ## Step:  AIC=23799.18
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1   16759587 95076294514 23797
    ## - product_colorsgreen            1   38458153 95097993080 23798
    ## - product_colorsblue             1   47595724 95107130651 23798
    ## - badge_product_quality1         1   55620927 95115155855 23798
    ## - discount_per                   1   75910431 95135445359 23798
    ## - origin_countryUS               1  116664362 95176199290 23799
    ## - rating                         1  119860252 95179395179 23799
    ## <none>                                        95059534928 23799
    ## - tags_count                     1  162746494 95222281422 23799
    ## - merchant_rating                1  178978413 95238513340 23800
    ## - product_colorswhite            1  188842915 95248377843 23800
    ## - price                          1  190722653 95250257581 23800
    ## - product_colorsgrey             1  271908565 95331443493 23801
    ## - product_variation_inventory    1  422240087 95481775015 23803
    ## - product_sizesS                 1  439167150 95498702078 23803
    ## - shipping_option_price          1  496478464 95556013392 23804
    ## - product_sizesOther_sizes       1  565007619 95624542546 23805
    ## - product_colorspurple           1  583478898 95643013826 23805
    ## - product_colorsblack            1  595429835 95654964762 23805
    ## - countries_shipped_to           1  597331743 95656866671 23805
    ## - product_colorsOther_colors     1  674308129 95733843056 23807
    ## - `price_classEUR10-20`          1  705408644 95764943572 23807
    ## - product_sizesXXS               1  810296478 95869831405 23808
    ## - product_sizesXXL               1  840695193 95900230121 23809
    ## - merchant_has_profile_picture1  1 1039675498 96099210426 23812
    ## - product_sizesXS                1 1529466534 96589001462 23818
    ## - merchant_rating_count          1 4084655152 99144190080 23852
    ## 
    ## Step:  AIC=23797.41
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   37437142 95113731656 23796
    ## - badge_product_quality1         1   49443797 95125738311 23796
    ## - product_colorsblue             1   50265482 95126559997 23796
    ## - discount_per                   1   78668145 95154962659 23797
    ## - origin_countryUS               1  116457267 95192751782 23797
    ## - rating                         1  125982893 95202277407 23797
    ## <none>                                        95076294514 23797
    ## - tags_count                     1  158495425 95234789940 23798
    ## - merchant_rating                1  167061087 95243355601 23798
    ## - product_colorswhite            1  191820844 95268115359 23798
    ## - price                          1  193265545 95269560059 23798
    ## - product_colorsgrey             1  271313888 95347608402 23799
    ## - product_sizesS                 1  439045719 95515340233 23802
    ## - product_variation_inventory    1  443455592 95519750107 23802
    ## - shipping_option_price          1  515745022 95592039536 23803
    ## - product_colorspurple           1  583610066 95659904581 23803
    ## - product_sizesOther_sizes       1  590375999 95666670513 23804
    ## - product_colorsblack            1  594987976 95671282490 23804
    ## - countries_shipped_to           1  601493603 95677788118 23804
    ## - product_colorsOther_colors     1  673032650 95749327165 23805
    ## - `price_classEUR10-20`          1  710888218 95787182733 23805
    ## - product_sizesXXS               1  812936880 95889231394 23807
    ## - product_sizesXXL               1  861639916 95937934430 23807
    ## - merchant_has_profile_picture1  1 1047378931 96123673445 23810
    ## - product_sizesXS                1 1532033047 96608327561 23816
    ## - merchant_rating_count          1 4107947893 99184242407 23851
    ## 
    ## Step:  AIC=23795.93
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsblue             1   35193525 95148925181 23794
    ## - badge_product_quality1         1   50860678 95164592334 23795
    ## - discount_per                   1   74376761 95188108417 23795
    ## - origin_countryUS               1  117527566 95231259222 23796
    ## - rating                         1  135583664 95249315320 23796
    ## <none>                                        95113731656 23796
    ## - tags_count                     1  157952888 95271684544 23796
    ## - product_colorswhite            1  159309458 95273041114 23796
    ## - merchant_rating                1  163112394 95276844050 23796
    ## - price                          1  195930447 95309662103 23797
    ## - product_colorsgrey             1  241846784 95355578440 23797
    ## - product_variation_inventory    1  417745761 95531477417 23800
    ## - product_sizesS                 1  440628031 95554359687 23800
    ## - shipping_option_price          1  519615929 95633347585 23801
    ## - product_colorspurple           1  550959069 95664690725 23802
    ## - product_colorsblack            1  558810479 95672542135 23802
    ## - product_sizesOther_sizes       1  585461656 95699193312 23802
    ## - countries_shipped_to           1  589439762 95703171418 23802
    ## - product_colorsOther_colors     1  639569912 95753301568 23803
    ## - `price_classEUR10-20`          1  711776153 95825507809 23804
    ## - product_sizesXXS               1  816253115 95929984771 23805
    ## - product_sizesXXL               1  861611271 95975342927 23806
    ## - merchant_has_profile_picture1  1 1069798414 96183530070 23809
    ## - product_sizesXS                1 1547632886 96661364542 23815
    ## - merchant_rating_count          1 4105923450 99219655106 23849
    ## 
    ## Step:  AIC=23794.41
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1   50316181 95199241362 23793
    ## - discount_per                   1   72165709 95221090890 23793
    ## - origin_countryUS               1  123480292 95272405473 23794
    ## - product_colorswhite            1  131501550 95280426731 23794
    ## - rating                         1  131951092 95280876273 23794
    ## <none>                                        95148925181 23794
    ## - tags_count                     1  157754603 95306679784 23795
    ## - merchant_rating                1  161383183 95310308364 23795
    ## - price                          1  195467971 95344393153 23795
    ## - product_colorsgrey             1  218285727 95367210908 23795
    ## - product_variation_inventory    1  437396775 95586321956 23798
    ## - product_sizesS                 1  447263003 95596188184 23799
    ## - shipping_option_price          1  515107732 95664032913 23800
    ## - product_colorspurple           1  523703547 95672628728 23800
    ## - product_colorsblack            1  524521282 95673446463 23800
    ## - countries_shipped_to           1  560124760 95709049942 23800
    ## - product_sizesOther_sizes       1  577118373 95726043554 23800
    ## - product_colorsOther_colors     1  605683307 95754608488 23801
    ## - `price_classEUR10-20`          1  704677942 95853603123 23802
    ## - product_sizesXXS               1  803984012 95952909193 23804
    ## - product_sizesXXL               1  850280588 95999205770 23804
    ## - merchant_has_profile_picture1  1 1060480107 96209405289 23807
    ## - product_sizesXS                1 1532953496 96681878677 23813
    ## - merchant_rating_count          1 4093255207 99242180388 23848
    ## 
    ## Step:  AIC=23793.11
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - discount_per                   1   75461571 95274702933 23792
    ## - origin_countryUS               1  116687060 95315928422 23793
    ## - product_colorswhite            1  130355686 95329597048 23793
    ## <none>                                        95199241362 23793
    ## - tags_count                     1  154193318 95353434680 23793
    ## - merchant_rating                1  183064639 95382306001 23794
    ## - rating                         1  189777044 95389018406 23794
    ## - price                          1  194026042 95393267404 23794
    ## - product_colorsgrey             1  230241486 95429482848 23794
    ## - product_sizesS                 1  446588507 95645829870 23797
    ## - product_variation_inventory    1  448507293 95647748655 23797
    ## - shipping_option_price          1  516128657 95715370019 23798
    ## - product_colorspurple           1  524550333 95723791695 23798
    ## - product_colorsblack            1  539073660 95738315022 23799
    ## - countries_shipped_to           1  570125332 95769366694 23799
    ## - product_sizesOther_sizes       1  581544785 95780786147 23799
    ## - product_colorsOther_colors     1  606807594 95806048956 23799
    ## - `price_classEUR10-20`          1  696676924 95895918286 23801
    ## - product_sizesXXS               1  814411802 96013653164 23802
    ## - product_sizesXXL               1  845727409 96044968771 23803
    ## - merchant_has_profile_picture1  1 1069590095 96268831457 23806
    ## - product_sizesXS                1 1519179408 96718420770 23812
    ## - merchant_rating_count          1 4190604464 99389845826 23848
    ## 
    ## Step:  AIC=23792.15
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorswhite            1  123165769 95397868702 23792
    ## - origin_countryUS               1  125283418 95399986351 23792
    ## - tags_count                     1  128049481 95402752414 23792
    ## <none>                                        95274702933 23792
    ## - rating                         1  183254327 95457957260 23793
    ## - merchant_rating                1  186256018 95460958951 23793
    ## - price                          1  207200953 95481903886 23793
    ## - product_colorsgrey             1  228856888 95503559821 23793
    ## - product_sizesS                 1  446655112 95721358045 23796
    ## - product_variation_inventory    1  452741000 95727443933 23796
    ## - shipping_option_price          1  512866289 95787569222 23797
    ## - product_colorspurple           1  523092608 95797795541 23797
    ## - product_colorsblack            1  544741059 95819443992 23798
    ## - product_sizesOther_sizes       1  576421231 95851124164 23798
    ## - countries_shipped_to           1  584089484 95858792417 23798
    ## - product_colorsOther_colors     1  608766296 95883469229 23799
    ## - `price_classEUR10-20`          1  719364265 95994067198 23800
    ## - product_sizesXXS               1  794319031 96069021964 23801
    ## - product_sizesXXL               1  830231978 96104934911 23802
    ## - merchant_has_profile_picture1  1 1077861712 96352564645 23805
    ## - product_sizesXS                1 1481164952 96755867885 23810
    ## - merchant_rating_count          1 4179004271 99453707204 23847
    ## 
    ## Step:  AIC=23791.84
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryUS               1  130139317 95528008019 23792
    ## - tags_count                     1  139111913 95536980615 23792
    ## <none>                                        95397868702 23792
    ## - product_colorsgrey             1  171160898 95569029600 23792
    ## - rating                         1  189698632 95587567334 23792
    ## - merchant_rating                1  204688756 95602557458 23793
    ## - price                          1  205868145 95603736846 23793
    ## - product_colorsblack            1  431315237 95829183939 23796
    ## - product_colorspurple           1  454322841 95852191543 23796
    ## - product_sizesS                 1  457402667 95855271369 23796
    ## - product_variation_inventory    1  474778868 95872647570 23796
    ## - product_colorsOther_colors     1  493441295 95891309996 23797
    ## - shipping_option_price          1  524625111 95922493813 23797
    ## - product_sizesOther_sizes       1  596829711 95994698413 23798
    ## - countries_shipped_to           1  601146425 95999015127 23798
    ## - `price_classEUR10-20`          1  702651248 96100519950 23800
    ## - product_sizesXXS               1  775029071 96172897772 23801
    ## - product_sizesXXL               1  826515803 96224384504 23801
    ## - merchant_has_profile_picture1  1 1064271864 96462140566 23804
    ## - product_sizesXS                1 1544765652 96942634354 23811
    ## - merchant_rating_count          1 4198034540 99595903242 23846
    ## 
    ## Step:  AIC=23791.63
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - tags_count                     1  126654816 95654662835 23791
    ## <none>                                        95528008019 23792
    ## - product_colorsgrey             1  162971816 95690979835 23792
    ## - rating                         1  181465489 95709473508 23792
    ## - price                          1  201418585 95729426604 23792
    ## - merchant_rating                1  213397458 95741405477 23793
    ## - product_colorsblack            1  422959436 95950967455 23795
    ## - product_sizesS                 1  439973403 95967981421 23796
    ## - product_colorsOther_colors     1  457039784 95985047803 23796
    ## - product_variation_inventory    1  458845667 95986853686 23796
    ## - product_colorspurple           1  459850528 95987858547 23796
    ## - shipping_option_price          1  516715701 96044723720 23797
    ## - product_sizesOther_sizes       1  586354723 96114362741 23798
    ## - countries_shipped_to           1  636645813 96164653831 23798
    ## - `price_classEUR10-20`          1  708708498 96236716517 23799
    ## - product_sizesXXL               1  810030740 96338038759 23801
    ## - product_sizesXXS               1  817349693 96345357712 23801
    ## - merchant_has_profile_picture1  1 1100391496 96628399515 23805
    ## - product_sizesXS                1 1517589394 97045597413 23810
    ## - merchant_rating_count          1 4258458390 99786466409 23847
    ## 
    ## Step:  AIC=23791.37
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## <none>                                        95654662835 23791
    ## - product_colorsgrey             1  175091535 95829754370 23792
    ## - rating                         1  180243190 95834906025 23792
    ## - price                          1  216901544 95871564379 23792
    ## - merchant_rating                1  245423293 95900086128 23793
    ## - product_sizesS                 1  416767248 96071430082 23795
    ## - product_colorsOther_colors     1  429146269 96083809104 23795
    ## - product_colorspurple           1  436517596 96091180431 23795
    ## - product_colorsblack            1  436813692 96091476527 23795
    ## - product_variation_inventory    1  482991302 96137654137 23796
    ## - shipping_option_price          1  503223755 96157886589 23796
    ## - product_sizesOther_sizes       1  520009410 96174672245 23797
    ## - countries_shipped_to           1  608762328 96263425162 23798
    ## - `price_classEUR10-20`          1  707395684 96362058518 23799
    ## - product_sizesXXS               1  755479313 96410142148 23800
    ## - product_sizesXXL               1  777800950 96432463785 23800
    ## - merchant_has_profile_picture1  1 1068155664 96722818498 23804
    ## - product_sizesXS                1 1436656623 97091319458 23809
    ## - merchant_rating_count          1 4173409299 99828072133 23845
    ## Start:  AIC=23772.44
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23772.44
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23772.44
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - product_colorspink             1       30583  91174853238 23770
    ## - origin_countryGB               1      321013  91175143667 23770
    ## - badge_product_quality1         1     1048488  91175871142 23771
    ## - merchant_rating                1     1156866  91175979521 23771
    ## - product_sizesM                 1     1565631  91176388285 23771
    ## - product_colorsyellow           1     1697178  91176519833 23771
    ## - shipping_is_express1           1     2846742  91177669396 23771
    ## - badge_local_product1           1     4520663  91179343318 23771
    ## - uses_ad_boosts1                1     5148605  91179971260 23771
    ## - product_colorsred              1     7393906  91182216560 23771
    ## - badges_count                   1     8098386  91182921040 23771
    ## - product_colorsgreen            1    11049343  91185871998 23771
    ## - retail_price                   1    12096429  91186919084 23771
    ## - origin_countryVE               1    14659514  91189482169 23771
    ## - shipping_nameOther_shipping    1    23945894  91198768548 23771
    ## - `price_classEUR20-30`          1    24307204  91199129858 23771
    ## - product_colorsblue             1    31204212  91206026867 23771
    ## - origin_countryUS               1    43241993  91218064647 23771
    ## - `price_classEUR40-50`          1    45352301  91220174955 23771
    ## - product_colorswhite            1    56843128  91231665782 23771
    ## - product_colorsblack            1    67400290  91242222945 23771
    ## - origin_countrySG               1    80630844  91255453498 23772
    ## - product_colorspurple           1    98113878  91272936532 23772
    ## - product_colorsOther_colors     1   102250035  91277072690 23772
    ## <none>                                          91174822654 23772
    ## - product_colorsgrey             1   141180614  91316003268 23773
    ## - discount_per                   1   146456063  91321278717 23773
    ## - rating                         1   147069619  91321892273 23773
    ## - product_sizesS                 1   160626465  91335449119 23773
    ## - product_variation_inventory    1   208485690  91383308345 23773
    ## - product_sizesXXL               1   227625351  91402448005 23774
    ## - merchant_has_profile_picture1  1   257036142  91431858797 23774
    ## - product_sizesOther_sizes       1   301855369  91476678023 23775
    ## - price                          1   354437709  91529260363 23776
    ## - product_sizesXXS               1   410074737  91584897391 23776
    ## - countries_shipped_to           1   450861396  91625684051 23777
    ## - tags_count                     1   466116118  91640938772 23777
    ## - product_sizesXS                1   469674572  91644497226 23777
    ## - shipping_option_price          1   672792445  91847615099 23780
    ## - `price_classEUR10-20`          1   730807188  91905629842 23781
    ## - merchant_rating_count          1 11719514756 102894337411 23929
    ## 
    ## Step:  AIC=23770.44
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - origin_countryGB               1      322874  91175176112 23768
    ## - badge_product_quality1         1     1049259  91175902497 23769
    ## - merchant_rating                1     1140487  91175993724 23769
    ## - product_sizesM                 1     1584235  91176437473 23769
    ## - shipping_is_express1           1     2848509  91177701747 23769
    ## - badge_local_product1           1     4545043  91179398281 23769
    ## - product_colorsyellow           1     4779941  91179633179 23769
    ## - uses_ad_boosts1                1     5130658  91179983895 23769
    ## - badges_count                   1     8122559  91182975796 23769
    ## - retail_price                   1    12074996  91186928234 23769
    ## - origin_countryVE               1    14671990  91189525227 23769
    ## - product_colorsred              1    14953542  91189806780 23769
    ## - product_colorsgreen            1    22825469  91197678707 23769
    ## - shipping_nameOther_shipping    1    24104049  91198957287 23769
    ## - `price_classEUR20-30`          1    24313412  91199166650 23769
    ## - origin_countryUS               1    43215568  91218068805 23769
    ## - `price_classEUR40-50`          1    45391096  91220244334 23769
    ## - product_colorsblue             1    67282728  91242135966 23769
    ## - origin_countrySG               1    80613982  91255467220 23770
    ## <none>                                          91174853238 23770
    ## - discount_per                   1   146517162  91321370400 23771
    ## - rating                         1   147154009  91322007247 23771
    ## - product_colorswhite            1   159762415  91334615653 23771
    ## - product_sizesS                 1   160705245  91335558482 23771
    ## - product_colorspurple           1   179180141  91354033379 23771
    ## - product_colorsblack            1   195298374  91370151611 23771
    ## - product_variation_inventory    1   208653623  91383506860 23771
    ## - product_sizesXXL               1   227723077  91402576315 23772
    ## - merchant_has_profile_picture1  1   257005577  91431858815 23772
    ## - product_colorsgrey             1   291060008  91465913245 23773
    ## - product_sizesOther_sizes       1   303528398  91478381636 23773
    ## - product_colorsOther_colors     1   308756166  91483609403 23773
    ## - price                          1   354567688  91529420926 23774
    ## - product_sizesXXS               1   410915254  91585768491 23774
    ## - countries_shipped_to           1   451476643  91626329881 23775
    ## - tags_count                     1   466204648  91641057885 23775
    ## - product_sizesXS                1   469644409  91644497647 23775
    ## - shipping_option_price          1   673073731  91847926969 23778
    ## - `price_classEUR10-20`          1   730856018  91905709256 23779
    ## - merchant_rating_count          1 11741922145 102916775383 23927
    ## 
    ## Step:  AIC=23768.44
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - badge_product_quality1         1     1047451  91176223563 23767
    ## - merchant_rating                1     1150203  91176326315 23767
    ## - product_sizesM                 1     1577284  91176753396 23767
    ## - shipping_is_express1           1     2830820  91178006932 23767
    ## - badge_local_product1           1     4544946  91179721059 23767
    ## - product_colorsyellow           1     4761793  91179937905 23767
    ## - uses_ad_boosts1                1     5227105  91180403218 23767
    ## - badges_count                   1     8127372  91183303484 23767
    ## - retail_price                   1    12053324  91187229436 23767
    ## - origin_countryVE               1    14660492  91189836604 23767
    ## - product_colorsred              1    14986111  91190162223 23767
    ## - product_colorsgreen            1    22870801  91198046913 23767
    ## - shipping_nameOther_shipping    1    24088026  91199264138 23767
    ## - `price_classEUR20-30`          1    24218234  91199394346 23767
    ## - origin_countryUS               1    43220313  91218396425 23767
    ## - `price_classEUR40-50`          1    45289939  91220466051 23767
    ## - product_colorsblue             1    66968522  91242144634 23767
    ## - origin_countrySG               1    80661243  91255837355 23768
    ## <none>                                          91175176112 23768
    ## - discount_per                   1   146304240  91321480353 23769
    ## - rating                         1   146921895  91322098007 23769
    ## - product_colorswhite            1   159730707  91334906819 23769
    ## - product_sizesS                 1   160668177  91335844289 23769
    ## - product_colorspurple           1   179257429  91354433541 23769
    ## - product_colorsblack            1   195275799  91370451911 23769
    ## - product_variation_inventory    1   209577231  91384753343 23770
    ## - product_sizesXXL               1   227639289  91402815401 23770
    ## - merchant_has_profile_picture1  1   257095163  91432271275 23770
    ## - product_colorsgrey             1   291197682  91466373794 23771
    ## - product_sizesOther_sizes       1   303341381  91478517493 23771
    ## - product_colorsOther_colors     1   308829241  91484005353 23771
    ## - price                          1   354411006  91529587118 23772
    ## - product_sizesXXS               1   410689093  91585865205 23772
    ## - countries_shipped_to           1   451628842  91626804954 23773
    ## - tags_count                     1   465881912  91641058024 23773
    ## - product_sizesXS                1   469971362  91645147474 23773
    ## - shipping_option_price          1   673755391  91848931504 23776
    ## - `price_classEUR10-20`          1   730552298  91905728410 23777
    ## - merchant_rating_count          1 11741965572 102917141685 23925
    ## 
    ## Step:  AIC=23766.46
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - merchant_rating                1     1165424  91177388988 23765
    ## - product_sizesM                 1     1611267  91177834831 23765
    ## - shipping_is_express1           1     2085046  91178308610 23765
    ## - product_colorsyellow           1     4580675  91180804238 23765
    ## - badge_local_product1           1     4807265  91181030828 23765
    ## - uses_ad_boosts1                1     5230927  91181454490 23765
    ## - retail_price                   1    11860089  91188083653 23765
    ## - origin_countryVE               1    14780964  91191004527 23765
    ## - product_colorsred              1    14964257  91191187820 23765
    ## - product_colorsgreen            1    23262161  91199485724 23765
    ## - `price_classEUR20-30`          1    24274523  91200498086 23765
    ## - shipping_nameOther_shipping    1    24388164  91200611727 23765
    ## - badges_count                   1    29844708  91206068271 23765
    ## - origin_countryUS               1    42849476  91219073040 23765
    ## - `price_classEUR40-50`          1    45449002  91221672565 23765
    ## - product_colorsblue             1    67132895  91243356458 23765
    ## - origin_countrySG               1    80793620  91257017183 23766
    ## <none>                                          91176223563 23767
    ## - discount_per                   1   145470377  91321693941 23767
    ## - rating                         1   145983628  91322207191 23767
    ## - product_colorswhite            1   160677701  91336901264 23767
    ## - product_sizesS                 1   161063853  91337287417 23767
    ## - product_colorspurple           1   179509361  91355732925 23767
    ## - product_colorsblack            1   196052636  91372276199 23767
    ## - product_variation_inventory    1   209664151  91385887715 23768
    ## - product_sizesXXL               1   226780295  91403003859 23768
    ## - merchant_has_profile_picture1  1   257823443  91434047006 23768
    ## - product_colorsgrey             1   291887638  91468111201 23769
    ## - product_sizesOther_sizes       1   302513554  91478737117 23769
    ## - product_colorsOther_colors     1   310186721  91486410285 23769
    ## - price                          1   354581964  91530805527 23770
    ## - product_sizesXXS               1   411455544  91587679108 23770
    ## - countries_shipped_to           1   451620994  91627844557 23771
    ## - tags_count                     1   466842333  91643065896 23771
    ## - product_sizesXS                1   472781733  91649005296 23771
    ## - shipping_option_price          1   672814323  91849037887 23774
    ## - `price_classEUR10-20`          1   730104842  91906328405 23775
    ## - merchant_rating_count          1 11762368937 102938592501 23924
    ## 
    ## Step:  AIC=23764.48
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - product_sizesM                 1     1591393  91178980380 23763
    ## - shipping_is_express1           1     1895529  91179284517 23763
    ## - product_colorsyellow           1     4589115  91181978103 23763
    ## - badge_local_product1           1     4638285  91182027273 23763
    ## - uses_ad_boosts1                1     5399759  91182788746 23763
    ## - retail_price                   1    12141722  91189530709 23763
    ## - origin_countryVE               1    15172186  91192561173 23763
    ## - product_colorsred              1    15368663  91192757651 23763
    ## - product_colorsgreen            1    23405563  91200794550 23763
    ## - `price_classEUR20-30`          1    24099487  91201488474 23763
    ## - shipping_nameOther_shipping    1    24360300  91201749287 23763
    ## - badges_count                   1    31328601  91208717589 23763
    ## - origin_countryUS               1    43399014  91220788001 23763
    ## - `price_classEUR40-50`          1    47515573  91224904561 23763
    ## - product_colorsblue             1    66993912  91244382899 23763
    ## - origin_countrySG               1    80648085  91258037073 23764
    ## <none>                                          91177388988 23765
    ## - discount_per                   1   146871272  91324260260 23765
    ## - rating                         1   158766285  91336155272 23765
    ## - product_sizesS                 1   161278662  91338667650 23765
    ## - product_colorswhite            1   163031483  91340420471 23765
    ## - product_colorspurple           1   180507358  91357896345 23765
    ## - product_colorsblack            1   198187817  91375576804 23765
    ## - product_variation_inventory    1   213631966  91391020953 23766
    ## - product_sizesXXL               1   230671343  91408060331 23766
    ## - merchant_has_profile_picture1  1   262705035  91440094022 23766
    ## - product_colorsgrey             1   292487720  91469876707 23767
    ## - product_sizesOther_sizes       1   302057144  91479446131 23767
    ## - product_colorsOther_colors     1   311837772  91489226760 23767
    ## - price                          1   354196155  91531585142 23768
    ## - product_sizesXXS               1   410991279  91588380267 23768
    ## - countries_shipped_to           1   451210169  91628599157 23769
    ## - product_sizesXS                1   475111553  91652500541 23769
    ## - tags_count                     1   476245860  91653634847 23769
    ## - shipping_option_price          1   673386229  91850775217 23772
    ## - `price_classEUR10-20`          1   730610067  91907999054 23773
    ## - merchant_rating_count          1 12089851029 103267240017 23926
    ## 
    ## Step:  AIC=23762.5
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - shipping_is_express1           1     1941675  91180922055 23761
    ## - product_colorsyellow           1     4299440  91183279820 23761
    ## - badge_local_product1           1     4339096  91183319476 23761
    ## - uses_ad_boosts1                1     5311961  91184292341 23761
    ## - retail_price                   1    12065881  91191046261 23761
    ## - origin_countryVE               1    15251447  91194231827 23761
    ## - product_colorsred              1    16106027  91195086408 23761
    ## - product_colorsgreen            1    24155680  91203136060 23761
    ## - shipping_nameOther_shipping    1    24248905  91203229286 23761
    ## - `price_classEUR20-30`          1    24315064  91203295444 23761
    ## - badges_count                   1    31322911  91210303292 23761
    ## - origin_countryUS               1    44588454  91223568834 23761
    ## - `price_classEUR40-50`          1    47508287  91226488667 23761
    ## - product_colorsblue             1    66523089  91245503469 23762
    ## - origin_countrySG               1    80715422  91259695803 23762
    ## <none>                                          91178980380 23763
    ## - discount_per                   1   146755011  91325735391 23763
    ## - rating                         1   158989857  91337970237 23763
    ## - product_colorswhite            1   162796496  91341776876 23763
    ## - product_colorspurple           1   181036891  91360017271 23763
    ## - product_colorsblack            1   197896453  91376876833 23763
    ## - product_variation_inventory    1   214126475  91393106856 23764
    ## - merchant_has_profile_picture1  1   261589388  91440569768 23764
    ## - product_sizesXXL               1   265733778  91444714158 23764
    ## - product_colorsgrey             1   292135853  91471116233 23765
    ## - product_colorsOther_colors     1   314253292  91493233672 23765
    ## - price                          1   352981432  91531961812 23766
    ## - countries_shipped_to           1   449802182  91628782563 23767
    ## - tags_count                     1   478020719  91657001099 23767
    ## - product_sizesS                 1   481783512  91660763892 23767
    ## - product_sizesOther_sizes       1   545350479  91724330859 23768
    ## - shipping_option_price          1   671883275  91850863655 23770
    ## - product_sizesXXS               1   693123135  91872103515 23770
    ## - `price_classEUR10-20`          1   729019786  91908000166 23771
    ## - product_sizesXS                1  1171187493  92350167874 23777
    ## - merchant_rating_count          1 12088336193 103267316574 23924
    ## 
    ## Step:  AIC=23760.53
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - product_colorsyellow           1     4337532  91185259587 23759
    ## - badge_local_product1           1     4821293  91185743348 23759
    ## - uses_ad_boosts1                1     5074218  91185996273 23759
    ## - retail_price                   1    12434887  91193356942 23759
    ## - origin_countryVE               1    15311106  91196233161 23759
    ## - product_colorsred              1    16316870  91197238925 23759
    ## - `price_classEUR20-30`          1    23104364  91204026418 23759
    ## - product_colorsgreen            1    24419142  91205341196 23759
    ## - shipping_nameOther_shipping    1    27195141  91208117196 23759
    ## - badges_count                   1    30100164  91211022219 23759
    ## - origin_countryUS               1    44348825  91225270880 23759
    ## - `price_classEUR40-50`          1    52574768  91233496823 23759
    ## - product_colorsblue             1    66349641  91247271696 23760
    ## - origin_countrySG               1    80354574  91261276629 23760
    ## <none>                                          91180922055 23761
    ## - discount_per                   1   147790753  91328712808 23761
    ## - rating                         1   161923282  91342845337 23761
    ## - product_colorswhite            1   162127700  91343049755 23761
    ## - product_colorspurple           1   180763051  91361685106 23761
    ## - product_colorsblack            1   197347243  91378269298 23761
    ## - product_variation_inventory    1   218311801  91399233856 23762
    ## - merchant_has_profile_picture1  1   261996576  91442918631 23762
    ## - product_sizesXXL               1   265298712  91446220767 23762
    ## - product_colorsgrey             1   292058805  91472980860 23763
    ## - product_colorsOther_colors     1   312467753  91493389808 23763
    ## - price                          1   351079087  91532001142 23764
    ## - countries_shipped_to           1   452647745  91633569800 23765
    ## - tags_count                     1   478832478  91659754532 23765
    ## - product_sizesS                 1   483693422  91664615477 23766
    ## - product_sizesOther_sizes       1   548001201  91728923256 23766
    ## - shipping_option_price          1   685623695  91866545750 23768
    ## - product_sizesXXS               1   695730137  91876652192 23769
    ## - `price_classEUR10-20`          1   733762237  91914684292 23769
    ## - product_sizesXS                1  1174059479  92354981534 23775
    ## - merchant_rating_count          1 12086624512 103267546567 23922
    ## 
    ## Step:  AIC=23758.59
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - badge_local_product1           1     5027948  91190287535 23757
    ## - uses_ad_boosts1                1     5235307  91190494894 23757
    ## - retail_price                   1    12705841  91197965429 23757
    ## - origin_countryVE               1    15269265  91200528852 23757
    ## - `price_classEUR20-30`          1    23050366  91208309953 23757
    ## - shipping_nameOther_shipping    1    26642032  91211901619 23757
    ## - product_colorsred              1    28759443  91214019030 23757
    ## - badges_count                   1    31379464  91216639052 23757
    ## - product_colorsgreen            1    40040961  91225300549 23757
    ## - origin_countryUS               1    46385357  91231644945 23757
    ## - `price_classEUR40-50`          1    52475497  91237735084 23757
    ## - origin_countrySG               1    80698065  91265957652 23758
    ## - product_colorsblue             1    96020681  91281280268 23758
    ## <none>                                          91185259587 23759
    ## - discount_per                   1   147690187  91332949774 23759
    ## - rating                         1   160337227  91345596815 23759
    ## - product_variation_inventory    1   220051452  91405311039 23760
    ## - product_colorspurple           1   227732790  91412992377 23760
    ## - product_colorswhite            1   253450573  91438710160 23760
    ## - merchant_has_profile_picture1  1   260858862  91446118449 23760
    ## - product_sizesXXL               1   265789193  91451048781 23760
    ## - product_colorsblack            1   309340755  91494600343 23761
    ## - price                          1   350777027  91536036615 23762
    ## - product_colorsgrey             1   372241392  91557500979 23762
    ## - countries_shipped_to           1   448312085  91633571672 23763
    ## - tags_count                     1   480792139  91666051726 23764
    ## - product_sizesS                 1   481358656  91666618243 23764
    ## - product_colorsOther_colors     1   494183849  91679443436 23764
    ## - product_sizesOther_sizes       1   545844184  91731103771 23764
    ## - shipping_option_price          1   687747142  91873006729 23766
    ## - product_sizesXXS               1   693473112  91878732700 23767
    ## - `price_classEUR10-20`          1   736071980  91921331568 23767
    ## - product_sizesXS                1  1179534636  92364794224 23774
    ## - merchant_rating_count          1 12082395283 103267654870 23920
    ## 
    ## Step:  AIC=23756.66
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - uses_ad_boosts1                1     5748118  91196035653 23755
    ## - retail_price                   1    12554284  91202841819 23755
    ## - origin_countryVE               1    14892374  91205179909 23755
    ## - `price_classEUR20-30`          1    23153084  91213440618 23755
    ## - shipping_nameOther_shipping    1    25971659  91216259194 23755
    ## - badges_count                   1    27491206  91217778741 23755
    ## - product_colorsred              1    27977975  91218265510 23755
    ## - product_colorsgreen            1    41247146  91231534681 23755
    ## - origin_countryUS               1    45070047  91235357582 23755
    ## - `price_classEUR40-50`          1    51843005  91242130540 23755
    ## - origin_countrySG               1    80425620  91270713155 23756
    ## - product_colorsblue             1    97277854  91287565389 23756
    ## <none>                                          91190287535 23757
    ## - discount_per                   1   147885387  91338172922 23757
    ## - rating                         1   170733809  91361021344 23757
    ## - product_variation_inventory    1   221668293  91411955827 23758
    ## - product_colorspurple           1   227097130  91417384665 23758
    ## - product_colorswhite            1   252987502  91443275037 23758
    ## - merchant_has_profile_picture1  1   265407886  91455695421 23759
    ## - product_sizesXXL               1   267554717  91457842252 23759
    ## - product_colorsblack            1   311459343  91501746878 23759
    ## - price                          1   353883494  91544171029 23760
    ## - product_colorsgrey             1   376696602  91566984137 23760
    ## - countries_shipped_to           1   451177754  91641465289 23761
    ## - tags_count                     1   476605476  91666893011 23762
    ## - product_sizesS                 1   481339297  91671626832 23762
    ## - product_colorsOther_colors     1   494920167  91685207702 23762
    ## - product_sizesOther_sizes       1   549642644  91739930179 23763
    ## - product_sizesXXS               1   692339450  91882626984 23765
    ## - shipping_option_price          1   693480992  91883768527 23765
    ## - `price_classEUR10-20`          1   742205372  91932492907 23765
    ## - product_sizesXS                1  1180247408  92370534943 23772
    ## - merchant_rating_count          1 12084024063 103274311598 23918
    ## 
    ## Step:  AIC=23754.74
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - retail_price                   1    12426561  91208462214 23753
    ## - origin_countryVE               1    16376403  91212412056 23753
    ## - `price_classEUR20-30`          1    24986559  91221022212 23753
    ## - shipping_nameOther_shipping    1    25265080  91221300733 23753
    ## - badges_count                   1    26311336  91222346989 23753
    ## - product_colorsred              1    27784921  91223820574 23753
    ## - product_colorsgreen            1    40364041  91236399694 23753
    ## - origin_countryUS               1    44842032  91240877685 23753
    ## - `price_classEUR40-50`          1    52511044  91248546697 23754
    ## - origin_countrySG               1    78895988  91274931641 23754
    ## - product_colorsblue             1    97614533  91293650186 23754
    ## <none>                                          91196035653 23755
    ## - discount_per                   1   146690498  91342726151 23755
    ## - rating                         1   173471477  91369507130 23755
    ## - product_colorspurple           1   228312754  91424348407 23756
    ## - product_variation_inventory    1   230679249  91426714902 23756
    ## - product_colorswhite            1   254488833  91450524486 23756
    ## - merchant_has_profile_picture1  1   265167825  91461203478 23757
    ## - product_sizesXXL               1   265595645  91461631298 23757
    ## - product_colorsblack            1   311275359  91507311012 23757
    ## - price                          1   361640158  91557675811 23758
    ## - product_colorsgrey             1   375320999  91571356652 23758
    ## - countries_shipped_to           1   449330191  91645365844 23759
    ## - product_sizesS                 1   477720874  91673756527 23760
    ## - tags_count                     1   478564932  91674600585 23760
    ## - product_colorsOther_colors     1   492983653  91689019306 23760
    ## - product_sizesOther_sizes       1   562113669  91758149322 23761
    ## - shipping_option_price          1   698772833  91894808486 23763
    ## - product_sizesXXS               1   700439802  91896475455 23763
    ## - `price_classEUR10-20`          1   749447600  91945483253 23764
    ## - product_sizesXS                1  1174808428  92370844081 23770
    ## - merchant_rating_count          1 12079659782 103275695435 23916
    ## 
    ## Step:  AIC=23752.92
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + rating + badges_count + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - origin_countryVE               1    16459384  91224921598 23751
    ## - `price_classEUR20-30`          1    22979199  91231441413 23751
    ## - badges_count                   1    26431476  91234893690 23751
    ## - shipping_nameOther_shipping    1    27486468  91235948682 23751
    ## - product_colorsred              1    27822521  91236284735 23751
    ## - product_colorsgreen            1    39520268  91247982482 23752
    ## - origin_countryUS               1    45151032  91253613246 23752
    ## - `price_classEUR40-50`          1    55520355  91263982569 23752
    ## - origin_countrySG               1    79758716  91288220930 23752
    ## - product_colorsblue             1    97112359  91305574573 23752
    ## <none>                                          91208462214 23753
    ## - rating                         1   172523695  91380985909 23753
    ## - discount_per                   1   214743542  91423205756 23754
    ## - product_colorspurple           1   229415320  91437877534 23754
    ## - product_variation_inventory    1   230596458  91439058673 23754
    ## - product_colorswhite            1   254334132  91462796346 23755
    ## - merchant_has_profile_picture1  1   269319483  91477781697 23755
    ## - product_sizesXXL               1   269908695  91478370909 23755
    ## - product_colorsblack            1   314717889  91523180103 23755
    ## - product_colorsgrey             1   372250048  91580712262 23756
    ## - price                          1   386093160  91594555374 23757
    ## - countries_shipped_to           1   448853852  91657316066 23757
    ## - product_sizesS                 1   475805536  91684267750 23758
    ## - tags_count                     1   478980214  91687442428 23758
    ## - product_colorsOther_colors     1   492533741  91700995955 23758
    ## - product_sizesOther_sizes       1   561595948  91770058162 23759
    ## - shipping_option_price          1   693084602  91901546816 23761
    ## - product_sizesXXS               1   708108351  91916570565 23761
    ## - `price_classEUR10-20`          1   746072170  91954534384 23762
    ## - product_sizesXS                1  1169991862  92378454076 23768
    ## - merchant_rating_count          1 12262245833 103470708047 23916
    ## 
    ## Step:  AIC=23751.16
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + rating + badges_count + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - `price_classEUR20-30`          1    22030274  91246951872 23750
    ## - badges_count                   1    25803236  91250724834 23750
    ## - shipping_nameOther_shipping    1    27188728  91252110326 23750
    ## - product_colorsred              1    27713188  91252634785 23750
    ## - product_colorsgreen            1    39282591  91264204189 23750
    ## - origin_countryUS               1    44518763  91269440360 23750
    ## - `price_classEUR40-50`          1    55061166  91279982763 23750
    ## - origin_countrySG               1    79888861  91304810459 23750
    ## - product_colorsblue             1    96931040  91321852638 23751
    ## <none>                                          91224921598 23751
    ## - rating                         1   192143776  91417065374 23752
    ## - discount_per                   1   219942388  91444863986 23752
    ## - product_colorspurple           1   227310776  91452232374 23752
    ## - product_variation_inventory    1   234114964  91459036561 23753
    ## - product_colorswhite            1   245666964  91470588562 23753
    ## - product_sizesXXL               1   266445551  91491367149 23753
    ## - merchant_has_profile_picture1  1   271342508  91496264106 23753
    ## - product_colorsblack            1   306718822  91531640420 23754
    ## - product_colorsgrey             1   371053408  91595975006 23755
    ## - price                          1   377556725  91602478322 23755
    ## - countries_shipped_to           1   448167261  91673088859 23756
    ## - product_sizesS                 1   471328418  91696250016 23756
    ## - tags_count                     1   479128109  91704049707 23756
    ## - product_colorsOther_colors     1   489757408  91714679006 23756
    ## - product_sizesOther_sizes       1   557876081  91782797679 23757
    ## - shipping_option_price          1   682137631  91907059229 23759
    ## - product_sizesXXS               1   702100245  91927021842 23759
    ## - `price_classEUR10-20`          1   739522022  91964443620 23760
    ## - product_sizesXS                1  1175942991  92400864589 23766
    ## - merchant_rating_count          1 12289180163 103514101760 23915
    ## 
    ## Step:  AIC=23749.48
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR40-50` + 
    ##     discount_per + rating + badges_count + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - product_colorsred              1    27010086  91273961957 23748
    ## - badges_count                   1    27225654  91274177526 23748
    ## - shipping_nameOther_shipping    1    27464559  91274416431 23748
    ## - product_colorsgreen            1    38718490  91285670362 23748
    ## - `price_classEUR40-50`          1    41358288  91288310159 23748
    ## - origin_countryUS               1    42177103  91289128974 23748
    ## - origin_countrySG               1    78278967  91325230839 23749
    ## - product_colorsblue             1    98739981  91345691853 23749
    ## <none>                                          91246951872 23750
    ## - rating                         1   200822183  91447774054 23750
    ## - product_colorspurple           1   226034386  91472986258 23751
    ## - discount_per                   1   229885158  91476837029 23751
    ## - product_colorswhite            1   240838207  91487790078 23751
    ## - product_variation_inventory    1   242235841  91489187713 23751
    ## - product_sizesXXL               1   264122708  91511074580 23751
    ## - merchant_has_profile_picture1  1   274783604  91521735475 23751
    ## - product_colorsblack            1   301550907  91548502779 23752
    ## - price                          1   368459271  91615411142 23753
    ## - product_colorsgrey             1   370633535  91617585407 23753
    ## - countries_shipped_to           1   456741382  91703693254 23754
    ## - product_sizesS                 1   472271766  91719223637 23754
    ## - product_colorsOther_colors     1   477765082  91724716954 23754
    ## - tags_count                     1   481933935  91728885806 23754
    ## - product_sizesOther_sizes       1   562948401  91809900272 23756
    ## - shipping_option_price          1   660187905  91907139777 23757
    ## - product_sizesXXS               1   713438306  91960390178 23758
    ## - `price_classEUR10-20`          1   783023156  92029975027 23759
    ## - product_sizesXS                1  1187439772  92434391643 23764
    ## - merchant_rating_count          1 12296840921 103543792793 23913
    ## 
    ## Step:  AIC=23747.86
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR40-50` + 
    ##     discount_per + rating + badges_count + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - product_colorsgreen            1    24644994  91298606951 23746
    ## - badges_count                   1    27353869  91301315826 23746
    ## - shipping_nameOther_shipping    1    30997148  91304959105 23746
    ## - `price_classEUR40-50`          1    36142502  91310104459 23746
    ## - origin_countryUS               1    44527088  91318489045 23747
    ## - origin_countrySG               1    77818322  91351780279 23747
    ## - product_colorsblue             1    78361966  91352323923 23747
    ## <none>                                          91273961957 23748
    ## - product_colorspurple           1   202302046  91476264003 23749
    ## - rating                         1   207032530  91480994487 23749
    ## - product_colorswhite            1   214055802  91488017760 23749
    ## - discount_per                   1   230293246  91504255203 23749
    ## - product_variation_inventory    1   239446185  91513408142 23749
    ## - product_sizesXXL               1   264714875  91538676832 23750
    ## - merchant_has_profile_picture1  1   268040053  91542002010 23750
    ## - product_colorsblack            1   276587987  91550549944 23750
    ## - product_colorsgrey             1   343662197  91617624154 23751
    ## - price                          1   361356455  91635318412 23751
    ## - product_colorsOther_colors     1   462827538  91736789496 23753
    ## - countries_shipped_to           1   468328078  91742290035 23753
    ## - product_sizesS                 1   478274227  91752236184 23753
    ## - tags_count                     1   494138301  91768100258 23753
    ## - product_sizesOther_sizes       1   566869591  91840831548 23754
    ## - shipping_option_price          1   651695188  91925657146 23755
    ## - product_sizesXXS               1   732372834  92006334791 23756
    ## - `price_classEUR10-20`          1   775255536  92049217493 23757
    ## - product_sizesXS                1  1192630447  92466592404 23763
    ## - merchant_rating_count          1 12294258368 103568220325 23912
    ## 
    ## Step:  AIC=23746.22
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR40-50` + 
    ##     discount_per + rating + badges_count + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - badges_count                   1    27378892  91325985843 23745
    ## - shipping_nameOther_shipping    1    31525309  91330132261 23745
    ## - `price_classEUR40-50`          1    32749144  91331356095 23745
    ## - origin_countryUS               1    45054569  91343661520 23745
    ## - product_colorsblue             1    63636959  91362243910 23745
    ## - origin_countrySG               1    77651076  91376258027 23745
    ## <none>                                          91298606951 23746
    ## - product_colorspurple           1   184129446  91482736397 23747
    ## - product_colorswhite            1   189899271  91488506222 23747
    ## - rating                         1   216133388  91514740339 23747
    ## - discount_per                   1   223060955  91521667906 23747
    ## - product_variation_inventory    1   224925876  91523532828 23747
    ## - product_colorsblack            1   251944103  91550551054 23748
    ## - product_sizesXXL               1   263860486  91562467437 23748
    ## - merchant_has_profile_picture1  1   275988223  91574595174 23748
    ## - product_colorsgrey             1   320691512  91619298463 23749
    ## - price                          1   362557825  91661164776 23749
    ## - product_colorsOther_colors     1   442051346  91740658297 23751
    ## - countries_shipped_to           1   463243260  91761850212 23751
    ## - product_sizesS                 1   477769985  91776376936 23751
    ## - tags_count                     1   493643372  91792250323 23751
    ## - product_sizesOther_sizes       1   561176836  91859783788 23752
    ## - shipping_option_price          1   653745186  91952352137 23754
    ## - product_sizesXXS               1   736368182  92034975133 23755
    ## - `price_classEUR10-20`          1   773711867  92072318818 23755
    ## - product_sizesXS                1  1198840864  92497447815 23761
    ## - merchant_rating_count          1 12294180951 103592787902 23910
    ## 
    ## Step:  AIC=23744.61
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR40-50` + 
    ##     discount_per + rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - shipping_nameOther_shipping    1    29859794  91355845637 23743
    ## - `price_classEUR40-50`          1    32682067  91358667910 23743
    ## - origin_countryUS               1    43264663  91369250507 23743
    ## - product_colorsblue             1    61320689  91387306532 23744
    ## - origin_countrySG               1    76009822  91401995665 23744
    ## <none>                                          91325985843 23745
    ## - product_colorspurple           1   183999571  91509985414 23745
    ## - product_colorswhite            1   189534708  91515520551 23745
    ## - product_variation_inventory    1   222543974  91548529817 23746
    ## - discount_per                   1   222575526  91548561369 23746
    ## - product_sizesXXL               1   255959870  91581945713 23746
    ## - product_colorsblack            1   256222287  91582208130 23746
    ## - rating                         1   262428289  91588414132 23746
    ## - merchant_has_profile_picture1  1   278648619  91604634462 23747
    ## - product_colorsgrey             1   333584598  91659570442 23747
    ## - price                          1   357374637  91683360480 23748
    ## - product_colorsOther_colors     1   445816422  91771802265 23749
    ## - countries_shipped_to           1   465266137  91791251980 23749
    ## - product_sizesS                 1   474447082  91800432926 23749
    ## - tags_count                     1   498032916  91824018759 23750
    ## - product_sizesOther_sizes       1   547941002  91873926845 23751
    ## - shipping_option_price          1   641101668  91967087512 23752
    ## - product_sizesXXS               1   740656471  92066642314 23753
    ## - `price_classEUR10-20`          1   761283475  92087269318 23754
    ## - product_sizesXS                1  1190877292  92516863135 23760
    ## - merchant_rating_count          1 12331914984 103657900827 23909
    ## 
    ## Step:  AIC=23743.04
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR40-50` + 
    ##     discount_per + rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - `price_classEUR40-50`          1    32538781  91388384418 23742
    ## - origin_countryUS               1    40931481  91396777118 23742
    ## - product_colorsblue             1    66331302  91422176939 23742
    ## - origin_countrySG               1    76504863  91432350499 23742
    ## <none>                                          91355845637 23743
    ## - product_colorspurple           1   192483053  91548328690 23744
    ## - product_colorswhite            1   195734464  91551580101 23744
    ## - discount_per                   1   221726905  91577572542 23744
    ## - product_variation_inventory    1   221962399  91577808036 23744
    ## - product_sizesXXL               1   258640818  91614486455 23745
    ## - rating                         1   260561393  91616407029 23745
    ## - product_colorsblack            1   263813439  91619659076 23745
    ## - merchant_has_profile_picture1  1   285657051  91641502688 23745
    ## - product_colorsgrey             1   339268983  91695114620 23746
    ## - price                          1   355462556  91711308193 23746
    ## - product_colorsOther_colors     1   445798473  91801644109 23747
    ## - countries_shipped_to           1   461636404  91817482041 23748
    ## - product_sizesS                 1   471058930  91826904567 23748
    ## - tags_count                     1   493786957  91849632593 23748
    ## - product_sizesOther_sizes       1   553803766  91909649402 23749
    ## - shipping_option_price          1   654838602  92010684238 23750
    ## - product_sizesXXS               1   732467160  92088312796 23752
    ## - `price_classEUR10-20`          1   754124630  92109970267 23752
    ## - product_sizesXS                1  1191670323  92547515959 23758
    ## - merchant_rating_count          1 12332180063 103688025699 23907
    ## 
    ## Step:  AIC=23741.51
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - origin_countryUS               1    39744357  91428128775 23740
    ## - product_colorsblue             1    72290309  91460674727 23741
    ## - origin_countrySG               1    74623616  91463008034 23741
    ## <none>                                          91388384418 23742
    ## - product_colorspurple           1   197821066  91586205484 23742
    ## - product_colorswhite            1   202877073  91591261491 23742
    ## - discount_per                   1   216640266  91605024684 23743
    ## - product_variation_inventory    1   227758935  91616143352 23743
    ## - product_sizesXXL               1   260867866  91649252284 23743
    ## - rating                         1   267086985  91655471403 23743
    ## - product_colorsblack            1   275493494  91663877912 23744
    ## - merchant_has_profile_picture1  1   292156496  91680540914 23744
    ## - price                          1   323030946  91711415364 23744
    ## - product_colorsgrey             1   347330835  91735715253 23745
    ## - product_colorsOther_colors     1   461469504  91849853922 23746
    ## - countries_shipped_to           1   464077950  91852462367 23746
    ## - product_sizesS                 1   481631108  91870015525 23746
    ## - tags_count                     1   501453312  91889837730 23747
    ## - product_sizesOther_sizes       1   609019341  91997403758 23748
    ## - shipping_option_price          1   641258508  92029642926 23749
    ## - `price_classEUR10-20`          1   737881655  92126266073 23750
    ## - product_sizesXXS               1   754818848  92143203266 23750
    ## - product_sizesXS                1  1212377676  92600762094 23757
    ## - merchant_rating_count          1 12322285213 103710669631 23906
    ## 
    ## Step:  AIC=23740.08
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + merchant_rating_count + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - origin_countrySG               1    74714046  91502842822 23739
    ## - product_colorsblue             1    76738920  91504867695 23739
    ## <none>                                          91428128775 23740
    ## - product_colorspurple           1   200709958  91628838733 23741
    ## - product_colorswhite            1   208713218  91636841993 23741
    ## - discount_per                   1   224650014  91652778790 23741
    ## - product_variation_inventory    1   228070022  91656198797 23741
    ## - product_sizesXXL               1   254533792  91682662568 23742
    ## - rating                         1   266607640  91694736415 23742
    ## - product_colorsblack            1   270228944  91698357720 23742
    ## - merchant_has_profile_picture1  1   302149710  91730278485 23742
    ## - price                          1   318623941  91746752716 23743
    ## - product_colorsgrey             1   346917543  91775046318 23743
    ## - product_colorsOther_colors     1   451248660  91879377435 23745
    ## - product_sizesS                 1   472488373  91900617148 23745
    ## - countries_shipped_to           1   487568279  91915697055 23745
    ## - tags_count                     1   490101858  91918230634 23745
    ## - product_sizesOther_sizes       1   597253442  92025382217 23747
    ## - shipping_option_price          1   638233606  92066362381 23747
    ## - `price_classEUR10-20`          1   740210972  92168339748 23749
    ## - product_sizesXXS               1   781401834  92209530610 23749
    ## - product_sizesXS                1  1198813706  92626942482 23755
    ## - merchant_rating_count          1 12428583407 103856712182 23905
    ## 
    ## Step:  AIC=23739.15
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## - product_colorsblue             1    77874973  91580717795 23738
    ## <none>                                          91502842822 23739
    ## - product_colorspurple           1   199962788  91702805609 23740
    ## - product_colorswhite            1   210769352  91713612173 23740
    ## - product_variation_inventory    1   224538431  91727381253 23740
    ## - discount_per                   1   229454017  91732296839 23740
    ## - product_sizesXXL               1   254567561  91757410383 23741
    ## - rating                         1   272742335  91775585156 23741
    ## - product_colorsblack            1   273548209  91776391031 23741
    ## - merchant_has_profile_picture1  1   299306092  91802148914 23741
    ## - price                          1   314668340  91817511162 23742
    ## - product_colorsgrey             1   373734664  91876577486 23743
    ## - product_colorsOther_colors     1   465291143  91968133965 23744
    ## - product_sizesS                 1   475376743  91978219564 23744
    ## - tags_count                     1   488517998  91991360819 23744
    ## - countries_shipped_to           1   499659078  92002501899 23744
    ## - product_sizesOther_sizes       1   598575549  92101418371 23746
    ## - shipping_option_price          1   641945401  92144788223 23746
    ## - `price_classEUR10-20`          1   733659107  92236501928 23748
    ## - product_sizesXXS               1   789427831  92292270652 23748
    ## - product_sizesXS                1  1184424634  92687267455 23754
    ## - merchant_rating_count          1 12410261607 103913104429 23904
    ## 
    ## Step:  AIC=23738.27
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df   Sum of Sq          RSS   AIC
    ## <none>                                          91580717795 23738
    ## - product_colorswhite            1   159759029  91740476824 23739
    ## - product_colorspurple           1   169513291  91750231086 23739
    ## - product_colorsblack            1   213035789  91793753584 23739
    ## - discount_per                   1   226001937  91806719732 23740
    ## - product_sizesXXL               1   242082843  91822800638 23740
    ## - product_variation_inventory    1   245479583  91826197378 23740
    ## - rating                         1   265655188  91846372983 23740
    ## - merchant_has_profile_picture1  1   282902113  91863619907 23740
    ## - price                          1   315096737  91895814532 23741
    ## - product_colorsgrey             1   328717545  91909435340 23741
    ## - product_colorsOther_colors     1   396100881  91976818676 23742
    ## - countries_shipped_to           1   460388982  92041106777 23743
    ## - product_sizesS                 1   475528626  92056246421 23743
    ## - tags_count                     1   481571484  92062289279 23743
    ## - product_sizesOther_sizes       1   577271116  92157988911 23745
    ## - shipping_option_price          1   638634575  92219352370 23745
    ## - `price_classEUR10-20`          1   731464782  92312182577 23747
    ## - product_sizesXXS               1   765837859  92346555654 23747
    ## - product_sizesXS                1  1166064979  92746782774 23753
    ## - merchant_rating_count          1 12476355741 104057073535 23904
    ## Start:  AIC=23823.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23823.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23823.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1       3119 94778120418 23821
    ## - shipping_is_express1           1      58361 94778175659 23821
    ## - product_sizesM                 1    1414328 94779531626 23821
    ## - badge_product_quality1         1    3088847 94781206146 23821
    ## - badges_count                   1    4669083 94782786382 23821
    ## - badge_local_product1           1    6438311 94784555610 23821
    ## - uses_ad_boosts1                1    8264892 94786382191 23821
    ## - product_colorspink             1   11429819 94789547117 23821
    ## - product_colorsyellow           1   12817779 94790935078 23822
    ## - shipping_nameOther_shipping    1   22646809 94800764108 23822
    ## - `price_classEUR40-50`          1   24752897 94802870195 23822
    ## - origin_countryVE               1   26893190 94805010488 23822
    ## - retail_price                   1   30948991 94809066290 23822
    ## - `price_classEUR20-30`          1   35036489 94813153788 23822
    ## - product_colorsred              1   38607286 94816724585 23822
    ## - discount_per                   1   39193928 94817311226 23822
    ## - product_colorsgreen            1   51250828 94829368127 23822
    ## - tags_count                     1   68213978 94846331277 23822
    ## - origin_countryUS               1   77427938 94855545237 23822
    ## - origin_countrySG               1   79211763 94857329062 23822
    ## - product_sizesS                 1  110767121 94888884420 23823
    ## - product_colorsblue             1  121984233 94900101532 23823
    ## - merchant_rating                1  123200778 94901318077 23823
    ## <none>                                        94778117299 23823
    ## - product_colorswhite            1  155334417 94933451715 23823
    ## - rating                         1  174219219 94952336518 23824
    ## - product_sizesOther_sizes       1  195258605 94973375904 23824
    ## - product_colorsgrey             1  214815034 94992932332 23824
    ## - product_colorsblack            1  252872275 95030989574 23825
    ## - product_colorsOther_colors     1  261673103 95039790402 23825
    ## - price                          1  299634218 95077751517 23825
    ## - product_variation_inventory    1  337093488 95115210787 23826
    ## - product_colorspurple           1  362698151 95140815450 23826
    ## - product_sizesXXS               1  401248081 95179365380 23827
    ## - product_sizesXS                1  511462398 95289579697 23828
    ## - product_sizesXXL               1  635407915 95413525214 23830
    ## - `price_classEUR10-20`          1  675078159 95453195458 23831
    ## - shipping_option_price          1  679812660 95457929959 23831
    ## - countries_shipped_to           1  706842438 95484959737 23831
    ## - merchant_has_profile_picture1  1  954083096 95732200394 23834
    ## - merchant_rating_count          1 4287036251 99065153550 23879
    ## 
    ## Step:  AIC=23821.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1      58137 94778178555 23819
    ## - product_sizesM                 1    1413659 94779534077 23819
    ## - badge_product_quality1         1    3088596 94781209013 23819
    ## - badges_count                   1    4669441 94782789859 23819
    ## - badge_local_product1           1    6438244 94784558662 23819
    ## - uses_ad_boosts1                1    8263887 94786384304 23819
    ## - product_colorspink             1   11427188 94789547606 23819
    ## - product_colorsyellow           1   12814732 94790935150 23820
    ## - shipping_nameOther_shipping    1   22649672 94800770089 23820
    ## - `price_classEUR40-50`          1   24765022 94802885440 23820
    ## - origin_countryVE               1   26894462 94805014880 23820
    ## - retail_price                   1   30953412 94809073830 23820
    ## - `price_classEUR20-30`          1   35060753 94813181171 23820
    ## - product_colorsred              1   38604624 94816725042 23820
    ## - discount_per                   1   39220244 94817340662 23820
    ## - product_colorsgreen            1   51248597 94829369015 23820
    ## - tags_count                     1   68278062 94846398479 23820
    ## - origin_countryUS               1   77425503 94855545921 23820
    ## - origin_countrySG               1   79208796 94857329214 23820
    ## - product_sizesS                 1  110769224 94888889642 23821
    ## - product_colorsblue             1  122283527 94900403945 23821
    ## - merchant_rating                1  123198473 94901318890 23821
    ## <none>                                        94778120418 23821
    ## - product_colorswhite            1  155333317 94933453735 23821
    ## - rating                         1  174352530 94952472948 23822
    ## - product_sizesOther_sizes       1  195297511 94973417929 23822
    ## - product_colorsgrey             1  214826003 94992946421 23822
    ## - product_colorsblack            1  252875210 95030995628 23823
    ## - product_colorsOther_colors     1  261682912 95039803329 23823
    ## - price                          1  299660081 95077780499 23823
    ## - product_variation_inventory    1  337434953 95115555371 23824
    ## - product_colorspurple           1  362713258 95140833676 23824
    ## - product_sizesXXS               1  401315694 95179436112 23825
    ## - product_sizesXS                1  511501462 95289621880 23826
    ## - product_sizesXXL               1  635413604 95413534022 23828
    ## - `price_classEUR10-20`          1  675576713 95453697131 23829
    ## - shipping_option_price          1  679959917 95458080335 23829
    ## - countries_shipped_to           1  706839768 95484960186 23829
    ## - merchant_has_profile_picture1  1  954097598 95732218016 23832
    ## - merchant_rating_count          1 4287041127 99065161545 23877
    ## 
    ## Step:  AIC=23819.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1    1423116 94779601671 23817
    ## - badge_product_quality1         1    3112040 94781290594 23817
    ## - badges_count                   1    4803210 94782981764 23817
    ## - badge_local_product1           1    6457411 94784635966 23817
    ## - uses_ad_boosts1                1    8328522 94786507077 23817
    ## - product_colorspink             1   11425754 94789604308 23818
    ## - product_colorsyellow           1   12805012 94790983566 23818
    ## - shipping_nameOther_shipping    1   22839884 94801018439 23818
    ## - `price_classEUR40-50`          1   25126883 94803305438 23818
    ## - origin_countryVE               1   26866528 94805045082 23818
    ## - retail_price                   1   30901209 94809079764 23818
    ## - `price_classEUR20-30`          1   35737577 94813916131 23818
    ## - product_colorsred              1   38571569 94816750123 23818
    ## - discount_per                   1   39179840 94817358394 23818
    ## - product_colorsgreen            1   51201176 94829379731 23818
    ## - tags_count                     1   68220777 94846399332 23818
    ## - origin_countryUS               1   77410173 94855588728 23818
    ## - origin_countrySG               1   79274965 94857453519 23818
    ## - product_sizesS                 1  110720027 94888898582 23819
    ## - product_colorsblue             1  122296063 94900474618 23819
    ## - merchant_rating                1  124006052 94902184607 23819
    ## <none>                                        94778178555 23819
    ## - product_colorswhite            1  155350104 94933528658 23819
    ## - rating                         1  174349543 94952528097 23820
    ## - product_sizesOther_sizes       1  195240331 94973418885 23820
    ## - product_colorsgrey             1  214820543 94992999098 23820
    ## - product_colorsblack            1  252884146 95031062701 23821
    ## - product_colorsOther_colors     1  261986328 95040164882 23821
    ## - price                          1  300895026 95079073581 23822
    ## - product_variation_inventory    1  337511377 95115689932 23822
    ## - product_colorspurple           1  362749514 95140928068 23822
    ## - product_sizesXXS               1  401464465 95179643020 23823
    ## - product_sizesXS                1  511849147 95290027701 23824
    ## - product_sizesXXL               1  635503699 95413682254 23826
    ## - shipping_option_price          1  684494464 95462673019 23827
    ## - `price_classEUR10-20`          1  688675891 95466854446 23827
    ## - countries_shipped_to           1  706801948 95484980503 23827
    ## - merchant_has_profile_picture1  1  954245044 95732423599 23830
    ## - merchant_rating_count          1 4289720580 99067899134 23875
    ## 
    ## Step:  AIC=23817.31
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1    3067627 94782669298 23815
    ## - badges_count                   1    4740139 94784341809 23815
    ## - badge_local_product1           1    6198138 94785799808 23815
    ## - uses_ad_boosts1                1    8468049 94788069719 23815
    ## - product_colorspink             1   11159152 94790760822 23816
    ## - product_colorsyellow           1   12303618 94791905288 23816
    ## - shipping_nameOther_shipping    1   23024921 94802626592 23816
    ## - `price_classEUR40-50`          1   25143873 94804745544 23816
    ## - origin_countryVE               1   26733231 94806334902 23816
    ## - retail_price                   1   30974689 94810576360 23816
    ## - `price_classEUR20-30`          1   35523444 94815125114 23816
    ## - product_colorsred              1   37968400 94817570070 23816
    ## - discount_per                   1   39310561 94818912232 23816
    ## - product_colorsgreen            1   50385035 94829986705 23816
    ## - tags_count                     1   67857732 94847459403 23816
    ## - origin_countryUS               1   76526391 94856128062 23816
    ## - origin_countrySG               1   79222466 94858824137 23816
    ## - product_colorsblue             1  122170976 94901772646 23817
    ## - merchant_rating                1  124011274 94903612945 23817
    ## <none>                                        94779601671 23817
    ## - product_colorswhite            1  154716070 94934317741 23818
    ## - rating                         1  174117701 94953719372 23818
    ## - product_colorsgrey             1  214241044 94993842714 23818
    ## - product_colorsblack            1  252164955 95031766625 23819
    ## - product_colorsOther_colors     1  260715918 95040317589 23819
    ## - price                          1  302922694 95082524365 23820
    ## - product_variation_inventory    1  337694420 95117296091 23820
    ## - product_colorspurple           1  361606536 95141208207 23820
    ## - product_sizesOther_sizes       1  473562786 95253164456 23822
    ## - product_sizesS                 1  534560177 95314161848 23823
    ## - shipping_option_price          1  688571159 95468172830 23825
    ## - `price_classEUR10-20`          1  694143809 95473745480 23825
    ## - countries_shipped_to           1  709591106 95489192777 23825
    ## - product_sizesXXL               1  868502916 95648104587 23827
    ## - product_sizesXXS               1  873692591 95653294262 23827
    ## - merchant_has_profile_picture1  1  957763608 95737365279 23829
    ## - product_sizesXS                1 1655761986 96435363657 23838
    ## - merchant_rating_count          1 4290377239 99069978910 23873
    ## 
    ## Step:  AIC=23815.35
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1    2318177 94784987475 23813
    ## - badge_local_product1           1    3347057 94786016355 23813
    ## - uses_ad_boosts1                1    8337249 94791006547 23814
    ## - product_colorspink             1   11143441 94793812739 23814
    ## - product_colorsyellow           1   11921260 94794590558 23814
    ## - shipping_nameOther_shipping    1   23442628 94806111926 23814
    ## - `price_classEUR40-50`          1   25956850 94808626148 23814
    ## - origin_countryVE               1   26568803 94809238101 23814
    ## - retail_price                   1   31684527 94814353825 23814
    ## - `price_classEUR20-30`          1   34605507 94817274805 23814
    ## - product_colorsred              1   37992249 94820661547 23814
    ## - discount_per                   1   40519049 94823188346 23814
    ## - product_colorsgreen            1   49724890 94832394188 23814
    ## - tags_count                     1   67430768 94850100065 23814
    ## - origin_countryUS               1   75610971 94858280268 23814
    ## - origin_countrySG               1   78806026 94861475324 23814
    ## - product_colorsblue             1  121891547 94904560845 23815
    ## - merchant_rating                1  123446343 94906115641 23815
    ## <none>                                        94782669298 23815
    ## - product_colorswhite            1  153856132 94936525430 23816
    ## - rating                         1  183583697 94966252995 23816
    ## - product_colorsgrey             1  213459907 94996129205 23816
    ## - product_colorsblack            1  250962873 95033632171 23817
    ## - product_colorsOther_colors     1  259420639 95042089937 23817
    ## - price                          1  301287985 95083957283 23818
    ## - product_variation_inventory    1  355685577 95138354875 23818
    ## - product_colorspurple           1  360861257 95143530555 23818
    ## - product_sizesOther_sizes       1  482440208 95265109506 23820
    ## - product_sizesS                 1  534353927 95317023224 23821
    ## - `price_classEUR10-20`          1  691327925 95473997223 23823
    ## - shipping_option_price          1  695213011 95477882309 23823
    ## - countries_shipped_to           1  715743337 95498412634 23823
    ## - product_sizesXXS               1  873190459 95655859757 23825
    ## - product_sizesXXL               1  873825650 95656494948 23825
    ## - merchant_has_profile_picture1  1  956994139 95739663437 23827
    ## - product_sizesXS                1 1652712182 96435381480 23836
    ## - merchant_rating_count          1 4318366745 99101036043 23872
    ## 
    ## Step:  AIC=23813.39
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badge_local_product1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1    1380150 94786367625 23811
    ## - uses_ad_boosts1                1    8546700 94793534175 23812
    ## - product_colorspink             1   10643780 94795631255 23812
    ## - product_colorsyellow           1   11792205 94796779679 23812
    ## - shipping_nameOther_shipping    1   24050097 94809037572 23812
    ## - `price_classEUR40-50`          1   26331366 94811318840 23812
    ## - origin_countryVE               1   26902913 94811890388 23812
    ## - retail_price                   1   31657532 94816645007 23812
    ## - `price_classEUR20-30`          1   34109801 94819097276 23812
    ## - product_colorsred              1   37877713 94822865187 23812
    ## - discount_per                   1   40451878 94825439352 23812
    ## - product_colorsgreen            1   49131485 94834118960 23812
    ## - tags_count                     1   67785828 94852773302 23812
    ## - origin_countryUS               1   76631511 94861618986 23812
    ## - origin_countrySG               1   79304009 94864291484 23813
    ## - product_colorsblue             1  121217595 94906205070 23813
    ## - merchant_rating                1  121273423 94906260898 23813
    ## <none>                                        94784987475 23813
    ## - product_colorswhite            1  153174985 94938162460 23814
    ## - rating                         1  183788649 94968776124 23814
    ## - product_colorsgrey             1  211488820 94996476295 23814
    ## - product_colorsblack            1  249363331 95034350806 23815
    ## - product_colorsOther_colors     1  258250574 95043238049 23815
    ## - price                          1  301599497 95086586972 23816
    ## - product_variation_inventory    1  355683438 95140670913 23816
    ## - product_colorspurple           1  360138087 95145125562 23816
    ## - product_sizesOther_sizes       1  483211221 95268198695 23818
    ## - product_sizesS                 1  534758949 95319746424 23819
    ## - `price_classEUR10-20`          1  691113496 95476100971 23821
    ## - shipping_option_price          1  697898240 95482885714 23821
    ## - countries_shipped_to           1  713904621 95498892096 23821
    ## - product_sizesXXS               1  871873103 95656860578 23823
    ## - product_sizesXXL               1  877463516 95662450991 23824
    ## - merchant_has_profile_picture1  1  954677353 95739664828 23825
    ## - product_sizesXS                1 1653122201 96438109675 23834
    ## - merchant_rating_count          1 4328208819 99113196294 23870
    ## 
    ## Step:  AIC=23811.41
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1    8072850 94794440474 23810
    ## - product_colorspink             1   10545177 94796912802 23810
    ## - product_colorsyellow           1   11666962 94798034587 23810
    ## - shipping_nameOther_shipping    1   24156074 94810523699 23810
    ## - `price_classEUR40-50`          1   26435280 94812802904 23810
    ## - origin_countryVE               1   26984838 94813352463 23810
    ## - retail_price                   1   31636139 94818003764 23810
    ## - `price_classEUR20-30`          1   34255889 94820623513 23810
    ## - product_colorsred              1   37837839 94824205464 23810
    ## - discount_per                   1   40334870 94826702494 23810
    ## - product_colorsgreen            1   48837603 94835205228 23810
    ## - tags_count                     1   68886025 94855253649 23810
    ## - origin_countryUS               1   77156530 94863524154 23811
    ## - origin_countrySG               1   79211742 94865579366 23811
    ## - product_colorsblue             1  120463771 94906831396 23811
    ## - merchant_rating                1  126769980 94913137605 23811
    ## <none>                                        94786367625 23811
    ## - product_colorswhite            1  152840306 94939207931 23812
    ## - rating                         1  183126795 94969494420 23812
    ## - product_colorsgrey             1  210912926 94997280551 23812
    ## - product_colorsblack            1  248627182 95034994807 23813
    ## - product_colorsOther_colors     1  257640729 95044008354 23813
    ## - price                          1  300643195 95087010820 23814
    ## - product_variation_inventory    1  354939599 95141307224 23814
    ## - product_colorspurple           1  359747241 95146114865 23814
    ## - product_sizesOther_sizes       1  481832606 95268200231 23816
    ## - product_sizesS                 1  534349055 95320716680 23817
    ## - `price_classEUR10-20`          1  689741168 95476108793 23819
    ## - shipping_option_price          1  696571706 95482939331 23819
    ## - countries_shipped_to           1  713271618 95499639243 23819
    ## - product_sizesXXS               1  871939976 95658307601 23821
    ## - product_sizesXXL               1  876264873 95662632497 23822
    ## - merchant_has_profile_picture1  1  953315857 95739683482 23823
    ## - product_sizesXS                1 1651847401 96438215026 23832
    ## - merchant_rating_count          1 4326869319 99113236944 23868
    ## 
    ## Step:  AIC=23809.52
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1    9941349 94804381824 23808
    ## - product_colorsyellow           1   10783323 94805223797 23808
    ## - shipping_nameOther_shipping    1   23201205 94817641680 23808
    ## - `price_classEUR40-50`          1   26690589 94821131063 23808
    ## - origin_countryVE               1   28997967 94823438441 23808
    ## - retail_price                   1   31477113 94825917587 23808
    ## - product_colorsred              1   36309267 94830749741 23808
    ## - `price_classEUR20-30`          1   36396112 94830836586 23808
    ## - discount_per                   1   39790871 94834231345 23808
    ## - product_colorsgreen            1   46618432 94841058906 23808
    ## - tags_count                     1   69588301 94864028775 23809
    ## - origin_countrySG               1   77365538 94871806012 23809
    ## - origin_countryUS               1   77593121 94872033595 23809
    ## - product_colorsblue             1  118939848 94913380322 23809
    ## - merchant_rating                1  127079936 94921520410 23809
    ## <none>                                        94794440474 23810
    ## - product_colorswhite            1  150875320 94945315794 23810
    ## - rating                         1  186867468 94981307942 23810
    ## - product_colorsgrey             1  207754648 95002195122 23810
    ## - product_colorsblack            1  244668861 95039109335 23811
    ## - product_colorsOther_colors     1  253702783 95048143257 23811
    ## - price                          1  305823372 95100263846 23812
    ## - product_colorspurple           1  357439953 95151880427 23813
    ## - product_variation_inventory    1  371277552 95165718026 23813
    ## - product_sizesOther_sizes       1  493503074 95287943548 23814
    ## - product_sizesS                 1  530627915 95325068389 23815
    ## - `price_classEUR10-20`          1  695998773 95490439247 23817
    ## - shipping_option_price          1  700631738 95495072212 23817
    ## - countries_shipped_to           1  711088994 95505529468 23817
    ## - product_sizesXXL               1  872132450 95666572924 23820
    ## - product_sizesXXS               1  880629687 95675070161 23820
    ## - merchant_has_profile_picture1  1  953677025 95748117499 23821
    ## - product_sizesXS                1 1645160127 96439600601 23830
    ## - merchant_rating_count          1 4319466419 99113906893 23866
    ## 
    ## Step:  AIC=23807.65
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1    1938923 94806320747 23806
    ## - shipping_nameOther_shipping    1   24835248 94829217072 23806
    ## - `price_classEUR40-50`          1   26979934 94831361758 23806
    ## - origin_countryVE               1   29085043 94833466866 23806
    ## - retail_price                   1   29610862 94833992685 23806
    ## - product_colorsred              1   30312774 94834694597 23806
    ## - `price_classEUR20-30`          1   36479477 94840861301 23806
    ## - discount_per                   1   39502139 94843883963 23806
    ## - product_colorsgreen            1   43718276 94848100100 23806
    ## - tags_count                     1   67439190 94871821014 23807
    ## - origin_countryUS               1   76340337 94880722161 23807
    ## - origin_countrySG               1   76941042 94881322866 23807
    ## - merchant_rating                1  124311223 94928693046 23807
    ## <none>                                        94804381824 23808
    ## - product_colorsblue             1  163886688 94968268512 23808
    ## - rating                         1  190887824 94995269648 23808
    ## - product_colorswhite            1  270414406 95074796230 23809
    ## - price                          1  305921549 95110303372 23810
    ## - product_colorsgrey             1  314668806 95119050630 23810
    ## - product_variation_inventory    1  365594776 95169976599 23811
    ## - product_colorsblack            1  498118887 95302500711 23813
    ## - product_sizesOther_sizes       1  498727411 95303109235 23813
    ## - product_colorspurple           1  519362629 95323744453 23813
    ## - product_sizesS                 1  523692952 95328074776 23813
    ## - product_colorsOther_colors     1  537771042 95342152866 23813
    ## - `price_classEUR10-20`          1  695236222 95499618045 23815
    ## - shipping_option_price          1  696400134 95500781958 23815
    ## - countries_shipped_to           1  715362494 95519744317 23816
    ## - product_sizesXXL               1  870836543 95675218366 23818
    ## - product_sizesXXS               1  888575702 95692957525 23818
    ## - merchant_has_profile_picture1  1  950011399 95754393223 23819
    ## - product_sizesXS                1 1635609985 96439991809 23828
    ## - merchant_rating_count          1 4332457184 99136839007 23864
    ## 
    ## Step:  AIC=23805.68
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1   25215154 94831535901 23804
    ## - `price_classEUR40-50`          1   27051346 94833372093 23804
    ## - origin_countryVE               1   29158779 94835479526 23804
    ## - retail_price                   1   29331274 94835652021 23804
    ## - product_colorsred              1   29608102 94835928849 23804
    ## - `price_classEUR20-30`          1   36483897 94842804644 23804
    ## - discount_per                   1   39711421 94846032168 23804
    ## - product_colorsgreen            1   43872935 94850193681 23804
    ## - tags_count                     1   67095570 94873416316 23805
    ## - origin_countryUS               1   74895281 94881216028 23805
    ## - origin_countrySG               1   76742231 94883062978 23805
    ## - merchant_rating                1  123993279 94930314025 23805
    ## <none>                                        94806320747 23806
    ## - product_colorsblue             1  181325445 94987646192 23806
    ## - rating                         1  191189034 94997509781 23806
    ## - price                          1  305729287 95112050034 23808
    ## - product_colorswhite            1  338896267 95145217013 23808
    ## - product_colorsgrey             1  355713546 95162034293 23809
    ## - product_variation_inventory    1  364520250 95170840997 23809
    ## - product_sizesOther_sizes       1  500720827 95307041573 23811
    ## - product_sizesS                 1  528492489 95334813236 23811
    ## - product_colorspurple           1  570742400 95377063146 23812
    ## - product_colorsblack            1  651135746 95457456492 23813
    ## - `price_classEUR10-20`          1  694149948 95500470694 23813
    ## - shipping_option_price          1  695100368 95501421115 23813
    ## - product_colorsOther_colors     1  725279551 95531600297 23814
    ## - countries_shipped_to           1  730454145 95536774892 23814
    ## - product_sizesXXL               1  871227485 95677548231 23816
    ## - product_sizesXXS               1  894140292 95700461039 23816
    ## - merchant_has_profile_picture1  1  951064526 95757385272 23817
    ## - product_sizesXS                1 1634006926 96440327672 23826
    ## - merchant_rating_count          1 4341134090 99147454837 23862
    ## 
    ## Step:  AIC=23804.03
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   27077830 94858613731 23802
    ## - origin_countryVE               1   28876228 94860412129 23802
    ## - retail_price                   1   32267369 94863803270 23803
    ## - product_colorsred              1   33050001 94864585902 23803
    ## - `price_classEUR20-30`          1   36744711 94868280612 23803
    ## - discount_per                   1   41654579 94873190480 23803
    ## - product_colorsgreen            1   46810344 94878346245 23803
    ## - tags_count                     1   67559703 94899095604 23803
    ## - origin_countryUS               1   71956804 94903492705 23803
    ## - origin_countrySG               1   77261836 94908797737 23803
    ## - merchant_rating                1  122420774 94953956675 23804
    ## <none>                                        94831535901 23804
    ## - rating                         1  189348398 95020884299 23805
    ## - product_colorsblue             1  193210251 95024746152 23805
    ## - price                          1  304646232 95136182133 23806
    ## - product_colorswhite            1  353469731 95185005632 23807
    ## - product_colorsgrey             1  367880160 95199416062 23807
    ## - product_variation_inventory    1  367916042 95199451943 23807
    ## - product_sizesOther_sizes       1  506364976 95337900877 23809
    ## - product_sizesS                 1  525733345 95357269246 23809
    ## - product_colorspurple           1  591832821 95423368723 23810
    ## - product_colorsblack            1  672075536 95503611437 23811
    ## - `price_classEUR10-20`          1  687671392 95519207293 23812
    ## - shipping_option_price          1  712073863 95543609764 23812
    ## - countries_shipped_to           1  726580885 95558116786 23812
    ## - product_colorsOther_colors     1  737113362 95568649263 23812
    ## - product_sizesXXL               1  874842344 95706378245 23814
    ## - product_sizesXXS               1  882748055 95714283956 23814
    ## - merchant_has_profile_picture1  1  962774093 95794309994 23815
    ## - product_sizesXS                1 1635343800 96466879701 23825
    ## - merchant_rating_count          1 4344742091 99176277992 23861
    ## 
    ## Step:  AIC=23802.41
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR20-30`          1   23766928 94882380659 23801
    ## - origin_countryVE               1   28175500 94886789231 23801
    ## - product_colorsred              1   28415325 94887029056 23801
    ## - retail_price                   1   34872167 94893485898 23801
    ## - product_colorsgreen            1   40451756 94899065487 23801
    ## - discount_per                   1   43176409 94901790140 23801
    ## - origin_countryUS               1   69664683 94928278414 23801
    ## - tags_count                     1   70346150 94928959881 23801
    ## - origin_countrySG               1   75078198 94933691930 23801
    ## - merchant_rating                1  134322904 94992936635 23802
    ## <none>                                        94858613731 23802
    ## - rating                         1  194482891 95053096622 23803
    ## - product_colorsblue             1  197252660 95055866391 23803
    ## - price                          1  278839236 95137452967 23804
    ## - product_colorswhite            1  349123001 95207736732 23805
    ## - product_colorsgrey             1  366661483 95225275214 23806
    ## - product_variation_inventory    1  374139069 95232752800 23806
    ## - product_sizesS                 1  536201026 95394814757 23808
    ## - product_sizesOther_sizes       1  553026044 95411639775 23808
    ## - product_colorspurple           1  589871730 95448485462 23809
    ## - product_colorsblack            1  666854083 95525467815 23810
    ## - shipping_option_price          1  692112850 95550726581 23810
    ## - `price_classEUR10-20`          1  698685741 95557299473 23810
    ## - product_colorsOther_colors     1  733833010 95592446741 23811
    ## - countries_shipped_to           1  734130664 95592744395 23811
    ## - product_sizesXXL               1  872175352 95730789084 23812
    ## - product_sizesXXS               1  908944656 95767558387 23813
    ## - merchant_has_profile_picture1  1  974530107 95833143839 23814
    ## - product_sizesXS                1 1655377476 96513991207 23823
    ## - merchant_rating_count          1 4334744410 99193358141 23859
    ## 
    ## Step:  AIC=23800.73
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   27330933 94909711592 23799
    ## - product_colorsred              1   28964557 94911345217 23799
    ## - retail_price                   1   31198946 94913579605 23799
    ## - product_colorsgreen            1   40644109 94923024769 23799
    ## - discount_per                   1   43045713 94925426372 23799
    ## - origin_countryUS               1   67294329 94949674988 23800
    ## - tags_count                     1   71159188 94953539847 23800
    ## - origin_countrySG               1   73931612 94956312271 23800
    ## - merchant_rating                1  133428090 95015808750 23801
    ## <none>                                        94882380659 23801
    ## - product_colorsblue             1  199061071 95081441730 23802
    ## - rating                         1  200749847 95083130506 23802
    ## - price                          1  255718636 95138099296 23802
    ## - product_colorswhite            1  347409090 95229789750 23804
    ## - product_colorsgrey             1  365443372 95247824031 23804
    ## - product_variation_inventory    1  384239569 95266620228 23804
    ## - product_sizesS                 1  534989850 95417370510 23806
    ## - product_sizesOther_sizes       1  547881143 95430261803 23806
    ## - product_colorspurple           1  588010088 95470390747 23807
    ## - product_colorsblack            1  657418963 95539799622 23808
    ## - shipping_option_price          1  669642656 95552023315 23808
    ## - `price_classEUR10-20`          1  709143936 95591524595 23809
    ## - product_colorsOther_colors     1  721908391 95604289050 23809
    ## - countries_shipped_to           1  738838383 95621219042 23809
    ## - product_sizesXXL               1  867658156 95750038815 23811
    ## - product_sizesXXS               1  918141621 95800522280 23811
    ## - merchant_has_profile_picture1  1  981443723 95863824382 23812
    ## - product_sizesXS                1 1662345920 96544726579 23822
    ## - merchant_rating_count          1 4344535169 99226915829 23858
    ## 
    ## Step:  AIC=23799.11
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1   28698261 94938409853 23798
    ## - retail_price                   1   31604983 94941316575 23798
    ## - product_colorsgreen            1   40293968 94950005560 23798
    ## - discount_per                   1   44288389 94953999981 23798
    ## - origin_countryUS               1   66232268 94975943860 23798
    ## - tags_count                     1   69917313 94979628905 23798
    ## - origin_countrySG               1   74167846 94983879439 23798
    ## - merchant_rating                1  139934813 95049646405 23799
    ## <none>                                        94909711592 23799
    ## - product_colorsblue             1  199235885 95108947477 23800
    ## - rating                         1  217473464 95127185056 23800
    ## - price                          1  247316135 95157027727 23801
    ## - product_colorswhite            1  337081098 95246792690 23802
    ## - product_colorsgrey             1  363518060 95273229652 23802
    ## - product_variation_inventory    1  387453966 95297165558 23803
    ## - product_sizesS                 1  528849305 95438560897 23804
    ## - product_sizesOther_sizes       1  542849681 95452561273 23805
    ## - product_colorspurple           1  584098295 95493809887 23805
    ## - product_colorsblack            1  643331043 95553042635 23806
    ## - shipping_option_price          1  656183869 95565895461 23806
    ## - `price_classEUR10-20`          1  704501463 95614213055 23807
    ## - product_colorsOther_colors     1  718531634 95628243226 23807
    ## - countries_shipped_to           1  737481312 95647192904 23807
    ## - product_sizesXXL               1  859771174 95769482767 23809
    ## - product_sizesXXS               1  909356209 95819067801 23810
    ## - merchant_has_profile_picture1  1  984759909 95894471501 23811
    ## - product_sizesXS                1 1664892143 96574603736 23820
    ## - merchant_rating_count          1 4343712399 99253423991 23856
    ## 
    ## Step:  AIC=23797.51
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   25532486 94963942339 23796
    ## - retail_price                   1   31004257 94969414110 23796
    ## - discount_per                   1   43368944 94981778797 23796
    ## - origin_countryUS               1   70430856 95008840709 23797
    ## - origin_countrySG               1   74101121 95012510974 23797
    ## - tags_count                     1   74641456 95013051309 23797
    ## <none>                                        94938409853 23798
    ## - merchant_rating                1  145733101 95084142954 23798
    ## - product_colorsblue             1  172289358 95110699211 23798
    ## - rating                         1  222737469 95161147322 23799
    ## - price                          1  245716147 95184126000 23799
    ## - product_colorswhite            1  312145026 95250554879 23800
    ## - product_colorsgrey             1  334832639 95273242492 23800
    ## - product_variation_inventory    1  383668735 95322078588 23801
    ## - product_sizesS                 1  530025126 95468434979 23803
    ## - product_sizesOther_sizes       1  541561858 95479971711 23803
    ## - product_colorspurple           1  555628898 95494038751 23803
    ## - product_colorsblack            1  642794899 95581204752 23804
    ## - shipping_option_price          1  649505134 95587914987 23805
    ## - `price_classEUR10-20`          1  701512064 95639921917 23805
    ## - product_colorsOther_colors     1  729062730 95667472583 23806
    ## - countries_shipped_to           1  755764705 95694174558 23806
    ## - product_sizesXXL               1  858141714 95796551567 23807
    ## - product_sizesXXS               1  923826448 95862236301 23808
    ## - merchant_has_profile_picture1  1  973697820 95912107673 23809
    ## - product_sizesXS                1 1665552014 96603961867 23818
    ## - merchant_rating_count          1 4348536035 99286945888 23854
    ## 
    ## Step:  AIC=23795.86
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1   28979545 94992921884 23794
    ## - discount_per                   1   39197712 95003140051 23794
    ## - origin_countryUS               1   71241521 95035183860 23795
    ## - origin_countrySG               1   74103187 95038045526 23795
    ## - tags_count                     1   74310386 95038252726 23795
    ## - merchant_rating                1  144396734 95108339073 23796
    ## <none>                                        94963942339 23796
    ## - product_colorsblue             1  151980421 95115922761 23796
    ## - rating                         1  231490025 95195432364 23797
    ## - price                          1  248920950 95212863290 23797
    ## - product_colorswhite            1  286615905 95250558245 23798
    ## - product_colorsgrey             1  310967372 95274909711 23798
    ## - product_variation_inventory    1  367702004 95331644343 23799
    ## - product_sizesS                 1  529872388 95493814728 23801
    ## - product_colorspurple           1  531298220 95495240559 23801
    ## - product_sizesOther_sizes       1  536038807 95499981146 23801
    ## - product_colorsblack            1  623969705 95587912044 23803
    ## - shipping_option_price          1  652087824 95616030163 23803
    ## - `price_classEUR10-20`          1  704966692 95668909031 23804
    ## - product_colorsOther_colors     1  715849846 95679792185 23804
    ## - countries_shipped_to           1  747507796 95711450135 23804
    ## - product_sizesXXL               1  857949346 95821891685 23806
    ## - product_sizesXXS               1  926548399 95890490739 23807
    ## - merchant_has_profile_picture1  1 1003763955 95967706294 23808
    ## - product_sizesXS                1 1672442543 96636384882 23817
    ## - merchant_rating_count          1 4353192706 99317135045 23853
    ## 
    ## Step:  AIC=23794.26
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - discount_per                   1   10980721 95003902605 23792
    ## - origin_countryUS               1   73231089 95066152973 23793
    ## - tags_count                     1   73318702 95066240587 23793
    ## - origin_countrySG               1   75449943 95068371827 23793
    ## <none>                                        94992921884 23794
    ## - merchant_rating                1  149607164 95142529048 23794
    ## - product_colorsblue             1  151470143 95144392027 23794
    ## - rating                         1  229378641 95222300525 23795
    ## - price                          1  276644057 95269565942 23796
    ## - product_colorswhite            1  288422017 95281343901 23796
    ## - product_colorsgrey             1  304908603 95297830487 23797
    ## - product_variation_inventory    1  366272272 95359194157 23797
    ## - product_sizesS                 1  524220314 95517142198 23800
    ## - product_colorspurple           1  533977327 95526899211 23800
    ## - product_sizesOther_sizes       1  540746122 95533668006 23800
    ## - shipping_option_price          1  641099833 95634021717 23801
    ## - product_colorsblack            1  643908020 95636829905 23801
    ## - `price_classEUR10-20`          1  693610140 95686532024 23802
    ## - product_colorsOther_colors     1  715985470 95708907354 23802
    ## - countries_shipped_to           1  748380256 95741302140 23803
    ## - product_sizesXXL               1  873936721 95866858605 23804
    ## - product_sizesXXS               1  944152593 95937074477 23805
    ## - merchant_has_profile_picture1  1 1020224054 96013145938 23806
    ## - product_sizesXS                1 1659813458 96652735342 23815
    ## - merchant_rating_count          1 4437675226 99430597111 23852
    ## 
    ## Step:  AIC=23792.41
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - tags_count                     1   67143390 95071045995 23791
    ## - origin_countryUS               1   75868146 95079770751 23792
    ## - origin_countrySG               1   76396926 95080299531 23792
    ## <none>                                        95003902605 23792
    ## - merchant_rating                1  149655341 95153557947 23793
    ## - product_colorsblue             1  151253234 95155155839 23793
    ## - rating                         1  229790481 95233693086 23794
    ## - price                          1  283463565 95287366170 23794
    ## - product_colorswhite            1  285868691 95289771296 23794
    ## - product_colorsgrey             1  304326961 95308229566 23795
    ## - product_variation_inventory    1  366163197 95370065802 23796
    ## - product_sizesS                 1  526536627 95530439232 23798
    ## - product_colorspurple           1  534904792 95538807398 23798
    ## - product_sizesOther_sizes       1  540751708 95544654314 23798
    ## - shipping_option_price          1  641770195 95645672800 23799
    ## - product_colorsblack            1  647072665 95650975270 23799
    ## - `price_classEUR10-20`          1  705835892 95709738497 23800
    ## - product_colorsOther_colors     1  715945981 95719848587 23800
    ## - countries_shipped_to           1  759034309 95762936914 23801
    ## - product_sizesXXL               1  868385484 95872288089 23802
    ## - product_sizesXXS               1  939203209 95943105814 23803
    ## - merchant_has_profile_picture1  1 1024562317 96028464922 23805
    ## - product_sizesXS                1 1649796545 96653699150 23813
    ## - merchant_rating_count          1 4428942191 99432844797 23850
    ## 
    ## Step:  AIC=23791.34
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryUS               1   68559216 95139605211 23790
    ## - origin_countrySG               1   75972852 95147018848 23790
    ## <none>                                        95071045995 23791
    ## - product_colorsblue             1  149867095 95220913090 23791
    ## - merchant_rating                1  171301949 95242347944 23792
    ## - rating                         1  224616684 95295662679 23792
    ## - price                          1  291799145 95362845140 23793
    ## - product_colorswhite            1  299753463 95370799458 23794
    ## - product_colorsgrey             1  316065004 95387110999 23794
    ## - product_variation_inventory    1  381497350 95452543345 23795
    ## - product_sizesOther_sizes       1  497058322 95568104317 23796
    ## - product_sizesS                 1  504859391 95575905386 23796
    ## - product_colorspurple           1  525222391 95596268386 23797
    ## - shipping_option_price          1  625546780 95696592775 23798
    ## - product_colorsblack            1  664447471 95735493466 23799
    ## - product_colorsOther_colors     1  696469934 95767515929 23799
    ## - `price_classEUR10-20`          1  704813328 95775859324 23799
    ## - countries_shipped_to           1  736156838 95807202833 23800
    ## - product_sizesXXL               1  842711987 95913757983 23801
    ## - product_sizesXXS               1  900294764 95971340759 23802
    ## - merchant_has_profile_picture1  1 1000574842 96071620838 23803
    ## - product_sizesXS                1 1595739247 96666785243 23811
    ## - merchant_rating_count          1 4368230712 99439276708 23848
    ## 
    ## Step:  AIC=23790.29
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countrySG               1   76368197 95215973408 23789
    ## <none>                                        95139605211 23790
    ## - product_colorsblue             1  159666070 95299271281 23791
    ## - merchant_rating                1  175567597 95315172808 23791
    ## - rating                         1  228611537 95368216748 23791
    ## - price                          1  283980928 95423586139 23792
    ## - product_colorsgrey             1  311206701 95450811912 23793
    ## - product_colorswhite            1  313429765 95453034976 23793
    ## - product_variation_inventory    1  375037694 95514642905 23793
    ## - product_sizesOther_sizes       1  494714263 95634319474 23795
    ## - product_sizesS                 1  497134427 95636739638 23795
    ## - product_colorspurple           1  534060445 95673665656 23796
    ## - shipping_option_price          1  616015306 95755620517 23797
    ## - product_colorsblack            1  661010620 95800615831 23797
    ## - product_colorsOther_colors     1  688765885 95828371096 23798
    ## - `price_classEUR10-20`          1  709446110 95849051321 23798
    ## - countries_shipped_to           1  757217768 95896822979 23799
    ## - product_sizesXXL               1  831448274 95971053484 23800
    ## - product_sizesXXS               1  922375266 96061980477 23801
    ## - merchant_has_profile_picture1  1 1029840811 96169446022 23802
    ## - product_sizesXS                1 1576074286 96715679497 23810
    ## - merchant_rating_count          1 4406811295 99546416506 23848
    ## 
    ## Step:  AIC=23789.34
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## <none>                                        95215973408 23789
    ## - product_colorsblue             1  161270448 95377243856 23790
    ## - merchant_rating                1  172701938 95388675346 23790
    ## - rating                         1  234461215 95450434623 23791
    ## - price                          1  280270917 95496244325 23791
    ## - product_colorswhite            1  315743951 95531717359 23792
    ## - product_colorsgrey             1  334897596 95550871004 23792
    ## - product_variation_inventory    1  370662549 95586635957 23792
    ## - product_sizesOther_sizes       1  496363113 95712336521 23794
    ## - product_sizesS                 1  500280526 95716253934 23794
    ## - product_colorspurple           1  533526058 95749499465 23795
    ## - shipping_option_price          1  619670001 95835643409 23796
    ## - product_colorsblack            1  666595525 95882568933 23797
    ## - `price_classEUR10-20`          1  703659355 95919632763 23797
    ## - product_colorsOther_colors     1  707586433 95923559841 23797
    ## - countries_shipped_to           1  771539822 95987513230 23798
    ## - product_sizesXXL               1  831641506 96047614914 23799
    ## - product_sizesXXS               1  931106201 96147079609 23800
    ## - merchant_has_profile_picture1  1 1026974455 96242947863 23801
    ## - product_sizesXS                1 1556883829 96772857237 23809
    ## - merchant_rating_count          1 4396918392 99612891800 23847
    ## Start:  AIC=23602.32
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23602.32
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23602.32
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1     352859 81128199692 23600
    ## - origin_countryGB               1     596504 81128443337 23600
    ## - product_colorspink             1    2553862 81130400695 23600
    ## - product_colorsyellow           1    4101162 81131947995 23600
    ## - product_colorsred              1    5989344 81133836177 23600
    ## - product_colorsgrey             1    8176183 81136023016 23601
    ## - product_sizesM                 1   10667340 81138514173 23601
    ## - shipping_nameOther_shipping    1   14899504 81142746337 23601
    ## - badge_local_product1           1   16132278 81143979111 23601
    ## - origin_countryVE               1   17845361 81145692194 23601
    ## - product_colorsgreen            1   19309933 81147156766 23601
    ## - badges_count                   1   20739326 81148586159 23601
    ## - `price_classEUR40-50`          1   26870869 81154717702 23601
    ## - uses_ad_boosts1                1   26917774 81154764607 23601
    ## - badge_product_quality1         1   32048743 81159895576 23601
    ## - product_colorswhite            1   54865338 81182712171 23601
    ## - product_colorsblue             1   55804412 81183651245 23601
    ## - product_sizesS                 1   59339764 81187186597 23601
    ## - `price_classEUR20-30`          1   64794804 81192641637 23601
    ## - retail_price                   1   91673596 81219520429 23602
    ## - origin_countryUS               1   92490177 81220337010 23602
    ## - origin_countrySG               1   99665480 81227512313 23602
    ## - product_colorsOther_colors     1  113085199 81240932032 23602
    ## - product_sizesOther_sizes       1  113904723 81241751556 23602
    ## - rating                         1  122835985 81250682818 23602
    ## <none>                                        81127846833 23602
    ## - product_variation_inventory    1  136616144 81264462977 23603
    ## - product_colorsblack            1  158797890 81286644723 23603
    ## - tags_count                     1  175019925 81302866758 23603
    ## - price                          1  219859136 81347705969 23604
    ## - discount_per                   1  221913045 81349759878 23604
    ## - product_colorspurple           1  231419490 81359266323 23604
    ## - product_sizesXXS               1  270279835 81398126668 23605
    ## - merchant_rating                1  301896994 81429743827 23605
    ## - product_sizesXS                1  350485647 81478332480 23606
    ## - shipping_option_price          1  489576597 81617423430 23608
    ## - product_sizesXXL               1  498822294 81626669127 23608
    ## - `price_classEUR10-20`          1  571111605 81698958438 23610
    ## - countries_shipped_to           1  794735710 81922582543 23613
    ## - merchant_has_profile_picture1  1  908051540 82035898373 23615
    ## - merchant_rating_count          1 3710745353 84838592186 23659
    ## 
    ## Step:  AIC=23600.32
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1     587893 81128787585 23598
    ## - product_colorspink             1    2568902 81130768594 23598
    ## - product_colorsyellow           1    4081872 81132281564 23598
    ## - product_colorsred              1    6045633 81134245325 23598
    ## - product_colorsgrey             1    8183890 81136383582 23599
    ## - product_sizesM                 1   10635174 81138834866 23599
    ## - shipping_nameOther_shipping    1   16124251 81144323943 23599
    ## - badge_local_product1           1   17368941 81145568633 23599
    ## - origin_countryVE               1   17911699 81146111391 23599
    ## - product_colorsgreen            1   19457878 81147657570 23599
    ## - badges_count                   1   24092852 81152292544 23599
    ## - uses_ad_boosts1                1   26732524 81154932216 23599
    ## - `price_classEUR40-50`          1   28885315 81157085008 23599
    ## - badge_product_quality1         1   36268659 81164468351 23599
    ## - product_colorswhite            1   54841197 81183040889 23599
    ## - product_colorsblue             1   55802956 81184002648 23599
    ## - product_sizesS                 1   59616035 81187815727 23599
    ## - `price_classEUR20-30`          1   64515957 81192715649 23599
    ## - retail_price                   1   92104292 81220303984 23600
    ## - origin_countryUS               1   92372568 81220572260 23600
    ## - origin_countrySG               1   99531562 81227731254 23600
    ## - product_colorsOther_colors     1  112897273 81241096966 23600
    ## - product_sizesOther_sizes       1  114062557 81242262249 23600
    ## - rating                         1  123532757 81251732449 23600
    ## <none>                                        81128199692 23600
    ## - product_variation_inventory    1  137284057 81265483749 23601
    ## - product_colorsblack            1  158839208 81287038900 23601
    ## - tags_count                     1  175655342 81303855034 23601
    ## - price                          1  219548785 81347748477 23602
    ## - discount_per                   1  222281683 81350481375 23602
    ## - product_colorspurple           1  231413251 81359612943 23602
    ## - product_sizesXXS               1  270906311 81399106003 23603
    ## - merchant_rating                1  301816439 81430016131 23603
    ## - product_sizesXS                1  351805363 81480005055 23604
    ## - shipping_option_price          1  496670473 81624870165 23606
    ## - product_sizesXXL               1  498669236 81626868928 23606
    ## - `price_classEUR10-20`          1  577834381 81706034073 23608
    ## - countries_shipped_to           1  795949578 81924149271 23611
    ## - merchant_has_profile_picture1  1  909137236 82037336928 23613
    ## - merchant_rating_count          1 3710665400 84838865092 23657
    ## 
    ## Step:  AIC=23598.33
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1    2591289 81131378874 23596
    ## - product_colorsyellow           1    4048047 81132835632 23596
    ## - product_colorsred              1    6086539 81134874124 23596
    ## - product_colorsgrey             1    8230256 81137017841 23597
    ## - product_sizesM                 1   10668680 81139456265 23597
    ## - shipping_nameOther_shipping    1   16094922 81144882507 23597
    ## - badge_local_product1           1   17369990 81146157575 23597
    ## - origin_countryVE               1   17896962 81146684546 23597
    ## - product_colorsgreen            1   19529763 81148317348 23597
    ## - badges_count                   1   24070272 81152857857 23597
    ## - uses_ad_boosts1                1   27062288 81155849873 23597
    ## - `price_classEUR40-50`          1   28759064 81157546649 23597
    ## - badge_product_quality1         1   36265369 81165052954 23597
    ## - product_colorswhite            1   54912067 81183699652 23597
    ## - product_colorsblue             1   55406812 81184194397 23597
    ## - product_sizesS                 1   59582780 81188370365 23597
    ## - `price_classEUR20-30`          1   64316450 81193104035 23597
    ## - retail_price                   1   92051411 81220838996 23598
    ## - origin_countryUS               1   92420264 81221207849 23598
    ## - origin_countrySG               1   99608791 81228396376 23598
    ## - product_colorsOther_colors     1  113062047 81241849631 23598
    ## - product_sizesOther_sizes       1  113879268 81242666853 23598
    ## - rating                         1  123206150 81251993735 23598
    ## <none>                                        81128787585 23598
    ## - product_variation_inventory    1  138142551 81266930136 23599
    ## - product_colorsblack            1  158976519 81287764104 23599
    ## - tags_count                     1  175263382 81304050967 23599
    ## - price                          1  219417157 81348204742 23600
    ## - discount_per                   1  221934421 81350722006 23600
    ## - product_colorspurple           1  231651176 81360438761 23600
    ## - product_sizesXXS               1  270624595 81399412180 23601
    ## - merchant_rating                1  302079250 81430866835 23601
    ## - product_sizesXS                1  352142604 81480930189 23602
    ## - shipping_option_price          1  497432138 81626219722 23604
    ## - product_sizesXXL               1  498510644 81627298229 23604
    ## - `price_classEUR10-20`          1  577282502 81706070087 23606
    ## - countries_shipped_to           1  796108434 81924896019 23609
    ## - merchant_has_profile_picture1  1  909546733 82038334318 23611
    ## - merchant_rating_count          1 3710590557 84839378142 23655
    ## 
    ## Step:  AIC=23596.38
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1    3588456 81134967330 23594
    ## - product_colorsgrey             1    6105654 81137484528 23595
    ## - product_sizesM                 1   10337779 81141716653 23595
    ## - shipping_nameOther_shipping    1   16460810 81147839684 23595
    ## - badge_local_product1           1   17010714 81148389587 23595
    ## - origin_countryVE               1   17974782 81149353656 23595
    ## - product_colorsgreen            1   22858109 81154236983 23595
    ## - product_colorsyellow           1   23308149 81154687023 23595
    ## - badges_count                   1   23737165 81155116038 23595
    ## - uses_ad_boosts1                1   26742294 81158121168 23595
    ## - `price_classEUR40-50`          1   28965755 81160344629 23595
    ## - badge_product_quality1         1   36176638 81167555512 23595
    ## - product_sizesS                 1   59858748 81191237622 23595
    ## - `price_classEUR20-30`          1   64378494 81195757368 23595
    ## - product_colorsblue             1   88569954 81219948827 23596
    ## - retail_price                   1   90327916 81221706790 23596
    ## - origin_countryUS               1   91825063 81223203937 23596
    ## - origin_countrySG               1   99390818 81230769692 23596
    ## - product_colorswhite            1  106721052 81238099926 23596
    ## - product_sizesOther_sizes       1  116495927 81247874801 23596
    ## <none>                                        81131378874 23596
    ## - rating                         1  124363482 81255742356 23596
    ## - product_variation_inventory    1  136466557 81267845430 23597
    ## - tags_count                     1  173606179 81304985053 23597
    ## - price                          1  220053120 81351431994 23598
    ## - discount_per                   1  220635744 81352014618 23598
    ## - product_colorsOther_colors     1  266386249 81397765123 23599
    ## - product_sizesXXS               1  275204254 81406583128 23599
    ## - merchant_rating                1  300424068 81431802942 23599
    ## - product_sizesXS                1  351995521 81483374395 23600
    ## - product_colorspurple           1  370234507 81501613381 23600
    ## - product_colorsblack            1  375753539 81507132412 23600
    ## - shipping_option_price          1  496066627 81627445501 23602
    ## - product_sizesXXL               1  499680229 81631059103 23602
    ## - `price_classEUR10-20`          1  577053027 81708431901 23604
    ## - countries_shipped_to           1  799158077 81930536951 23607
    ## - merchant_has_profile_picture1  1  908302217 82039681091 23609
    ## - merchant_rating_count          1 3717667034 84849045908 23653
    ## 
    ## Step:  AIC=23594.43
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgrey             1    3538589 81138505918 23593
    ## - product_sizesM                 1    9580524 81144547854 23593
    ## - badge_local_product1           1   17379093 81152346422 23593
    ## - shipping_nameOther_shipping    1   17457873 81152425203 23593
    ## - origin_countryVE               1   17813187 81152780517 23593
    ## - product_colorsgreen            1   19272032 81154239362 23593
    ## - badges_count                   1   24018069 81158985399 23593
    ## - uses_ad_boosts1                1   27114668 81162081998 23593
    ## - `price_classEUR40-50`          1   27421315 81162388645 23593
    ## - badge_product_quality1         1   36189591 81171156920 23593
    ## - product_colorsyellow           1   38112601 81173079931 23593
    ## - product_sizesS                 1   62492812 81197460141 23593
    ## - `price_classEUR20-30`          1   63755761 81198723091 23594
    ## - product_colorsblue             1   89289590 81224256920 23594
    ## - retail_price                   1   90126412 81225093741 23594
    ## - origin_countryUS               1   91688976 81226656306 23594
    ## - origin_countrySG               1   99081881 81234049211 23594
    ## - product_colorswhite            1  117993049 81252960379 23594
    ## - product_sizesOther_sizes       1  119798372 81254765702 23594
    ## <none>                                        81134967330 23594
    ## - rating                         1  127334258 81262301588 23595
    ## - product_variation_inventory    1  135410288 81270377618 23595
    ## - tags_count                     1  176098221 81311065551 23595
    ## - price                          1  218277191 81353244520 23596
    ## - discount_per                   1  221903518 81356870848 23596
    ## - product_sizesXXS               1  283030352 81417997682 23597
    ## - merchant_rating                1  303500225 81438467555 23597
    ## - product_colorsOther_colors     1  327583150 81462550480 23598
    ## - product_sizesXS                1  357465758 81492433087 23598
    ## - product_colorspurple           1  393341858 81528309187 23599
    ## - product_colorsblack            1  462700350 81597667680 23600
    ## - shipping_option_price          1  493941844 81628909173 23600
    ## - product_sizesXXL               1  502386899 81637354229 23601
    ## - `price_classEUR10-20`          1  574085425 81709052755 23602
    ## - countries_shipped_to           1  820795214 81955762544 23606
    ## - merchant_has_profile_picture1  1  905197778 82040165107 23607
    ## - merchant_rating_count          1 3721288429 84856255759 23651
    ## 
    ## Step:  AIC=23592.49
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1   10031621 81148537539 23591
    ## - product_colorsgreen            1   16235726 81154741644 23591
    ## - badge_local_product1           1   16679924 81155185843 23591
    ## - origin_countryVE               1   17683342 81156189260 23591
    ## - shipping_nameOther_shipping    1   18403287 81156909205 23591
    ## - badges_count                   1   23258250 81161764168 23591
    ## - uses_ad_boosts1                1   26478620 81164984539 23591
    ## - `price_classEUR40-50`          1   28003142 81166509060 23591
    ## - badge_product_quality1         1   35593902 81174099820 23591
    ## - product_colorsyellow           1   48275266 81186781184 23591
    ## - product_sizesS                 1   61364692 81199870611 23592
    ## - `price_classEUR20-30`          1   63551268 81202057187 23592
    ## - product_colorsblue             1   86424015 81224929933 23592
    ## - retail_price                   1   88841137 81227347056 23592
    ## - origin_countryUS               1   90744214 81229250132 23592
    ## - origin_countrySG               1  102106723 81240612641 23592
    ## - product_sizesOther_sizes       1  118570190 81257076108 23592
    ## - product_colorswhite            1  119768908 81258274826 23592
    ## <none>                                        81138505918 23593
    ## - rating                         1  130926032 81269431950 23593
    ## - product_variation_inventory    1  135219381 81273725299 23593
    ## - tags_count                     1  177484821 81315990740 23593
    ## - price                          1  218212509 81356718427 23594
    ## - discount_per                   1  219953248 81358459166 23594
    ## - product_sizesXXS               1  281609886 81420115805 23595
    ## - merchant_rating                1  303816030 81442321948 23595
    ## - product_sizesXS                1  355224004 81493729922 23596
    ## - product_colorsOther_colors     1  356700253 81495206171 23596
    ## - product_colorspurple           1  397788672 81536294591 23597
    ## - shipping_option_price          1  493740178 81632246096 23598
    ## - product_sizesXXL               1  500271189 81638777108 23599
    ## - product_colorsblack            1  508147722 81646653640 23599
    ## - `price_classEUR10-20`          1  575733647 81714239565 23600
    ## - countries_shipped_to           1  826192046 81964697965 23604
    ## - merchant_has_profile_picture1  1  901688564 82040194482 23605
    ## - merchant_rating_count          1 3776444855 84914950773 23650
    ## 
    ## Step:  AIC=23590.65
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   15206097 81163743636 23589
    ## - badge_local_product1           1   15592038 81164129577 23589
    ## - origin_countryVE               1   17497959 81166035498 23589
    ## - shipping_nameOther_shipping    1   18763785 81167301324 23589
    ## - badges_count                   1   23034571 81171572110 23589
    ## - uses_ad_boosts1                1   26764009 81175301548 23589
    ## - `price_classEUR40-50`          1   28198433 81176735972 23589
    ## - badge_product_quality1         1   35510704 81184048243 23589
    ## - product_colorsyellow           1   50317411 81198854950 23590
    ## - `price_classEUR20-30`          1   62703503 81211241042 23590
    ## - origin_countryUS               1   87545608 81236083148 23590
    ## - retail_price                   1   89749871 81238287411 23590
    ## - product_colorsblue             1   89906440 81238443979 23590
    ## - origin_countrySG               1  102162446 81250699985 23590
    ## - product_colorswhite            1  123246357 81271783896 23591
    ## <none>                                        81148537539 23591
    ## - rating                         1  129870015 81278407554 23591
    ## - product_variation_inventory    1  134111085 81282648624 23591
    ## - tags_count                     1  174815139 81323352678 23592
    ## - discount_per                   1  220545638 81369083178 23592
    ## - price                          1  221415350 81369952889 23592
    ## - merchant_rating                1  304801698 81453339237 23594
    ## - product_sizesOther_sizes       1  348814844 81497352383 23594
    ## - product_colorsOther_colors     1  354612909 81503150448 23594
    ## - product_colorspurple           1  397329360 81545866900 23595
    ## - product_sizesS                 1  407713223 81556250762 23595
    ## - shipping_option_price          1  497776472 81646314011 23597
    ## - product_colorsblack            1  514729745 81663267284 23597
    ## - `price_classEUR10-20`          1  586195628 81734733167 23598
    ## - product_sizesXXS               1  673821312 81822358852 23600
    ## - product_sizesXXL               1  723739084 81872276623 23600
    ## - countries_shipped_to           1  831757264 81980294803 23602
    ## - merchant_has_profile_picture1  1  908676268 82057213807 23603
    ## - product_sizesXS                1 1278523257 82427060796 23609
    ## - merchant_rating_count          1 3786440357 84934977896 23648
    ## 
    ## Step:  AIC=23588.9
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1   14084801 81177828437 23587
    ## - origin_countryVE               1   17454191 81181197827 23587
    ## - shipping_nameOther_shipping    1   18843152 81182586789 23587
    ## - badges_count                   1   21279554 81185023190 23587
    ## - `price_classEUR40-50`          1   25516439 81189260075 23587
    ## - uses_ad_boosts1                1   26403508 81190147144 23587
    ## - badge_product_quality1         1   33530265 81197273901 23587
    ## - `price_classEUR20-30`          1   61184580 81224928216 23588
    ## - product_colorsyellow           1   67324473 81231068109 23588
    ## - product_colorsblue             1   77761273 81241504909 23588
    ## - origin_countryUS               1   87273936 81251017572 23588
    ## - retail_price                   1   88184230 81251927866 23588
    ## - origin_countrySG               1  100260154 81264003790 23589
    ## - product_colorswhite            1  108282864 81272026500 23589
    ## <none>                                        81163743636 23589
    ## - product_variation_inventory    1  126737140 81290480776 23589
    ## - rating                         1  134109174 81297852810 23589
    ## - tags_count                     1  172046665 81335790301 23590
    ## - discount_per                   1  215477218 81379220854 23590
    ## - price                          1  218502895 81382246531 23590
    ## - merchant_rating                1  302743687 81466487323 23592
    ## - product_colorsOther_colors     1  346285556 81510029193 23593
    ## - product_sizesOther_sizes       1  347378534 81511122170 23593
    ## - product_colorspurple           1  382173059 81545916695 23593
    ## - product_sizesS                 1  409887199 81573630835 23594
    ## - shipping_option_price          1  495199311 81658942947 23595
    ## - product_colorsblack            1  511950461 81675694097 23595
    ## - `price_classEUR10-20`          1  579861359 81743604995 23596
    ## - product_sizesXXS               1  679524496 81843268133 23598
    ## - product_sizesXXL               1  724088293 81887831930 23599
    ## - countries_shipped_to           1  826725373 81990469009 23600
    ## - merchant_has_profile_picture1  1  932100131 82095843767 23602
    ## - product_sizesXS                1 1282074696 82445818333 23607
    ## - merchant_rating_count          1 3777873481 84941617118 23647
    ## 
    ## Step:  AIC=23587.13
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1    7289274 81185117711 23585
    ## - origin_countryVE               1   17554641 81195383078 23585
    ## - shipping_nameOther_shipping    1   19888217 81197716654 23585
    ## - badge_product_quality1         1   20771781 81198600218 23586
    ## - uses_ad_boosts1                1   24605284 81202433721 23586
    ## - `price_classEUR40-50`          1   27433334 81205261771 23586
    ## - `price_classEUR20-30`          1   59898866 81237727302 23586
    ## - product_colorsyellow           1   68378865 81246207302 23586
    ## - product_colorsblue             1   76917565 81254746002 23586
    ## - retail_price                   1   90391139 81268219575 23587
    ## - origin_countryUS               1   90639916 81268468353 23587
    ## - origin_countrySG               1   99671775 81277500211 23587
    ## - product_colorswhite            1  107832108 81285660544 23587
    ## <none>                                        81177828437 23587
    ## - rating                         1  135326746 81313155182 23587
    ## - product_variation_inventory    1  138428759 81316257196 23587
    ## - tags_count                     1  173020542 81350848979 23588
    ## - price                          1  215826398 81393654835 23589
    ## - discount_per                   1  219846781 81397675218 23589
    ## - merchant_rating                1  306021659 81483850096 23590
    ## - product_colorsOther_colors     1  343074370 81520902807 23591
    ## - product_sizesOther_sizes       1  358050457 81535878894 23591
    ## - product_colorspurple           1  383492653 81561321090 23591
    ## - product_sizesS                 1  409719368 81587547805 23592
    ## - shipping_option_price          1  497725070 81675553507 23593
    ## - product_colorsblack            1  506153171 81683981608 23593
    ## - `price_classEUR10-20`          1  572695175 81750523612 23594
    ## - product_sizesXXS               1  679695730 81857524167 23596
    ## - product_sizesXXL               1  726194295 81904022732 23597
    ## - countries_shipped_to           1  827551718 82005380154 23598
    ## - merchant_has_profile_picture1  1  922052020 82099880457 23600
    ## - product_sizesXS                1 1270946812 82448775249 23606
    ## - merchant_rating_count          1 3781904903 84959733340 23645
    ## 
    ## Step:  AIC=23585.24
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1   15671279 81200788990 23584
    ## - origin_countryVE               1   17366979 81202484690 23584
    ## - shipping_nameOther_shipping    1   20272255 81205389966 23584
    ## - uses_ad_boosts1                1   26588037 81211705748 23584
    ## - `price_classEUR40-50`          1   27830720 81212948431 23584
    ## - `price_classEUR20-30`          1   58506668 81243624379 23584
    ## - product_colorsyellow           1   68459017 81253576727 23584
    ## - product_colorsblue             1   79143066 81264260777 23585
    ## - origin_countryUS               1   90237009 81275354720 23585
    ## - retail_price                   1   91405319 81276523030 23585
    ## - origin_countrySG               1   99707246 81284824956 23585
    ## - product_colorswhite            1  108113013 81293230724 23585
    ## <none>                                        81185117711 23585
    ## - rating                         1  138235337 81323353048 23586
    ## - product_variation_inventory    1  146656203 81331773913 23586
    ## - tags_count                     1  170175523 81355293234 23586
    ## - price                          1  216685015 81401802726 23587
    ## - discount_per                   1  223038967 81408156678 23587
    ## - merchant_rating                1  298921242 81484038953 23588
    ## - product_colorsOther_colors     1  342988393 81528106104 23589
    ## - product_sizesOther_sizes       1  372847929 81557965640 23589
    ## - product_colorspurple           1  383693177 81568810888 23589
    ## - product_sizesS                 1  410297485 81595415196 23590
    ## - product_colorsblack            1  506950073 81692067784 23591
    ## - shipping_option_price          1  508838975 81693956685 23591
    ## - `price_classEUR10-20`          1  575472382 81760590093 23593
    ## - product_sizesXXS               1  680252270 81865369981 23594
    ## - product_sizesXXL               1  740416914 81925534624 23595
    ## - countries_shipped_to           1  832433260 82017550971 23597
    ## - merchant_has_profile_picture1  1  925614720 82110732431 23598
    ## - product_sizesXS                1 1272706070 82457823781 23604
    ## - merchant_rating_count          1 3802216906 84987334617 23643
    ## 
    ## Step:  AIC=23583.5
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   16358120 81217147110 23582
    ## - shipping_nameOther_shipping    1   19601737 81220390727 23582
    ## - uses_ad_boosts1                1   25774864 81226563854 23582
    ## - `price_classEUR40-50`          1   27724864 81228513854 23582
    ## - `price_classEUR20-30`          1   58955501 81259744491 23582
    ## - product_colorsyellow           1   71400110 81272189100 23583
    ## - product_colorsblue             1   77496740 81278285730 23583
    ## - origin_countryUS               1   86748571 81287537561 23583
    ## - retail_price                   1   91624700 81292413690 23583
    ## - origin_countrySG               1   98382350 81299171340 23583
    ## - product_colorswhite            1  104547125 81305336115 23583
    ## <none>                                        81200788990 23584
    ## - product_variation_inventory    1  152057164 81352846154 23584
    ## - tags_count                     1  167871663 81368660653 23584
    ## - rating                         1  176495242 81377284232 23584
    ## - price                          1  216006273 81416795263 23585
    ## - discount_per                   1  222805371 81423594361 23585
    ## - merchant_rating                1  313165602 81513954592 23587
    ## - product_colorsOther_colors     1  337755900 81538544890 23587
    ## - product_sizesOther_sizes       1  374379345 81575168335 23588
    ## - product_colorspurple           1  380161584 81580950574 23588
    ## - product_sizesS                 1  409353768 81610142758 23588
    ## - product_colorsblack            1  506172896 81706961886 23590
    ## - shipping_option_price          1  510334385 81711123375 23590
    ## - `price_classEUR10-20`          1  571817149 81772606139 23591
    ## - product_sizesXXS               1  687499754 81888288744 23593
    ## - product_sizesXXL               1  736155001 81936943991 23593
    ## - countries_shipped_to           1  839944422 82040733412 23595
    ## - merchant_has_profile_picture1  1  931707172 82132496162 23597
    ## - product_sizesXS                1 1266824638 82467613628 23602
    ## - merchant_rating_count          1 3863217058 85064006048 23642
    ## 
    ## Step:  AIC=23581.76
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1   19483137 81236630247 23580
    ## - `price_classEUR40-50`          1   27159083 81244306193 23580
    ## - uses_ad_boosts1                1   28870316 81246017425 23580
    ## - `price_classEUR20-30`          1   57037479 81274184589 23581
    ## - product_colorsyellow           1   70578321 81287725431 23581
    ## - product_colorsblue             1   77769552 81294916662 23581
    ## - origin_countryUS               1   85788597 81302935707 23581
    ## - retail_price                   1   92156275 81309303385 23581
    ## - product_colorswhite            1   98586719 81315733828 23581
    ## - origin_countrySG               1   98757781 81315904891 23581
    ## <none>                                        81217147110 23582
    ## - product_variation_inventory    1  153756999 81370904108 23582
    ## - tags_count                     1  167058406 81384205516 23583
    ## - rating                         1  194457174 81411604283 23583
    ## - price                          1  208773713 81425920822 23583
    ## - discount_per                   1  226848639 81443995748 23583
    ## - merchant_rating                1  319914499 81537061608 23585
    ## - product_colorsOther_colors     1  336142714 81553289823 23585
    ## - product_sizesOther_sizes       1  370554701 81587701810 23586
    ## - product_colorspurple           1  377655158 81594802268 23586
    ## - product_sizesS                 1  405137075 81622284185 23586
    ## - product_colorsblack            1  496537414 81713684524 23588
    ## - shipping_option_price          1  500014562 81717161671 23588
    ## - `price_classEUR10-20`          1  565329432 81782476542 23589
    ## - product_sizesXXS               1  680899112 81898046222 23591
    ## - product_sizesXXL               1  730487367 81947634477 23592
    ## - countries_shipped_to           1  840205399 82057352508 23593
    ## - merchant_has_profile_picture1  1  936041381 82153188491 23595
    ## - product_sizesXS                1 1272459226 82489606336 23600
    ## - merchant_rating_count          1 3863699477 85080846587 23641
    ## 
    ## Step:  AIC=23580.08
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1   27153279 81263783526 23579
    ## - `price_classEUR40-50`          1   27188209 81263818456 23579
    ## - `price_classEUR20-30`          1   57401903 81294032150 23579
    ## - product_colorsyellow           1   71281318 81307911565 23579
    ## - product_colorsblue             1   81799293 81318429540 23579
    ## - origin_countryUS               1   83158303 81319788550 23579
    ## - retail_price                   1   97652357 81334282604 23580
    ## - origin_countrySG               1   99213426 81335843673 23580
    ## - product_colorswhite            1  102333941 81338964188 23580
    ## <none>                                        81236630247 23580
    ## - product_variation_inventory    1  156416587 81393046834 23581
    ## - tags_count                     1  166764014 81403394262 23581
    ## - rating                         1  193915956 81430546203 23581
    ## - price                          1  206393351 81443023598 23581
    ## - discount_per                   1  232496932 81469127179 23582
    ## - merchant_rating                1  318690836 81555321083 23583
    ## - product_colorsOther_colors     1  336951486 81573581733 23584
    ## - product_sizesOther_sizes       1  371624068 81608254316 23584
    ## - product_colorspurple           1  386901289 81623531536 23584
    ## - product_sizesS                 1  404764270 81641394517 23585
    ## - product_colorsblack            1  503630823 81740261070 23586
    ## - shipping_option_price          1  508379374 81745009622 23586
    ## - `price_classEUR10-20`          1  559465839 81796096086 23587
    ## - product_sizesXXS               1  672992703 81909622950 23589
    ## - product_sizesXXL               1  733590742 81970220990 23590
    ## - countries_shipped_to           1  834526624 82071156871 23592
    ## - merchant_has_profile_picture1  1  945847720 82182477968 23593
    ## - product_sizesXS                1 1269205253 82505835500 23598
    ## - merchant_rating_count          1 3862912446 85099542694 23639
    ## 
    ## Step:  AIC=23578.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   28306958 81292090484 23577
    ## - `price_classEUR20-30`          1   63995365 81327778891 23578
    ## - product_colorsyellow           1   72603371 81336386897 23578
    ## - product_colorsblue             1   83422171 81347205697 23578
    ## - origin_countryUS               1   83533565 81347317092 23578
    ## - origin_countrySG               1   95257845 81359041371 23578
    ## - retail_price                   1   96035935 81359819461 23578
    ## - product_colorswhite            1  103716002 81367499528 23578
    ## <none>                                        81263783526 23579
    ## - tags_count                     1  169934171 81433717697 23579
    ## - product_variation_inventory    1  171247065 81435030592 23579
    ## - rating                         1  201349915 81465133441 23580
    ## - price                          1  216537635 81480321162 23580
    ## - discount_per                   1  229526437 81493309963 23580
    ## - merchant_rating                1  321351371 81585134897 23582
    ## - product_colorsOther_colors     1  335032012 81598815538 23582
    ## - product_colorspurple           1  389717551 81653501078 23583
    ## - product_sizesOther_sizes       1  390738168 81654521695 23583
    ## - product_sizesS                 1  398408207 81662191733 23583
    ## - product_colorsblack            1  498382763 81762166289 23585
    ## - shipping_option_price          1  515964540 81779748066 23585
    ## - `price_classEUR10-20`          1  571737888 81835521414 23586
    ## - product_sizesXXS               1  685200419 81948983945 23588
    ## - product_sizesXXL               1  727983999 81991767525 23588
    ## - countries_shipped_to           1  826083195 82089866721 23590
    ## - merchant_has_profile_picture1  1  939103912 82202887438 23592
    ## - product_sizesXS                1 1255221835 82519005361 23597
    ## - merchant_rating_count          1 3838962023 85102745549 23637
    ## 
    ## Step:  AIC=23576.97
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR20-30`          1   46627244 81338717729 23576
    ## - product_colorsyellow           1   68450951 81360541435 23576
    ## - origin_countryUS               1   81470023 81373560507 23576
    ## - product_colorsblue             1   91610332 81383700817 23576
    ## - origin_countrySG               1   93237826 81385328310 23577
    ## - retail_price                   1  102362699 81394453184 23577
    ## - product_colorswhite            1  108008055 81400098540 23577
    ## <none>                                        81292090484 23577
    ## - tags_count                     1  174975781 81467066265 23578
    ## - product_variation_inventory    1  175905212 81467995697 23578
    ## - price                          1  188239174 81480329658 23578
    ## - rating                         1  204640016 81496730501 23578
    ## - discount_per                   1  236615554 81528706038 23579
    ## - merchant_rating                1  343407260 81635497745 23581
    ## - product_colorsOther_colors     1  347822585 81639913069 23581
    ## - product_colorspurple           1  395141487 81687231971 23581
    ## - product_sizesS                 1  409263939 81701354423 23582
    ## - product_sizesOther_sizes       1  435788831 81727879316 23582
    ## - shipping_option_price          1  498965854 81791056338 23583
    ## - product_colorsblack            1  511065701 81803156185 23583
    ## - `price_classEUR10-20`          1  567959932 81860050416 23584
    ## - product_sizesXXS               1  708330825 82000421310 23586
    ## - product_sizesXXL               1  726922734 82019013219 23587
    ## - countries_shipped_to           1  834136756 82126227241 23588
    ## - merchant_has_profile_picture1  1  942139814 82234230298 23590
    ## - product_sizesXS                1 1281698361 82573788845 23596
    ## - merchant_rating_count          1 3832351470 85124441955 23635
    ## 
    ## Step:  AIC=23575.72
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1   69277772 81407995500 23575
    ## - origin_countryUS               1   77912360 81416630089 23575
    ## - retail_price                   1   89969517 81428687246 23575
    ## - origin_countrySG               1   91220317 81429938045 23575
    ## - product_colorsblue             1   93500221 81432217950 23575
    ## - product_colorswhite            1  103409465 81442127193 23575
    ## <none>                                        81338717729 23576
    ## - price                          1  145747341 81484465070 23576
    ## - tags_count                     1  178044633 81516762361 23577
    ## - product_variation_inventory    1  184065306 81522783035 23577
    ## - rating                         1  215644688 81554362416 23577
    ## - discount_per                   1  232897362 81571615090 23578
    ## - product_colorsOther_colors     1  333468472 81672186200 23579
    ## - merchant_rating                1  339492891 81678210620 23579
    ## - product_colorspurple           1  391389826 81730107554 23580
    ## - product_sizesS                 1  404421210 81743138939 23580
    ## - product_sizesOther_sizes       1  428997243 81767714971 23581
    ## - shipping_option_price          1  462731711 81801449439 23581
    ## - product_colorsblack            1  497574946 81836292675 23582
    ## - `price_classEUR10-20`          1  532342236 81871059965 23582
    ## - product_sizesXXS               1  717953402 82056671131 23585
    ## - product_sizesXXL               1  721052487 82059770216 23585
    ## - countries_shipped_to           1  841507069 82180224798 23587
    ## - merchant_has_profile_picture1  1  949425666 82288143394 23589
    ## - product_sizesXS                1 1289887729 82628605457 23594
    ## - merchant_rating_count          1 3843625451 85182343180 23634
    ## 
    ## Step:  AIC=23574.84
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryUS               1   85937164 81493932665 23574
    ## - retail_price                   1   90319850 81498315350 23574
    ## - origin_countrySG               1   94602486 81502597986 23574
    ## <none>                                        81407995500 23575
    ## - product_colorsblue             1  131824846 81539820346 23575
    ## - price                          1  148792652 81556788153 23575
    ## - product_colorswhite            1  162957827 81570953327 23576
    ## - product_variation_inventory    1  177287529 81585283029 23576
    ## - tags_count                     1  188472513 81596468013 23576
    ## - discount_per                   1  225769323 81633764823 23577
    ## - rating                         1  229343507 81637339007 23577
    ## - merchant_rating                1  345421981 81753417481 23578
    ## - product_sizesS                 1  393105164 81801100664 23579
    ## - product_sizesOther_sizes       1  422317404 81830312904 23580
    ## - product_colorspurple           1  455165566 81863161066 23580
    ## - shipping_option_price          1  470596488 81878591989 23580
    ## - product_colorsOther_colors     1  471665974 81879661474 23580
    ## - `price_classEUR10-20`          1  544829294 81952824795 23582
    ## - product_colorsblack            1  651332531 82059328031 23583
    ## - product_sizesXXS               1  711626530 82119622031 23584
    ## - product_sizesXXL               1  717645588 82125641089 23584
    ## - countries_shipped_to           1  817063853 82225059353 23586
    ## - merchant_has_profile_picture1  1  931913073 82339908574 23588
    ## - product_sizesXS                1 1308984420 82716979920 23594
    ## - merchant_rating_count          1 3842336580 85250332080 23633
    ## 
    ## Step:  AIC=23574.22
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countrySG               1   94802209 81588734873 23574
    ## - retail_price                   1   96488880 81590421545 23574
    ## <none>                                        81493932665 23574
    ## - price                          1  141771240 81635703905 23575
    ## - product_colorsblue             1  142093037 81636025702 23575
    ## - product_colorswhite            1  170738230 81664670895 23575
    ## - product_variation_inventory    1  175179516 81669112181 23575
    ## - tags_count                     1  176416220 81670348885 23575
    ## - rating                         1  228651176 81722583841 23576
    ## - discount_per                   1  242288745 81736221409 23576
    ## - merchant_rating                1  351043533 81844976198 23578
    ## - product_sizesS                 1  383986034 81877918698 23578
    ## - product_sizesOther_sizes       1  413077127 81907009792 23579
    ## - product_colorsOther_colors     1  451352839 81945285503 23580
    ## - shipping_option_price          1  459900883 81953833547 23580
    ## - product_colorspurple           1  460289589 81954222254 23580
    ## - `price_classEUR10-20`          1  550064409 82043997073 23581
    ## - product_colorsblack            1  641796816 82135729481 23583
    ## - product_sizesXXL               1  704283270 82198215935 23584
    ## - product_sizesXXS               1  725779309 82219711974 23584
    ## - countries_shipped_to           1  865772567 82359705232 23586
    ## - merchant_has_profile_picture1  1  961251753 82455184418 23588
    ## - product_sizesXS                1 1288419471 82782352136 23593
    ## - merchant_rating_count          1 3890284127 85384216792 23633
    ## 
    ## Step:  AIC=23573.75
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1   98978626 81687713500 23573
    ## <none>                                        81588734873 23574
    ## - price                          1  138684256 81727419130 23574
    ## - product_colorsblue             1  140605249 81729340122 23574
    ## - product_colorswhite            1  167765430 81756500304 23574
    ## - product_variation_inventory    1  171598313 81760333187 23575
    ## - tags_count                     1  176829537 81765564411 23575
    ## - rating                         1  237181671 81825916544 23576
    ## - discount_per                   1  248811739 81837546612 23576
    ## - merchant_rating                1  347045968 81935780841 23577
    ## - product_sizesS                 1  387123930 81975858804 23578
    ## - product_sizesOther_sizes       1  414426471 82003161345 23578
    ## - product_colorspurple           1  454617944 82043352817 23579
    ## - product_colorsOther_colors     1  458409144 82047144017 23579
    ## - shipping_option_price          1  464530357 82053265230 23579
    ## - `price_classEUR10-20`          1  544885029 82133619902 23581
    ## - product_colorsblack            1  637302254 82226037128 23582
    ## - product_sizesXXL               1  703793598 82292528472 23583
    ## - product_sizesXXS               1  734609704 82323344577 23584
    ## - countries_shipped_to           1  883741821 82472476694 23586
    ## - merchant_has_profile_picture1  1  951222980 82539957853 23587
    ## - product_sizesXS                1 1271353573 82860088447 23592
    ## - merchant_rating_count          1 3887828800 85476563673 23633
    ## 
    ## Step:  AIC=23573.34
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## <none>                                        81687713500 23573
    ## - product_colorsblue             1  144628655 81832342155 23574
    ## - discount_per                   1  162460912 81850174411 23574
    ## - product_variation_inventory    1  165587727 81853301227 23574
    ## - tags_count                     1  170978922 81858692421 23574
    ## - product_colorswhite            1  171884726 81859598226 23574
    ## - price                          1  185836090 81873549590 23574
    ## - rating                         1  231734712 81919448211 23575
    ## - merchant_rating                1  362635921 82050349420 23577
    ## - product_sizesS                 1  380461672 82068175172 23577
    ## - product_sizesOther_sizes       1  417494096 82105207596 23578
    ## - shipping_option_price          1  454069852 82141783352 23579
    ## - product_colorsOther_colors     1  460793712 82148507212 23579
    ## - product_colorspurple           1  464681887 82152395387 23579
    ## - `price_classEUR10-20`          1  542321179 82230034679 23580
    ## - product_colorsblack            1  677262597 82364976097 23582
    ## - product_sizesXXL               1  725242698 82412956198 23583
    ## - product_sizesXXS               1  770335806 82458049305 23584
    ## - countries_shipped_to           1  891299598 82579013098 23586
    ## - merchant_has_profile_picture1  1  972566625 82660280125 23587
    ## - product_sizesXS                1 1261726828 82949440327 23591
    ## - merchant_rating_count          1 3937885092 85625598592 23633
    ## Start:  AIC=23857.13
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23857.13
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23857.13
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1       5903 95994767450 23855
    ## - shipping_is_express1           1      57810 95994819358 23855
    ## - product_sizesS                 1    3240548 95998002095 23855
    ## - badge_local_product1           1    9535885 96004297433 23855
    ## - badges_count                   1   10605741 96005367288 23855
    ## - origin_countryVE               1   10670447 96005431994 23855
    ## - badge_product_quality1         1   11575740 96006337287 23855
    ## - product_colorsyellow           1   12815660 96007577208 23855
    ## - `price_classEUR40-50`          1   21631052 96016392599 23855
    ## - shipping_nameOther_shipping    1   22282071 96017043619 23855
    ## - product_colorspink             1   26305843 96021067391 23856
    ## - retail_price                   1   27962027 96022723575 23856
    ## - product_sizesOther_sizes       1   46870811 96041632359 23856
    ## - uses_ad_boosts1                1   53680368 96048441916 23856
    ## - product_colorsred              1   56549959 96051311507 23856
    ## - product_colorsgreen            1   61400535 96056162083 23856
    ## - product_sizesM                 1   62822191 96057583738 23856
    ## - `price_classEUR20-30`          1   64356388 96059117936 23856
    ## - origin_countryUS               1   76260411 96071021959 23856
    ## - origin_countrySG               1   77194555 96071956103 23856
    ## - product_colorsblue             1   97894390 96092655937 23857
    ## - tags_count                     1  111189912 96105951459 23857
    ## - merchant_rating                1  117691310 96112452857 23857
    ## - discount_per                   1  121947812 96116709360 23857
    ## - rating                         1  124180883 96118942430 23857
    ## - product_sizesXXS               1  132787403 96127548951 23857
    ## <none>                                        95994761548 23857
    ## - product_sizesXS                1  170811926 96165573474 23858
    ## - product_colorswhite            1  176980449 96171741997 23858
    ## - product_colorspurple           1  208750650 96203512198 23858
    ## - product_colorsgrey             1  240886835 96235648383 23858
    ## - product_colorsblack            1  274723195 96269484743 23859
    ## - product_colorsOther_colors     1  279575527 96274337075 23859
    ## - product_variation_inventory    1  299988709 96294750257 23859
    ## - price                          1  300790569 96295552117 23859
    ## - product_sizesXXL               1  575389365 96570150913 23863
    ## - countries_shipped_to           1  584114786 96578876334 23863
    ## - shipping_option_price          1  646960858 96641722406 23864
    ## - `price_classEUR10-20`          1  658735474 96653497022 23864
    ## - merchant_has_profile_picture1  1 1292785161 97287546708 23873
    ## - merchant_rating_count          1 3928201546 99922963094 23908
    ## 
    ## Step:  AIC=23855.13
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1      58160 95994825611 23853
    ## - product_sizesS                 1    3241455 95998008905 23853
    ## - badge_local_product1           1    9535891 96004303342 23853
    ## - badges_count                   1   10605700 96005373150 23853
    ## - origin_countryVE               1   10671088 96005438539 23853
    ## - badge_product_quality1         1   11574681 96006342132 23853
    ## - product_colorsyellow           1   12811246 96007578697 23853
    ## - `price_classEUR40-50`          1   21644762 96016412212 23853
    ## - shipping_nameOther_shipping    1   22285636 96017053087 23853
    ## - product_colorspink             1   26300928 96021068379 23854
    ## - retail_price                   1   27969980 96022737431 23854
    ## - product_sizesOther_sizes       1   46889128 96041656579 23854
    ## - uses_ad_boosts1                1   53710114 96048477565 23854
    ## - product_colorsred              1   56544079 96051311529 23854
    ## - product_colorsgreen            1   61394891 96056162341 23854
    ## - product_sizesM                 1   62817030 96057584480 23854
    ## - `price_classEUR20-30`          1   64406809 96059174259 23854
    ## - origin_countryUS               1   76257617 96071025067 23854
    ## - origin_countrySG               1   77189911 96071957362 23854
    ## - product_colorsblue             1   98144221 96092911671 23855
    ## - tags_count                     1  111278369 96106045820 23855
    ## - merchant_rating                1  117686343 96112453793 23855
    ## - discount_per                   1  122036639 96116804089 23855
    ## - rating                         1  124274741 96119042192 23855
    ## - product_sizesXXS               1  132829325 96127596775 23855
    ## <none>                                        95994767450 23855
    ## - product_sizesXS                1  170807466 96165574916 23856
    ## - product_colorswhite            1  176974898 96171742348 23856
    ## - product_colorspurple           1  208748052 96203515502 23856
    ## - product_colorsgrey             1  240890051 96235657501 23856
    ## - product_colorsblack            1  274718749 96269486199 23857
    ## - product_colorsOther_colors     1  279575293 96274342744 23857
    ## - product_variation_inventory    1  300299485 96295066936 23857
    ## - price                          1  300832602 96295600053 23857
    ## - product_sizesXXL               1  575398723 96570166173 23861
    ## - countries_shipped_to           1  584109101 96578876552 23861
    ## - shipping_option_price          1  647072289 96641839740 23862
    ## - `price_classEUR10-20`          1  659319843 96654087293 23862
    ## - merchant_has_profile_picture1  1 1292828490 97287595941 23871
    ## - merchant_rating_count          1 3928215387 99922982838 23906
    ## 
    ## Step:  AIC=23853.13
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesS                 1    3246639 95998072249 23851
    ## - badge_local_product1           1    9950263 96004775874 23851
    ## - origin_countryVE               1   10699601 96005525212 23851
    ## - badges_count                   1   12134636 96006960247 23851
    ## - product_colorsyellow           1   12828413 96007654023 23851
    ## - badge_product_quality1         1   13098872 96007924483 23851
    ## - shipping_nameOther_shipping    1   23221288 96018046899 23852
    ## - `price_classEUR40-50`          1   23309656 96018135267 23852
    ## - product_colorspink             1   26310542 96021136153 23852
    ## - retail_price                   1   28084725 96022910336 23852
    ## - product_sizesOther_sizes       1   46857863 96041683474 23852
    ## - uses_ad_boosts1                1   53653117 96048478727 23852
    ## - product_colorsred              1   56621666 96051447277 23852
    ## - product_colorsgreen            1   61613424 96056439034 23852
    ## - product_sizesM                 1   62837327 96057662937 23852
    ## - `price_classEUR20-30`          1   64719434 96059545045 23852
    ## - origin_countryUS               1   76215994 96071041604 23852
    ## - origin_countrySG               1   77149083 96071974694 23852
    ## - product_colorsblue             1   98149060 96092974671 23853
    ## - tags_count                     1  111521690 96106347300 23853
    ## - merchant_rating                1  117852740 96112678351 23853
    ## - discount_per                   1  122279322 96117104932 23853
    ## - rating                         1  124538051 96119363661 23853
    ## - product_sizesXXS               1  132877972 96127703583 23853
    ## <none>                                        95994825611 23853
    ## - product_sizesXS                1  170917661 96165743272 23854
    ## - product_colorswhite            1  177002529 96171828140 23854
    ## - product_colorspurple           1  208774211 96203599821 23854
    ## - product_colorsgrey             1  240936249 96235761859 23854
    ## - product_colorsblack            1  274794289 96269619900 23855
    ## - product_colorsOther_colors     1  279525555 96274351166 23855
    ## - price                          1  301069634 96295895245 23855
    ## - product_variation_inventory    1  301192150 96296017760 23855
    ## - product_sizesXXL               1  575400012 96570225623 23859
    ## - countries_shipped_to           1  584424804 96579250415 23859
    ## - shipping_option_price          1  655981342 96650806953 23860
    ## - `price_classEUR10-20`          1  668470505 96663296116 23860
    ## - merchant_has_profile_picture1  1 1293754876 97288580487 23869
    ## - merchant_rating_count          1 3929288601 99924114212 23904
    ## 
    ## Step:  AIC=23851.18
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgreen + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspink + product_colorspurple + product_colorsred + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1   10160046 96008232296 23849
    ## - origin_countryVE               1   10558131 96008630381 23849
    ## - badges_count                   1   12341657 96010413906 23849
    ## - badge_product_quality1         1   13117847 96011190096 23849
    ## - product_colorsyellow           1   13293675 96011365925 23849
    ## - `price_classEUR40-50`          1   23378381 96021450631 23850
    ## - shipping_nameOther_shipping    1   23563800 96021636049 23850
    ## - product_colorspink             1   26537572 96024609821 23850
    ## - retail_price                   1   27408563 96025480812 23850
    ## - uses_ad_boosts1                1   53221133 96051293383 23850
    ## - product_colorsred              1   58269627 96056341876 23850
    ## - product_colorsgreen            1   62555937 96060628186 23850
    ## - `price_classEUR20-30`          1   64662058 96062734307 23850
    ## - origin_countrySG               1   77271919 96075344168 23850
    ## - origin_countryUS               1   77309269 96075381518 23850
    ## - product_sizesOther_sizes       1   82859380 96080931629 23850
    ## - product_colorsblue             1   98067366 96096139616 23851
    ## - tags_count                     1  111486562 96109558811 23851
    ## - merchant_rating                1  118053536 96116125786 23851
    ## - discount_per                   1  121484721 96119556970 23851
    ## - rating                         1  127717567 96125789817 23851
    ## <none>                                        95998072249 23851
    ## - product_colorswhite            1  177515038 96175587287 23852
    ## - product_colorspurple           1  209562673 96207634923 23852
    ## - product_colorsgrey             1  241041425 96239113674 23853
    ## - product_sizesXXS               1  260314156 96258386405 23853
    ## - product_colorsblack            1  276070509 96274142758 23853
    ## - product_colorsOther_colors     1  281448139 96279520388 23853
    ## - price                          1  299310749 96297382998 23853
    ## - product_variation_inventory    1  302682827 96300755077 23853
    ## - product_sizesM                 1  376985610 96375057859 23854
    ## - countries_shipped_to           1  581478312 96579550562 23857
    ## - shipping_option_price          1  652998041 96651070291 23858
    ## - `price_classEUR10-20`          1  665268439 96663340689 23858
    ## - product_sizesXS                1  668764268 96666836518 23858
    ## - product_sizesXXL               1  698528154 96696600403 23859
    ## - merchant_has_profile_picture1  1 1293165615 97291237864 23867
    ## - merchant_rating_count          1 3929013782 99927086032 23902
    ## 
    ## Step:  AIC=23849.32
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1    2442869 96010675165 23847
    ## - badge_product_quality1         1    3431459 96011663754 23847
    ## - origin_countryVE               1   10706234 96018938530 23848
    ## - product_colorsyellow           1   12838414 96021070710 23848
    ## - shipping_nameOther_shipping    1   24382242 96032614538 23848
    ## - `price_classEUR40-50`          1   25186123 96033418419 23848
    ## - product_colorspink             1   26167504 96034399799 23848
    ## - retail_price                   1   27622496 96035854792 23848
    ## - uses_ad_boosts1                1   51476151 96059708447 23848
    ## - product_colorsred              1   59499615 96067731911 23848
    ## - product_colorsgreen            1   61552807 96069785102 23848
    ## - `price_classEUR20-30`          1   62986397 96071218693 23848
    ## - origin_countrySG               1   77096357 96085328652 23848
    ## - origin_countryUS               1   79568477 96087800773 23848
    ## - product_sizesOther_sizes       1   87384623 96095616918 23849
    ## - product_colorsblue             1   97585304 96105817600 23849
    ## - tags_count                     1  111519935 96119752230 23849
    ## - merchant_rating                1  119362197 96127594492 23849
    ## - discount_per                   1  123936589 96132168885 23849
    ## - rating                         1  127598275 96135830570 23849
    ## <none>                                        96008232296 23849
    ## - product_colorswhite            1  177845288 96186077583 23850
    ## - product_colorspurple           1  210343029 96218575325 23850
    ## - product_colorsgrey             1  238585697 96246817993 23851
    ## - product_sizesXXS               1  259859850 96268092146 23851
    ## - product_colorsblack            1  274629447 96282861743 23851
    ## - product_colorsOther_colors     1  280042542 96288274838 23851
    ## - price                          1  295727819 96303960115 23851
    ## - product_variation_inventory    1  321899113 96330131408 23852
    ## - product_sizesM                 1  374329773 96382562069 23852
    ## - countries_shipped_to           1  582104985 96590337280 23855
    ## - shipping_option_price          1  653799138 96662031434 23856
    ## - `price_classEUR10-20`          1  657478760 96665711055 23856
    ## - product_sizesXS                1  660194942 96668427237 23856
    ## - product_sizesXXL               1  700132006 96708364301 23857
    ## - merchant_has_profile_picture1  1 1283989082 97292221378 23865
    ## - merchant_rating_count          1 3937358930 99945591226 23900
    ## 
    ## Step:  AIC=23847.35
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1     995571 96011670736 23845
    ## - origin_countryVE               1   10592056 96021267221 23846
    ## - product_colorsyellow           1   12565576 96023240741 23846
    ## - shipping_nameOther_shipping    1   24832382 96035507546 23846
    ## - product_colorspink             1   25908952 96036584117 23846
    ## - `price_classEUR40-50`          1   25936244 96036611409 23846
    ## - retail_price                   1   27676573 96038351737 23846
    ## - uses_ad_boosts1                1   52447816 96063122981 23846
    ## - product_colorsred              1   58815119 96069490284 23846
    ## - product_colorsgreen            1   60681833 96071356998 23846
    ## - `price_classEUR20-30`          1   62604293 96073279457 23846
    ## - origin_countrySG               1   77057515 96087732680 23846
    ## - origin_countryUS               1   79656425 96090331590 23846
    ## - product_sizesOther_sizes       1   90488172 96101163336 23847
    ## - product_colorsblue             1   97318970 96107994135 23847
    ## - tags_count                     1  110467353 96121142518 23847
    ## - merchant_rating                1  116945872 96127621037 23847
    ## - discount_per                   1  124178267 96134853431 23847
    ## - rating                         1  129628660 96140303825 23847
    ## <none>                                        96010675165 23847
    ## - product_colorswhite            1  176595574 96187270739 23848
    ## - product_colorspurple           1  209264099 96219939264 23848
    ## - product_colorsgrey             1  237601442 96248276607 23849
    ## - product_sizesXXS               1  260574345 96271249509 23849
    ## - product_colorsblack            1  273131532 96283806697 23849
    ## - product_colorsOther_colors     1  279013192 96289688357 23849
    ## - price                          1  297160803 96307835968 23849
    ## - product_variation_inventory    1  327524825 96338199990 23850
    ## - product_sizesM                 1  376484415 96387159579 23851
    ## - countries_shipped_to           1  584007047 96594682212 23853
    ## - product_sizesXS                1  660351695 96671026859 23854
    ## - `price_classEUR10-20`          1  660788944 96671464109 23854
    ## - shipping_option_price          1  660911601 96671586766 23854
    ## - product_sizesXXL               1  713443427 96724118592 23855
    ## - merchant_has_profile_picture1  1 1285726983 97296402148 23863
    ## - merchant_rating_count          1 3952602514 99963277679 23898
    ## 
    ## Step:  AIC=23845.37
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryVE               1   10382785  96022053521 23844
    ## - product_colorsyellow           1   12541729  96024212465 23844
    ## - shipping_nameOther_shipping    1   24530182  96036200918 23844
    ## - `price_classEUR40-50`          1   25850298  96037521034 23844
    ## - product_colorspink             1   26426745  96038097481 23844
    ## - retail_price                   1   27579817  96039250553 23844
    ## - uses_ad_boosts1                1   52296595  96063967331 23844
    ## - product_colorsred              1   59023153  96070693889 23844
    ## - product_colorsgreen            1   61295137  96072965873 23844
    ## - `price_classEUR20-30`          1   62714025  96074384761 23844
    ## - origin_countrySG               1   76733880  96088404615 23844
    ## - origin_countryUS               1   78886510  96090557246 23844
    ## - product_sizesOther_sizes       1   91013365  96102684101 23845
    ## - product_colorsblue             1   97707075  96109377811 23845
    ## - tags_count                     1  110018410  96121689146 23845
    ## - merchant_rating                1  121085915  96132756651 23845
    ## - discount_per                   1  124247038  96135917774 23845
    ## - rating                         1  145511187  96157181923 23845
    ## <none>                                         96011670736 23845
    ## - product_colorswhite            1  177182286  96188853022 23846
    ## - product_colorspurple           1  209416936  96221087672 23846
    ## - product_colorsgrey             1  240377977  96252048713 23847
    ## - product_sizesXXS               1  261983104  96273653840 23847
    ## - product_colorsblack            1  274509689  96286180425 23847
    ## - product_colorsOther_colors     1  279797066  96291467802 23847
    ## - price                          1  296997918  96308668654 23847
    ## - product_variation_inventory    1  329321906  96340992642 23848
    ## - product_sizesM                 1  376324374  96387995110 23849
    ## - countries_shipped_to           1  586226286  96597897022 23851
    ## - `price_classEUR10-20`          1  660144406  96671815142 23852
    ## - product_sizesXS                1  660167015  96671837750 23852
    ## - shipping_option_price          1  661218238  96672888974 23852
    ## - product_sizesXXL               1  713711824  96725382560 23853
    ## - merchant_has_profile_picture1  1 1287460209  97299130945 23861
    ## - merchant_rating_count          1 3989869704 100001540440 23897
    ## 
    ## Step:  AIC=23843.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgreen + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspink + product_colorspurple + product_colorsred + 
    ##     product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsyellow           1   12668907  96034722428 23842
    ## - shipping_nameOther_shipping    1   24507338  96046560859 23842
    ## - `price_classEUR40-50`          1   25688385  96047741907 23842
    ## - product_colorspink             1   26683781  96048737302 23842
    ## - retail_price                   1   27714475  96049767996 23842
    ## - uses_ad_boosts1                1   55514148  96077567670 23842
    ## - product_colorsred              1   59223114  96081276636 23842
    ## - product_colorsgreen            1   61496881  96083550402 23842
    ## - `price_classEUR20-30`          1   61604385  96083657907 23842
    ## - origin_countrySG               1   77054793  96099108314 23843
    ## - origin_countryUS               1   78431305  96100484826 23843
    ## - product_sizesOther_sizes       1   90431867  96112485388 23843
    ## - product_colorsblue             1   98026000  96120079521 23843
    ## - tags_count                     1  109993053  96132046574 23843
    ## - merchant_rating                1  121336548  96143390070 23843
    ## - discount_per                   1  127154874  96149208395 23843
    ## <none>                                         96022053521 23844
    ## - rating                         1  160396326  96182449848 23844
    ## - product_colorswhite            1  174498513  96196552035 23844
    ## - product_colorspurple           1  208700934  96230754456 23844
    ## - product_colorsgrey             1  240437917  96262491438 23845
    ## - product_sizesXXS               1  259328808  96281382329 23845
    ## - product_colorsblack            1  272809683  96294863204 23845
    ## - product_colorsOther_colors     1  279800797  96301854319 23845
    ## - price                          1  291066218  96313119739 23846
    ## - product_variation_inventory    1  332215707  96354269228 23846
    ## - product_sizesM                 1  376630955  96398684476 23847
    ## - countries_shipped_to           1  586973968  96609027490 23850
    ## - shipping_option_price          1  652495237  96674548758 23850
    ## - `price_classEUR10-20`          1  655119427  96677172948 23850
    ## - product_sizesXS                1  671545172  96693598694 23851
    ## - product_sizesXXL               1  711137256  96733190777 23851
    ## - merchant_has_profile_picture1  1 1290304592  97312358114 23859
    ## - merchant_rating_count          1 3991682472 100013735994 23895
    ## 
    ## Step:  AIC=23841.68
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgreen + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspink + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorspink             1   14487980  96049210409 23840
    ## - retail_price                   1   24934166  96059656594 23840
    ## - shipping_nameOther_shipping    1   26094331  96060816759 23840
    ## - `price_classEUR40-50`          1   26179561  96060901990 23840
    ## - uses_ad_boosts1                1   53696105  96088418534 23840
    ## - product_colorsred              1   60063401  96094785830 23841
    ## - product_colorsgreen            1   61326607  96096049036 23841
    ## - `price_classEUR20-30`          1   62025447  96096747876 23841
    ## - origin_countryUS               1   74518584  96109241012 23841
    ## - origin_countrySG               1   76426546  96111148974 23841
    ## - product_sizesOther_sizes       1   96405264  96131127692 23841
    ## - tags_count                     1  106658862  96141381291 23841
    ## - merchant_rating                1  118185147  96152907575 23841
    ## - product_colorsblue             1  122959508  96157681936 23841
    ## - discount_per                   1  124664990  96159387418 23841
    ## <none>                                         96034722428 23842
    ## - rating                         1  163645648  96198368077 23842
    ## - product_sizesXXS               1  267872733  96302595162 23843
    ## - product_colorspurple           1  278448793  96313171222 23844
    ## - price                          1  294993024  96329715452 23844
    ## - product_variation_inventory    1  327451633  96362174061 23844
    ## - product_colorswhite            1  329329838  96364052267 23844
    ## - product_sizesM                 1  373862004  96408584433 23845
    ## - product_colorsgrey             1  374702660  96409425088 23845
    ## - product_colorsblack            1  582961538  96617683966 23848
    ## - countries_shipped_to           1  601475387  96636197816 23848
    ## - product_colorsOther_colors     1  631179590  96665902018 23848
    ## - shipping_option_price          1  651048432  96685770860 23849
    ## - `price_classEUR10-20`          1  657023149  96691745578 23849
    ## - product_sizesXS                1  663334389  96698056817 23849
    ## - product_sizesXXL               1  712792751  96747515180 23849
    ## - merchant_has_profile_picture1  1 1293411017  97328133446 23857
    ## - merchant_rating_count          1 4013319967 100048042396 23893
    ## 
    ## Step:  AIC=23839.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgreen + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - retail_price                   1   23534060  96072744468 23838
    ## - `price_classEUR40-50`          1   25966723  96075177131 23838
    ## - shipping_nameOther_shipping    1   26389737  96075600145 23838
    ## - product_colorsred              1   45777765  96094988174 23839
    ## - product_colorsgreen            1   47198462  96096408870 23839
    ## - uses_ad_boosts1                1   54300699  96103511107 23839
    ## - `price_classEUR20-30`          1   61641364  96110851773 23839
    ## - origin_countrySG               1   76445519  96125655928 23839
    ## - origin_countryUS               1   78089003  96127299412 23839
    ## - product_sizesOther_sizes       1  101074123  96150284532 23839
    ## - tags_count                     1  109161264  96158371673 23839
    ## - product_colorsblue             1  109909746  96159120155 23839
    ## - merchant_rating                1  116105646  96165316055 23840
    ## - discount_per                   1  122017062  96171227471 23840
    ## <none>                                         96049210409 23840
    ## - rating                         1  162990036  96212200444 23840
    ## - product_colorspurple           1  268537940  96317748348 23842
    ## - product_sizesXXS               1  273434749  96322645158 23842
    ## - price                          1  294804564  96344014973 23842
    ## - product_variation_inventory    1  323908906  96373119315 23842
    ## - product_colorswhite            1  364172057  96413382466 23843
    ## - product_sizesM                 1  369612843  96418823252 23843
    ## - product_colorsgrey             1  381206079  96430416488 23843
    ## - countries_shipped_to           1  593000208  96642210617 23846
    ## - shipping_option_price          1  650664708  96699875117 23847
    ## - `price_classEUR10-20`          1  657223834  96706434242 23847
    ## - product_sizesXS                1  668279208  96717489617 23847
    ## - product_colorsblack            1  688098604  96737309012 23847
    ## - product_sizesXXL               1  716128393  96765338802 23848
    ## - product_colorsOther_colors     1  762791424  96812001832 23848
    ## - merchant_has_profile_picture1  1 1288577690  97337788098 23855
    ## - merchant_rating_count          1 4014913693 100064124102 23892
    ## 
    ## Step:  AIC=23838.2
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgreen + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - `price_classEUR40-50`          1   28366923  96101111391 23837
    ## - shipping_nameOther_shipping    1   28998522  96101742990 23837
    ## - product_colorsred              1   45688190  96118432659 23837
    ## - product_colorsgreen            1   46229091  96118973560 23837
    ## - uses_ad_boosts1                1   54119871  96126864339 23837
    ## - `price_classEUR20-30`          1   55184029  96127928498 23837
    ## - origin_countrySG               1   77676539  96150421007 23837
    ## - origin_countryUS               1   80290654  96153035123 23837
    ## - product_sizesOther_sizes       1  102250043  96174994511 23838
    ## - tags_count                     1  107518673  96180263141 23838
    ## - product_colorsblue             1  108860855  96181605323 23838
    ## - merchant_rating                1  119335050  96192079518 23838
    ## - discount_per                   1  134534927  96207279395 23838
    ## <none>                                         96072744468 23838
    ## - rating                         1  160603452  96233347921 23838
    ## - product_colorspurple           1  271162164  96343906633 23840
    ## - product_sizesXXS               1  284869269  96357613738 23840
    ## - product_variation_inventory    1  321550017  96394294485 23841
    ## - price                          1  322941434  96395685902 23841
    ## - product_sizesM                 1  365490405  96438234873 23841
    ## - product_colorswhite            1  367851207  96440595675 23841
    ## - product_colorsgrey             1  377221755  96449966223 23841
    ## - countries_shipped_to           1  594189441  96666933910 23844
    ## - shipping_option_price          1  643900910  96716645378 23845
    ## - `price_classEUR10-20`          1  649707607  96722452076 23845
    ## - product_sizesXS                1  669487552  96742232020 23845
    ## - product_colorsblack            1  698866432  96771610901 23846
    ## - product_sizesXXL               1  739251446  96811995915 23846
    ## - product_colorsOther_colors     1  760540315  96833284783 23847
    ## - merchant_has_profile_picture1  1 1306003977  97378748446 23854
    ## - merchant_rating_count          1 4094779529 100167523997 23891
    ## 
    ## Step:  AIC=23836.59
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - shipping_nameOther_shipping    1   30126705  96131238097 23835
    ## - `price_classEUR20-30`          1   38764734  96139876125 23835
    ## - product_colorsgreen            1   39721511  96140832903 23835
    ## - product_colorsred              1   42156612  96143268004 23835
    ## - uses_ad_boosts1                1   56468114  96157579506 23835
    ## - origin_countrySG               1   75753103  96176864495 23836
    ## - origin_countryUS               1   78397806  96179509197 23836
    ## - product_colorsblue             1  111003223  96212114615 23836
    ## - tags_count                     1  111531285  96212642677 23836
    ## - product_sizesOther_sizes       1  122176648  96223288039 23836
    ## - merchant_rating                1  133420748  96234532139 23836
    ## - discount_per                   1  133504576  96234615967 23836
    ## <none>                                         96101111391 23837
    ## - rating                         1  167094889  96268206280 23837
    ## - product_colorspurple           1  268762607  96369873999 23838
    ## - product_sizesXXS               1  294956902  96396068293 23839
    ## - price                          1  295784909  96396896301 23839
    ## - product_variation_inventory    1  325663842  96426775234 23839
    ## - product_colorswhite            1  362525026  96463636418 23840
    ## - product_sizesM                 1  370331188  96471442580 23840
    ## - product_colorsgrey             1  375926745  96477038136 23840
    ## - countries_shipped_to           1  601217584  96702328976 23843
    ## - shipping_option_price          1  623355670  96724467062 23843
    ## - `price_classEUR10-20`          1  651797269  96752908660 23844
    ## - product_sizesXS                1  678032797  96779144189 23844
    ## - product_colorsblack            1  693531330  96794642721 23844
    ## - product_sizesXXL               1  734685431  96835796822 23845
    ## - product_colorsOther_colors     1  754030659  96855142050 23845
    ## - merchant_has_profile_picture1  1 1315478053  97416589444 23852
    ## - merchant_rating_count          1 4084156713 100185268105 23889
    ## 
    ## Step:  AIC=23835
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + uses_ad_boosts1 + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgreen + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - `price_classEUR20-30`          1   38867661  96170105758 23834
    ## - product_colorsgreen            1   42097700  96173335796 23834
    ## - product_colorsred              1   45713885  96176951982 23834
    ## - uses_ad_boosts1                1   53433318  96184671415 23834
    ## - origin_countryUS               1   75569550  96206807647 23834
    ## - origin_countrySG               1   76309371  96207547468 23834
    ## - tags_count                     1  111974648  96243212745 23835
    ## - product_colorsblue             1  119529393  96250767490 23835
    ## - product_sizesOther_sizes       1  126223942  96257462039 23835
    ## - discount_per                   1  130567949  96261806046 23835
    ## - merchant_rating                1  131615545  96262853641 23835
    ## <none>                                         96131238097 23835
    ## - rating                         1  165117912  96296356009 23835
    ## - product_colorspurple           1  281511419  96412749516 23837
    ## - product_sizesXXS               1  287907395  96419145492 23837
    ## - price                          1  294900435  96426138531 23837
    ## - product_variation_inventory    1  331882624  96463120721 23838
    ## - product_sizesM                 1  372602993  96503841090 23838
    ## - product_colorswhite            1  374496087  96505734184 23838
    ## - product_colorsgrey             1  386337860  96517575957 23838
    ## - countries_shipped_to           1  599658660  96730896757 23841
    ## - shipping_option_price          1  637514838  96768752935 23842
    ## - `price_classEUR10-20`          1  643128233  96774366330 23842
    ## - product_sizesXS                1  680863438  96812101535 23842
    ## - product_colorsblack            1  713297887  96844535983 23843
    ## - product_sizesXXL               1  743155220  96874393317 23843
    ## - product_colorsOther_colors     1  761703389  96892941486 23843
    ## - merchant_has_profile_picture1  1 1334867669  97466105766 23851
    ## - merchant_rating_count          1 4089775715 100221013812 23888
    ## 
    ## Step:  AIC=23833.53
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsgreen            1   43456842  96213562600 23832
    ## - product_colorsred              1   46368097  96216473855 23832
    ## - uses_ad_boosts1                1   61169741  96231275500 23832
    ## - origin_countryUS               1   72091716  96242197474 23833
    ## - origin_countrySG               1   74904052  96245009810 23833
    ## - tags_count                     1  114111817  96284217576 23833
    ## - product_colorsblue             1  121200975  96291306733 23833
    ## - product_sizesOther_sizes       1  122586754  96292692512 23833
    ## - merchant_rating                1  128761348  96298867107 23833
    ## - discount_per                   1  144967652  96315073411 23834
    ## <none>                                         96170105758 23834
    ## - rating                         1  173787581  96343893340 23834
    ## - price                          1  256122711  96426228469 23835
    ## - product_colorspurple           1  279124514  96449230273 23835
    ## - product_sizesXXS               1  293599084  96463704842 23836
    ## - product_variation_inventory    1  341058210  96511163968 23836
    ## - product_sizesM                 1  366589106  96536694865 23837
    ## - product_colorswhite            1  368582630  96538688388 23837
    ## - product_colorsgrey             1  386308830  96556414588 23837
    ## - shipping_option_price          1  603954574  96774060333 23840
    ## - countries_shipped_to           1  604781579  96774887338 23840
    ## - `price_classEUR10-20`          1  626617108  96796722866 23840
    ## - product_sizesXS                1  692983222  96863088981 23841
    ## - product_colorsblack            1  701264092  96871369850 23841
    ## - product_sizesXXL               1  738522757  96908628515 23842
    ## - product_colorsOther_colors     1  745907973  96916013732 23842
    ## - merchant_has_profile_picture1  1 1343121040  97513226798 23850
    ## - merchant_rating_count          1 4101169256 100271275015 23886
    ## 
    ## Step:  AIC=23832.12
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsred              1   26501456  96240064056 23831
    ## - uses_ad_boosts1                1   59277758  96272840358 23831
    ## - origin_countrySG               1   74564546  96288127146 23831
    ## - origin_countryUS               1   75244638  96288807239 23831
    ## - product_colorsblue             1   91196377  96304758977 23831
    ## - tags_count                     1  117443654  96331006254 23832
    ## - product_sizesOther_sizes       1  119660883  96333223483 23832
    ## - merchant_rating                1  129306681  96342869282 23832
    ## - discount_per                   1  136964440  96350527040 23832
    ## <none>                                         96213562600 23832
    ## - rating                         1  187529686  96401092286 23833
    ## - product_colorspurple           1  244460061  96458022661 23834
    ## - price                          1  260564339  96474126939 23834
    ## - product_sizesXXS               1  296043404  96509606004 23834
    ## - product_variation_inventory    1  315109271  96528671871 23834
    ## - product_colorswhite            1  325442802  96539005403 23835
    ## - product_colorsgrey             1  344826065  96558388665 23835
    ## - product_sizesM                 1  364955243  96578517844 23835
    ## - countries_shipped_to           1  599855967  96813418567 23838
    ## - shipping_option_price          1  604950386  96818512986 23838
    ## - `price_classEUR10-20`          1  633111705  96846674306 23839
    ## - product_colorsblack            1  670391742  96883954343 23839
    ## - product_sizesXS                1  698951022  96912513622 23840
    ## - product_colorsOther_colors     1  722144018  96935706618 23840
    ## - product_sizesXXL               1  737248137  96950810737 23840
    ## - merchant_has_profile_picture1  1 1362779373  97576341973 23849
    ## - merchant_rating_count          1 4103364929 100316927529 23885
    ## 
    ## Step:  AIC=23830.48
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + uses_ad_boosts1 + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - uses_ad_boosts1                1   59526850  96299590906 23829
    ## - product_colorsblue             1   73885896  96313949952 23830
    ## - origin_countrySG               1   74458772  96314522828 23830
    ## - origin_countryUS               1   78620600  96318684656 23830
    ## - product_sizesOther_sizes       1  120830435  96360894492 23830
    ## - tags_count                     1  122659058  96362723115 23830
    ## - merchant_rating                1  136896008  96376960064 23830
    ## - discount_per                   1  137786571  96377850627 23830
    ## <none>                                         96240064056 23831
    ## - rating                         1  187762878  96427826934 23831
    ## - product_colorspurple           1  223377821  96463441877 23832
    ## - price                          1  258003285  96498067341 23832
    ## - product_colorswhite            1  299470444  96539534500 23833
    ## - product_sizesXXS               1  305106439  96545170496 23833
    ## - product_variation_inventory    1  313749770  96553813826 23833
    ## - product_colorsgrey             1  319367535  96559431591 23833
    ## - product_sizesM                 1  362754355  96602818411 23833
    ## - shipping_option_price          1  600160750  96840224806 23837
    ## - countries_shipped_to           1  614005266  96854069322 23837
    ## - `price_classEUR10-20`          1  630415761  96870479817 23837
    ## - product_colorsblack            1  656495965  96896560021 23837
    ## - product_sizesXS                1  700262064  96940326120 23838
    ## - product_colorsOther_colors     1  713239396  96953303452 23838
    ## - product_sizesXXL               1  735532519  96975596575 23839
    ## - merchant_has_profile_picture1  1 1345385045  97585449101 23847
    ## - merchant_rating_count          1 4102092329 100342156385 23883
    ## 
    ## Step:  AIC=23829.3
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countrySG               1   69505960  96369096865 23828
    ## - product_colorsblue             1   78637809  96378228714 23828
    ## - origin_countryUS               1   79405282  96378996188 23828
    ## - tags_count                     1  123154575  96422745480 23829
    ## - discount_per                   1  138353413  96437944319 23829
    ## - merchant_rating                1  140910077  96440500983 23829
    ## - product_sizesOther_sizes       1  141566145  96441157051 23829
    ## <none>                                         96299590906 23829
    ## - rating                         1  196124832  96495715738 23830
    ## - product_colorspurple           1  229290375  96528881281 23830
    ## - price                          1  269305912  96568896818 23831
    ## - product_colorswhite            1  305238394  96604829299 23832
    ## - product_colorsgrey             1  316117007  96615707913 23832
    ## - product_sizesXXS               1  331939500  96631530406 23832
    ## - product_variation_inventory    1  346851561  96646442467 23832
    ## - product_sizesM                 1  354343361  96653934267 23832
    ## - countries_shipped_to           1  606499045  96906089951 23836
    ## - shipping_option_price          1  612210620  96911801526 23836
    ## - `price_classEUR10-20`          1  638890252  96938481158 23836
    ## - product_colorsblack            1  654073650  96953664556 23836
    ## - product_sizesXS                1  697986646  96997577552 23837
    ## - product_colorsOther_colors     1  704461652  97004052558 23837
    ## - product_sizesXXL               1  729568949  97029159855 23837
    ## - merchant_has_profile_picture1  1 1333040566  97632631472 23845
    ## - merchant_rating_count          1 4068580946 100368171852 23882
    ## 
    ## Step:  AIC=23828.24
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsblue + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsblue             1   79637194  96448734059 23827
    ## - origin_countryUS               1   79666485  96448763350 23827
    ## - tags_count                     1  123215782  96492312647 23828
    ## - merchant_rating                1  138491887  96507588752 23828
    ## - product_sizesOther_sizes       1  141340607  96510437472 23828
    ## - discount_per                   1  141949003  96511045869 23828
    ## <none>                                         96369096865 23828
    ## - rating                         1  201477480  96570574345 23829
    ## - product_colorspurple           1  228412600  96597509465 23829
    ## - price                          1  266057240  96635154105 23830
    ## - product_colorswhite            1  307049140  96676146005 23830
    ## - product_sizesXXS               1  336349893  96705446759 23831
    ## - product_colorsgrey             1  339744383  96708841249 23831
    ## - product_variation_inventory    1  342848615  96711945480 23831
    ## - product_sizesM                 1  356407845  96725504710 23831
    ## - shipping_option_price          1  616177783  96985274648 23835
    ## - countries_shipped_to           1  618999409  96988096274 23835
    ## - `price_classEUR10-20`          1  633775155  97002872020 23835
    ## - product_colorsblack            1  659269929  97028366794 23835
    ## - product_sizesXS                1  681307749  97050404614 23836
    ## - product_colorsOther_colors     1  722204489  97091301354 23836
    ## - product_sizesXXL               1  729103233  97098200099 23836
    ## - merchant_has_profile_picture1  1 1328736232  97697833097 23844
    ## - merchant_rating_count          1 4059418034 100428514899 23880
    ## 
    ## Step:  AIC=23827.33
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesXS + product_sizesXXL + 
    ##     product_sizesXXS + product_colorsblack + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryUS               1   88392901  96537126960 23827
    ## - tags_count                     1  127752678  96576486736 23827
    ## - product_sizesOther_sizes       1  130270854  96579004913 23827
    ## - merchant_rating                1  132711454  96581445513 23827
    ## - discount_per                   1  137736796  96586470855 23827
    ## <none>                                         96448734059 23827
    ## - product_colorspurple           1  196421881  96645155940 23828
    ## - rating                         1  196696967  96645431025 23828
    ## - product_colorswhite            1  246067840  96694801899 23829
    ## - price                          1  265118997  96713853056 23829
    ## - product_colorsgrey             1  294221228  96742955287 23829
    ## - product_sizesXXS               1  322462687  96771196746 23830
    ## - product_variation_inventory    1  363855251  96812589310 23830
    ## - product_sizesM                 1  377257809  96825991868 23831
    ## - countries_shipped_to           1  566080422  97014814481 23833
    ## - product_colorsblack            1  581416053  97030150112 23833
    ## - shipping_option_price          1  612293871  97061027930 23834
    ## - `price_classEUR10-20`          1  629620824  97078354882 23834
    ## - product_colorsOther_colors     1  644527073  97093261132 23834
    ## - product_sizesXS                1  658207389  97106941448 23834
    ## - product_sizesXXL               1  718785037  97167519096 23835
    ## - merchant_has_profile_picture1  1 1309678889  97758412948 23843
    ## - merchant_rating_count          1 4048639917 100497373976 23879
    ## 
    ## Step:  AIC=23826.53
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - tags_count                     1  116656908  96653783868 23826
    ## - product_sizesOther_sizes       1  128802674  96665929634 23826
    ## - merchant_rating                1  136267205  96673394165 23826
    ## <none>                                         96537126960 23827
    ## - discount_per                   1  152790596  96689917556 23827
    ## - rating                         1  195614371  96732741331 23827
    ## - product_colorspurple           1  199292978  96736419938 23827
    ## - product_colorswhite            1  254329854  96791456815 23828
    ## - price                          1  259457002  96796583962 23828
    ## - product_colorsgrey             1  287326982  96824453942 23828
    ## - product_sizesXXS               1  356109969  96893236929 23829
    ## - product_variation_inventory    1  362479862  96899606822 23830
    ## - product_sizesM                 1  368314273  96905441233 23830
    ## - product_colorsblack            1  572905808  97110032768 23832
    ## - countries_shipped_to           1  600590042  97137717002 23833
    ## - shipping_option_price          1  605293661  97142420621 23833
    ## - product_colorsOther_colors     1  624784130  97161911090 23833
    ## - `price_classEUR10-20`          1  635316993  97172443953 23833
    ## - product_sizesXS                1  654003486  97191130446 23833
    ## - product_sizesXXL               1  711571236  97248698196 23834
    ## - merchant_has_profile_picture1  1 1341258520  97878385480 23843
    ## - merchant_rating_count          1 4098585171 100635712131 23879
    ## 
    ## Step:  AIC=23826.12
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_sizesOther_sizes       1  100512307  96754296175 23826
    ## - discount_per                   1  121293694  96775077561 23826
    ## <none>                                         96653783868 23826
    ## - merchant_rating                1  167448764  96821232632 23826
    ## - product_colorspurple           1  188361975  96842145843 23827
    ## - rating                         1  192060075  96845843943 23827
    ## - product_colorswhite            1  265209491  96918993359 23828
    ## - price                          1  277367956  96931151824 23828
    ## - product_colorsgrey             1  303305262  96957089130 23828
    ## - product_sizesXXS               1  327141318  96980925185 23829
    ## - product_sizesM                 1  342895237  96996679104 23829
    ## - product_variation_inventory    1  390076207  97043860075 23829
    ## - countries_shipped_to           1  572651351  97226435219 23832
    ## - shipping_option_price          1  588560384  97242344252 23832
    ## - product_colorsblack            1  591492627  97245276495 23832
    ## - product_colorsOther_colors     1  597101489  97250885357 23832
    ## - product_sizesXS                1  610338751  97264122619 23832
    ## - `price_classEUR10-20`          1  642881921  97296665788 23833
    ## - product_sizesXXL               1  687492474  97341276342 23833
    ## - merchant_has_profile_picture1  1 1307761988  97961545855 23842
    ## - merchant_rating_count          1 4007883604 100661667472 23878
    ## 
    ## Step:  AIC=23825.48
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesM + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - discount_per                   1  121310933  96875607107 23825
    ## <none>                                         96754296175 23826
    ## - merchant_rating                1  155989494  96910285669 23826
    ## - rating                         1  181870637  96936166812 23826
    ## - product_colorspurple           1  185347557  96939643732 23826
    ## - product_colorswhite            1  268414348  97022710522 23827
    ## - price                          1  274945878  97029242053 23827
    ## - product_sizesXXS               1  280328763  97034624937 23827
    ## - product_colorsgrey             1  304453571  97058749746 23828
    ## - product_sizesM                 1  432023663  97186319838 23829
    ## - product_variation_inventory    1  462615226  97216911401 23830
    ## - product_sizesXS                1  529359411  97283655586 23831
    ## - product_colorsblack            1  567517456  97321813630 23831
    ## - shipping_option_price          1  589453662  97343749837 23832
    ## - product_colorsOther_colors     1  592044508  97346340683 23832
    ## - countries_shipped_to           1  626872392  97381168567 23832
    ## - `price_classEUR10-20`          1  641617956  97395914131 23832
    ## - product_sizesXXL               1  660883999  97415180173 23832
    ## - merchant_has_profile_picture1  1 1265123547  98019419721 23841
    ## - merchant_rating_count          1 4029687745 100783983919 23877
    ## 
    ## Step:  AIC=23825.13
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesM + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## <none>                                         96875607107 23825
    ## - merchant_rating                1  159412179  97035019286 23825
    ## - rating                         1  180473705  97056080812 23826
    ## - product_colorspurple           1  189325954  97064933061 23826
    ## - product_colorswhite            1  255653982  97131261089 23827
    ## - product_sizesXXS               1  270012671  97145619778 23827
    ## - price                          1  285883170  97161490277 23827
    ## - product_colorsgrey             1  300666579  97176273686 23827
    ## - product_sizesM                 1  429520285  97305127392 23829
    ## - product_variation_inventory    1  457903003  97333510110 23829
    ## - product_sizesXS                1  495241066  97370848173 23830
    ## - product_colorsblack            1  570333524  97445940631 23831
    ## - shipping_option_price          1  583589705  97459196812 23831
    ## - product_colorsOther_colors     1  596346548  97471953655 23831
    ## - countries_shipped_to           1  647687657  97523294764 23832
    ## - `price_classEUR10-20`          1  661097954  97536705061 23832
    ## - product_sizesXXL               1  665176645  97540783752 23832
    ## - merchant_has_profile_picture1  1 1283010453  98158617560 23840
    ## - merchant_rating_count          1 4028098451 100903705558 23877
    ## Start:  AIC=23779.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23779.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23779.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1      76533 95350360664 23778
    ## - shipping_is_express1           1     398205 95350682335 23778
    ## - product_colorsyellow           1     842441 95351126571 23778
    ## - shipping_nameOther_shipping    1     993409 95351277539 23778
    ## - product_colorspink             1    2604799 95352888930 23778
    ## - uses_ad_boosts1                1    4502495 95354786626 23778
    ## - product_sizesM                 1    5330951 95355615081 23778
    ## - badges_count                   1    8981334 95359265465 23778
    ## - badge_product_quality1         1   10780948 95361065079 23778
    ## - badge_local_product1           1   11479939 95361764069 23778
    ## - product_colorsred              1   12900065 95363184195 23778
    ## - product_colorsgreen            1   13147319 95363431449 23778
    ## - `price_classEUR40-50`          1   36554314 95386838444 23778
    ## - origin_countryVE               1   40645356 95390929487 23778
    ## - product_colorsblue             1   50620548 95400904679 23779
    ## - origin_countrySG               1   74427817 95424711948 23779
    ## - retail_price                   1   83152784 95433436914 23779
    ## - origin_countryUS               1   98092458 95448376589 23779
    ## - merchant_rating                1  108740795 95459024926 23779
    ## - product_colorswhite            1  114223184 95464507315 23779
    ## - rating                         1  122324360 95472608491 23780
    ## - discount_per                   1  134490390 95484774520 23780
    ## <none>                                        95350284131 23780
    ## - product_colorsgrey             1  160901791 95511185921 23780
    ## - product_colorsOther_colors     1  176284202 95526568332 23780
    ## - product_colorsblack            1  177484862 95527768993 23780
    ## - product_sizesS                 1  212388555 95562672686 23781
    ## - tags_count                     1  218925198 95569209329 23781
    ## - `price_classEUR20-30`          1  226919393 95577203524 23781
    ## - product_colorspurple           1  237533521 95587817651 23781
    ## - product_sizesOther_sizes       1  346478030 95696762161 23783
    ## - product_variation_inventory    1  354444026 95704728156 23783
    ## - price                          1  400860857 95751144988 23783
    ## - product_sizesXXS               1  552734537 95903018668 23785
    ## - countries_shipped_to           1  583525982 95933810112 23786
    ## - merchant_has_profile_picture1  1  616559364 95966843495 23786
    ## - product_sizesXS                1  633756040 95984040171 23787
    ## - product_sizesXXL               1  800658200 96150942331 23789
    ## - shipping_option_price          1  811404648 96161688779 23789
    ## - `price_classEUR10-20`          1  903868492 96254152622 23790
    ## - merchant_rating_count          1 4302137719 99652421849 23836
    ## 
    ## Step:  AIC=23777.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1     395063 95350755726 23776
    ## - product_colorsyellow           1     848271 95351208935 23776
    ## - shipping_nameOther_shipping    1     991208 95351351871 23776
    ## - product_colorspink             1    2612195 95352972859 23776
    ## - uses_ad_boosts1                1    4549997 95354910661 23776
    ## - product_sizesM                 1    5322331 95355682994 23776
    ## - badges_count                   1    8980180 95359340843 23776
    ## - badge_product_quality1         1   10785569 95361146232 23776
    ## - badge_local_product1           1   11480256 95361840919 23776
    ## - product_colorsred              1   12921538 95363282202 23776
    ## - product_colorsgreen            1   13170970 95363531634 23776
    ## - `price_classEUR40-50`          1   36512620 95386873283 23776
    ## - origin_countryVE               1   40637478 95390998142 23776
    ## - product_colorsblue             1   50545210 95400905874 23777
    ## - origin_countrySG               1   74451641 95424812304 23777
    ## - retail_price                   1   83128812 95433489475 23777
    ## - origin_countryUS               1   98104371 95448465035 23777
    ## - merchant_rating                1  108812934 95459173598 23777
    ## - product_colorswhite            1  114261028 95464621691 23777
    ## - rating                         1  122248542 95472609205 23778
    ## - discount_per                   1  134416040 95484776704 23778
    ## <none>                                        95350360664 23778
    ## - product_colorsgrey             1  160990721 95511351384 23778
    ## - product_colorsOther_colors     1  176359466 95526720130 23778
    ## - product_colorsblack            1  177542123 95527902786 23778
    ## - product_sizesS                 1  212367597 95562728260 23779
    ## - tags_count                     1  218858000 95569218664 23779
    ## - `price_classEUR20-30`          1  226842873 95577203536 23779
    ## - product_colorspurple           1  237626411 95587987075 23779
    ## - product_sizesOther_sizes       1  346405044 95696765708 23781
    ## - product_variation_inventory    1  355261639 95705622302 23781
    ## - price                          1  400793803 95751154467 23781
    ## - product_sizesXXS               1  552658083 95903018746 23783
    ## - countries_shipped_to           1  583565153 95933925816 23784
    ## - merchant_has_profile_picture1  1  616637718 95966998382 23784
    ## - product_sizesXS                1  633950090 95984310753 23785
    ## - product_sizesXXL               1  800602214 96150962878 23787
    ## - shipping_option_price          1  811850618 96162211282 23787
    ## - `price_classEUR10-20`          1  904066893 96254427556 23788
    ## - merchant_rating_count          1 4302080625 99652441289 23834
    ## 
    ## Step:  AIC=23775.89
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1     862512 95351618238 23774
    ## - shipping_nameOther_shipping    1    1231467 95351987193 23774
    ## - product_colorspink             1    2625521 95353381247 23774
    ## - uses_ad_boosts1                1    4459326 95355215052 23774
    ## - product_sizesM                 1    5359687 95356115413 23774
    ## - badges_count                   1   11082367 95361838093 23774
    ## - badge_local_product1           1   12587430 95363343157 23774
    ## - badge_product_quality1         1   13013361 95363769087 23774
    ## - product_colorsred              1   13022312 95363778038 23774
    ## - product_colorsgreen            1   13313349 95364069075 23774
    ## - `price_classEUR40-50`          1   39055543 95389811269 23774
    ## - origin_countryVE               1   40745773 95391501499 23774
    ## - product_colorsblue             1   50592716 95401348442 23775
    ## - origin_countrySG               1   74333077 95425088803 23775
    ## - retail_price                   1   83644121 95434399847 23775
    ## - origin_countryUS               1   97919887 95448675613 23775
    ## - merchant_rating                1  108417896 95459173622 23775
    ## - product_colorswhite            1  114295988 95465051714 23776
    ## - rating                         1  122735020 95473490747 23776
    ## - discount_per                   1  134918377 95485674103 23776
    ## <none>                                        95350755726 23776
    ## - product_colorsgrey             1  160991572 95511747298 23776
    ## - product_colorsOther_colors     1  176143874 95526899600 23776
    ## - product_colorsblack            1  177622876 95528378602 23776
    ## - product_sizesS                 1  213048041 95563803768 23777
    ## - tags_count                     1  219534322 95570290048 23777
    ## - `price_classEUR20-30`          1  227261658 95578017384 23777
    ## - product_colorspurple           1  237681313 95588437039 23777
    ## - product_sizesOther_sizes       1  346911124 95697666851 23779
    ## - product_variation_inventory    1  356718286 95707474013 23779
    ## - price                          1  400645330 95751401056 23779
    ## - product_sizesXXS               1  553931385 95904687112 23782
    ## - countries_shipped_to           1  584581701 95935337427 23782
    ## - merchant_has_profile_picture1  1  616922540 95967678267 23782
    ## - product_sizesXS                1  636025807 95986781533 23783
    ## - product_sizesXXL               1  801348169 96152103895 23785
    ## - shipping_option_price          1  823114559 96173870285 23785
    ## - `price_classEUR10-20`          1  915922279 96266678005 23786
    ## - merchant_rating_count          1 4301766582 99652522308 23832
    ## 
    ## Step:  AIC=23773.9
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1    1266283 95352884521 23772
    ## - product_colorspink             1    1989695 95353607934 23772
    ## - uses_ad_boosts1                1    4369184 95355987423 23772
    ## - product_sizesM                 1    5660757 95357278995 23772
    ## - badges_count                   1   10930830 95362549069 23772
    ## - badge_local_product1           1   12392241 95364010479 23772
    ## - badge_product_quality1         1   12853695 95364471933 23772
    ## - product_colorsred              1   19446069 95371064307 23772
    ## - product_colorsgreen            1   20016686 95371634924 23772
    ## - `price_classEUR40-50`          1   39251958 95390870196 23772
    ## - origin_countryVE               1   40795772 95392414011 23773
    ## - origin_countrySG               1   74167309 95425785548 23773
    ## - retail_price                   1   82887282 95434505521 23773
    ## - product_colorsblue             1   94766144 95446384382 23773
    ## - origin_countryUS               1   97121187 95448739425 23773
    ## - merchant_rating                1  107873707 95459491945 23773
    ## - rating                         1  124165917 95475784156 23774
    ## - discount_per                   1  134585824 95486204062 23774
    ## <none>                                        95351618238 23774
    ## - product_sizesS                 1  214914046 95566532285 23775
    ## - tags_count                     1  218711153 95570329391 23775
    ## - `price_classEUR20-30`          1  227336168 95578954407 23775
    ## - product_colorswhite            1  292758248 95644376486 23776
    ## - product_colorsgrey             1  307576778 95659195017 23776
    ## - product_sizesOther_sizes       1  350689786 95702308024 23777
    ## - product_variation_inventory    1  355857989 95707476227 23777
    ## - price                          1  401030380 95752648619 23777
    ## - product_colorspurple           1  409827584 95761445823 23778
    ## - product_colorsblack            1  479145017 95830763255 23779
    ## - product_colorsOther_colors     1  504883587 95856501825 23779
    ## - product_sizesXXS               1  560158921 95911777160 23780
    ## - countries_shipped_to           1  592285437 95943903675 23780
    ## - merchant_has_profile_picture1  1  617759493 95969377732 23780
    ## - product_sizesXS                1  636732724 95988350963 23781
    ## - product_sizesXXL               1  803060303 96154678542 23783
    ## - shipping_option_price          1  822267823 96173886061 23783
    ## - `price_classEUR10-20`          1  915303082 96266921320 23784
    ## - merchant_rating_count          1 4311223625 99662841863 23830
    ## 
    ## Step:  AIC=23771.92
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1    1974406 95354858927 23770
    ## - uses_ad_boosts1                1    4242748 95357127269 23770
    ## - product_sizesM                 1    5588730 95358473251 23770
    ## - badges_count                   1   11267341 95364151862 23770
    ## - badge_local_product1           1   12715271 95365599792 23770
    ## - badge_product_quality1         1   13018966 95365903487 23770
    ## - product_colorsred              1   19885599 95372770120 23770
    ## - product_colorsgreen            1   20404894 95373289414 23770
    ## - `price_classEUR40-50`          1   39287043 95392171564 23771
    ## - origin_countryVE               1   40706590 95393591111 23771
    ## - origin_countrySG               1   74262583 95427147104 23771
    ## - retail_price                   1   84314358 95437198879 23771
    ## - origin_countryUS               1   96382871 95449267392 23771
    ## - product_colorsblue             1   96521574 95449406095 23771
    ## - merchant_rating                1  107791728 95460676249 23771
    ## - rating                         1  124060732 95476945253 23772
    ## - discount_per                   1  135566470 95488450991 23772
    ## <none>                                        95352884521 23772
    ## - product_sizesS                 1  214510297 95567394818 23773
    ## - tags_count                     1  218527628 95571412149 23773
    ## - `price_classEUR20-30`          1  228092723 95580977244 23773
    ## - product_colorswhite            1  294931278 95647815799 23774
    ## - product_colorsgrey             1  309619675 95662504196 23774
    ## - product_sizesOther_sizes       1  350457793 95703342313 23775
    ## - product_variation_inventory    1  356026976 95708911497 23775
    ## - price                          1  400529975 95753414496 23775
    ## - product_colorspurple           1  414033773 95766918294 23776
    ## - product_colorsblack            1  482853114 95835737635 23777
    ## - product_colorsOther_colors     1  506352656 95859237177 23777
    ## - product_sizesXXS               1  559030850 95911915371 23778
    ## - countries_shipped_to           1  591633368 95944517889 23778
    ## - merchant_has_profile_picture1  1  620720062 95973604583 23778
    ## - product_sizesXS                1  636729264 95989613785 23779
    ## - product_sizesXXL               1  803646422 96156530943 23781
    ## - shipping_option_price          1  826743332 96179627853 23781
    ## - `price_classEUR10-20`          1  914346687 96267231208 23782
    ## - merchant_rating_count          1 4311434180 99664318701 23828
    ## 
    ## Step:  AIC=23769.94
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1    4271099 95359130026 23768
    ## - product_sizesM                 1    5483955 95360342882 23768
    ## - badges_count                   1   11317499 95366176426 23768
    ## - badge_local_product1           1   12705615 95367564542 23768
    ## - badge_product_quality1         1   13298838 95368157765 23768
    ## - product_colorsred              1   18162588 95373021515 23768
    ## - product_colorsgreen            1   18704451 95373563379 23768
    ## - `price_classEUR40-50`          1   39174073 95394033000 23769
    ## - origin_countryVE               1   40720234 95395579161 23769
    ## - origin_countrySG               1   74303991 95429162919 23769
    ## - retail_price                   1   83538818 95438397746 23769
    ## - origin_countryUS               1   97976118 95452835045 23769
    ## - product_colorsblue             1  104568091 95459427018 23769
    ## - merchant_rating                1  107458675 95462317602 23769
    ## - rating                         1  123571079 95478430007 23770
    ## - discount_per                   1  134898367 95489757295 23770
    ## <none>                                        95354858927 23770
    ## - product_sizesS                 1  213156068 95568014995 23771
    ## - tags_count                     1  218809532 95573668459 23771
    ## - `price_classEUR20-30`          1  227626837 95582485764 23771
    ## - product_colorsgrey             1  344622435 95699481362 23773
    ## - product_sizesOther_sizes       1  350111403 95704970330 23773
    ## - product_variation_inventory    1  355327748 95710186675 23773
    ## - product_colorswhite            1  372842024 95727700951 23773
    ## - price                          1  400143615 95755002542 23773
    ## - product_colorspurple           1  446513599 95801372526 23774
    ## - product_sizesXXS               1  558239518 95913098446 23776
    ## - countries_shipped_to           1  589734061 95944592988 23776
    ## - merchant_has_profile_picture1  1  619166687 95974025614 23776
    ## - product_colorsblack            1  627756281 95982615208 23777
    ## - product_sizesXS                1  635480219 95990339146 23777
    ## - product_colorsOther_colors     1  670663015 96025521942 23777
    ## - product_sizesXXL               1  802985758 96157844685 23779
    ## - shipping_option_price          1  826358304 96181217232 23779
    ## - `price_classEUR10-20`          1  914020661 96268879589 23780
    ## - merchant_rating_count          1 4310882688 99665741615 23826
    ## 
    ## Step:  AIC=23768
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1    5509095 95364639121 23766
    ## - badges_count                   1   11240363 95370370388 23766
    ## - badge_local_product1           1   12035281 95371165306 23766
    ## - badge_product_quality1         1   13137535 95372267560 23766
    ## - product_colorsred              1   18131617 95377261642 23766
    ## - product_colorsgreen            1   18155130 95377285156 23766
    ## - `price_classEUR40-50`          1   39739782 95398869807 23767
    ## - origin_countryVE               1   42342291 95401472316 23767
    ## - origin_countrySG               1   73052521 95432182547 23767
    ## - retail_price                   1   82916744 95442046770 23767
    ## - origin_countryUS               1   98575776 95457705801 23767
    ## - product_colorsblue             1  104490102 95463620128 23767
    ## - merchant_rating                1  108810946 95467940972 23768
    ## - rating                         1  125374923 95484504948 23768
    ## - discount_per                   1  133972346 95493102372 23768
    ## <none>                                        95359130026 23768
    ## - product_sizesS                 1  212331004 95571461030 23769
    ## - tags_count                     1  220115519 95579245545 23769
    ## - `price_classEUR20-30`          1  233936519 95593066544 23769
    ## - product_colorsgrey             1  343386425 95702516450 23771
    ## - product_sizesOther_sizes       1  355347323 95714477348 23771
    ## - product_variation_inventory    1  365427275 95724557301 23771
    ## - product_colorswhite            1  373861697 95732991723 23771
    ## - price                          1  407139529 95766269554 23772
    ## - product_colorspurple           1  447749639 95806879665 23772
    ## - product_sizesXXS               1  563332902 95922462928 23774
    ## - countries_shipped_to           1  586729531 95945859556 23774
    ## - merchant_has_profile_picture1  1  617691880 95976821906 23775
    ## - product_colorsblack            1  624647901 95983777926 23775
    ## - product_sizesXS                1  633402366 95992532391 23775
    ## - product_colorsOther_colors     1  668260988 96027391014 23775
    ## - product_sizesXXL               1  802493276 96161623302 23777
    ## - shipping_option_price          1  830558902 96189688927 23777
    ## - `price_classEUR10-20`          1  922961518 96282091543 23779
    ## - merchant_rating_count          1 4306931693 99666061719 23824
    ## 
    ## Step:  AIC=23766.08
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1   11142470 95375781590 23764
    ## - badge_local_product1           1   12368568 95377007688 23764
    ## - badge_product_quality1         1   13082666 95377721786 23764
    ## - product_colorsgreen            1   18842309 95383481430 23764
    ## - product_colorsred              1   19097992 95383737113 23764
    ## - `price_classEUR40-50`          1   39832347 95404471467 23765
    ## - origin_countryVE               1   42777415 95407416536 23765
    ## - origin_countrySG               1   73119663 95437758784 23765
    ## - retail_price                   1   82545185 95447184305 23765
    ## - origin_countryUS               1  101069394 95465708515 23766
    ## - product_colorsblue             1  101992603 95466631724 23766
    ## - merchant_rating                1  108766269 95473405389 23766
    ## - rating                         1  126614725 95491253845 23766
    ## - discount_per                   1  133229055 95497868176 23766
    ## <none>                                        95364639121 23766
    ## - tags_count                     1  220486081 95585125202 23767
    ## - `price_classEUR20-30`          1  236389549 95601028670 23767
    ## - product_colorsgrey             1  340638586 95705277706 23769
    ## - product_variation_inventory    1  365456671 95730095792 23769
    ## - product_colorswhite            1  370348015 95734987136 23769
    ## - price                          1  404799015 95769438136 23770
    ## - product_colorspurple           1  446196508 95810835629 23770
    ## - countries_shipped_to           1  584393863 95949032984 23772
    ## - product_sizesS                 1  591069738 95955708858 23772
    ## - product_sizesOther_sizes       1  610803067 95975442188 23772
    ## - merchant_has_profile_picture1  1  614359044 95978998165 23773
    ## - product_colorsblack            1  619997829 95984636950 23773
    ## - product_colorsOther_colors     1  669731058 96034370178 23773
    ## - shipping_option_price          1  827368656 96192007777 23775
    ## - `price_classEUR10-20`          1  918366575 96283005695 23777
    ## - product_sizesXXL               1  938093407 96302732528 23777
    ## - product_sizesXXS               1  942612543 96307251664 23777
    ## - product_sizesXS                1 1539686610 96904325731 23785
    ## - merchant_rating_count          1 4305625546 99670264667 23822
    ## 
    ## Step:  AIC=23764.23
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_local_product1 + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1    1664743 95377446333 23762
    ## - badge_product_quality1         1    1940584 95377722174 23762
    ## - product_colorsgreen            1   17638201 95393419791 23763
    ## - product_colorsred              1   19967447 95395749037 23763
    ## - `price_classEUR40-50`          1   42173346 95417954936 23763
    ## - origin_countryVE               1   42735899 95418517489 23763
    ## - origin_countrySG               1   72763476 95448545066 23763
    ## - retail_price                   1   85007533 95460789124 23763
    ## - product_colorsblue             1  102692136 95478473726 23764
    ## - origin_countryUS               1  103498785 95479280375 23764
    ## - merchant_rating                1  105103293 95480884884 23764
    ## - rating                         1  130625882 95506407472 23764
    ## - discount_per                   1  136124466 95511906056 23764
    ## <none>                                        95375781590 23764
    ## - tags_count                     1  219095960 95594877550 23765
    ## - `price_classEUR20-30`          1  232901142 95608682732 23765
    ## - product_colorsgrey             1  341423918 95717205508 23767
    ## - product_colorswhite            1  369945152 95745726743 23767
    ## - product_variation_inventory    1  389821115 95765602705 23768
    ## - price                          1  403047494 95778829084 23768
    ## - product_colorspurple           1  447406684 95823188274 23768
    ## - countries_shipped_to           1  585877995 95961659585 23770
    ## - product_sizesS                 1  590058253 95965839843 23770
    ## - merchant_has_profile_picture1  1  610981375 95986762966 23771
    ## - product_colorsblack            1  615007039 95990788629 23771
    ## - product_sizesOther_sizes       1  631047440 96006829031 23771
    ## - product_colorsOther_colors     1  665576584 96041358174 23771
    ## - shipping_option_price          1  841690819 96217472409 23774
    ## - `price_classEUR10-20`          1  913552137 96289333727 23775
    ## - product_sizesXXL               1  939302539 96315084129 23775
    ## - product_sizesXXS               1  942229627 96318011217 23775
    ## - product_sizesXS                1 1531611965 96907393555 23783
    ## - merchant_rating_count          1 4312734243 99688515833 23820
    ## 
    ## Step:  AIC=23762.25
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1    2160142 95379606475 23760
    ## - product_colorsgreen            1   17524611 95394970944 23761
    ## - product_colorsred              1   20074902 95397521236 23761
    ## - `price_classEUR40-50`          1   42246499 95419692832 23761
    ## - origin_countryVE               1   42749690 95420196023 23761
    ## - origin_countrySG               1   72737550 95450183884 23761
    ## - retail_price                   1   85181707 95462628040 23761
    ## - product_colorsblue             1  101842782 95479289116 23762
    ## - origin_countryUS               1  104351244 95481797577 23762
    ## - merchant_rating                1  109400951 95486847284 23762
    ## - rating                         1  129459172 95506905506 23762
    ## - discount_per                   1  135966606 95513412940 23762
    ## <none>                                        95377446333 23762
    ## - tags_count                     1  221136438 95598582771 23763
    ## - `price_classEUR20-30`          1  233015470 95610461803 23763
    ## - product_colorsgrey             1  341200562 95718646896 23765
    ## - product_colorswhite            1  370250623 95747696956 23765
    ## - product_variation_inventory    1  388788449 95766234782 23766
    ## - price                          1  401795002 95779241336 23766
    ## - product_colorspurple           1  447828034 95825274367 23766
    ## - countries_shipped_to           1  585353149 95962799482 23768
    ## - product_sizesS                 1  589471289 95966917622 23768
    ## - merchant_has_profile_picture1  1  609335400 95986781733 23769
    ## - product_colorsblack            1  613729032 95991175365 23769
    ## - product_sizesOther_sizes       1  629615155 96007061488 23769
    ## - product_colorsOther_colors     1  665449340 96042895673 23769
    ## - shipping_option_price          1  840026109 96217472442 23772
    ## - `price_classEUR10-20`          1  911923173 96289369506 23773
    ## - product_sizesXXL               1  939104195 96316550528 23773
    ## - product_sizesXXS               1  941705079 96319151412 23773
    ## - product_sizesXS                1 1530012591 96907458924 23781
    ## - merchant_rating_count          1 4311294678 99688741011 23818
    ## 
    ## Step:  AIC=23760.28
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   17524710 95397131185 23759
    ## - product_colorsred              1   19976712 95399583187 23759
    ## - `price_classEUR40-50`          1   42017761 95421624237 23759
    ## - origin_countryVE               1   42556440 95422162915 23759
    ## - origin_countrySG               1   72261888 95451868364 23759
    ## - retail_price                   1   85524747 95465131222 23760
    ## - product_colorsblue             1  101771642 95481378117 23760
    ## - origin_countryUS               1  103110106 95482716582 23760
    ## - merchant_rating                1  113908317 95493514792 23760
    ## - discount_per                   1  136741840 95516348315 23760
    ## <none>                                        95379606475 23760
    ## - rating                         1  146564046 95526170522 23760
    ## - tags_count                     1  221230942 95600837418 23761
    ## - `price_classEUR20-30`          1  232917574 95612524049 23762
    ## - product_colorsgrey             1  344077762 95723684237 23763
    ## - product_colorswhite            1  369368782 95748975258 23763
    ## - product_variation_inventory    1  392589217 95772195692 23764
    ## - price                          1  400707578 95780314054 23764
    ## - product_colorspurple           1  447810323 95827416798 23764
    ## - countries_shipped_to           1  587433315 95967039790 23766
    ## - product_sizesS                 1  588097008 95967703483 23766
    ## - merchant_has_profile_picture1  1  609652997 95989259473 23767
    ## - product_colorsblack            1  616215143 95995821618 23767
    ## - product_sizesOther_sizes       1  629513805 96009120280 23767
    ## - product_colorsOther_colors     1  665383057 96044989532 23767
    ## - shipping_option_price          1  839663529 96219270005 23770
    ## - `price_classEUR10-20`          1  910088168 96289694643 23771
    ## - product_sizesXXL               1  937638054 96317244529 23771
    ## - product_sizesXXS               1  942424597 96322031072 23771
    ## - product_sizesXS                1 1528048010 96907654486 23779
    ## - merchant_rating_count          1 4352140773 99731747249 23817
    ## 
    ## Step:  AIC=23758.52
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1   11706918 95408838103 23757
    ## - `price_classEUR40-50`          1   37572242 95434703427 23757
    ## - origin_countryVE               1   42148261 95439279445 23757
    ## - origin_countrySG               1   72014516 95469145701 23758
    ## - retail_price                   1   83490345 95480621530 23758
    ## - product_colorsblue             1   86102746 95483233931 23758
    ## - origin_countryUS               1  105187222 95502318407 23758
    ## - merchant_rating                1  115041784 95512172969 23758
    ## - discount_per                   1  131569935 95528701120 23758
    ## <none>                                        95397131185 23759
    ## - rating                         1  155581736 95552712921 23759
    ## - tags_count                     1  223065212 95620196397 23760
    ## - `price_classEUR20-30`          1  230481360 95627612545 23760
    ## - product_colorsgrey             1  327042328 95724173513 23761
    ## - product_colorswhite            1  362071844 95759203029 23762
    ## - product_variation_inventory    1  379637818 95776769003 23762
    ## - price                          1  396734948 95793866133 23762
    ## - product_colorspurple           1  430585930 95827717115 23762
    ## - countries_shipped_to           1  584188030 95981319215 23765
    ## - product_sizesS                 1  588035122 95985166307 23765
    ## - merchant_has_profile_picture1  1  618975221 96016106406 23765
    ## - product_sizesOther_sizes       1  625355679 96022486864 23765
    ## - product_colorsblack            1  631034043 96028165228 23765
    ## - product_colorsOther_colors     1  690633562 96087764747 23766
    ## - shipping_option_price          1  835661159 96232792344 23768
    ## - `price_classEUR10-20`          1  901840549 96298971734 23769
    ## - product_sizesXXL               1  934710479 96331841664 23769
    ## - product_sizesXXS               1  946001177 96343132362 23769
    ## - product_sizesXS                1 1530383731 96927514916 23777
    ## - merchant_rating_count          1 4349153985 99746285170 23815
    ## 
    ## Step:  AIC=23756.68
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   35277316 95444115418 23755
    ## - origin_countryVE               1   42076463 95450914566 23755
    ## - origin_countrySG               1   71935831 95480773934 23756
    ## - product_colorsblue             1   76535150 95485373252 23756
    ## - retail_price                   1   83844105 95492682208 23756
    ## - origin_countryUS               1  107551432 95516389535 23756
    ## - merchant_rating                1  116840510 95525678613 23756
    ## - discount_per                   1  132228459 95541066561 23757
    ## <none>                                        95408838103 23757
    ## - rating                         1  157306776 95566144878 23757
    ## - tags_count                     1  227291149 95636129251 23758
    ## - `price_classEUR20-30`          1  230177878 95639015981 23758
    ## - product_colorsgrey             1  315376782 95724214885 23759
    ## - product_colorswhite            1  355985898 95764824000 23760
    ## - product_variation_inventory    1  381302557 95790140660 23760
    ## - price                          1  393349970 95802188073 23760
    ## - product_colorspurple           1  418888201 95827726304 23760
    ## - product_sizesS                 1  586985729 95995823832 23763
    ## - countries_shipped_to           1  594706115 96003544218 23763
    ## - merchant_has_profile_picture1  1  612669874 96021507977 23763
    ## - product_sizesOther_sizes       1  625458671 96034296774 23763
    ## - product_colorsblack            1  639708596 96048546698 23763
    ## - product_colorsOther_colors     1  705703602 96114541705 23764
    ## - shipping_option_price          1  830782914 96239621017 23766
    ## - `price_classEUR10-20`          1  898169742 96307007845 23767
    ## - product_sizesXXL               1  934063286 96342901388 23767
    ## - product_sizesXXS               1  953740666 96362578769 23768
    ## - product_sizesXS                1 1531373381 96940211484 23776
    ## - merchant_rating_count          1 4349236166 99758074268 23813
    ## 
    ## Step:  AIC=23755.17
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   41090708 95485206126 23754
    ## - origin_countrySG               1   69660789 95513776208 23754
    ## - product_colorsblue             1   83985439 95528100857 23754
    ## - retail_price                   1   90190520 95534305938 23754
    ## - origin_countryUS               1  103925546 95548040964 23755
    ## - merchant_rating                1  131124405 95575239823 23755
    ## - discount_per                   1  137088609 95581204027 23755
    ## <none>                                        95444115418 23755
    ## - rating                         1  160447814 95604563232 23755
    ## - `price_classEUR20-30`          1  200377133 95644492551 23756
    ## - tags_count                     1  231612819 95675728238 23756
    ## - product_colorsgrey             1  322380981 95766496399 23758
    ## - price                          1  359484992 95803600411 23758
    ## - product_colorswhite            1  360082093 95804197511 23758
    ## - product_variation_inventory    1  389897808 95834013226 23759
    ## - product_colorspurple           1  425270943 95869386362 23759
    ## - product_sizesS                 1  600938313 96045053731 23761
    ## - countries_shipped_to           1  602382506 96046497925 23761
    ## - merchant_has_profile_picture1  1  614808812 96058924231 23762
    ## - product_colorsblack            1  650145403 96094260821 23762
    ## - product_sizesOther_sizes       1  689628657 96133744075 23763
    ## - product_colorsOther_colors     1  719785383 96163900802 23763
    ## - shipping_option_price          1  807436650 96251552068 23764
    ## - `price_classEUR10-20`          1  910510424 96354625842 23766
    ## - product_sizesXXL               1  936377324 96380492743 23766
    ## - product_sizesXXS               1  982031670 96426147089 23767
    ## - product_sizesXS                1 1560814995 97004930413 23774
    ## - merchant_rating_count          1 4337525605 99781641023 23811
    ## 
    ## Step:  AIC=23753.73
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countrySG               1   69800718 95555006844 23753
    ## - product_colorsblue             1   83825941 95569032067 23753
    ## - retail_price                   1   90868526 95576074652 23753
    ## - origin_countryUS               1  102617460 95587823586 23753
    ## - discount_per                   1  137543434 95622749560 23754
    ## - merchant_rating                1  140704075 95625910201 23754
    ## <none>                                        95485206126 23754
    ## - rating                         1  170206280 95655412406 23754
    ## - `price_classEUR20-30`          1  197790955 95682997081 23754
    ## - tags_count                     1  227431797 95712637923 23755
    ## - product_colorsgrey             1  321131695 95806337821 23756
    ## - price                          1  349123541 95834329667 23757
    ## - product_colorswhite            1  351080248 95836286374 23757
    ## - product_variation_inventory    1  390878457 95876084583 23757
    ## - product_colorspurple           1  422781019 95907987145 23758
    ## - product_sizesS                 1  593453659 96078659785 23760
    ## - countries_shipped_to           1  599641244 96084847370 23760
    ## - merchant_has_profile_picture1  1  617871898 96103078024 23760
    ## - product_colorsblack            1  630770235 96115976361 23760
    ## - product_sizesOther_sizes       1  682828956 96168035082 23761
    ## - product_colorsOther_colors     1  716865932 96202072058 23762
    ## - shipping_option_price          1  791638568 96276844693 23763
    ## - `price_classEUR10-20`          1  906983176 96392189302 23764
    ## - product_sizesXXL               1  928355993 96413562119 23764
    ## - product_sizesXXS               1  973367828 96458573954 23765
    ## - product_sizesXS                1 1556837552 97042043678 23773
    ## - merchant_rating_count          1 4337169705 99822375831 23810
    ## 
    ## Step:  AIC=23752.69
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsblue             1   84828270 95639835115 23752
    ## - retail_price                   1   93035162 95648042007 23752
    ## - origin_countryUS               1  102967027 95657973871 23752
    ## - merchant_rating                1  138296904 95693303748 23753
    ## - discount_per                   1  141980189 95696987033 23753
    ## <none>                                        95555006844 23753
    ## - rating                         1  175281068 95730287912 23753
    ## - `price_classEUR20-30`          1  195036368 95750043212 23753
    ## - tags_count                     1  226729801 95781736646 23754
    ## - price                          1  343374706 95898381551 23755
    ## - product_colorsgrey             1  346181366 95901188211 23755
    ## - product_colorswhite            1  353131662 95908138506 23756
    ## - product_variation_inventory    1  386912609 95941919453 23756
    ## - product_colorspurple           1  421388758 95976395602 23756
    ## - product_sizesS                 1  596689261 96151696105 23759
    ## - countries_shipped_to           1  611371449 96166378294 23759
    ## - merchant_has_profile_picture1  1  613302656 96168309500 23759
    ## - product_colorsblack            1  634820883 96189827728 23759
    ## - product_sizesOther_sizes       1  684351261 96239358105 23760
    ## - product_colorsOther_colors     1  734316742 96289323586 23761
    ## - shipping_option_price          1  794574185 96349581029 23762
    ## - `price_classEUR10-20`          1  898415114 96453421958 23763
    ## - product_sizesXXL               1  929083231 96484090075 23763
    ## - product_sizesXXS               1  981844177 96536851022 23764
    ## - product_sizesXS                1 1540962973 97095969818 23772
    ## - merchant_rating_count          1 4327088185 99882095030 23809
    ## 
    ## Step:  AIC=23751.85
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1   91319205 95731154320 23751
    ## - origin_countryUS               1  112283381 95752118496 23751
    ## - discount_per                   1  137261506 95777096621 23752
    ## - merchant_rating                1  138260750 95778095865 23752
    ## <none>                                        95639835115 23752
    ## - rating                         1  172763102 95812598217 23752
    ## - `price_classEUR20-30`          1  196792708 95836627823 23753
    ## - tags_count                     1  224419732 95864254846 23753
    ## - product_colorswhite            1  287562981 95927398096 23754
    ## - product_colorsgrey             1  300380956 95940216071 23754
    ## - price                          1  345039292 95984874407 23755
    ## - product_colorspurple           1  378180103 96018015218 23755
    ## - product_variation_inventory    1  406367169 96046202284 23755
    ## - product_colorsblack            1  553891291 96193726406 23757
    ## - countries_shipped_to           1  557807499 96197642613 23758
    ## - merchant_has_profile_picture1  1  596147988 96235983103 23758
    ## - product_sizesS                 1  604359970 96244195085 23758
    ## - product_colorsOther_colors     1  652450439 96292285554 23759
    ## - product_sizesOther_sizes       1  668227915 96308063030 23759
    ## - shipping_option_price          1  789151803 96428986918 23761
    ## - `price_classEUR10-20`          1  897794055 96537629170 23762
    ## - product_sizesXXL               1  909189300 96549024415 23762
    ## - product_sizesXXS               1  967927247 96607762362 23763
    ## - product_sizesXS                1 1524165376 97164000491 23771
    ## - merchant_rating_count          1 4304688899 99944524014 23808
    ## 
    ## Step:  AIC=23751.1
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - discount_per                   1   46694062  95777848382 23750
    ## - origin_countryUS               1  118131610  95849285930 23751
    ## <none>                                         95731154320 23751
    ## - merchant_rating                1  148775501  95879929821 23751
    ## - rating                         1  168758912  95899913232 23751
    ## - `price_classEUR20-30`          1  175884222  95907038542 23752
    ## - tags_count                     1  220504009  95951658329 23752
    ## - product_colorswhite            1  292116584  96023270904 23753
    ## - product_colorsgrey             1  294026820  96025181140 23753
    ## - product_colorspurple           1  382526501  96113680821 23754
    ## - price                          1  388971221  96120125541 23754
    ## - product_variation_inventory    1  401405128  96132559448 23755
    ## - countries_shipped_to           1  558698786  96289853106 23757
    ## - product_colorsblack            1  584578083  96315732403 23757
    ## - product_sizesS                 1  596272482  96327426802 23757
    ## - merchant_has_profile_picture1  1  624325974  96355480294 23758
    ## - product_colorsOther_colors     1  653712213  96384866533 23758
    ## - product_sizesOther_sizes       1  675601615  96406755935 23758
    ## - shipping_option_price          1  760757310  96491911630 23760
    ## - `price_classEUR10-20`          1  868227944  96599382264 23761
    ## - product_sizesXXL               1  925376709  96656531029 23762
    ## - product_sizesXXS               1 1000490379  96731644699 23763
    ## - product_sizesXS                1 1506246219  97237400539 23770
    ## - merchant_rating_count          1 4407514235 100138668555 23808
    ## 
    ## Step:  AIC=23749.74
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryUS               1  125184110  95903032492 23749
    ## <none>                                         95777848382 23750
    ## - merchant_rating                1  149122813  95926971195 23750
    ## - rating                         1  164722364  95942570746 23750
    ## - `price_classEUR20-30`          1  194033378  95971881760 23750
    ## - tags_count                     1  197730085  95975578467 23750
    ## - product_colorswhite            1  288457689  96066306071 23752
    ## - product_colorsgrey             1  294848775  96072697157 23752
    ## - product_colorspurple           1  382782788  96160631170 23753
    ## - product_variation_inventory    1  402384252  96180232633 23753
    ## - price                          1  409521506  96187369888 23753
    ## - countries_shipped_to           1  567612139  96345460521 23756
    ## - product_colorsblack            1  593190882  96371039264 23756
    ## - product_sizesS                 1  603195605  96381043987 23756
    ## - merchant_has_profile_picture1  1  628894448  96406742829 23756
    ## - product_colorsOther_colors     1  659314044  96437162425 23757
    ## - product_sizesOther_sizes       1  674958884  96452807266 23757
    ## - shipping_option_price          1  761708128  96539556510 23758
    ## - `price_classEUR10-20`          1  898467719  96676316101 23760
    ## - product_sizesXXL               1  909144311  96686992693 23760
    ## - product_sizesXXS               1  988473812  96766322194 23761
    ## - product_sizesXS                1 1476933682  97254782063 23768
    ## - merchant_rating_count          1 4387434559 100165282941 23806
    ## 
    ## Step:  AIC=23749.45
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## <none>                                         95903032492 23749
    ## - merchant_rating                1  154125849  96057158341 23750
    ## - rating                         1  162875474  96065907966 23750
    ## - tags_count                     1  180924695  96083957187 23750
    ## - `price_classEUR20-30`          1  186802666  96089835159 23750
    ## - product_colorsgrey             1  284775978  96187808470 23751
    ## - product_colorswhite            1  297443013  96200475506 23752
    ## - product_colorspurple           1  387090856  96290123348 23753
    ## - product_variation_inventory    1  396443415  96299475907 23753
    ## - price                          1  400201332  96303233824 23753
    ## - product_colorsblack            1  583437111  96486469603 23755
    ## - product_sizesS                 1  594758314  96497790806 23756
    ## - countries_shipped_to           1  609024799  96512057291 23756
    ## - product_colorsOther_colors     1  625423538  96528456030 23756
    ## - merchant_has_profile_picture1  1  655604225  96558636717 23756
    ## - product_sizesOther_sizes       1  663605728  96566638220 23757
    ## - shipping_option_price          1  754753894  96657786386 23758
    ## - product_sizesXXL               1  894838048  96797870540 23760
    ## - `price_classEUR10-20`          1  897857623  96800890115 23760
    ## - product_sizesXXS               1 1038767806  96941800298 23762
    ## - product_sizesXS                1 1456037365  97359069857 23767
    ## - merchant_rating_count          1 4449360286 100352392778 23807
    ## Start:  AIC=23785.26
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23785.26
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23785.26
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1      88601 92070467502 23783
    ## - shipping_is_express1           1    1824430 92072203331 23783
    ## - uses_ad_boosts1                1    2508996 92072887898 23783
    ## - shipping_nameOther_shipping    1    3314745 92073693647 23783
    ## - product_colorsyellow           1    5619987 92075998889 23783
    ## - badge_local_product1           1    5694149 92076073051 23783
    ## - product_colorspink             1    6661951 92077040853 23783
    ## - product_sizesM                 1    8004573 92078383474 23783
    ## - badges_count                   1   13841402 92084220304 23784
    ## - product_colorsred              1   20746015 92091124917 23784
    ## - origin_countryVE               1   25074764 92095453666 23784
    ## - badge_product_quality1         1   26524890 92096903792 23784
    ## - `price_classEUR40-50`          1   41502468 92111881370 23784
    ## - product_colorsgreen            1   47916423 92118295325 23784
    ## - tags_count                     1   52689193 92123068095 23784
    ## - origin_countryUS               1   72331661 92142710563 23784
    ## - `price_classEUR20-30`          1   73656239 92144035141 23784
    ## - origin_countrySG               1   78643783 92149022684 23784
    ## - product_colorsblue             1   81291087 92151669989 23784
    ## - retail_price                   1   85233785 92155612687 23785
    ## - product_sizesS                 1  100993420 92171372322 23785
    ## - product_colorswhite            1  111331337 92181710239 23785
    ## - rating                         1  128167190 92198546092 23785
    ## <none>                                        92070378902 23785
    ## - product_colorsgrey             1  154968027 92225346928 23786
    ## - product_colorsOther_colors     1  162991508 92233370410 23786
    ## - product_sizesOther_sizes       1  185144146 92255523048 23786
    ## - product_colorsblack            1  185271750 92255650651 23786
    ## - product_variation_inventory    1  190764875 92261143777 23786
    ## - merchant_rating                1  201279546 92271658447 23786
    ## - discount_per                   1  236425201 92306804102 23787
    ## - product_colorspurple           1  300719106 92371098008 23788
    ## - price                          1  382034913 92452413815 23789
    ## - product_sizesXXS               1  430011907 92500390809 23789
    ## - countries_shipped_to           1  531096778 92601475679 23791
    ## - product_sizesXXL               1  537098853 92607477755 23791
    ## - product_sizesXS                1  564447750 92634826651 23791
    ## - `price_classEUR10-20`          1  785654408 92856033309 23794
    ## - merchant_has_profile_picture1  1  879571802 92949950704 23796
    ## - shipping_option_price          1  881285076 92951663977 23796
    ## - merchant_rating_count          1 2681138750 94751517651 23821
    ## 
    ## Step:  AIC=23783.26
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1    1831912 92072299414 23781
    ## - uses_ad_boosts1                1    2546607 92073014109 23781
    ## - shipping_nameOther_shipping    1    3312286 92073779788 23781
    ## - product_colorsyellow           1    5636456 92076103959 23781
    ## - badge_local_product1           1    5696498 92076164000 23781
    ## - product_colorspink             1    6676158 92077143660 23781
    ## - product_sizesM                 1    8014117 92078481619 23781
    ## - badges_count                   1   13842393 92084309895 23782
    ## - product_colorsred              1   20779828 92091247330 23782
    ## - origin_countryVE               1   25067622 92095535125 23782
    ## - badge_product_quality1         1   26534344 92097001847 23782
    ## - `price_classEUR40-50`          1   41457651 92111925153 23782
    ## - product_colorsgreen            1   47970235 92118437737 23782
    ## - tags_count                     1   52614784 92123082286 23782
    ## - origin_countryUS               1   72341730 92142809232 23782
    ## - `price_classEUR20-30`          1   73585122 92144052624 23782
    ## - origin_countrySG               1   78669476 92149136979 23782
    ## - product_colorsblue             1   81221168 92151688670 23782
    ## - retail_price                   1   85218517 92155686020 23783
    ## - product_sizesS                 1  100983256 92171450758 23783
    ## - product_colorswhite            1  111382212 92181849714 23783
    ## - rating                         1  128082181 92198549683 23783
    ## <none>                                        92070467502 23783
    ## - product_colorsgrey             1  155065177 92225532680 23784
    ## - product_colorsOther_colors     1  163079701 92233547203 23784
    ## - product_sizesOther_sizes       1  185071721 92255539223 23784
    ## - product_colorsblack            1  185340865 92255808368 23784
    ## - product_variation_inventory    1  191311079 92261778582 23784
    ## - merchant_rating                1  201352154 92271819656 23784
    ## - discount_per                   1  236337221 92306804723 23785
    ## - product_colorspurple           1  300825564 92371293066 23786
    ## - price                          1  381955672 92452423174 23787
    ## - product_sizesXXS               1  429928385 92500395888 23787
    ## - countries_shipped_to           1  531178254 92601645756 23789
    ## - product_sizesXXL               1  537046237 92607513739 23789
    ## - product_sizesXS                1  564669568 92635137070 23789
    ## - `price_classEUR10-20`          1  785761879 92856229382 23792
    ## - merchant_has_profile_picture1  1  879678708 92950146210 23794
    ## - shipping_option_price          1  881706619 92952174121 23794
    ## - merchant_rating_count          1 2681102184 94751569686 23819
    ## 
    ## Step:  AIC=23781.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1    2640393 92074939807 23779
    ## - uses_ad_boosts1                1    2646214 92074945628 23779
    ## - badge_local_product1           1    4805123 92077104537 23779
    ## - product_colorsyellow           1    5655185 92077954599 23779
    ## - product_colorspink             1    6665106 92078964520 23779
    ## - product_sizesM                 1    8145050 92080444465 23779
    ## - badges_count                   1   12113391 92084412805 23780
    ## - product_colorsred              1   20580504 92092879918 23780
    ## - badge_product_quality1         1   24702866 92097002281 23780
    ## - origin_countryVE               1   24935900 92097235314 23780
    ## - `price_classEUR40-50`          1   39626027 92111925441 23780
    ## - product_colorsgreen            1   47595853 92119895268 23780
    ## - tags_count                     1   52212580 92124511994 23780
    ## - origin_countryUS               1   72727995 92145027409 23780
    ## - `price_classEUR20-30`          1   77057159 92149356573 23780
    ## - origin_countrySG               1   78993225 92151292639 23780
    ## - product_colorsblue             1   81252059 92153551473 23780
    ## - retail_price                   1   84451669 92156751084 23781
    ## - product_sizesS                 1  100201631 92172501046 23781
    ## - product_colorswhite            1  111509960 92183809374 23781
    ## - rating                         1  126859083 92199158497 23781
    ## <none>                                        92072299414 23781
    ## - product_colorsgrey             1  155098666 92227398080 23782
    ## - product_colorsOther_colors     1  163998742 92236298157 23782
    ## - product_sizesOther_sizes       1  184369446 92256668860 23782
    ## - product_colorsblack            1  185357132 92257656546 23782
    ## - product_variation_inventory    1  190410416 92262709830 23782
    ## - merchant_rating                1  203809365 92276108779 23782
    ## - discount_per                   1  235512313 92307811727 23783
    ## - product_colorspurple           1  301130557 92373429972 23784
    ## - price                          1  386179733 92458479147 23785
    ## - product_sizesXXS               1  428550020 92500849434 23785
    ## - countries_shipped_to           1  530206815 92602506229 23787
    ## - product_sizesXXL               1  537333419 92609632834 23787
    ## - product_sizesXS                1  562956475 92635255889 23787
    ## - `price_classEUR10-20`          1  810287903 92882587318 23791
    ## - merchant_has_profile_picture1  1  878569375 92950868789 23792
    ## - shipping_option_price          1  882275350 92954574764 23792
    ## - merchant_rating_count          1 2686908784 94759208198 23817
    ## 
    ## Step:  AIC=23779.33
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1    2395573 92077335380 23777
    ## - badge_local_product1           1    5071210 92080011017 23777
    ## - product_colorsyellow           1    6099989 92081039796 23777
    ## - product_colorspink             1    7051575 92081991382 23777
    ## - product_sizesM                 1    8255805 92083195612 23777
    ## - badges_count                   1   12558995 92087498802 23778
    ## - product_colorsred              1   21855651 92096795458 23778
    ## - origin_countryVE               1   24953724 92099893531 23778
    ## - badge_product_quality1         1   25057716 92099997523 23778
    ## - `price_classEUR40-50`          1   40025803 92114965610 23778
    ## - product_colorsgreen            1   49020021 92123959828 23778
    ## - tags_count                     1   52365149 92127304956 23778
    ## - origin_countryUS               1   71962409 92146902216 23778
    ## - `price_classEUR20-30`          1   77630041 92152569848 23778
    ## - origin_countrySG               1   79058741 92153998548 23779
    ## - product_colorsblue             1   84515156 92159454963 23779
    ## - retail_price                   1   86208609 92161148416 23779
    ## - product_sizesS                 1   99672446 92174612253 23779
    ## - product_colorswhite            1  115220165 92190159972 23779
    ## - rating                         1  126455452 92201395259 23779
    ## <none>                                        92074939807 23779
    ## - product_colorsgrey             1  159772979 92234712786 23780
    ## - product_colorsOther_colors     1  167623250 92242563057 23780
    ## - product_sizesOther_sizes       1  184562619 92259502426 23780
    ## - product_colorsblack            1  189928396 92264868203 23780
    ## - product_variation_inventory    1  190729852 92265669659 23780
    ## - merchant_rating                1  202867153 92277806960 23780
    ## - discount_per                   1  237036007 92311975814 23781
    ## - product_colorspurple           1  308308941 92383248748 23782
    ## - price                          1  386976859 92461916667 23783
    ## - product_sizesXXS               1  426619318 92501559125 23783
    ## - countries_shipped_to           1  529054602 92603994409 23785
    ## - product_sizesXXL               1  537666288 92612606095 23785
    ## - product_sizesXS                1  562431764 92637371571 23785
    ## - `price_classEUR10-20`          1  809474717 92884414524 23789
    ## - merchant_has_profile_picture1  1  884262137 92959201944 23790
    ## - shipping_option_price          1  891562788 92966502595 23790
    ## - merchant_rating_count          1 2687017725 94761957532 23815
    ## 
    ## Step:  AIC=23777.36
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1    4721510 92082056889 23775
    ## - product_colorsyellow           1    5926201 92083261581 23775
    ## - product_colorspink             1    6816583 92084151962 23776
    ## - product_sizesM                 1    8340058 92085675438 23776
    ## - badges_count                   1   12365473 92089700853 23776
    ## - product_colorsred              1   21453111 92098788490 23776
    ## - badge_product_quality1         1   24846732 92102182111 23776
    ## - origin_countryVE               1   26174300 92103509680 23776
    ## - `price_classEUR40-50`          1   40387687 92117723067 23776
    ## - product_colorsgreen            1   48261692 92125597072 23776
    ## - tags_count                     1   52782439 92130117819 23776
    ## - origin_countryUS               1   72513430 92149848810 23776
    ## - origin_countrySG               1   78098052 92155433432 23777
    ## - `price_classEUR20-30`          1   80513908 92157849287 23777
    ## - product_colorsblue             1   83941777 92161277157 23777
    ## - retail_price                   1   86000785 92163336164 23777
    ## - product_sizesS                 1   98720100 92176055480 23777
    ## - product_colorswhite            1  114517422 92191852802 23777
    ## - rating                         1  127771168 92205106547 23777
    ## <none>                                        92077335380 23777
    ## - product_colorsgrey             1  158894521 92236229900 23778
    ## - product_colorsOther_colors     1  166318686 92243654066 23778
    ## - product_sizesOther_sizes       1  186534664 92263870043 23778
    ## - product_colorsblack            1  188433588 92265768968 23778
    ## - product_variation_inventory    1  196978287 92274313667 23778
    ## - merchant_rating                1  203663236 92280998615 23778
    ## - discount_per                   1  236720016 92314055395 23779
    ## - product_colorspurple           1  308124288 92385459667 23780
    ## - price                          1  393134862 92470470242 23781
    ## - product_sizesXXS               1  428513704 92505849084 23782
    ## - countries_shipped_to           1  527170337 92604505717 23783
    ## - product_sizesXXL               1  536193941 92613529320 23783
    ## - product_sizesXS                1  560478982 92637814361 23783
    ## - `price_classEUR10-20`          1  816108489 92893443869 23787
    ## - merchant_has_profile_picture1  1  882113892 92959449272 23788
    ## - shipping_option_price          1  895644233 92972979612 23788
    ## - merchant_rating_count          1 2685463220 94762798600 23813
    ## 
    ## Step:  AIC=23775.43
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1    5674036 92087730926 23774
    ## - product_colorspink             1    6518452 92088575341 23774
    ## - product_sizesM                 1    7813221 92089870111 23774
    ## - badges_count                   1   10459448 92092516337 23774
    ## - product_colorsred              1   21390336 92103447226 23774
    ## - origin_countryVE               1   26171787 92108228677 23774
    ## - badge_product_quality1         1   28736964 92110793854 23774
    ## - `price_classEUR40-50`          1   42352994 92124409883 23774
    ## - product_colorsgreen            1   46827470 92128884359 23774
    ## - tags_count                     1   53170768 92135227658 23774
    ## - origin_countryUS               1   74101567 92156158457 23775
    ## - origin_countrySG               1   77912877 92159969767 23775
    ## - `price_classEUR20-30`          1   79347360 92161404250 23775
    ## - product_colorsblue             1   82833199 92164890089 23775
    ## - retail_price                   1   86871991 92168928881 23775
    ## - product_sizesS                 1  100174374 92182231263 23775
    ## - product_colorswhite            1  113349235 92195406124 23775
    ## - rating                         1  128710579 92210767469 23775
    ## <none>                                        92082056889 23775
    ## - product_colorsgrey             1  157077468 92239134358 23776
    ## - product_colorsOther_colors     1  164276343 92246333233 23776
    ## - product_colorsblack            1  186118135 92268175025 23776
    ## - product_sizesOther_sizes       1  193253922 92275310811 23776
    ## - merchant_rating                1  205496383 92287553272 23776
    ## - product_variation_inventory    1  206718812 92288775702 23776
    ## - discount_per                   1  239800667 92321857556 23777
    ## - product_colorspurple           1  305923016 92387979905 23778
    ## - price                          1  391542491 92473599380 23779
    ## - product_sizesXXS               1  431456355 92513513244 23780
    ## - countries_shipped_to           1  527889786 92609946675 23781
    ## - product_sizesXXL               1  539562774 92621619664 23781
    ## - product_sizesXS                1  560636051 92642692941 23781
    ## - `price_classEUR10-20`          1  812082677 92894139566 23785
    ## - merchant_has_profile_picture1  1  877792746 92959849635 23786
    ## - shipping_option_price          1  900712210 92982769099 23786
    ## - merchant_rating_count          1 2684255640 94766312529 23811
    ## 
    ## Step:  AIC=23773.51
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1    1348373 92089079299 23772
    ## - product_sizesM                 1    7009928 92094740854 23772
    ## - badges_count                   1   10519310 92098250236 23772
    ## - product_colorsred              1   18202063 92105932988 23772
    ## - origin_countryVE               1   26372378 92114103304 23772
    ## - badge_product_quality1         1   28904440 92116635365 23772
    ## - `price_classEUR40-50`          1   42514484 92130245410 23772
    ## - tags_count                     1   51928910 92139659836 23772
    ## - product_colorsgreen            1   54713116 92142444042 23772
    ## - origin_countryUS               1   72102771 92159833697 23773
    ## - origin_countrySG               1   77461033 92165191959 23773
    ## - `price_classEUR20-30`          1   79326267 92167057192 23773
    ## - retail_price                   1   85041673 92172772599 23773
    ## - product_sizesS                 1  103257191 92190988117 23773
    ## - product_colorsblue             1  115518768 92203249694 23773
    ## - rating                         1  129800918 92217531844 23773
    ## <none>                                        92087730926 23774
    ## - product_sizesOther_sizes       1  200969397 92288700323 23774
    ## - product_variation_inventory    1  203112951 92290843876 23774
    ## - merchant_rating                1  203273709 92291004635 23774
    ## - product_colorswhite            1  212903077 92300634003 23775
    ## - discount_per                   1  239100210 92326831136 23775
    ## - product_colorsgrey             1  243857798 92331588724 23775
    ## - product_colorsOther_colors     1  353218827 92440949753 23777
    ## - product_colorsblack            1  388818737 92476549663 23777
    ## - price                          1  392562389 92480293315 23777
    ## - product_sizesXXS               1  444135120 92531866046 23778
    ## - product_colorspurple           1  451855639 92539586565 23778
    ## - countries_shipped_to           1  540649694 92628380620 23779
    ## - product_sizesXXL               1  543707368 92631438294 23779
    ## - product_sizesXS                1  563794186 92651525112 23780
    ## - `price_classEUR10-20`          1  811238038 92898968964 23783
    ## - merchant_has_profile_picture1  1  882160023 92969890949 23784
    ## - shipping_option_price          1  898917612 92986648538 23784
    ## - merchant_rating_count          1 2694086577 94781817502 23809
    ## 
    ## Step:  AIC=23771.53
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1    7155153 92096234452 23770
    ## - badges_count                   1   10489101 92099568400 23770
    ## - product_colorsred              1   17406627 92106485926 23770
    ## - origin_countryVE               1   26335603 92115414901 23770
    ## - badge_product_quality1         1   29288948 92118368247 23770
    ## - `price_classEUR40-50`          1   42588166 92131667465 23770
    ## - tags_count                     1   51656838 92140736137 23770
    ## - product_colorsgreen            1   57374717 92146454016 23770
    ## - origin_countryUS               1   72625213 92161704511 23771
    ## - origin_countrySG               1   77521627 92166600926 23771
    ## - `price_classEUR20-30`          1   79338346 92168417644 23771
    ## - retail_price                   1   84604466 92173683765 23771
    ## - product_sizesS                 1  102378045 92191457344 23771
    ## - product_colorsblue             1  128459377 92217538676 23771
    ## - rating                         1  130048401 92219127700 23771
    ## <none>                                        92089079299 23772
    ## - product_sizesOther_sizes       1  200875063 92289954362 23772
    ## - product_variation_inventory    1  202639234 92291718533 23772
    ## - merchant_rating                1  202686512 92291765811 23772
    ## - discount_per                   1  238683647 92327762946 23773
    ## - product_colorswhite            1  272460453 92361539751 23773
    ## - product_colorsgrey             1  276512066 92365591365 23774
    ## - price                          1  392382877 92481462176 23775
    ## - product_sizesXXS               1  443462709 92532542007 23776
    ## - product_colorsOther_colors     1  473024889 92562104188 23776
    ## - product_colorspurple           1  493540027 92582619326 23777
    ## - product_colorsblack            1  513653120 92602732419 23777
    ## - countries_shipped_to           1  539438209 92628517508 23777
    ## - product_sizesXXL               1  543147358 92632226657 23777
    ## - product_sizesXS                1  562955814 92652035113 23778
    ## - `price_classEUR10-20`          1  812028268 92901107567 23781
    ## - merchant_has_profile_picture1  1  881136423 92970215722 23782
    ## - shipping_option_price          1  898055326 92987134625 23782
    ## - merchant_rating_count          1 2694874796 94783954095 23807
    ## 
    ## Step:  AIC=23769.63
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1   11525751 92107760203 23768
    ## - product_colorsred              1   16500693 92112735145 23768
    ## - origin_countryVE               1   26148848 92122383300 23768
    ## - badge_product_quality1         1   30735908 92126970360 23768
    ## - `price_classEUR40-50`          1   42262985 92138497437 23768
    ## - tags_count                     1   50597904 92146832356 23768
    ## - product_colorsgreen            1   55791397 92152025849 23768
    ## - origin_countryUS               1   70725018 92166959470 23769
    ## - origin_countrySG               1   77329887 92173564339 23769
    ## - `price_classEUR20-30`          1   78189498 92174423950 23769
    ## - retail_price                   1   84694415 92180928867 23769
    ## - rating                         1  130010583 92226245035 23770
    ## - product_colorsblue             1  132799148 92229033600 23770
    ## <none>                                        92096234452 23770
    ## - product_variation_inventory    1  202257309 92298491761 23771
    ## - merchant_rating                1  202686513 92298920966 23771
    ## - discount_per                   1  238750464 92334984917 23771
    ## - product_colorswhite            1  277885376 92374119828 23772
    ## - product_colorsgrey             1  280385763 92376620216 23772
    ## - price                          1  396051803 92492286255 23773
    ## - product_colorsOther_colors     1  472647956 92568882408 23774
    ## - product_colorspurple           1  493971663 92590206116 23775
    ## - product_colorsblack            1  522713057 92618947510 23775
    ## - product_sizesOther_sizes       1  539451624 92635686076 23775
    ## - countries_shipped_to           1  545349591 92641584043 23775
    ## - product_sizesS                 1  582437399 92678671851 23776
    ## - product_sizesXXL               1  773459777 92869694229 23779
    ## - `price_classEUR10-20`          1  821683906 92917918358 23779
    ## - merchant_has_profile_picture1  1  887553202 92983787655 23780
    ## - shipping_option_price          1  905643275 93001877728 23781
    ## - product_sizesXXS               1 1017379928 93113614380 23782
    ## - product_sizesXS                1 1910310152 94006544604 23795
    ## - merchant_rating_count          1 2695311223 94791545675 23806
    ## 
    ## Step:  AIC=23767.8
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1   16322169 92124082372 23766
    ## - badge_product_quality1         1   21733315 92129493519 23766
    ## - origin_countryVE               1   25929166 92133689370 23766
    ## - `price_classEUR40-50`          1   44090536 92151850739 23766
    ## - tags_count                     1   48499114 92156259317 23767
    ## - product_colorsgreen            1   54858069 92162618272 23767
    ## - origin_countryUS               1   70394904 92178155107 23767
    ## - `price_classEUR20-30`          1   77200548 92184960751 23767
    ## - origin_countrySG               1   77354791 92185114994 23767
    ## - retail_price                   1   85460935 92193221138 23767
    ## - rating                         1  134370222 92242130425 23768
    ## - product_colorsblue             1  135351913 92243112116 23768
    ## <none>                                        92107760203 23768
    ## - merchant_rating                1  193611704 92301371907 23769
    ## - product_variation_inventory    1  214891462 92322651665 23769
    ## - discount_per                   1  241729920 92349490123 23769
    ## - product_colorswhite            1  276395486 92384155690 23770
    ## - product_colorsgrey             1  279039603 92386799807 23770
    ## - price                          1  400337698 92508097901 23772
    ## - product_colorsOther_colors     1  469986534 92577746737 23773
    ## - product_colorspurple           1  495246693 92603006896 23773
    ## - product_colorsblack            1  520839415 92628599618 23773
    ## - countries_shipped_to           1  548898961 92656659164 23774
    ## - product_sizesOther_sizes       1  558869040 92666629244 23774
    ## - product_sizesS                 1  582140956 92689901159 23774
    ## - product_sizesXXL               1  790471905 92898232108 23777
    ## - `price_classEUR10-20`          1  829976301 92937736505 23778
    ## - merchant_has_profile_picture1  1  889545197 92997305400 23778
    ## - shipping_option_price          1  924432436 93032192639 23779
    ## - product_sizesXXS               1 1016577664 93124337868 23780
    ## - product_sizesXS                1 1909826418 94017586622 23793
    ## - merchant_rating_count          1 2718701277 94826461481 23804
    ## 
    ## Step:  AIC=23766.03
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1   21078978 92145161350 23764
    ## - origin_countryVE               1   25642849 92149725221 23764
    ## - `price_classEUR40-50`          1   39814961 92163897333 23765
    ## - product_colorsgreen            1   43057351 92167139724 23765
    ## - tags_count                     1   51707054 92175789426 23765
    ## - origin_countryUS               1   71954223 92196036595 23765
    ## - `price_classEUR20-30`          1   76609422 92200691794 23765
    ## - origin_countrySG               1   77064259 92201146631 23765
    ## - retail_price                   1   86523656 92210606028 23765
    ## - product_colorsblue             1  119711182 92243793554 23766
    ## - rating                         1  137944039 92262026411 23766
    ## <none>                                        92124082372 23766
    ## - merchant_rating                1  199219949 92323302321 23767
    ## - product_variation_inventory    1  212974134 92337056506 23767
    ## - discount_per                   1  244354873 92368437245 23768
    ## - product_colorsgrey             1  263096660 92387179032 23768
    ## - product_colorswhite            1  266357516 92390439888 23768
    ## - price                          1  395476759 92519559131 23770
    ## - product_colorsOther_colors     1  479961105 92604043477 23771
    ## - product_colorspurple           1  480024892 92604107264 23771
    ## - product_colorsblack            1  533476659 92657559031 23772
    ## - countries_shipped_to           1  559153511 92683235883 23772
    ## - product_sizesOther_sizes       1  561926725 92686009097 23772
    ## - product_sizesS                 1  587368016 92711450388 23772
    ## - product_sizesXXL               1  789887276 92913969648 23775
    ## - `price_classEUR10-20`          1  824461856 92948544229 23776
    ## - merchant_has_profile_picture1  1  880484534 93004566906 23777
    ## - shipping_option_price          1  918660929 93042743301 23777
    ## - product_sizesXXS               1 1032675633 93156758006 23779
    ## - product_sizesXS                1 1915074323 94039156695 23791
    ## - merchant_rating_count          1 2719018297 94843100669 23802
    ## 
    ## Step:  AIC=23764.33
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   24166241 92169327591 23763
    ## - `price_classEUR40-50`          1   40078543 92185239893 23763
    ## - product_colorsgreen            1   45352791 92190514141 23763
    ## - tags_count                     1   50167219 92195328569 23763
    ## - origin_countryUS               1   69351580 92214512930 23763
    ## - origin_countrySG               1   75669854 92220831204 23763
    ## - `price_classEUR20-30`          1   77336816 92222498166 23763
    ## - retail_price                   1   87225764 92232387113 23764
    ## - product_colorsblue             1  120572259 92265733609 23764
    ## <none>                                        92145161350 23764
    ## - rating                         1  180038029 92325199379 23765
    ## - merchant_rating                1  215844738 92361006088 23765
    ## - product_variation_inventory    1  221434641 92366595991 23766
    ## - discount_per                   1  246744589 92391905939 23766
    ## - product_colorswhite            1  265981042 92411142392 23766
    ## - product_colorsgrey             1  268530253 92413691603 23766
    ## - price                          1  393138818 92538300168 23768
    ## - product_colorspurple           1  471098334 92616259683 23769
    ## - product_colorsOther_colors     1  481803788 92626965138 23769
    ## - product_colorsblack            1  542305989 92687467338 23770
    ## - countries_shipped_to           1  566554660 92711716009 23770
    ## - product_sizesOther_sizes       1  567228952 92712390302 23770
    ## - product_sizesS                 1  585807218 92730968568 23771
    ## - product_sizesXXL               1  785404801 92930566151 23774
    ## - `price_classEUR10-20`          1  820300127 92965461477 23774
    ## - merchant_has_profile_picture1  1  887602870 93032764219 23775
    ## - shipping_option_price          1  917859719 93063021069 23775
    ## - product_sizesXXS               1 1037600220 93182761570 23777
    ## - product_sizesXS                1 1906862810 94052024160 23789
    ## - merchant_rating_count          1 2773290241 94918451590 23801
    ## 
    ## Step:  AIC=23762.67
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   39604527 92208932118 23761
    ## - product_colorsgreen            1   45100759 92214428350 23761
    ## - tags_count                     1   49535567 92218863158 23761
    ## - origin_countryUS               1   68224565 92237552156 23762
    ## - `price_classEUR20-30`          1   75146810 92244474401 23762
    ## - origin_countrySG               1   75906353 92245233944 23762
    ## - retail_price                   1   87682406 92257009997 23762
    ## - product_colorsblue             1  120661533 92289989124 23762
    ## <none>                                        92169327591 23763
    ## - rating                         1  201487501 92370815092 23764
    ## - merchant_rating                1  222936764 92392264355 23764
    ## - product_variation_inventory    1  224794259 92394121850 23764
    ## - discount_per                   1  251480857 92420808448 23764
    ## - product_colorswhite            1  254493578 92423821169 23764
    ## - product_colorsgrey             1  266628490 92435956081 23765
    ## - price                          1  382330873 92551658464 23766
    ## - product_colorspurple           1  467163824 92636491415 23767
    ## - product_colorsOther_colors     1  477786674 92647114265 23768
    ## - product_colorsblack            1  529351218 92698678809 23768
    ## - product_sizesOther_sizes       1  563221144 92732548735 23769
    ## - countries_shipped_to           1  566462530 92735790121 23769
    ## - product_sizesS                 1  579561401 92748888992 23769
    ## - product_sizesXXL               1  778231347 92947558938 23772
    ## - `price_classEUR10-20`          1  812119752 92981447343 23772
    ## - merchant_has_profile_picture1  1  890178946 93059506537 23773
    ## - shipping_option_price          1  902157253 93071484844 23774
    ## - product_sizesXXS               1 1028501889 93197829480 23775
    ## - product_sizesXS                1 1915709181 94085036772 23788
    ## - merchant_rating_count          1 2772983852 94942311443 23800
    ## 
    ## Step:  AIC=23761.24
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   42861181 92251793298 23760
    ## - tags_count                     1   51659638 92260591756 23760
    ## - `price_classEUR20-30`          1   52701130 92261633248 23760
    ## - origin_countryUS               1   65679360 92274611478 23760
    ## - origin_countrySG               1   73422412 92282354529 23760
    ## - retail_price                   1   93878374 92302810491 23761
    ## - product_colorsblue             1  128630130 92337562248 23761
    ## <none>                                        92208932118 23761
    ## - rating                         1  203577057 92412509174 23762
    ## - product_variation_inventory    1  232790376 92441722493 23763
    ## - merchant_rating                1  236102346 92445034463 23763
    ## - product_colorswhite            1  258950335 92467882453 23763
    ## - discount_per                   1  259171670 92468103788 23763
    ## - product_colorsgrey             1  272535960 92481468077 23763
    ## - price                          1  343364600 92552296718 23764
    ## - product_colorspurple           1  469329652 92678261770 23766
    ## - product_colorsOther_colors     1  483798402 92692730520 23766
    ## - product_colorsblack            1  536457118 92745389235 23767
    ## - countries_shipped_to           1  573484468 92782416585 23767
    ## - product_sizesS                 1  597200251 92806132369 23768
    ## - product_sizesOther_sizes       1  610928025 92819860142 23768
    ## - product_sizesXXL               1  777358027 92986290144 23770
    ## - `price_classEUR10-20`          1  805691469 93014623587 23771
    ## - shipping_option_price          1  873943068 93082875185 23772
    ## - merchant_has_profile_picture1  1  895437200 93104369317 23772
    ## - product_sizesXXS               1 1063121791 93272053909 23774
    ## - product_sizesXS                1 1952377769 94161309887 23787
    ## - merchant_rating_count          1 2760266206 94969198323 23798
    ## 
    ## Step:  AIC=23759.85
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - tags_count                     1   49637128 92301430426 23759
    ## - `price_classEUR20-30`          1   52203568 92303996866 23759
    ## - origin_countryUS               1   65724111 92317517409 23759
    ## - origin_countrySG               1   73374644 92325167942 23759
    ## - retail_price                   1   88808430 92340601729 23759
    ## - product_colorsblue             1  104931800 92356725098 23759
    ## <none>                                        92251793298 23760
    ## - product_variation_inventory    1  210552948 92462346247 23761
    ## - rating                         1  211733662 92463526960 23761
    ## - product_colorswhite            1  220655447 92472448746 23761
    ## - merchant_rating                1  237163331 92488956629 23761
    ## - product_colorsgrey             1  241415821 92493209119 23761
    ## - discount_per                   1  248547536 92500340834 23761
    ## - price                          1  347192085 92598985383 23763
    ## - product_colorspurple           1  437018877 92688812175 23764
    ## - product_colorsOther_colors     1  440941268 92692734566 23764
    ## - product_colorsblack            1  493596994 92745390292 23765
    ## - countries_shipped_to           1  561148492 92812941790 23766
    ## - product_sizesS                 1  595317843 92847111141 23766
    ## - product_sizesOther_sizes       1  603421864 92855215162 23766
    ## - product_sizesXXL               1  776318814 93028112112 23769
    ## - `price_classEUR10-20`          1  803739012 93055532311 23769
    ## - shipping_option_price          1  874682507 93126475805 23770
    ## - merchant_has_profile_picture1  1  934270048 93186063346 23771
    ## - product_sizesXXS               1 1065188993 93316982291 23773
    ## - product_sizesXS                1 1959719796 94211513094 23785
    ## - merchant_rating_count          1 2763471344 95015264642 23797
    ## 
    ## Step:  AIC=23758.55
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR20-30`          1   53005250 92354435676 23757
    ## - origin_countryUS               1   60763852 92362194278 23757
    ## - origin_countrySG               1   73122359 92374552785 23758
    ## - retail_price                   1   86311552 92387741978 23758
    ## - product_colorsblue             1  104885785 92406316210 23758
    ## <none>                                        92301430426 23759
    ## - rating                         1  209654703 92511085129 23760
    ## - product_variation_inventory    1  221964432 92523394858 23760
    ## - discount_per                   1  228834483 92530264909 23760
    ## - product_colorswhite            1  229357414 92530787840 23760
    ## - product_colorsgrey             1  253009174 92554439600 23760
    ## - merchant_rating                1  261309414 92562739840 23760
    ## - price                          1  357263333 92658693759 23762
    ## - product_colorspurple           1  428428720 92729859146 23763
    ## - product_colorsOther_colors     1  429469488 92730899914 23763
    ## - product_colorsblack            1  512165265 92813595691 23764
    ## - countries_shipped_to           1  547202835 92848633261 23764
    ## - product_sizesOther_sizes       1  566395637 92867826063 23765
    ## - product_sizesS                 1  581223410 92882653836 23765
    ## - product_sizesXXL               1  755881444 93057311870 23767
    ## - `price_classEUR10-20`          1  803221048 93104651474 23768
    ## - shipping_option_price          1  859810665 93161241091 23769
    ## - merchant_has_profile_picture1  1  921772308 93223202734 23770
    ## - product_sizesXXS               1 1030191411 93331621837 23771
    ## - product_sizesXS                1 1913897439 94215327865 23784
    ## - merchant_rating_count          1 2722427691 95023858117 23795
    ## 
    ## Step:  AIC=23757.3
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryUS               1   57436800 92411872475 23756
    ## - origin_countrySG               1   71247466 92425683141 23756
    ## - retail_price                   1   73493950 92427929625 23756
    ## - product_colorsblue             1  107351383 92461787058 23757
    ## <none>                                        92354435676 23757
    ## - rating                         1  223327006 92577762682 23759
    ## - product_colorswhite            1  223456147 92577891822 23759
    ## - discount_per                   1  223625664 92578061339 23759
    ## - product_variation_inventory    1  229988214 92584423890 23759
    ## - product_colorsgrey             1  250641006 92605076682 23759
    ## - merchant_rating                1  260664395 92615100071 23759
    ## - price                          1  304396216 92658831891 23760
    ## - product_colorsOther_colors     1  411645789 92766081465 23761
    ## - product_colorspurple           1  423832404 92778268079 23761
    ## - product_colorsblack            1  498456041 92852891717 23762
    ## - countries_shipped_to           1  554274509 92908710184 23763
    ## - product_sizesOther_sizes       1  561200478 92915636154 23763
    ## - product_sizesS                 1  575173657 92929609333 23763
    ## - product_sizesXXL               1  748388197 93102823873 23766
    ## - `price_classEUR10-20`          1  775664513 93130100189 23766
    ## - shipping_option_price          1  813025552 93167461228 23767
    ## - merchant_has_profile_picture1  1  925739767 93280175443 23768
    ## - product_sizesXXS               1 1046100368 93400536044 23770
    ## - product_sizesXS                1 1926064875 94280500551 23782
    ## - merchant_rating_count          1 2730406562 95084842237 23794
    ## 
    ## Step:  AIC=23756.12
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countrySG               1   71539797 92483412272 23755
    ## - retail_price                   1   77955773 92489828248 23755
    ## - product_colorsblue             1  113612660 92525485135 23756
    ## <none>                                        92411872475 23756
    ## - rating                         1  224862864 92636735340 23757
    ## - product_colorswhite            1  228189295 92640061770 23757
    ## - product_variation_inventory    1  228787566 92640660041 23757
    ## - discount_per                   1  239482254 92651354729 23758
    ## - product_colorsgrey             1  244510206 92656382681 23758
    ## - merchant_rating                1  266111390 92677983865 23758
    ## - price                          1  296940538 92708813013 23758
    ## - product_colorsOther_colors     1  394376270 92806248746 23760
    ## - product_colorspurple           1  426119752 92837992228 23760
    ## - product_colorsblack            1  491078293 92902950769 23761
    ## - product_sizesOther_sizes       1  559503506 92971375981 23762
    ## - product_sizesS                 1  575939945 92987812421 23762
    ## - countries_shipped_to           1  591798941 93003671416 23763
    ## - product_sizesXXL               1  740084346 93151956822 23765
    ## - `price_classEUR10-20`          1  784736967 93196609442 23765
    ## - shipping_option_price          1  806458065 93218330541 23766
    ## - merchant_has_profile_picture1  1  946617203 93358489678 23768
    ## - product_sizesXXS               1 1080198389 93492070865 23769
    ## - product_sizesXS                1 1922262173 94334134648 23781
    ## - merchant_rating_count          1 2764412339 95176284815 23793
    ## 
    ## Step:  AIC=23755.14
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1   80165569 92563577842 23754
    ## - product_colorsblue             1  114884417 92598296690 23755
    ## <none>                                        92483412272 23755
    ## - product_variation_inventory    1  225534240 92708946512 23756
    ## - product_colorswhite            1  230099998 92713512270 23756
    ## - rating                         1  230426359 92713838632 23756
    ## - discount_per                   1  245440518 92728852790 23757
    ## - merchant_rating                1  262948296 92746360569 23757
    ## - product_colorsgrey             1  265481806 92748894079 23757
    ## - price                          1  292820793 92776233065 23757
    ## - product_colorsOther_colors     1  407617392 92891029664 23759
    ## - product_colorspurple           1  425291485 92908703758 23759
    ## - product_colorsblack            1  495323383 92978735656 23760
    ## - product_sizesOther_sizes       1  561479939 93044892212 23761
    ## - product_sizesS                 1  579451644 93062863916 23761
    ## - countries_shipped_to           1  603911082 93087323354 23762
    ## - product_sizesXXL               1  740139503 93223551775 23764
    ## - `price_classEUR10-20`          1  778587472 93261999744 23764
    ## - shipping_option_price          1  811582294 93294994566 23765
    ## - merchant_has_profile_picture1  1  942334973 93425747246 23766
    ## - product_sizesXXS               1 1089569560 93572981833 23769
    ## - product_sizesXS                1 1904712193 94388124465 23780
    ## - merchant_rating_count          1 2755230056 95238642328 23792
    ## 
    ## Step:  AIC=23754.27
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsblue             1  120387043 92683964885 23754
    ## <none>                                        92563577842 23754
    ## - discount_per                   1  189828883 92753406725 23755
    ## - product_variation_inventory    1  215954021 92779531863 23755
    ## - rating                         1  223699110 92787276952 23755
    ## - product_colorswhite            1  237575589 92801153431 23756
    ## - product_colorsgrey             1  258423254 92822001096 23756
    ## - merchant_rating                1  272896889 92836474731 23756
    ## - price                          1  348245845 92911823687 23757
    ## - product_colorsOther_colors     1  414879829 92978457671 23758
    ## - product_colorspurple           1  431056221 92994634063 23758
    ## - product_colorsblack            1  528119726 93091697567 23760
    ## - product_sizesS                 1  568889088 93132466930 23760
    ## - product_sizesOther_sizes       1  575461483 93139039325 23760
    ## - countries_shipped_to           1  617717258 93181295100 23761
    ## - product_sizesXXL               1  767852411 93331430253 23763
    ## - `price_classEUR10-20`          1  769726681 93333304523 23763
    ## - shipping_option_price          1  790199200 93353777041 23763
    ## - merchant_has_profile_picture1  1  962841423 93526419265 23766
    ## - product_sizesXXS               1 1123412031 93686989873 23768
    ## - product_sizesXS                1 1895700728 94459278569 23779
    ## - merchant_rating_count          1 2851833940 95415411781 23792
    ## 
    ## Step:  AIC=23753.98
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## <none>                                        92683964885 23754
    ## - product_colorswhite            1  169194675 92853159560 23754
    ## - discount_per                   1  184985498 92868950383 23755
    ## - product_colorsgrey             1  207686405 92891651290 23755
    ## - rating                         1  218682911 92902647796 23755
    ## - product_variation_inventory    1  242798670 92926763555 23755
    ## - merchant_rating                1  262772484 92946737369 23756
    ## - product_colorsOther_colors     1  324700406 93008665291 23757
    ## - price                          1  343931910 93027896795 23757
    ## - product_colorspurple           1  379545893 93063510778 23757
    ## - product_colorsblack            1  427504892 93111469777 23758
    ## - product_sizesOther_sizes       1  548499667 93232464552 23760
    ## - countries_shipped_to           1  556129185 93240094070 23760
    ## - product_sizesS                 1  575188985 93259153870 23760
    ## - product_sizesXXL               1  746154668 93430119553 23763
    ## - `price_classEUR10-20`          1  767986685 93451951570 23763
    ## - shipping_option_price          1  773946068 93457910953 23763
    ## - merchant_has_profile_picture1  1  937699679 93621664564 23765
    ## - product_sizesXXS               1 1106326584 93790291469 23768
    ## - product_sizesXS                1 1867715959 94551680844 23778
    ## - merchant_rating_count          1 2840240132 95524205017 23792
    ## Start:  AIC=23707.09
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23707.09
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=23707.09
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_sizesM                 1          8 87877341620 23705
    ## - origin_countryGB               1       7508 87877349120 23705
    ## - shipping_nameOther_shipping    1    1967826 87879309438 23705
    ## - product_colorsyellow           1    3029539 87880371150 23705
    ## - uses_ad_boosts1                1    3229948 87880571560 23705
    ## - shipping_is_express1           1    5063311 87882404922 23705
    ## - product_colorspink             1    9059917 87886401529 23705
    ## - product_colorsred              1   11173977 87888515589 23705
    ## - `price_classEUR40-50`          1   17949941 87895291553 23705
    ## - origin_countryVE               1   18120524 87895462136 23705
    ## - badges_count                   1   19849577 87897191189 23705
    ## - badge_local_product1           1   22181002 87899522614 23705
    ## - product_colorsgreen            1   29584107 87906925719 23706
    ## - tags_count                     1   35664797 87913006409 23706
    ## - badge_product_quality1         1   45922104 87923263716 23706
    ## - product_colorsblue             1   52938761 87930280373 23706
    ## - retail_price                   1   61761146 87939102758 23706
    ## - `price_classEUR20-30`          1   70098182 87947439794 23706
    ## - origin_countrySG               1   81003641 87958345253 23706
    ## - discount_per                   1   85997792 87963339404 23706
    ## - rating                         1   88620869 87965962481 23706
    ## - origin_countryUS               1  104061846 87981403458 23707
    ## - product_colorsOther_colors     1  122190029 87999531641 23707
    ## - product_colorswhite            1  125415029 88002756641 23707
    ## <none>                                        87877341612 23707
    ## - product_colorsgrey             1  148214472 88025556084 23707
    ## - merchant_rating                1  158105657 88035447269 23707
    ## - product_colorsblack            1  170275321 88047616933 23708
    ## - product_sizesS                 1  190113988 88067455600 23708
    ## - product_sizesOther_sizes       1  269599974 88146941586 23709
    ## - price                          1  274755473 88152097085 23709
    ## - product_variation_inventory    1  292453780 88169795392 23709
    ## - product_colorspurple           1  311691323 88189032935 23710
    ## - product_sizesXXS               1  393032572 88270374184 23711
    ## - `price_classEUR10-20`          1  549082131 88426423743 23713
    ## - product_sizesXS                1  573185058 88450526669 23714
    ## - shipping_option_price          1  591660428 88469002040 23714
    ## - countries_shipped_to           1  617467836 88494809448 23714
    ## - product_sizesXXL               1  622388423 88499730035 23714
    ## - merchant_has_profile_picture1  1 1077485141 88954826753 23721
    ## - merchant_rating_count          1 2662499651 90539841263 23744
    ## 
    ## Step:  AIC=23705.09
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryGB               1       7511 87877349131 23703
    ## - shipping_nameOther_shipping    1    1968817 87879310437 23703
    ## - product_colorsyellow           1    3044780 87880386401 23703
    ## - uses_ad_boosts1                1    3230117 87880571737 23703
    ## - shipping_is_express1           1    5064512 87882406133 23703
    ## - product_colorspink             1    9074025 87886415646 23703
    ## - product_colorsred              1   11233661 87888575282 23703
    ## - `price_classEUR40-50`          1   17950632 87895292252 23703
    ## - origin_countryVE               1   18121034 87895462655 23703
    ## - badges_count                   1   19850618 87897192238 23703
    ## - badge_local_product1           1   22230957 87899572577 23703
    ## - product_colorsgreen            1   29716309 87907057929 23704
    ## - tags_count                     1   35704005 87913045625 23704
    ## - badge_product_quality1         1   45922115 87923263736 23704
    ## - product_colorsblue             1   52948011 87930289632 23704
    ## - retail_price                   1   61761465 87939103086 23704
    ## - `price_classEUR20-30`          1   70133594 87947475215 23704
    ## - origin_countrySG               1   81003943 87958345564 23704
    ## - discount_per                   1   85998789 87963340409 23704
    ## - rating                         1   88642264 87965983884 23704
    ## - origin_countryUS               1  104346715 87981688336 23705
    ## - product_colorsOther_colors     1  122642644 87999984264 23705
    ## - product_colorswhite            1  125522133 88002863753 23705
    ## <none>                                        87877341620 23705
    ## - product_colorsgrey             1  148304873 88025646493 23705
    ## - merchant_rating                1  158117760 88035459380 23705
    ## - product_colorsblack            1  170366011 88047707631 23706
    ## - price                          1  275280323 88152621943 23707
    ## - product_variation_inventory    1  292491757 88169833377 23707
    ## - product_colorspurple           1  312223584 88189565205 23708
    ## - product_sizesOther_sizes       1  542884603 88420226223 23711
    ## - `price_classEUR10-20`          1  550567637 88427909258 23711
    ## - shipping_option_price          1  592865127 88470206747 23712
    ## - countries_shipped_to           1  618485946 88495827567 23712
    ## - product_sizesS                 1  689218448 88566560068 23713
    ## - product_sizesXXS               1  732485797 88609827418 23714
    ## - product_sizesXXL               1  788470320 88665811941 23715
    ## - merchant_has_profile_picture1  1 1078405066 88955746687 23719
    ## - product_sizesXS                1 1576305381 89453647002 23726
    ## - merchant_rating_count          1 2662596629 90539938249 23742
    ## 
    ## Step:  AIC=23703.09
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_nameOther_shipping    1    1968543 87879317674 23701
    ## - product_colorsyellow           1    3041773 87880390904 23701
    ## - uses_ad_boosts1                1    3223038 87880572169 23701
    ## - shipping_is_express1           1    5060666 87882409798 23701
    ## - product_colorspink             1    9079749 87886428880 23701
    ## - product_colorsred              1   11241285 87888590416 23701
    ## - `price_classEUR40-50`          1   17944863 87895293994 23701
    ## - origin_countryVE               1   18119193 87895468324 23701
    ## - badges_count                   1   19850203 87897199334 23701
    ## - badge_local_product1           1   22231919 87899581050 23701
    ## - product_colorsgreen            1   29732210 87907081341 23702
    ## - tags_count                     1   35700243 87913049374 23702
    ## - badge_product_quality1         1   45925051 87923274182 23702
    ## - product_colorsblue             1   53000979 87930350110 23702
    ## - retail_price                   1   61756854 87939105985 23702
    ## - `price_classEUR20-30`          1   70130138 87947479269 23702
    ## - origin_countrySG               1   81012907 87958362038 23702
    ## - discount_per                   1   85997474 87963346605 23702
    ## - rating                         1   88650510 87965999641 23702
    ## - origin_countryUS               1  104350753 87981699884 23703
    ## - product_colorsOther_colors     1  122672856 88000021987 23703
    ## - product_colorswhite            1  125542683 88002891814 23703
    ## <none>                                        87877349131 23703
    ## - product_colorsgrey             1  148343870 88025693001 23703
    ## - merchant_rating                1  158154364 88035503495 23703
    ## - product_colorsblack            1  170391894 88047741025 23704
    ## - price                          1  275275929 88152625060 23705
    ## - product_variation_inventory    1  292974499 88170323630 23706
    ## - product_colorspurple           1  312284346 88189633477 23706
    ## - product_sizesOther_sizes       1  542913722 88420262853 23709
    ## - `price_classEUR10-20`          1  550820742 88428169873 23709
    ## - shipping_option_price          1  593056688 88470405819 23710
    ## - countries_shipped_to           1  618515024 88495864155 23710
    ## - product_sizesS                 1  689248035 88566597166 23711
    ## - product_sizesXXS               1  732534751 88609883882 23712
    ## - product_sizesXXL               1  788463573 88665812704 23713
    ## - merchant_has_profile_picture1  1 1078498982 88955848113 23717
    ## - product_sizesXS                1 1577530497 89454879628 23724
    ## - merchant_rating_count          1 2662589275 90539938406 23740
    ## 
    ## Step:  AIC=23701.12
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsyellow           1    2776291 87882093965 23699
    ## - uses_ad_boosts1                1    3409015 87882726689 23699
    ## - shipping_is_express1           1    5780921 87885098595 23699
    ## - product_colorspink             1    9622007 87888939681 23699
    ## - product_colorsred              1   12078618 87891396292 23699
    ## - `price_classEUR40-50`          1   17644599 87896962273 23699
    ## - origin_countryVE               1   18048339 87897366014 23699
    ## - badges_count                   1   19798738 87899116412 23699
    ## - badge_local_product1           1   22388811 87901706485 23700
    ## - product_colorsgreen            1   30721150 87910038825 23700
    ## - tags_count                     1   35848406 87915166080 23700
    ## - badge_product_quality1         1   45470675 87924788349 23700
    ## - product_colorsblue             1   55592860 87934910534 23700
    ## - retail_price                   1   62933585 87942251259 23700
    ## - `price_classEUR20-30`          1   70893132 87950210807 23700
    ## - origin_countrySG               1   81121625 87960439299 23700
    ## - discount_per                   1   86651065 87965968739 23700
    ## - rating                         1   88795196 87968112871 23700
    ## - origin_countryUS               1  103546969 87982864643 23701
    ## - product_colorsOther_colors     1  125667102 88004984776 23701
    ## - product_colorswhite            1  129008581 88008326255 23701
    ## <none>                                        87879317674 23701
    ## - product_colorsgrey             1  151949947 88031267622 23701
    ## - merchant_rating                1  158191184 88037508859 23702
    ## - product_colorsblack            1  174361191 88053678865 23702
    ## - price                          1  275596745 88154914419 23703
    ## - product_variation_inventory    1  292101529 88171419203 23704
    ## - product_colorspurple           1  319323993 88198641667 23704
    ## - product_sizesOther_sizes       1  545515364 88424833038 23707
    ## - `price_classEUR10-20`          1  550540650 88429858324 23707
    ## - shipping_option_price          1  597372276 88476689950 23708
    ## - countries_shipped_to           1  617253474 88496571148 23708
    ## - product_sizesS                 1  688781377 88568099051 23709
    ## - product_sizesXXS               1  730929438 88610247112 23710
    ## - product_sizesXXL               1  787315207 88666632881 23711
    ## - merchant_has_profile_picture1  1 1084679833 88963997507 23715
    ## - product_sizesXS                1 1579759907 89459077582 23723
    ## - merchant_rating_count          1 2663731583 90543049257 23738
    ## 
    ## Step:  AIC=23699.16
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - uses_ad_boosts1                1    3179408 87885273373 23697
    ## - shipping_is_express1           1    5634551 87887728516 23697
    ## - `price_classEUR40-50`          1   17566738 87899660703 23697
    ## - origin_countryVE               1   17946172 87900040137 23697
    ## - badges_count                   1   20385949 87902479914 23698
    ## - badge_local_product1           1   23027260 87905121225 23698
    ## - tags_count                     1   37014480 87919108445 23698
    ## - product_colorspink             1   43919208 87926013173 23698
    ## - badge_product_quality1         1   46316373 87928410338 23698
    ## - product_colorsred              1   48811953 87930905918 23698
    ## - retail_price                   1   63861095 87945955060 23698
    ## - `price_classEUR20-30`          1   70649746 87952743711 23698
    ## - origin_countrySG               1   81507398 87963601363 23698
    ## - discount_per                   1   86467145 87968561110 23698
    ## - rating                         1   87758314 87969852279 23699
    ## - product_colorsgreen            1  102285634 87984379599 23699
    ## - origin_countryUS               1  106474521 87988568486 23699
    ## <none>                                        87882093965 23699
    ## - merchant_rating                1  159607398 88041701363 23700
    ## - product_colorsblue             1  168389460 88050483425 23700
    ## - price                          1  274461548 88156555512 23701
    ## - product_variation_inventory    1  295389370 88177483335 23702
    ## - product_colorsgrey             1  384686092 88266780057 23703
    ## - product_colorswhite            1  466562463 88348656428 23704
    ## - product_colorsOther_colors     1  494654511 88376748476 23705
    ## - product_sizesOther_sizes       1  542759334 88424853299 23705
    ## - `price_classEUR10-20`          1  550186309 88432280274 23705
    ## - shipping_option_price          1  597477050 88479571015 23706
    ## - countries_shipped_to           1  614729766 88496823731 23706
    ## - product_colorsblack            1  629441836 88511535801 23707
    ## - product_colorspurple           1  654589690 88536683655 23707
    ## - product_sizesS                 1  690040819 88572134784 23707
    ## - product_sizesXXS               1  728372785 88610466750 23708
    ## - product_sizesXXL               1  787574923 88669668888 23709
    ## - merchant_has_profile_picture1  1 1084983519 88967077484 23713
    ## - product_sizesXS                1 1594455292 89476549257 23721
    ## - merchant_rating_count          1 2660957909 90543051874 23736
    ## 
    ## Step:  AIC=23697.21
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - shipping_is_express1           1    5678780 87890952153 23695
    ## - origin_countryVE               1   17121744 87902395117 23696
    ## - `price_classEUR40-50`          1   17450832 87902724204 23696
    ## - badges_count                   1   20098946 87905372318 23696
    ## - badge_local_product1           1   23302676 87908576049 23696
    ## - tags_count                     1   36539575 87921812948 23696
    ## - product_colorspink             1   43679747 87928953120 23696
    ## - badge_product_quality1         1   46048330 87931321703 23696
    ## - product_colorsred              1   48574067 87933847439 23696
    ## - retail_price                   1   64371786 87949645159 23696
    ## - `price_classEUR20-30`          1   68959648 87954233021 23696
    ## - origin_countrySG               1   82923468 87968196841 23696
    ## - rating                         1   86455421 87971728793 23697
    ## - discount_per                   1   87204550 87972477923 23697
    ## - product_colorsgreen            1  103113677 87988387050 23697
    ## - origin_countryUS               1  106070045 87991343418 23697
    ## <none>                                        87885273373 23697
    ## - merchant_rating                1  159223510 88044496883 23698
    ## - product_colorsblue             1  167577791 88052851163 23698
    ## - price                          1  271768487 88157041860 23699
    ## - product_variation_inventory    1  292219418 88177492791 23700
    ## - product_colorsgrey             1  384745775 88270019148 23701
    ## - product_colorswhite            1  465163232 88350436605 23702
    ## - product_colorsOther_colors     1  495692080 88380965453 23703
    ## - product_sizesOther_sizes       1  539597675 88424871048 23703
    ## - `price_classEUR10-20`          1  547378425 88432651798 23703
    ## - shipping_option_price          1  594812172 88480085545 23704
    ## - countries_shipped_to           1  616972049 88502245422 23704
    ## - product_colorsblack            1  630637431 88515910804 23705
    ## - product_colorspurple           1  652865590 88538138963 23705
    ## - product_sizesS                 1  695847475 88581120848 23706
    ## - product_sizesXXS               1  725617161 88610890534 23706
    ## - product_sizesXXL               1  790478100 88675751473 23707
    ## - merchant_has_profile_picture1  1 1084678167 88969951540 23711
    ## - product_sizesXS                1 1604263318 89489536691 23719
    ## - merchant_rating_count          1 2677732385 90563005758 23735
    ## 
    ## Step:  AIC=23695.29
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryVE               1   16864546 87907816700 23694
    ## - badges_count                   1   26222383 87917174536 23694
    ## - `price_classEUR40-50`          1   26815883 87917768036 23694
    ## - badge_local_product1           1   29192566 87920144719 23694
    ## - tags_count                     1   37793400 87928745554 23694
    ## - product_colorspink             1   43706970 87934659123 23694
    ## - product_colorsred              1   50008954 87940961108 23694
    ## - badge_product_quality1         1   55147519 87946099672 23694
    ## - `price_classEUR20-30`          1   63762366 87954714520 23694
    ## - retail_price                   1   66792961 87957745114 23694
    ## - origin_countrySG               1   82269736 87973221890 23695
    ## - rating                         1   86575969 87977528122 23695
    ## - discount_per                   1   90539685 87981491838 23695
    ## - origin_countryUS               1  104559987 87995512140 23695
    ## - product_colorsgreen            1  105023272 87995975426 23695
    ## <none>                                        87890952153 23695
    ## - merchant_rating                1  156230539 88047182693 23696
    ## - product_colorsblue             1  168703642 88059655795 23696
    ## - price                          1  278714569 88169666722 23697
    ## - product_variation_inventory    1  303051605 88194003758 23698
    ## - product_colorsgrey             1  386203341 88277155494 23699
    ## - product_colorswhite            1  464389098 88355341251 23700
    ## - product_colorsOther_colors     1  494055912 88385008066 23701
    ## - product_sizesOther_sizes       1  547634913 88438587066 23701
    ## - `price_classEUR10-20`          1  574402532 88465354685 23702
    ## - shipping_option_price          1  593754166 88484706320 23702
    ## - countries_shipped_to           1  617870197 88508822350 23703
    ## - product_colorsblack            1  630390014 88521342167 23703
    ## - product_colorspurple           1  652982862 88543935015 23703
    ## - product_sizesS                 1  697088770 88588040923 23704
    ## - product_sizesXXS               1  729387700 88620339853 23704
    ## - product_sizesXXL               1  788883099 88679835252 23705
    ## - merchant_has_profile_picture1  1 1087322515 88978274669 23709
    ## - product_sizesXS                1 1614270631 89505222784 23717
    ## - merchant_rating_count          1 2673745897 90564698050 23733
    ## 
    ## Step:  AIC=23693.54
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badges_count                   1   25886705 87933703404 23692
    ## - `price_classEUR40-50`          1   26197643 87934014343 23692
    ## - badge_local_product1           1   29069303 87936886003 23692
    ## - tags_count                     1   38031716 87945848415 23692
    ## - product_colorspink             1   43963178 87951779878 23692
    ## - product_colorsred              1   49599331 87957416030 23692
    ## - badge_product_quality1         1   53713458 87961530157 23692
    ## - `price_classEUR20-30`          1   61861966 87969678665 23693
    ## - retail_price                   1   67376655 87975193355 23693
    ## - origin_countrySG               1   82356552 87990173251 23693
    ## - discount_per                   1   94358015 88002174714 23693
    ## - rating                         1  101939140 88009755839 23693
    ## - origin_countryUS               1  103634508 88011451207 23693
    ## - product_colorsgreen            1  104811548 88012628248 23693
    ## <none>                                        87907816700 23694
    ## - merchant_rating                1  161516500 88069333200 23694
    ## - product_colorsblue             1  168956043 88076772743 23694
    ## - price                          1  271102922 88178919622 23696
    ## - product_variation_inventory    1  308314772 88216131472 23696
    ## - product_colorsgrey             1  384675384 88292492084 23697
    ## - product_colorswhite            1  453728650 88361545350 23698
    ## - product_colorsOther_colors     1  491690408 88399507107 23699
    ## - product_sizesOther_sizes       1  544757758 88452574458 23700
    ## - `price_classEUR10-20`          1  566316367 88474133067 23700
    ## - shipping_option_price          1  584851290 88492667990 23700
    ## - countries_shipped_to           1  619375503 88527192203 23701
    ## - product_colorsblack            1  622782178 88530598878 23701
    ## - product_colorspurple           1  649060321 88556877021 23701
    ## - product_sizesS                 1  688681923 88596498623 23702
    ## - product_sizesXXS               1  722977457 88630794157 23702
    ## - product_sizesXXL               1  783065674 88690882374 23703
    ## - merchant_has_profile_picture1  1 1090348843 88998165542 23708
    ## - product_sizesXS                1 1620439707 89528256406 23716
    ## - merchant_rating_count          1 2673887273 90581703973 23731
    ## 
    ## Step:  AIC=23691.93
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_local_product1 + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_local_product1           1    4141673 87937845077 23690
    ## - `price_classEUR40-50`          1   30426381 87964129786 23690
    ## - tags_count                     1   37206728 87970910132 23691
    ## - product_colorspink             1   44640915 87978344319 23691
    ## - badge_product_quality1         1   48040360 87981743765 23691
    ## - product_colorsred              1   51741053 87985444457 23691
    ## - `price_classEUR20-30`          1   59058707 87992762111 23691
    ## - retail_price                   1   69969555 88003672959 23691
    ## - origin_countrySG               1   82131644 88015835048 23691
    ## - discount_per                   1   99650966 88033354371 23691
    ## - product_colorsgreen            1  103216314 88036919718 23692
    ## - rating                         1  104207476 88037910880 23692
    ## - origin_countryUS               1  107235408 88040938812 23692
    ## <none>                                        87933703404 23692
    ## - merchant_rating                1  157427251 88091130655 23692
    ## - product_colorsblue             1  170808255 88104511659 23693
    ## - price                          1  263460615 88197164020 23694
    ## - product_variation_inventory    1  341874802 88275578206 23695
    ## - product_colorsgrey             1  381119416 88314822820 23696
    ## - product_colorswhite            1  453925172 88387628576 23697
    ## - product_colorsOther_colors     1  488985086 88422688490 23697
    ## - `price_classEUR10-20`          1  556914117 88490617522 23698
    ## - product_sizesOther_sizes       1  571442945 88505146349 23698
    ## - shipping_option_price          1  585406475 88519109879 23699
    ## - product_colorsblack            1  616856341 88550559745 23699
    ## - countries_shipped_to           1  623574164 88557277568 23699
    ## - product_colorspurple           1  651813983 88585517387 23700
    ## - product_sizesS                 1  686780786 88620484190 23700
    ## - product_sizesXXS               1  723527187 88657230591 23701
    ## - product_sizesXXL               1  798224221 88731927625 23702
    ## - merchant_has_profile_picture1  1 1077153322 89010856726 23706
    ## - product_sizesXS                1 1605788249 89539491653 23714
    ## - merchant_rating_count          1 2693050650 90626754054 23730
    ## 
    ## Step:  AIC=23689.99
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR40-50`          1   30881761 87968726838 23689
    ## - tags_count                     1   38346224 87976191300 23689
    ## - product_colorspink             1   44370511 87982215588 23689
    ## - badge_product_quality1         1   49020187 87986865264 23689
    ## - product_colorsred              1   51977809 87989822885 23689
    ## - `price_classEUR20-30`          1   59541089 87997386165 23689
    ## - retail_price                   1   69857882 88007702959 23689
    ## - origin_countrySG               1   82177617 88020022693 23689
    ## - discount_per                   1   99028483 88036873560 23690
    ## - product_colorsgreen            1  102709638 88040554715 23690
    ## - rating                         1  103058846 88040903923 23690
    ## - origin_countryUS               1  108376420 88046221496 23690
    ## <none>                                        87937845077 23690
    ## - merchant_rating                1  164877870 88102722947 23690
    ## - product_colorsblue             1  168880933 88106726009 23691
    ## - price                          1  264060546 88201905623 23692
    ## - product_variation_inventory    1  338718787 88276563864 23693
    ## - product_colorsgrey             1  380247679 88318092756 23694
    ## - product_colorswhite            1  454081161 88391926238 23695
    ## - product_colorsOther_colors     1  487117675 88424962751 23695
    ## - `price_classEUR10-20`          1  555161896 88493006972 23696
    ## - product_sizesOther_sizes       1  567522137 88505367213 23696
    ## - shipping_option_price          1  584407011 88522252087 23697
    ## - product_colorsblack            1  614570178 88552415254 23697
    ## - countries_shipped_to           1  621415157 88559260234 23697
    ## - product_colorspurple           1  652364268 88590209344 23698
    ## - product_sizesS                 1  687186544 88625031621 23698
    ## - product_sizesXXS               1  723521072 88661366148 23699
    ## - product_sizesXXL               1  794322948 88732168025 23700
    ## - merchant_has_profile_picture1  1 1073604840 89011449916 23704
    ## - product_sizesXS                1 1604459816 89542304893 23712
    ## - merchant_rating_count          1 2689788332 90627633409 23728
    ## 
    ## Step:  AIC=23688.45
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - tags_count                     1   40868317 88009595155 23687
    ## - `price_classEUR20-30`          1   42968706 88011695544 23687
    ## - product_colorspink             1   44718182 88013445020 23687
    ## - product_colorsred              1   45584381 88014311219 23687
    ## - badge_product_quality1         1   48438892 88017165730 23687
    ## - retail_price                   1   74948805 88043675643 23688
    ## - origin_countrySG               1   79760612 88048487450 23688
    ## - product_colorsgreen            1   97770243 88066497080 23688
    ## - discount_per                   1  103232529 88071959367 23688
    ## - origin_countryUS               1  105506204 88074233042 23688
    ## - rating                         1  105634812 88074361650 23688
    ## <none>                                        87968726838 23689
    ## - product_colorsblue             1  172822976 88141549814 23689
    ## - merchant_rating                1  172918359 88141645196 23689
    ## - price                          1  233241211 88201968049 23690
    ## - product_variation_inventory    1  348314201 88317041038 23692
    ## - product_colorsgrey             1  382368869 88351095707 23692
    ## - product_colorswhite            1  450993955 88419720793 23693
    ## - product_colorsOther_colors     1  486029846 88454756684 23694
    ## - `price_classEUR10-20`          1  543597435 88512324273 23695
    ## - shipping_option_price          1  562719023 88531445860 23695
    ## - product_sizesOther_sizes       1  610697850 88579424688 23696
    ## - product_colorsblack            1  612375267 88581102105 23696
    ## - countries_shipped_to           1  628892055 88597618893 23696
    ## - product_colorspurple           1  652916854 88621643692 23696
    ## - product_sizesS                 1  701586560 88670313397 23697
    ## - product_sizesXXS               1  743987985 88712714823 23698
    ## - product_sizesXXL               1  793093838 88761820675 23698
    ## - merchant_has_profile_picture1  1 1079653331 89048380169 23702
    ## - product_sizesXS                1 1628995870 89597722708 23711
    ## - merchant_rating_count          1 2681762316 90650489154 23726
    ## 
    ## Step:  AIC=23687.06
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - `price_classEUR20-30`          1   43793282 88053388437 23686
    ## - product_colorspink             1   45686821 88055281975 23686
    ## - badge_product_quality1         1   46698503 88056293657 23686
    ## - product_colorsred              1   51148328 88060743483 23686
    ## - retail_price                   1   73186400 88082781555 23686
    ## - origin_countrySG               1   79494238 88089089393 23686
    ## - discount_per                   1   92002485 88101597640 23686
    ## - origin_countryUS               1   99017289 88108612443 23687
    ## - product_colorsgreen            1  100814576 88110409730 23687
    ## - rating                         1  103288306 88112883461 23687
    ## <none>                                        88009595155 23687
    ## - product_colorsblue             1  175493911 88185089066 23688
    ## - merchant_rating                1  189746372 88199341526 23688
    ## - price                          1  241974164 88251569318 23689
    ## - product_variation_inventory    1  361252834 88370847989 23690
    ## - product_colorsgrey             1  401954416 88411549570 23691
    ## - product_colorswhite            1  469237693 88478832847 23692
    ## - product_colorsOther_colors     1  483769681 88493364836 23692
    ## - `price_classEUR10-20`          1  545582091 88555177245 23693
    ## - shipping_option_price          1  554446019 88564041174 23693
    ## - product_sizesOther_sizes       1  576248690 88585843845 23694
    ## - countries_shipped_to           1  615009921 88624605076 23694
    ## - product_colorsblack            1  636980296 88646575451 23695
    ## - product_colorspurple           1  651664925 88661260080 23695
    ## - product_sizesS                 1  686908989 88696504144 23695
    ## - product_sizesXXS               1  716932823 88726527977 23696
    ## - product_sizesXXL               1  774489211 88784084366 23697
    ## - merchant_has_profile_picture1  1 1069361645 89078956799 23701
    ## - product_sizesXS                1 1590410600 89600005754 23709
    ## - merchant_rating_count          1 2644909320 90654504475 23724
    ## 
    ## Step:  AIC=23685.71
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorspink             1   45627342 88099015779 23684
    ## - badge_product_quality1         1   47254193 88100642630 23684
    ## - product_colorsred              1   52383065 88105771502 23685
    ## - retail_price                   1   59906457 88113294893 23685
    ## - origin_countrySG               1   77852554 88131240991 23685
    ## - discount_per                   1   86121223 88139509660 23685
    ## - origin_countryUS               1   94736721 88148125158 23685
    ## - product_colorsgreen            1  100487514 88153875950 23685
    ## - rating                         1  107128586 88160517023 23685
    ## <none>                                        88053388437 23686
    ## - product_colorsblue             1  178286659 88231675096 23686
    ## - merchant_rating                1  189911872 88243300309 23687
    ## - price                          1  199906127 88253294564 23687
    ## - product_variation_inventory    1  364652455 88418040892 23689
    ## - product_colorsgrey             1  401965718 88455354155 23690
    ## - product_colorswhite            1  464161580 88517550017 23691
    ## - product_colorsOther_colors     1  474422059 88527810495 23691
    ## - `price_classEUR10-20`          1  510666798 88564055235 23691
    ## - shipping_option_price          1  517094159 88570482596 23691
    ## - product_sizesOther_sizes       1  568102077 88621490514 23692
    ## - countries_shipped_to           1  618469288 88671857725 23693
    ## - product_colorsblack            1  627768896 88681157333 23693
    ## - product_colorspurple           1  650821874 88704210311 23693
    ## - product_sizesS                 1  674900548 88728288985 23694
    ## - product_sizesXXS               1  723830695 88777219132 23694
    ## - product_sizesXXL               1  766261546 88819649983 23695
    ## - merchant_has_profile_picture1  1 1077215399 89130603835 23700
    ## - product_sizesXS                1 1591995537 89645383974 23707
    ## - merchant_rating_count          1 2655049146 90708437583 23723
    ## 
    ## Step:  AIC=23684.39
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsred              1   23928792 88122944570 23683
    ## - badge_product_quality1         1   52065617 88151081396 23683
    ## - retail_price                   1   57304879 88156320657 23683
    ## - product_colorsgreen            1   62929209 88161944988 23683
    ## - origin_countrySG               1   78127896 88177143675 23684
    ## - discount_per                   1   83845458 88182861237 23684
    ## - origin_countryUS               1  101139618 88200155397 23684
    ## - rating                         1  104201668 88203217447 23684
    ## - product_colorsblue             1  134041419 88233057198 23684
    ## <none>                                        88099015779 23684
    ## - merchant_rating                1  188161419 88287177198 23685
    ## - price                          1  200378152 88299393930 23685
    ## - product_variation_inventory    1  355173612 88454189390 23688
    ## - product_colorsgrey             1  358109964 88457125742 23688
    ## - product_colorswhite            1  446369719 88545385498 23689
    ## - product_colorsOther_colors     1  463844847 88562860625 23689
    ## - shipping_option_price          1  514340077 88613355856 23690
    ## - `price_classEUR10-20`          1  514383624 88613399403 23690
    ## - product_sizesOther_sizes       1  574475329 88673491107 23691
    ## - countries_shipped_to           1  601069102 88700084881 23691
    ## - product_colorspurple           1  608538439 88707554218 23691
    ## - product_colorsblack            1  642046440 88741062219 23692
    ## - product_sizesS                 1  660311316 88759327095 23692
    ## - product_sizesXXS               1  727095291 88826111070 23693
    ## - product_sizesXXL               1  766290232 88865306010 23694
    ## - merchant_has_profile_picture1  1 1069273481 89168289260 23698
    ## - product_sizesXS                1 1586661376 89685677155 23706
    ## - merchant_rating_count          1 2653789979 90752805757 23721
    ## 
    ## Step:  AIC=23682.75
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsgreen            1   47099496 88170044066 23681
    ## - badge_product_quality1         1   50408129 88173352700 23682
    ## - retail_price                   1   58108826 88181053396 23682
    ## - origin_countrySG               1   78063548 88201008118 23682
    ## - discount_per                   1   84326424 88207270995 23682
    ## - origin_countryUS               1  105298841 88228243412 23682
    ## - rating                         1  108286132 88231230703 23682
    ## - product_colorsblue             1  113130944 88236075514 23682
    ## <none>                                        88122944570 23683
    ## - merchant_rating                1  191039338 88313983908 23684
    ## - price                          1  197865377 88320809947 23684
    ## - product_colorsgrey             1  334181208 88457125778 23686
    ## - product_variation_inventory    1  352962755 88475907325 23686
    ## - product_colorswhite            1  432485053 88555429623 23687
    ## - product_colorsOther_colors     1  454779360 88577723931 23688
    ## - shipping_option_price          1  506874663 88629819233 23688
    ## - `price_classEUR10-20`          1  512818766 88635763337 23688
    ## - product_sizesOther_sizes       1  576393803 88699338373 23689
    ## - product_colorspurple           1  584925021 88707869591 23689
    ## - countries_shipped_to           1  609763654 88732708224 23690
    ## - product_colorsblack            1  645386224 88768330794 23690
    ## - product_sizesS                 1  663743417 88786687988 23691
    ## - product_sizesXXS               1  737385368 88860329939 23692
    ## - product_sizesXXL               1  764940004 88887884574 23692
    ## - merchant_has_profile_picture1  1 1058145855 89181090426 23696
    ## - product_sizesXS                1 1592770452 89715715022 23704
    ## - merchant_rating_count          1 2649524646 90772469217 23720
    ## 
    ## Step:  AIC=23681.45
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - badge_product_quality1         1   53844284 88223888350 23680
    ## - retail_price                   1   55592166 88225636232 23680
    ## - origin_countrySG               1   77807075 88247851141 23681
    ## - discount_per                   1   78494061 88248538127 23681
    ## - product_colorsblue             1   87512325 88257556391 23681
    ## - origin_countryUS               1  107531471 88277575537 23681
    ## - rating                         1  116730784 88286774850 23681
    ## <none>                                        88170044066 23681
    ## - merchant_rating                1  187038172 88357082238 23682
    ## - price                          1  200441475 88370485542 23682
    ## - product_colorsgrey             1  296646691 88466690757 23684
    ## - product_variation_inventory    1  329147759 88499191825 23684
    ## - product_colorswhite            1  385776915 88555820981 23685
    ## - product_colorsOther_colors     1  407680315 88577724382 23686
    ## - shipping_option_price          1  509415424 88679459491 23687
    ## - `price_classEUR10-20`          1  515644049 88685688115 23687
    ## - product_colorspurple           1  545548679 88715592746 23688
    ## - product_sizesOther_sizes       1  575236808 88745280875 23688
    ## - product_colorsblack            1  599268161 88769312228 23688
    ## - countries_shipped_to           1  601633861 88771677927 23688
    ## - product_sizesS                 1  662402388 88832446454 23689
    ## - product_sizesXXS               1  736646489 88906690555 23690
    ## - product_sizesXXL               1  763693866 88933737932 23691
    ## - merchant_has_profile_picture1  1 1089782748 89259826814 23696
    ## - product_sizesXS                1 1597564686 89767608753 23703
    ## - merchant_rating_count          1 2651023162 90821067229 23718
    ## 
    ## Step:  AIC=23680.25
    ## .outcome ~ price + `price_classEUR10-20` + retail_price + discount_per + 
    ##     rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - retail_price                   1   55987170 88279875520 23679
    ## - origin_countrySG               1   75120997 88299009347 23679
    ## - discount_per                   1   78775797 88302664147 23679
    ## - product_colorsblue             1   88983691 88312872041 23680
    ## - origin_countryUS               1  101353443 88325241793 23680
    ## <none>                                        88223888350 23680
    ## - rating                         1  170534400 88394422750 23681
    ## - price                          1  194411631 88418299981 23681
    ## - merchant_rating                1  211111864 88435000214 23681
    ## - product_colorsgrey             1  318413528 88542301878 23683
    ## - product_variation_inventory    1  335285356 88559173706 23683
    ## - product_colorswhite            1  385475391 88609363741 23684
    ## - product_colorsOther_colors     1  409044971 88632933321 23684
    ## - shipping_option_price          1  504738145 88728626495 23686
    ## - `price_classEUR10-20`          1  504739172 88728627523 23686
    ## - product_colorspurple           1  544609964 88768498314 23686
    ## - product_sizesOther_sizes       1  580564317 88804452667 23687
    ## - product_colorsblack            1  612452983 88836341333 23687
    ## - countries_shipped_to           1  617648296 88841536646 23687
    ## - product_sizesS                 1  657429914 88881318264 23688
    ## - product_sizesXXS               1  750334582 88974222932 23689
    ## - product_sizesXXL               1  755765473 88979653823 23689
    ## - merchant_has_profile_picture1  1 1103386036 89327274386 23695
    ## - product_sizesXS                1 1589231935 89813120285 23702
    ## - merchant_rating_count          1 2742837816 90966726166 23718
    ## 
    ## Step:  AIC=23679.08
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - discount_per                   1   23733962 88303609483 23677
    ## - origin_countrySG               1   77161654 88357037175 23678
    ## - product_colorsblue             1   89232344 88369107864 23678
    ## - origin_countryUS               1  103785191 88383660712 23679
    ## <none>                                        88279875520 23679
    ## - rating                         1  164409319 88444284840 23680
    ## - merchant_rating                1  219832303 88499707824 23680
    ## - price                          1  230567252 88510442772 23681
    ## - product_colorsgrey             1  311693761 88591569281 23682
    ## - product_variation_inventory    1  329008651 88608884172 23682
    ## - product_colorswhite            1  396620927 88676496447 23683
    ## - product_colorsOther_colors     1  409008733 88688884253 23683
    ## - shipping_option_price          1  490609470 88770484990 23684
    ## - `price_classEUR10-20`          1  498913376 88778788896 23685
    ## - product_colorspurple           1  547452181 88827327702 23685
    ## - product_sizesOther_sizes       1  582215072 88862090592 23686
    ## - countries_shipped_to           1  620085513 88899961033 23686
    ## - product_colorsblack            1  639366700 88919242220 23687
    ## - product_sizesS                 1  650071453 88929946973 23687
    ## - product_sizesXXS               1  766785402 89046660923 23688
    ## - product_sizesXXL               1  774217161 89054092681 23689
    ## - merchant_has_profile_picture1  1 1122369838 89402245358 23694
    ## - product_sizesXS                1 1572673020 89852548540 23700
    ## - merchant_rating_count          1 2816219828 91096095348 23718
    ## 
    ## Step:  AIC=23677.43
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countrySG               1   78706683 88382316165 23677
    ## - product_colorsblue             1   88368653 88391978135 23677
    ## - origin_countryUS               1  111193424 88414802907 23677
    ## <none>                                        88303609483 23677
    ## - rating                         1  163350301 88466959784 23678
    ## - merchant_rating                1  217563161 88521172644 23679
    ## - price                          1  235666628 88539276110 23679
    ## - product_colorsgrey             1  308731703 88612341186 23680
    ## - product_variation_inventory    1  330028696 88633638179 23680
    ## - product_colorswhite            1  390446843 88694056326 23681
    ## - product_colorsOther_colors     1  412321446 88715930929 23682
    ## - shipping_option_price          1  489272512 88792881995 23683
    ## - `price_classEUR10-20`          1  507459081 88811068563 23683
    ## - product_colorspurple           1  555113270 88858722753 23684
    ## - product_sizesOther_sizes       1  582262637 88885872119 23684
    ## - countries_shipped_to           1  633834457 88937443940 23685
    ## - product_colorsblack            1  641918568 88945528051 23685
    ## - product_sizesS                 1  651861266 88955470748 23685
    ## - product_sizesXXS               1  756477045 89060086528 23687
    ## - product_sizesXXL               1  768935865 89072545348 23687
    ## - merchant_has_profile_picture1  1 1127728689 89431338171 23692
    ## - product_sizesXS                1 1555760662 89859370145 23698
    ## - merchant_rating_count          1 2815727368 91119336851 23717
    ## 
    ## Step:  AIC=23676.6
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - product_colorsblue             1   89568534 88471884699 23676
    ## - origin_countryUS               1  111589348 88493905513 23676
    ## <none>                                        88382316165 23677
    ## - rating                         1  168364470 88550680635 23677
    ## - merchant_rating                1  214875523 88597191689 23678
    ## - price                          1  232439691 88614755856 23678
    ## - product_variation_inventory    1  325594025 88707910190 23679
    ## - product_colorsgrey             1  334540284 88716856449 23680
    ## - product_colorswhite            1  392685577 88775001742 23680
    ## - product_colorsOther_colors     1  426613687 88808929852 23681
    ## - shipping_option_price          1  493005687 88875321852 23682
    ## - `price_classEUR10-20`          1  501889982 88884206147 23682
    ## - product_colorspurple           1  554020299 88936336464 23683
    ## - product_sizesOther_sizes       1  585288021 88967604186 23683
    ## - countries_shipped_to           1  647444554 89029760719 23684
    ## - product_colorsblack            1  648126499 89030442664 23684
    ## - product_sizesS                 1  655750809 89038066974 23684
    ## - product_sizesXXS               1  764828682 89147144847 23686
    ## - product_sizesXXL               1  769483287 89151799452 23686
    ## - merchant_has_profile_picture1  1 1122786120 89505102285 23691
    ## - product_sizesXS                1 1537511015 89919827180 23697
    ## - merchant_rating_count          1 2807193793 91189509959 23716
    ## 
    ## Step:  AIC=23675.93
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## - origin_countryUS               1  120383925 88592268624 23676
    ## <none>                                        88471884699 23676
    ## - rating                         1  164482636 88636367334 23676
    ## - merchant_rating                1  212968453 88684853152 23677
    ## - price                          1  236435230 88708319929 23677
    ## - product_colorsgrey             1  286440478 88758325177 23678
    ## - product_colorswhite            1  321771345 88793656044 23679
    ## - product_variation_inventory    1  347563350 88819448049 23679
    ## - product_colorsOther_colors     1  351004846 88822889544 23679
    ## - shipping_option_price          1  493762107 88965646805 23681
    ## - `price_classEUR10-20`          1  503212599 88975097297 23681
    ## - product_colorspurple           1  503940370 88975825069 23681
    ## - product_sizesOther_sizes       1  560544322 89032429021 23682
    ## - product_colorsblack            1  562257445 89034142143 23682
    ## - countries_shipped_to           1  595728805 89067613503 23683
    ## - product_sizesS                 1  657665778 89129550477 23684
    ## - product_sizesXXS               1  747919145 89219803844 23685
    ## - product_sizesXXL               1  749703415 89221588114 23685
    ## - merchant_has_profile_picture1  1 1102824894 89574709593 23690
    ## - product_sizesXS                1 1518929432 89990814130 23696
    ## - merchant_rating_count          1 2788901617 91260786315 23715
    ## 
    ## Step:  AIC=23675.71
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq         RSS   AIC
    ## <none>                                        88592268624 23676
    ## - rating                         1  166379228 88758647852 23676
    ## - merchant_rating                1  216506104 88808774728 23677
    ## - price                          1  228930032 88821198656 23677
    ## - product_colorsgrey             1  285462046 88877730671 23678
    ## - product_colorsOther_colors     1  330825240 88923093864 23679
    ## - product_colorswhite            1  335451881 88927720505 23679
    ## - product_variation_inventory    1  344655042 88936923666 23679
    ## - shipping_option_price          1  486422028 89078690652 23681
    ## - `price_classEUR10-20`          1  501713059 89093981683 23681
    ## - product_colorspurple           1  511342228 89103610852 23681
    ## - product_colorsblack            1  551323702 89143592326 23682
    ## - product_sizesOther_sizes       1  555728967 89147997592 23682
    ## - countries_shipped_to           1  618704465 89210973089 23683
    ## - product_sizesS                 1  646432849 89238701473 23683
    ## - product_sizesXXL               1  736222618 89328491242 23685
    ## - product_sizesXXS               1  795050679 89387319303 23685
    ## - merchant_has_profile_picture1  1 1140747405 89733016029 23691
    ## - product_sizesXS                1 1491421388 90083690012 23696
    ## - merchant_rating_count          1 2827776402 91420045026 23715
    ## Start:  AIC=26418.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + badge_fast_shipping1 + product_variation_inventory + 
    ##     shipping_option_price + shipping_is_express1 + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=26418.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + price_drop + discount_per + 
    ##     uses_ad_boosts1 + rating + badges_count + badge_local_product1 + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     shipping_is_express1 + countries_shipped_to + origin_countryGB + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ## 
    ## Step:  AIC=26418.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + shipping_is_express1 + 
    ##     countries_shipped_to + origin_countryGB + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesM + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite + 
    ##     product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - shipping_is_express1           1        252 103221906603 26417
    ## - origin_countryGB               1       9185 103221915536 26417
    ## - product_colorsyellow           1     747422 103222653773 26417
    ## - product_sizesM                 1    1389285 103223295636 26417
    ## - product_colorspink             1    5710252 103227616603 26417
    ## - badge_local_product1           1   10850885 103232757236 26417
    ## - shipping_nameOther_shipping    1   11881736 103233788087 26417
    ## - badges_count                   1   12070578 103233976929 26417
    ## - uses_ad_boosts1                1   12958768 103234865119 26417
    ## - product_colorsred              1   18469498 103240375849 26417
    ## - badge_product_quality1         1   21012812 103242919163 26417
    ## - origin_countryVE               1   26084011 103247990362 26417
    ## - `price_classEUR40-50`          1   28576736 103250483087 26417
    ## - product_colorsgreen            1   33623309 103255529660 26417
    ## - retail_price                   1   47801383 103269707733 26417
    ## - product_colorsblue             1   66212547 103288118898 26418
    ## - `price_classEUR20-30`          1   69793193 103291699544 26418
    ## - origin_countrySG               1   80951671 103302858022 26418
    ## - origin_countryUS               1  106707873 103328614223 26418
    ## - discount_per                   1  114328581 103336234932 26418
    ## - tags_count                     1  122042776 103343949127 26419
    ## - product_colorswhite            1  122406121 103344312472 26419
    ## - product_sizesS                 1  126855229 103348761579 26419
    ## - rating                         1  136720688 103358627039 26419
    ## <none>                                        103221906351 26419
    ## - product_colorsgrey             1  152429705 103374336056 26419
    ## - merchant_rating                1  168190998 103390097349 26419
    ## - product_colorsOther_colors     1  188560528 103410466879 26419
    ## - product_colorsblack            1  205769196 103427675546 26420
    ## - product_sizesOther_sizes       1  242295919 103464202270 26420
    ## - product_colorspurple           1  282752735 103504659086 26421
    ## - product_variation_inventory    1  314382098 103536288449 26421
    ## - price                          1  318853934 103540760285 26421
    ## - product_sizesXXS               1  432788066 103654694417 26423
    ## - product_sizesXS                1  536707355 103758613706 26424
    ## - product_sizesXXL               1  684380909 103906287260 26426
    ## - countries_shipped_to           1  696495996 103918402347 26427
    ## - shipping_option_price          1  705828583 103927734933 26427
    ## - `price_classEUR10-20`          1  729051500 103950957851 26427
    ## - merchant_has_profile_picture1  1  976727204 104198633555 26431
    ## - merchant_rating_count          1 4428074407 107649980757 26478
    ## 
    ## Step:  AIC=26416.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryGB + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesM + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryGB               1       9212 103221915815 26415
    ## - product_colorsyellow           1     747301 103222653904 26415
    ## - product_sizesM                 1    1389905 103223296508 26415
    ## - product_colorspink             1    5710110 103227616713 26415
    ## - badge_local_product1           1   11135326 103233041929 26415
    ## - shipping_nameOther_shipping    1   12146017 103234052620 26415
    ## - uses_ad_boosts1                1   12975893 103234882496 26415
    ## - badges_count                   1   13043309 103234949913 26415
    ## - product_colorsred              1   18471594 103240378198 26415
    ## - badge_product_quality1         1   22558220 103244464824 26415
    ## - origin_countryVE               1   26085544 103247992147 26415
    ## - `price_classEUR40-50`          1   29572934 103251479538 26415
    ## - product_colorsgreen            1   33636841 103255543444 26415
    ## - retail_price                   1   47839966 103269746570 26415
    ## - product_colorsblue             1   66213291 103288119895 26416
    ## - `price_classEUR20-30`          1   70529911 103292436514 26416
    ## - origin_countrySG               1   80965640 103302872243 26416
    ## - origin_countryUS               1  106729080 103328635683 26416
    ## - discount_per                   1  114371437 103336278040 26416
    ## - tags_count                     1  122110606 103344017209 26417
    ## - product_colorswhite            1  122407839 103344314443 26417
    ## - product_sizesS                 1  126929249 103348835852 26417
    ## - rating                         1  136875898 103358782501 26417
    ## <none>                                        103221906603 26417
    ## - product_colorsgrey             1  152429454 103374336057 26417
    ## - merchant_rating                1  168784545 103390691149 26417
    ## - product_colorsOther_colors     1  188647758 103410554362 26417
    ## - product_colorsblack            1  205768964 103427675567 26418
    ## - product_sizesOther_sizes       1  242325696 103464232300 26418
    ## - product_colorspurple           1  282754874 103504661477 26419
    ## - product_variation_inventory    1  314777734 103536684338 26419
    ## - price                          1  319905703 103541812306 26419
    ## - product_sizesXXS               1  433056452 103654963055 26421
    ## - product_sizesXS                1  537307826 103759214429 26422
    ## - product_sizesXXL               1  684403512 103906310115 26424
    ## - countries_shipped_to           1  696712482 103918619085 26425
    ## - shipping_option_price          1  711122816 103933029419 26425
    ## - `price_classEUR10-20`          1  741729062 103963635666 26425
    ## - merchant_has_profile_picture1  1  977009941 104198916544 26429
    ## - merchant_rating_count          1 4429937186 107651843789 26476
    ## 
    ## Step:  AIC=26414.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite + product_colorsyellow
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsyellow           1     749176 103222664991 26413
    ## - product_sizesM                 1    1391181 103223306996 26413
    ## - product_colorspink             1    5714430 103227630245 26413
    ## - badge_local_product1           1   11134927 103233050742 26413
    ## - shipping_nameOther_shipping    1   12143219 103234059033 26413
    ## - uses_ad_boosts1                1   13015104 103234930919 26413
    ## - badges_count                   1   13041281 103234957096 26413
    ## - product_colorsred              1   18482155 103240397970 26413
    ## - badge_product_quality1         1   22557979 103244473794 26413
    ## - origin_countryVE               1   26083475 103247999290 26413
    ## - `price_classEUR40-50`          1   29563913 103251479728 26413
    ## - product_colorsgreen            1   33652436 103255568251 26413
    ## - retail_price                   1   47834996 103269750811 26413
    ## - product_colorsblue             1   66270395 103288186210 26414
    ## - `price_classEUR20-30`          1   70522911 103292438726 26414
    ## - origin_countrySG               1   80974607 103302890422 26414
    ## - origin_countryUS               1  106734858 103328650672 26414
    ## - discount_per                   1  114371350 103336287165 26414
    ## - tags_count                     1  122128542 103344044357 26415
    ## - product_colorswhite            1  122426290 103344342105 26415
    ## - product_sizesS                 1  126924830 103348840645 26415
    ## - rating                         1  136891146 103358806961 26415
    ## <none>                                        103221915815 26415
    ## - product_colorsgrey             1  152469365 103374385180 26415
    ## - merchant_rating                1  168821389 103390737204 26415
    ## - product_colorsOther_colors     1  188686980 103410602795 26415
    ## - product_colorsblack            1  205796106 103427711921 26416
    ## - product_sizesOther_sizes       1  242322540 103464238355 26416
    ## - product_colorspurple           1  282802876 103504718691 26417
    ## - product_variation_inventory    1  315274965 103537190780 26417
    ## - price                          1  319897649 103541813464 26417
    ## - product_sizesXXS               1  433070051 103654985866 26419
    ## - product_sizesXS                1  537401180 103759316995 26420
    ## - product_sizesXXL               1  684394327 103906310142 26422
    ## - countries_shipped_to           1  696730537 103918646352 26423
    ## - shipping_option_price          1  711363344 103933279159 26423
    ## - `price_classEUR10-20`          1  742026940 103963942754 26423
    ## - merchant_has_profile_picture1  1  977085082 104199000897 26427
    ## - merchant_rating_count          1 4429928449 107651844264 26474
    ## 
    ## Step:  AIC=26412.76
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesM + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspink + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_sizesM                 1    1276256 103223941246 26411
    ## - product_colorspink             1    7036838 103229701829 26411
    ## - badge_local_product1           1   10943454 103233608444 26411
    ## - shipping_nameOther_shipping    1   12475092 103235140083 26411
    ## - uses_ad_boosts1                1   12836602 103235501592 26411
    ## - badges_count                   1   12856511 103235521502 26411
    ## - badge_product_quality1         1   22341299 103245006290 26411
    ## - origin_countryVE               1   26149974 103248814965 26411
    ## - `price_classEUR40-50`          1   29667654 103252332645 26411
    ## - product_colorsred              1   30256424 103252921414 26411
    ## - retail_price                   1   47299594 103269964585 26411
    ## - product_colorsgreen            1   58710284 103281375275 26412
    ## - `price_classEUR20-30`          1   70537253 103293202244 26412
    ## - origin_countrySG               1   80812338 103303477329 26412
    ## - origin_countryUS               1  106019746 103328684736 26412
    ## - discount_per                   1  114135191 103336800181 26412
    ## - tags_count                     1  121485395 103344150386 26413
    ## - product_colorsblue             1  125400601 103348065592 26413
    ## - product_sizesS                 1  128132391 103350797382 26413
    ## - rating                         1  137640623 103360305614 26413
    ## <none>                                        103222664991 26413
    ## - merchant_rating                1  168248544 103390913535 26413
    ## - product_sizesOther_sizes       1  246138220 103468803210 26414
    ## - product_colorsgrey             1  292203968 103514868959 26415
    ## - product_colorswhite            1  310031021 103532696012 26415
    ## - product_variation_inventory    1  314527013 103537192004 26415
    ## - price                          1  320271869 103542936860 26415
    ## - product_sizesXXS               1  438806036 103661471026 26417
    ## - product_colorspurple           1  488006735 103710671725 26418
    ## - product_colorsOther_colors     1  530918604 103753583595 26418
    ## - product_sizesXS                1  537978538 103760643529 26418
    ## - product_colorsblack            1  550554626 103773219617 26419
    ## - product_sizesXXL               1  686071329 103908736320 26420
    ## - countries_shipped_to           1  704848528 103927513519 26421
    ## - shipping_option_price          1  710686233 103933351223 26421
    ## - `price_classEUR10-20`          1  741713845 103964378835 26421
    ## - merchant_has_profile_picture1  1  977289987 104199954977 26425
    ## - merchant_rating_count          1 4439670321 107662335311 26472
    ## 
    ## Step:  AIC=26410.78
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspink + 
    ##     product_colorspurple + product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorspink             1    7137261 103231078507 26409
    ## - badge_local_product1           1   10678239 103234619485 26409
    ## - shipping_nameOther_shipping    1   12541693 103236482939 26409
    ## - badges_count                   1   12823468 103236764715 26409
    ## - uses_ad_boosts1                1   12924858 103236866104 26409
    ## - badge_product_quality1         1   22341303 103246282549 26409
    ## - origin_countryVE               1   26076048 103250017294 26409
    ## - `price_classEUR40-50`          1   29653111 103253594357 26409
    ## - product_colorsred              1   29910678 103253851924 26409
    ## - retail_price                   1   47455030 103271396276 26409
    ## - product_colorsgreen            1   58347473 103282288720 26410
    ## - `price_classEUR20-30`          1   70191826 103294133072 26410
    ## - origin_countrySG               1   80784371 103304725617 26410
    ## - origin_countryUS               1  105116420 103329057666 26410
    ## - discount_per                   1  114256871 103338198117 26410
    ## - tags_count                     1  120952754 103344894000 26411
    ## - product_colorsblue             1  127280290 103351221536 26411
    ## - rating                         1  137262044 103361203290 26411
    ## <none>                                        103223941246 26411
    ## - merchant_rating                1  168501350 103392442596 26411
    ## - product_colorsgrey             1  293977851 103517919097 26413
    ## - product_colorswhite            1  312482200 103536423446 26413
    ## - product_variation_inventory    1  314324263 103538265510 26413
    ## - price                          1  322103475 103546044721 26413
    ## - product_colorspurple           1  488529004 103712470250 26416
    ## - product_colorsOther_colors     1  530684834 103754626081 26416
    ## - product_colorsblack            1  554428037 103778369283 26417
    ## - product_sizesOther_sizes       1  561126759 103785068005 26417
    ## - product_sizesS                 1  565326640 103789267886 26417
    ## - countries_shipped_to           1  707591700 103931532946 26419
    ## - shipping_option_price          1  713814734 103937755980 26419
    ## - `price_classEUR10-20`          1  746829832 103970771079 26419
    ## - product_sizesXXS               1  905244344 104129185590 26422
    ## - product_sizesXXL               1  906431466 104130372712 26422
    ## - merchant_has_profile_picture1  1  980705403 104204646650 26423
    ## - product_sizesXS                1 1651682160 104875623406 26432
    ## - merchant_rating_count          1 4440983775 107664925021 26470
    ## 
    ## Step:  AIC=26408.88
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_local_product1 + badge_product_quality1 + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - badge_local_product1           1   10664058 103241742565 26407
    ## - shipping_nameOther_shipping    1   12766339 103243844846 26407
    ## - badges_count                   1   12896329 103243974836 26407
    ## - uses_ad_boosts1                1   12898037 103243976544 26407
    ## - product_colorsred              1   22929726 103254008233 26407
    ## - badge_product_quality1         1   23060044 103254138551 26407
    ## - origin_countryVE               1   26144547 103257223054 26407
    ## - `price_classEUR40-50`          1   29692826 103260771333 26407
    ## - retail_price                   1   46363670 103277442177 26408
    ## - product_colorsgreen            1   51507283 103282585790 26408
    ## - `price_classEUR20-30`          1   70166720 103301245227 26408
    ## - origin_countrySG               1   80893019 103311971526 26408
    ## - origin_countryUS               1  107851791 103338930298 26408
    ## - discount_per                   1  113012409 103344090916 26409
    ## - tags_count                     1  121097352 103352175859 26409
    ## - product_colorsblue             1  126076629 103357155136 26409
    ## - rating                         1  136682829 103367761336 26409
    ## <none>                                        103231078507 26409
    ## - merchant_rating                1  167200892 103398279399 26409
    ## - product_colorsgrey             1  309146941 103540225448 26411
    ## - product_variation_inventory    1  312328909 103543407416 26411
    ## - price                          1  322619644 103553698151 26411
    ## - product_colorswhite            1  366090148 103597168655 26412
    ## - product_colorspurple           1  509057876 103740136383 26414
    ## - product_sizesS                 1  560272681 103791351188 26415
    ## - product_sizesOther_sizes       1  563390727 103794469234 26415
    ## - product_colorsOther_colors     1  663865689 103894944196 26416
    ## - product_colorsblack            1  685134444 103916212951 26417
    ## - countries_shipped_to           1  702112883 103933191390 26417
    ## - shipping_option_price          1  713482665 103944561172 26417
    ## - `price_classEUR10-20`          1  748393974 103979472481 26417
    ## - product_sizesXXS               1  905735933 104136814440 26420
    ## - product_sizesXXL               1  906378709 104137457216 26420
    ## - merchant_has_profile_picture1  1  977451608 104208530115 26421
    ## - product_sizesXS                1 1649421731 104880500238 26430
    ## - merchant_rating_count          1 4440283517 107671362024 26468
    ## 
    ## Step:  AIC=26407.03
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badges_count + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - badges_count                   1    2385767 103244128332 26405
    ## - uses_ad_boosts1                1   11942214 103253684779 26405
    ## - badge_product_quality1         1   13021789 103254764355 26405
    ## - shipping_nameOther_shipping    1   13514586 103255257151 26405
    ## - product_colorsred              1   24131332 103265873897 26405
    ## - origin_countryVE               1   26247848 103267990413 26405
    ## - `price_classEUR40-50`          1   31483017 103273225583 26406
    ## - retail_price                   1   47576068 103289318633 26406
    ## - product_colorsgreen            1   49866545 103291609110 26406
    ## - `price_classEUR20-30`          1   68779619 103310522184 26406
    ## - origin_countrySG               1   80604679 103322347244 26406
    ## - origin_countryUS               1  110902371 103352644936 26407
    ## - discount_per                   1  115391206 103357133771 26407
    ## - tags_count                     1  122022245 103363764811 26407
    ## - product_colorsblue             1  125250081 103366992646 26407
    ## - rating                         1  137795655 103379538220 26407
    ## <none>                                        103241742565 26407
    ## - merchant_rating                1  168936749 103410679314 26407
    ## - product_colorsgrey             1  306625012 103548367577 26409
    ## - price                          1  319590229 103561332795 26410
    ## - product_variation_inventory    1  329486721 103571229286 26410
    ## - product_colorswhite            1  366077635 103607820200 26410
    ## - product_colorspurple           1  510295758 103752038323 26412
    ## - product_sizesS                 1  559394810 103801137375 26413
    ## - product_sizesOther_sizes       1  574563946 103816306512 26413
    ## - product_colorsOther_colors     1  660237730 103901980295 26414
    ## - product_colorsblack            1  679921056 103921663622 26415
    ## - countries_shipped_to           1  702603715 103944346280 26415
    ## - shipping_option_price          1  716663548 103958406113 26415
    ## - `price_classEUR10-20`          1  741727779 103983470344 26416
    ## - product_sizesXXS               1  905315487 104147058052 26418
    ## - product_sizesXXL               1  908690027 104150432593 26418
    ## - merchant_has_profile_picture1  1  969440693 104211183258 26419
    ## - product_sizesXS                1 1640014122 104881756688 26428
    ## - merchant_rating_count          1 4444896533 107686639098 26466
    ## 
    ## Step:  AIC=26405.06
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + uses_ad_boosts1 + 
    ##     rating + badge_product_quality1 + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + origin_countryVE + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     shipping_nameOther_shipping + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - uses_ad_boosts1                1   12624673 103256753005 26403
    ## - shipping_nameOther_shipping    1   13629862 103257758195 26403
    ## - badge_product_quality1         1   15619067 103259747399 26403
    ## - product_colorsred              1   24092449 103268220781 26403
    ## - origin_countryVE               1   26116469 103270244801 26403
    ## - `price_classEUR40-50`          1   31834921 103275963253 26404
    ## - retail_price                   1   47899021 103292027354 26404
    ## - product_colorsgreen            1   49652958 103293781290 26404
    ## - `price_classEUR20-30`          1   68067690 103312196023 26404
    ## - origin_countrySG               1   80614114 103324742447 26404
    ## - origin_countryUS               1  110716810 103354845142 26405
    ## - discount_per                   1  116485352 103360613684 26405
    ## - tags_count                     1  120711252 103364839584 26405
    ## - product_colorsblue             1  126584322 103370712654 26405
    ## - rating                         1  139904810 103384033142 26405
    ## <none>                                        103244128332 26405
    ## - merchant_rating                1  166569426 103410697758 26405
    ## - product_colorsgrey             1  306424649 103550552981 26407
    ## - price                          1  320463791 103564592124 26408
    ## - product_variation_inventory    1  337962554 103582090887 26408
    ## - product_colorswhite            1  365754867 103609883200 26408
    ## - product_colorspurple           1  510131743 103754260075 26410
    ## - product_sizesS                 1  559820440 103803948772 26411
    ## - product_sizesOther_sizes       1  586963302 103831091634 26411
    ## - product_colorsOther_colors     1  659713740 103903842073 26412
    ## - product_colorsblack            1  679826625 103923954958 26413
    ## - countries_shipped_to           1  704749568 103948877901 26413
    ## - shipping_option_price          1  725933232 103970061565 26413
    ## - `price_classEUR10-20`          1  743937539 103988065872 26414
    ## - product_sizesXXS               1  905373165 104149501497 26416
    ## - product_sizesXXL               1  919250285 104163378617 26416
    ## - merchant_has_profile_picture1  1  971668280 104215796613 26417
    ## - product_sizesXS                1 1640680377 104884808709 26426
    ## - merchant_rating_count          1 4458489095 107702617427 26465
    ## 
    ## Step:  AIC=26403.24
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + shipping_nameOther_shipping + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - shipping_nameOther_shipping    1   12666289 103269419295 26401
    ## - badge_product_quality1         1   15210733 103271963739 26402
    ## - product_colorsred              1   24158171 103280911176 26402
    ## - origin_countryVE               1   28702792 103285455798 26402
    ## - `price_classEUR40-50`          1   32611324 103289364330 26402
    ## - retail_price                   1   47299120 103304052125 26402
    ## - product_colorsgreen            1   48531937 103305284942 26402
    ## - `price_classEUR20-30`          1   73038081 103329791087 26402
    ## - origin_countrySG               1   78413120 103335166125 26402
    ## - origin_countryUS               1  111338241 103368091247 26403
    ## - discount_per                   1  115109169 103371862174 26403
    ## - tags_count                     1  122157186 103378910192 26403
    ## - product_colorsblue             1  127987036 103384740041 26403
    ## <none>                                        103256753005 26403
    ## - rating                         1  143156455 103399909461 26403
    ## - merchant_rating                1  167917638 103424670644 26404
    ## - product_colorsgrey             1  305211008 103561964013 26406
    ## - price                          1  330422726 103587175732 26406
    ## - product_variation_inventory    1  354912676 103611665682 26406
    ## - product_colorswhite            1  368115122 103624868128 26406
    ## - product_colorspurple           1  513612500 103770365506 26409
    ## - product_sizesS                 1  554779190 103811532196 26409
    ## - product_sizesOther_sizes       1  605178104 103861931109 26410
    ## - product_colorsOther_colors     1  656660230 103913413235 26411
    ## - product_colorsblack            1  676970230 103933723235 26411
    ## - countries_shipped_to           1  700467242 103957220248 26411
    ## - shipping_option_price          1  734388772 103991141777 26412
    ## - `price_classEUR10-20`          1  756158972 104012911978 26412
    ## - product_sizesXXL               1  915360356 104172113361 26414
    ## - product_sizesXXS               1  917847673 104174600678 26414
    ## - merchant_has_profile_picture1  1  969034750 104225787755 26415
    ## - product_sizesXS                1 1631934895 104888687900 26424
    ## - merchant_rating_count          1 4446440997 107703194003 26463
    ## 
    ## Step:  AIC=26401.42
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     badge_product_quality1 + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     origin_countryVE + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorsred + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - badge_product_quality1         1   14313132 103283732426 26400
    ## - product_colorsred              1   26209102 103295628397 26400
    ## - origin_countryVE               1   28472092 103297891386 26400
    ## - `price_classEUR40-50`          1   32775892 103302195187 26400
    ## - retail_price                   1   50099705 103319519000 26400
    ## - product_colorsgreen            1   50137043 103319556338 26400
    ## - `price_classEUR20-30`          1   73330991 103342750286 26401
    ## - origin_countrySG               1   78685598 103348104893 26401
    ## - origin_countryUS               1  108808759 103378228054 26401
    ## - discount_per                   1  117712660 103387131954 26401
    ## - tags_count                     1  122119636 103391538931 26401
    ## - product_colorsblue             1  134708684 103404127979 26401
    ## <none>                                        103269419295 26401
    ## - rating                         1  142858546 103412277841 26401
    ## - merchant_rating                1  166746475 103436165769 26402
    ## - product_colorsgrey             1  313007466 103582426761 26404
    ## - price                          1  329243303 103598662598 26404
    ## - product_variation_inventory    1  355854787 103625274081 26404
    ## - product_colorswhite            1  377894134 103647313428 26405
    ## - product_colorspurple           1  526830463 103796249758 26407
    ## - product_sizesS                 1  554432181 103823851476 26407
    ## - product_sizesOther_sizes       1  609361849 103878781144 26408
    ## - product_colorsOther_colors     1  664463990 103933883285 26409
    ## - product_colorsblack            1  691235777 103960655071 26409
    ## - countries_shipped_to           1  697819208 103967238503 26409
    ## - shipping_option_price          1  745412789 104014832083 26410
    ## - `price_classEUR10-20`          1  751988290 104021407585 26410
    ## - product_sizesXXS               1  911367811 104180787105 26412
    ## - product_sizesXXL               1  918138956 104187558251 26412
    ## - merchant_has_profile_picture1  1  979201874 104248621169 26413
    ## - product_sizesXS                1 1634791139 104904210434 26422
    ## - merchant_rating_count          1 4449278872 107718698167 26461
    ## 
    ## Step:  AIC=26399.62
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorsred + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsred              1   25509172 103309241598 26398
    ## - origin_countryVE               1   27264708 103310997135 26398
    ## - `price_classEUR40-50`          1   32524350 103316256777 26398
    ## - retail_price                   1   50391065 103334123491 26398
    ## - product_colorsgreen            1   50925880 103334658306 26398
    ## - `price_classEUR20-30`          1   73799680 103357532106 26399
    ## - origin_countrySG               1   77440781 103361173207 26399
    ## - origin_countryUS               1  105556752 103389289178 26399
    ## - discount_per                   1  118812206 103402544633 26399
    ## - tags_count                     1  120540014 103404272440 26399
    ## - product_colorsblue             1  134436171 103418168598 26400
    ## <none>                                        103283732426 26400
    ## - rating                         1  178694938 103462427364 26400
    ## - merchant_rating                1  178787808 103462520234 26400
    ## - product_colorsgrey             1  320792382 103604524808 26402
    ## - price                          1  327647203 103611379630 26402
    ## - product_variation_inventory    1  362816480 103646548907 26403
    ## - product_colorswhite            1  375945019 103659677446 26403
    ## - product_colorspurple           1  524650764 103808383190 26405
    ## - product_sizesS                 1  552151174 103835883600 26405
    ## - product_sizesOther_sizes       1  612075976 103895808402 26406
    ## - product_colorsOther_colors     1  663797114 103947529540 26407
    ## - product_colorsblack            1  695863717 103979596143 26407
    ## - countries_shipped_to           1  704780909 103988513335 26408
    ## - shipping_option_price          1  744882308 104028614734 26408
    ## - `price_classEUR10-20`          1  748555921 104032288347 26408
    ## - product_sizesXXL               1  914260635 104197993061 26411
    ## - product_sizesXXS               1  916356919 104200089345 26411
    ## - merchant_has_profile_picture1  1  983167149 104266899575 26411
    ## - product_sizesXS                1 1627866973 104911599400 26420
    ## - merchant_rating_count          1 4513349267 107797081694 26460
    ## 
    ## Step:  AIC=26397.98
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + origin_countryVE + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryVE               1   26943104 103336184702 26396
    ## - `price_classEUR40-50`          1   28242628 103337484227 26396
    ## - product_colorsgreen            1   35459034 103344700632 26397
    ## - retail_price                   1   50682545 103359924144 26397
    ## - `price_classEUR20-30`          1   73011167 103382252766 26397
    ## - origin_countrySG               1   77167728 103386409326 26397
    ## - origin_countryUS               1  109428951 103418670550 26398
    ## - product_colorsblue             1  112461534 103421703132 26398
    ## - discount_per                   1  119214663 103428456262 26398
    ## - tags_count                     1  126874402 103436116000 26398
    ## <none>                                        103309241598 26398
    ## - rating                         1  183528238 103492769837 26399
    ## - merchant_rating                1  184944525 103494186123 26399
    ## - product_colorsgrey             1  295313897 103604555495 26400
    ## - price                          1  322078815 103631320414 26401
    ## - product_colorswhite            1  356578705 103665820304 26401
    ## - product_variation_inventory    1  358651471 103667893070 26401
    ## - product_colorspurple           1  499176693 103808418292 26403
    ## - product_sizesS                 1  556330491 103865572089 26404
    ## - product_sizesOther_sizes       1  615911352 103925152950 26405
    ## - product_colorsOther_colors     1  670872968 103980114567 26405
    ## - product_colorsblack            1  703003287 104012244885 26406
    ## - countries_shipped_to           1  719911842 104029153441 26406
    ## - shipping_option_price          1  737868258 104047109857 26406
    ## - `price_classEUR10-20`          1  741174316 104050415914 26406
    ## - product_sizesXXL               1  913441475 104222683074 26409
    ## - product_sizesXXS               1  933467332 104242708930 26409
    ## - merchant_has_profile_picture1  1  971512007 104280753605 26410
    ## - product_sizesXS                1 1634704844 104943946442 26419
    ## - merchant_rating_count          1 4511927059 107821168657 26458
    ## 
    ## Step:  AIC=26396.36
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     `price_classEUR40-50` + retail_price + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgreen + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - `price_classEUR40-50`          1   27617139 103363801842 26395
    ## - product_colorsgreen            1   35162262 103371346964 26395
    ## - retail_price                   1   51030953 103387215655 26395
    ## - `price_classEUR20-30`          1   70880850 103407065552 26395
    ## - origin_countrySG               1   77379285 103413563987 26396
    ## - origin_countryUS               1  108307780 103444492483 26396
    ## - product_colorsblue             1  112605455 103448790157 26396
    ## - discount_per                   1  122492476 103458677178 26396
    ## - tags_count                     1  126016153 103462200856 26396
    ## <none>                                        103336184702 26396
    ## - merchant_rating                1  191435746 103527620448 26397
    ## - rating                         1  204987772 103541172474 26397
    ## - product_colorsgrey             1  293401928 103629586630 26399
    ## - price                          1  311760992 103647945695 26399
    ## - product_colorswhite            1  343285188 103679469890 26399
    ## - product_variation_inventory    1  363277796 103699462499 26400
    ## - product_colorspurple           1  495317314 103831502016 26401
    ## - product_sizesS                 1  550021573 103886206275 26402
    ## - product_sizesOther_sizes       1  611750740 103947935442 26403
    ## - product_colorsOther_colors     1  666739141 104002923843 26404
    ## - product_colorsblack            1  688011632 104024196334 26404
    ## - countries_shipped_to           1  719703708 104055888410 26405
    ## - shipping_option_price          1  722794204 104058978906 26405
    ## - `price_classEUR10-20`          1  732743755 104068928457 26405
    ## - product_sizesXXL               1  905927641 104242112344 26407
    ## - product_sizesXXS               1  924697684 104260882386 26407
    ## - merchant_has_profile_picture1  1  975752474 104311937176 26408
    ## - product_sizesXS                1 1642053628 104978238331 26417
    ## - merchant_rating_count          1 4511843559 107848028261 26457
    ## 
    ## Step:  AIC=26394.75
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgreen + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsgreen            1   31232902 103395034743 26393
    ## - `price_classEUR20-30`          1   53774350 103417576192 26394
    ## - retail_price                   1   54752125 103418553967 26394
    ## - origin_countrySG               1   75397649 103439199490 26394
    ## - origin_countryUS               1  105614493 103469416335 26394
    ## - product_colorsblue             1  118351177 103482153019 26394
    ## - discount_per                   1  126068294 103489870135 26395
    ## - tags_count                     1  129074328 103492876170 26395
    ## <none>                                        103363801842 26395
    ## - merchant_rating                1  206160473 103569962314 26396
    ## - rating                         1  209475725 103573277567 26396
    ## - price                          1  285014858 103648816699 26397
    ## - product_colorsgrey             1  296899773 103660701615 26397
    ## - product_colorswhite            1  344700431 103708502273 26398
    ## - product_variation_inventory    1  369040539 103732842381 26398
    ## - product_colorspurple           1  497652047 103861453889 26400
    ## - product_sizesS                 1  560743994 103924545835 26401
    ## - product_sizesOther_sizes       1  664707040 104028508882 26402
    ## - product_colorsOther_colors     1  671834765 104035636607 26402
    ## - product_colorsblack            1  692090121 104055891963 26403
    ## - shipping_option_price          1  703406571 104067208413 26403
    ## - countries_shipped_to           1  725803095 104089604937 26403
    ## - `price_classEUR10-20`          1  741797766 104105599608 26403
    ## - product_sizesXXL               1  904602137 104268403979 26405
    ## - product_sizesXXS               1  949192750 104312994591 26406
    ## - merchant_has_profile_picture1  1  983617686 104347419528 26407
    ## - product_sizesXS                1 1666434076 105030235918 26416
    ## - merchant_rating_count          1 4500289530 107864091372 26455
    ## 
    ## Step:  AIC=26393.19
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     retail_price + discount_per + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countrySG + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - retail_price                   1   52461323 103447496067 26392
    ## - `price_classEUR20-30`          1   53842867 103448877610 26392
    ## - origin_countrySG               1   75306327 103470341071 26392
    ## - product_colorsblue             1   98070096 103493104840 26393
    ## - origin_countryUS               1  107006635 103502041379 26393
    ## - discount_per                   1  119673540 103514708283 26393
    ## - tags_count                     1  128443325 103523478069 26393
    ## <none>                                        103395034743 26393
    ## - merchant_rating                1  204543261 103599578005 26394
    ## - rating                         1  220288655 103615323398 26394
    ## - product_colorsgrey             1  270153812 103665188555 26395
    ## - price                          1  288584341 103683619084 26395
    ## - product_colorswhite            1  313488076 103708522820 26396
    ## - product_variation_inventory    1  349265409 103744300153 26396
    ## - product_colorspurple           1  469903656 103864938399 26398
    ## - product_sizesS                 1  560110192 103955144935 26399
    ## - product_colorsOther_colors     1  648062374 104043097118 26400
    ## - product_sizesOther_sizes       1  657617743 104052652487 26400
    ## - product_colorsblack            1  666610451 104061645194 26401
    ## - shipping_option_price          1  705581325 104100616069 26401
    ## - countries_shipped_to           1  716634945 104111669688 26401
    ## - `price_classEUR10-20`          1  744063724 104139098467 26402
    ## - product_sizesXXL               1  903531208 104298565951 26404
    ## - product_sizesXXS               1  950429073 104345463816 26405
    ## - merchant_has_profile_picture1  1 1008223778 104403258521 26405
    ## - product_sizesXS                1 1672395264 105067430007 26415
    ## - merchant_rating_count          1 4502162495 107897197238 26453
    ## 
    ## Step:  AIC=26391.93
    ## .outcome ~ price + `price_classEUR10-20` + `price_classEUR20-30` + 
    ##     discount_per + rating + product_variation_inventory + shipping_option_price + 
    ##     countries_shipped_to + origin_countrySG + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - `price_classEUR20-30`          1   44315974 103491812040 26391
    ## - discount_per                   1   70662464 103518158530 26391
    ## - origin_countrySG               1   76905280 103524401347 26391
    ## - product_colorsblue             1   98739642 103546235709 26391
    ## - origin_countryUS               1  110245926 103557741993 26392
    ## - tags_count                     1  126065903 103573561970 26392
    ## <none>                                        103447496067 26392
    ## - merchant_rating                1  212712585 103660208652 26393
    ## - rating                         1  217058577 103664554643 26393
    ## - product_colorsgrey             1  264102935 103711599001 26394
    ## - product_colorswhite            1  317598288 103765094355 26394
    ## - price                          1  320072800 103767568866 26394
    ## - product_variation_inventory    1  345204312 103792700379 26395
    ## - product_colorspurple           1  474399209 103921895275 26397
    ## - product_sizesS                 1  552027576 103999523643 26398
    ## - product_colorsOther_colors     1  647886312 104095382379 26399
    ## - product_sizesOther_sizes       1  661666296 104109162362 26399
    ## - shipping_option_price          1  687999731 104135495798 26400
    ## - product_colorsblack            1  690747364 104138243430 26400
    ## - countries_shipped_to           1  719148126 104166644192 26400
    ## - `price_classEUR10-20`          1  724361812 104171857879 26400
    ## - product_sizesXXL               1  924459829 104371955896 26403
    ## - product_sizesXXS               1  973656113 104421152180 26404
    ## - merchant_has_profile_picture1  1 1028898933 104476394999 26404
    ## - product_sizesXS                1 1658462593 105105958660 26413
    ## - merchant_rating_count          1 4597549270 108045045336 26453
    ## 
    ## Step:  AIC=26390.55
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countrySG + origin_countryUS + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + tags_count + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countrySG               1   75166621 103566978661 26390
    ## - discount_per                   1   81444504 103573256544 26390
    ## - product_colorsblue             1  100141733 103591953774 26390
    ## - origin_countryUS               1  105934995 103597747035 26390
    ## - tags_count                     1  127946892 103619758933 26390
    ## <none>                                        103491812040 26391
    ## - merchant_rating                1  209550957 103701362997 26392
    ## - rating                         1  228324117 103720136157 26392
    ## - product_colorsgrey             1  262386536 103754198576 26392
    ## - price                          1  275760071 103767572112 26392
    ## - product_colorswhite            1  309650824 103801462864 26393
    ## - product_variation_inventory    1  357192182 103849004222 26394
    ## - product_colorspurple           1  470668754 103962480794 26395
    ## - product_sizesS                 1  546451689 104038263730 26396
    ## - product_colorsOther_colors     1  628846279 104120658319 26397
    ## - shipping_option_price          1  650278006 104142090046 26398
    ## - product_sizesOther_sizes       1  654152039 104145964079 26398
    ## - product_colorsblack            1  674075414 104165887454 26398
    ## - `price_classEUR10-20`          1  700742171 104192554212 26398
    ## - countries_shipped_to           1  724307854 104216119894 26399
    ## - product_sizesXXL               1  916673979 104408486019 26401
    ## - product_sizesXXS               1  981913090 104473725130 26402
    ## - merchant_has_profile_picture1  1 1034922056 104526734097 26403
    ## - product_sizesXS                1 1665601023 105157413063 26412
    ## - merchant_rating_count          1 4605260056 108097072096 26452
    ## 
    ## Step:  AIC=26389.61
    ## .outcome ~ price + `price_classEUR10-20` + discount_per + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     origin_countryUS + merchant_rating_count + merchant_rating + 
    ##     merchant_has_profile_picture1 + tags_count + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsblue + product_colorsgrey + 
    ##     product_colorsOther_colors + product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - discount_per                   1   84083616 103651062277 26389
    ## - product_colorsblue             1  101328636 103668307297 26389
    ## - origin_countryUS               1  106310970 103673289631 26389
    ## - tags_count                     1  127568496 103694547157 26389
    ## <none>                                        103566978661 26390
    ## - merchant_rating                1  206702583 103773681244 26391
    ## - rating                         1  233890653 103800869314 26391
    ## - price                          1  272270828 103839249489 26391
    ## - product_colorsgrey             1  283745925 103850724586 26392
    ## - product_colorswhite            1  311848082 103878826743 26392
    ## - product_variation_inventory    1  353159429 103920138090 26393
    ## - product_colorspurple           1  469763200 104036741861 26394
    ## - product_sizesS                 1  549653720 104116632381 26395
    ## - product_colorsOther_colors     1  645201013 104212179674 26397
    ## - shipping_option_price          1  653946737 104220925398 26397
    ## - product_sizesOther_sizes       1  656083502 104223062163 26397
    ## - product_colorsblack            1  679393184 104246371845 26397
    ## - `price_classEUR10-20`          1  694777551 104261756212 26397
    ## - countries_shipped_to           1  737288498 104304267159 26398
    ## - product_sizesXXL               1  917189063 104484167724 26401
    ## - product_sizesXXS               1  990735689 104557714350 26402
    ## - merchant_has_profile_picture1  1 1030461908 104597440569 26402
    ## - product_sizesXS                1 1649083299 105216061960 26411
    ## - merchant_rating_count          1 4595658680 108162637341 26451
    ## 
    ## Step:  AIC=26388.79
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsblue + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - product_colorsblue             1   98023594 103749085870 26388
    ## - tags_count                     1  102987692 103754049968 26388
    ## - origin_countryUS               1  115940197 103767002473 26388
    ## <none>                                        103651062277 26389
    ## - merchant_rating                1  207737050 103858799327 26390
    ## - rating                         1  230672022 103881734298 26390
    ## - product_colorsgrey             1  282230490 103933292766 26391
    ## - price                          1  284932839 103935995116 26391
    ## - product_colorswhite            1  301367674 103952429950 26391
    ## - product_variation_inventory    1  355864297 104006926574 26392
    ## - product_colorspurple           1  472993439 104124055716 26393
    ## - product_sizesS                 1  551679954 104202742231 26395
    ## - product_colorsOther_colors     1  645528746 104296591023 26396
    ## - shipping_option_price          1  648348043 104299410320 26396
    ## - product_sizesOther_sizes       1  648583163 104299645440 26396
    ## - product_colorsblack            1  685596473 104336658749 26396
    ## - `price_classEUR10-20`          1  714633015 104365695292 26397
    ## - countries_shipped_to           1  756037059 104407099335 26397
    ## - product_sizesXXL               1  903425109 104554487385 26399
    ## - product_sizesXXS               1  968647666 104619709942 26400
    ## - merchant_has_profile_picture1  1 1043638633 104694700909 26401
    ## - product_sizesXS                1 1605292263 105256354540 26409
    ## - merchant_rating_count          1 4575862557 108226924834 26450
    ## 
    ## Step:  AIC=26388.17
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     tags_count + product_sizesOther_sizes + product_sizesS + 
    ##     product_sizesXS + product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - tags_count                     1  102250642 103851336512 26388
    ## - origin_countryUS               1  125685576 103874771446 26388
    ## <none>                                        103749085870 26388
    ## - merchant_rating                1  203062294 103952148164 26389
    ## - rating                         1  225344841 103974430711 26389
    ## - product_colorswhite            1  233460495 103982546365 26389
    ## - product_colorsgrey             1  235700848 103984786718 26390
    ## - price                          1  286029518 104035115388 26390
    ## - product_variation_inventory    1  383009450 104132095320 26392
    ## - product_colorspurple           1  423402090 104172487961 26392
    ## - product_colorsOther_colors     1  555804883 104304890753 26394
    ## - product_sizesS                 1  558704862 104307790733 26394
    ## - product_colorsblack            1  592933635 104342019506 26395
    ## - product_sizesOther_sizes       1  628920550 104378006420 26395
    ## - shipping_option_price          1  645191771 104394277641 26395
    ## - countries_shipped_to           1  693478995 104442564865 26396
    ## - `price_classEUR10-20`          1  711415887 104460501757 26396
    ## - product_sizesXXL               1  886028339 104635114209 26399
    ## - product_sizesXXS               1  948624892 104697710762 26399
    ## - merchant_has_profile_picture1  1 1021784339 104770870210 26401
    ## - product_sizesXS                1 1586887609 105335973479 26408
    ## - merchant_rating_count          1 4561256428 108310342298 26449
    ## 
    ## Step:  AIC=26387.61
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + origin_countryUS + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## - origin_countryUS               1  113619136 103964955648 26387
    ## <none>                                        103851336512 26388
    ## - rating                         1  220650582 104071987094 26389
    ## - merchant_rating                1  234042200 104085378712 26389
    ## - product_colorswhite            1  248776998 104100113510 26389
    ## - product_colorsgrey             1  250821370 104102157882 26389
    ## - price                          1  297667078 104149003590 26390
    ## - product_variation_inventory    1  405456362 104256792874 26391
    ## - product_colorspurple           1  409753837 104261090350 26391
    ## - product_sizesS                 1  534207484 104385543996 26393
    ## - product_colorsOther_colors     1  535414658 104386751170 26393
    ## - product_sizesOther_sizes       1  567706414 104419042926 26394
    ## - product_colorsblack            1  616900855 104468237367 26394
    ## - shipping_option_price          1  627290809 104478627321 26394
    ## - countries_shipped_to           1  666220376 104517556888 26395
    ## - `price_classEUR10-20`          1  708885189 104560221701 26396
    ## - product_sizesXXL               1  853379908 104704716420 26398
    ## - product_sizesXXS               1  898203348 104749539860 26398
    ## - merchant_has_profile_picture1  1  995166252 104846502764 26400
    ## - product_sizesXS                1 1513926381 105365262893 26407
    ## - merchant_rating_count          1 4478641745 108329978258 26447
    ## 
    ## Step:  AIC=26387.2
    ## .outcome ~ price + `price_classEUR10-20` + rating + product_variation_inventory + 
    ##     shipping_option_price + countries_shipped_to + merchant_rating_count + 
    ##     merchant_rating + merchant_has_profile_picture1 + product_sizesOther_sizes + 
    ##     product_sizesS + product_sizesXS + product_sizesXXL + product_sizesXXS + 
    ##     product_colorsblack + product_colorsgrey + product_colorsOther_colors + 
    ##     product_colorspurple + product_colorswhite
    ## 
    ##                                 Df  Sum of Sq          RSS   AIC
    ## <none>                                        103964955648 26387
    ## - rating                         1  219392088 104184347736 26388
    ## - merchant_rating                1  238619166 104203574814 26389
    ## - product_colorsgrey             1  242742663 104207698311 26389
    ## - product_colorswhite            1  255792496 104220748144 26389
    ## - price                          1  289751908 104254707556 26389
    ## - product_variation_inventory    1  398814560 104363770208 26391
    ## - product_colorspurple           1  415067357 104380023005 26391
    ## - product_colorsOther_colors     1  510181481 104475137129 26392
    ## - product_sizesS                 1  524789552 104489745200 26393
    ## - product_sizesOther_sizes       1  560089837 104525045485 26393
    ## - product_colorsblack            1  603520813 104568476461 26394
    ## - shipping_option_price          1  618312602 104583268250 26394
    ## - countries_shipped_to           1  707629222 104672584870 26395
    ## - `price_classEUR10-20`          1  714364966 104679320614 26395
    ## - product_sizesXXL               1  839536218 104804491866 26397
    ## - product_sizesXXS               1  940527866 104905483514 26398
    ## - merchant_has_profile_picture1  1 1029534798 104994490446 26400
    ## - product_sizesXS                1 1493630907 105458586555 26406
    ## - merchant_rating_count          1 4543830722 108508786370 26448

``` r
model_step$finalModel
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ price + `price_classEUR10-20` + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite, data = dat)
    ## 
    ## Coefficients:
    ##                   (Intercept)                          price  
    ##                   -5828.51089                      347.23684  
    ##         `price_classEUR10-20`                         rating  
    ##                   -2501.25237                      849.25644  
    ##   product_variation_inventory          shipping_option_price  
    ##                      28.83109                    -1545.44448  
    ##          countries_shipped_to          merchant_rating_count  
    ##                     -36.65946                        0.02652  
    ##               merchant_rating  merchant_has_profile_picture1  
    ##                    2238.60005                     2442.46288  
    ##      product_sizesOther_sizes                 product_sizesS  
    ##                   -2789.78735                    -1740.22959  
    ##               product_sizesXS               product_sizesXXL  
    ##                   -3543.57047                    -7083.71321  
    ##              product_sizesXXS            product_colorsblack  
    ##                   -4017.85637                     1874.60452  
    ##            product_colorsgrey     product_colorsOther_colors  
    ##                    1911.64051                     1664.79434  
    ##          product_colorspurple            product_colorswhite  
    ##                    3045.08009                     1276.18090

``` r
summary(model_step)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ price + `price_classEUR10-20` + rating + 
    ##     product_variation_inventory + shipping_option_price + countries_shipped_to + 
    ##     merchant_rating_count + merchant_rating + merchant_has_profile_picture1 + 
    ##     product_sizesOther_sizes + product_sizesS + product_sizesXS + 
    ##     product_sizesXXL + product_sizesXXS + product_colorsblack + 
    ##     product_colorsgrey + product_colorsOther_colors + product_colorspurple + 
    ##     product_colorswhite, data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -46882  -3849  -1778   1332  91194 
    ## 
    ## Coefficients:
    ##                                   Estimate   Std. Error t value
    ## (Intercept)                   -5828.510886  4876.298129  -1.195
    ## price                           347.236840   173.511293   2.001
    ## `price_classEUR10-20`         -2501.252369   795.999036  -3.142
    ## rating                          849.256438   487.689572   1.741
    ## product_variation_inventory      28.831086    12.279781   2.348
    ## shipping_option_price         -1545.444480   528.645404  -2.923
    ## countries_shipped_to            -36.659460    11.721907  -3.127
    ## merchant_rating_count             0.026525     0.003347   7.925
    ## merchant_rating                2238.600046  1232.647641   1.816
    ## merchant_has_profile_picture1  2442.462876   647.474335   3.772
    ## product_sizesOther_sizes      -2789.787352  1002.668467  -2.782
    ## product_sizesS                -1740.229592   646.143621  -2.693
    ## product_sizesXS               -3543.570466   779.892115  -4.544
    ## product_sizesXXL              -7083.713214  2079.487692  -3.406
    ## product_sizesXXS              -4017.856369  1114.355282  -3.606
    ## product_colorsblack            1874.604522   649.050721   2.888
    ## product_colorsgrey             1911.640508  1043.634125   1.832
    ## product_colorsOther_colors     1664.794336   626.921764   2.656
    ## product_colorspurple           3045.080089  1271.318984   2.395
    ## product_colorswhite            1276.180896   678.708740   1.880
    ##                                          Pr(>|t|)    
    ## (Intercept)                              0.232177    
    ## price                                    0.045555 *  
    ## `price_classEUR10-20`                    0.001711 ** 
    ## rating                                   0.081830 .  
    ## product_variation_inventory              0.019017 *  
    ## shipping_option_price                    0.003516 ** 
    ## countries_shipped_to                     0.001799 ** 
    ## merchant_rating_count         0.00000000000000454 ***
    ## merchant_rating                          0.069565 .  
    ## merchant_has_profile_picture1            0.000168 ***
    ## product_sizesOther_sizes                 0.005467 ** 
    ## product_sizesS                           0.007158 ** 
    ## product_sizesXS               0.00000599155005105 ***
    ## product_sizesXXL                         0.000676 ***
    ## product_sizesXXS                         0.000322 ***
    ## product_colorsblack                      0.003932 ** 
    ## product_colorsgrey                       0.067201 .  
    ## product_colorsOther_colors               0.008007 ** 
    ## product_colorspurple                     0.016738 *  
    ## product_colorswhite                      0.060268 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8506 on 1437 degrees of freedom
    ## Multiple R-squared:  0.1258, Adjusted R-squared:  0.1143 
    ## F-statistic: 10.89 on 19 and 1437 DF,  p-value: < 0.00000000000000022

## 7 Predictive Aanalysis

The machine learning technique is to build models and choose the best
one to predict *how well a product is going to sell*.

``` r
model_mlr <- cloth3
```

## Legality

## Reference

<https://www.kaggle.com/jmmvutu/summer-products-and-sales-in-ecommerce-wish>
