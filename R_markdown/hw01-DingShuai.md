---
title: "Homework 01"
author: "Shuai Ding"
date: "9/17/2019"
output:
  html_document:
    toc: yes
    toc_float: yes
    theme: spacelab
  html_notebook:
    toc: yes
    toc_float: yes
    theme: spacelab
always_allow_html: yes
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, cache=FALSE)
```

# 1. Create R Markdown notebook html document
# 2. Get some data
```{r}
library(fivethirtyeight)
library(tidyverse)
data(package = "fivethirtyeight")
?bechdel
```
#### The name of my dataset
bechdel

#### Description
The raw data behind the story "The Dollar-And-Cents Case Against Hollywood's Exclusion of Women" https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/.

#### Format
A data frame with 1794 rows representing movies and 15 variables:

#### Variable names and varable descriptions

Variable name  | Variable description
-------------- | --------------------
year           | Year of release
imdb           | Text to construct IMDB url. Ex: https://www.imdb.com/title/tt1711425
title          | Movie test
test           | bechdel test result (detailed, with discrepancies indicated)
clean_test     | bechdel test result (detailed): ok = passes test, dubious, men = women only talk about men, notalk = women don't talk to each other, nowomen = fewer than two women
binary         | Bechdel Test PASS vs FAIL binary
budget         | Film budget
domgross       | Domestic (US) gross
intgross       | Total International (i.e., worldwide) gross
code           | Bechdel Code
budget_2013    | Budget in 2013 inflation adjusted dollars
domgross_2013  | Domestic gross (US) in 2013 inflation adjusted dollars
intgross_2013  | Total International (i.e., worldwide) gross in 2013 inflation adjusted dollars
period_code    | NULL
decade_code    | NULL

```{r}
my_data <- bechdel
# change the unit of gross profits into million 
my_data <- mutate(my_data,
                  budget = budget/1e6,
                  domgross = domgross/1e6,
                  intgross = intgross/1e6,
                  budget_2013 = budget_2013/1e6,
                  domgross_2013 = domgross_2013/1e6,
                  intgross_2013 = intgross_2013/1e6
)

```

# 3. Show the data
#### Provide a simple summary of the data 
```{r}
summary(my_data)
```

#### Use the command datatable to show an interactive table
```{r}
library(DT)
datatable(my_data, class = 'cell-border stripe', 
          colnames = c('Year','Imdb',
          'FilmTitle','Test','Clean_test','Binary','Budget','DomesticGross',
          'InternationalGross','BechdelCode','Budget_2013',
          'DomesticGross_2013','InternationalGross_2013',
          'PeriodCode','DecadeCode'),
          caption = "The Bechdel Test Raw Data Table.")

```
#### Add a non-interactive table using the kable function. Prettify the column names a bit.
```{r}
colnames(my_data) <- c('Year','Imdb',
          'FilmTitle','Test','Clean_test','Binary','Budget','DomesticGross',
          'InternationalGross','BechdelCode','Budget_2013',
          'DomesticGross_2013','InternationalGross_2013',
          'PeriodCode','DecadeCode')

library(knitr)
library(kableExtra)

HighBudget_data <- subset(my_data, Budget >= 220)[c(1,3,7:9)] 
kable(HighBudget_data, format = "html",caption = "High budget films") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
  
```
# 4. Add some Latex formulas

Data on budget and gross income of films were given to predict the prominence of women in those films, which were featured by whether a film has one conversation between two women about something rather than men. The binary logistic regression model is utilized to model the result of the Bechdel test. The formula is shown as follows: $$ h_{\theta }(x) = \frac{1}{1 +e^{-\theta ^{T}x}} $$ This logistic model has two possible values: pass/fail.  

# 5. Add a plot
```{r}
library(ggplot2)
Year_BudgetPlot <- ggplot(my_data, aes(x = Year, y = Budget)) + geom_point(size = 0.5, alpha = 0.8, aes(color = Clean_test)) + scale_y_continuous(limits=c(0,500)) 

Year_BudgetPlot + labs(x ="Year", y = "Budget", title = "Plot of budget by year")

```

# 6. Add a picture

![Funny picture about the Bechdel test](images/bechdel.jpg)

- The Bechdel test is the easiest test in the world!


# 7. More Markdown
#### Add a footnote ^[The Bechdel test website: https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/]

#### Add a block quote

> "She goes on to say that it isn’t surprising that Frozen passed the Bechdel Test considering its two main characters are female. In fact there are, many other Disney movies pass the Bechdel Test with flying colors."