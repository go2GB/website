---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: pic10.jpg
keywords: ""
slug: ipsum
title: Ipsum
---


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```

```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
```


# Yield Curve inversion

Every so often, we hear warnings from commentators on the "inverted yield curve" and its predictive power with respect to recessions. An explainer what a [inverted yield curve is can be found here](https://www.reuters.com/article/us-usa-economy-yieldcurve-explainer/explainer-what-is-an-inverted-yield-curve-idUSKBN1O50GA). If you'd rather listen to something, here is a great podcast from [NPR on yield curve indicators](https://www.podbean.com/media/share/dir-4zgj9-6aefd11)

In addition, many articles and commentators think that, e.g., [*Yield curve inversion is viewed as a harbinger of recession*](https://www.bloomberg.com/news/articles/2019-08-14/u-k-yield-curve-inverts-for-first-time-since-financial-crisis). One can always doubt whether inversions are truly a harbinger of recessions, and [use the attached parable on yield curve inversions](https://twitter.com/5_min_macro/status/1161627360946511873).


In our case we will look at US data and use the [FRED database](https://fred.stlouisfed.org/) to download historical yield curve rates, and plot the yield curves since 1999 to see when the yield curves flatten. 

```{r download_historical_yield_curve, warning=FALSE, include=FALSE}

yield_curve <- read_csv("/Users/guidobassi/Desktop/website/content/blogs/2021-10-20-inflation/yield_curve.csv")


```


## Plotting the yield curve

```{r setup_US-recessions, warning=FALSE}


# get US recession dates after 1946 from Wikipedia 
# https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States

recessions <- tibble(
  from = c("1948-11-01", "1953-07-01", "1957-08-01", "1960-04-01", "1969-12-01", "1973-11-01", "1980-01-01","1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01","2020-02-01"),  
  to = c("1949-10-01", "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-30") 
  )  %>% 
  mutate(From = ymd(from), 
         To=ymd(to),
         duration_days = To-From)


recessions

recessions59up<-filter(recessions,year(from)>1959)

```

```{r, fig.width=15,fig.height=10}
library(scales)
drop_na(yield_curve, maturity)

yield_10y <-
  yield_curve %>%
  filter(duration == "10-Year Treasury Rate") %>%
  summarise(Date = date,
            Value = value / 100,
            Maturity = maturity)

yield_3m <-
  yield_curve %>%
  filter(duration == "3-Month Treasury Bill") %>%
  summarise(Date = date,
            Value = value / 100,
            Maturity = maturity)

yield_3m10y <-
  inner_join(yield_3m, yield_10y, by = "Date") %>%
  transmute(Date = Date, delta = (Value.y - Value.x))

ggplot() +
  geom_rect(
    data = recessions59up,
    aes(xmin = From, xmax = To),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2
  ) +
  geom_line(data = yield_3m10y, aes(x = Date, y = delta * 100)) +
  geom_area(data = yield_3m10y,
            color = "#5DADE2",
            aes(x = (Date), y = ifelse(delta < 0, delta * 100, 0)),
            fill =
              "#F5B7B1") +
  geom_area(data = yield_3m10y,
            color = "#7FB3D5",
            aes(x = (Date), y = ifelse(delta > 0, delta * 100, 0)),
            fill =
              "#AED6F1") +
  theme_bw() +
  labs(
    x = "",
    y = "Difference (10 year-3 month) yield in %",
    caption = "Source: St.Louis Federal Reserve Economic Database (FRED)",
    title = "Yield Curve inversion: 10-year minus 3-month U.S. Treasury rates",
    subtitle = "Difference in % points monthly averages.\nShaded area corresponds to recessions"
  ) +
  theme(plot.title = element_text(face = "bold"))+
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y"), limits = c(as.Date("1960-12-01"), as.Date("2021-01-01")))


  






             
```



