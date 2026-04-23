---
  title: "Week 1 ESM 262: Quarto Data Analysis in the Southern California Sierra"
author: "Lauren Chetley & Kalena Sapp"
date: "April 20th, 2026" 
format: pdf
editor: visual
execute:
  echo: true
warning: false
message: false
---
  
  ## Analysis 1: Correlation between Water Availability (Precip) and Net Primary Productivity (NPP)
  
  Motivation The analysis looks at the correlation between annual total precipitation and plant net primary productivity. We wanted to determine whether precipitation is a significant driver of plant growth, using a linear regression model.

Results The analysis confirmed our hypothesis that precipitation does positively influence primary productivity, however a correlation coefficient of 0.15 indicates a very weak relationship suggesting that precipitation may not be the main driving factor for plant growth.

```{r}
1 + 1

#| label: load-packages
#| include: false
#| echo: false
a = 2 * 2

library(here)
library(janitor)
library(tidyverse)
library(dplyr)
library(ggplot2)


year_totals<-p301_data |>
  group_by(year)|>
  filter(streamflow>0)|>
  summarise(
    precip=sum(precip, na.rm=TRUE), streamflow=sum(streamflow, na.rm=TRUE))
ggplot(year_totals,aes(x=precip,y=streamflow))+
  geom_point(size=2, color="royalblue")+
  geom_smooth(method="lm", color="black")+
  labs(
    title="Annual Totals: Streamflow vs Precipitation",
    x="Annual Total Precipitation (mm)",
    y="Annual Total Streamflow (m/m²/day)"
  )

--
  
  ```

```{r}

library(here)
library(janitor)
library(tidyverse)
library(dplyr)
library(ggplot2)

#analysis #2: Does temperature impact carbon storage in plants?
p301_data <- p301_data |> 
  mutate(mean_temp = (tmax + tmin) / 2)

ann_npre <-p301_data |>
  group_by(year) |>
  summarize(ann_precip = sum(precip, na.rm = TRUE),
            ann_npp = sum(NPP, na.rm = TRUE))
cor_value_annual <- cor(ann_npre$ann_precip, ann_npre$ann_npp,
                        use = "complete.obs")
ggplot(ann_npre, aes (x=ann_precip, y = ann_npp)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_smooth(method = "lm",color = "black", se = TRUE ) +
  labs(title = "Average Annual Precipitation and Total Plant Growth",
       subtitle = paste("Annual Correlation Coefficient:",
                        round(cor_value_annual, 2)),
       x = "Annual Precipitation (mm)", y = "Annual NPP (gC/m²/yr)") +
  theme_minimal()


```

The `echo: false` option disables the printing of code (only output is displayed).

```{r}

install.packages("usethis")
usethis::git_sitrep()
library(usethis)
use_git()
usethis::use_github()

Change this
commit working?
  
How about now?
  


                  
```