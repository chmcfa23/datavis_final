---
title: "Exploring Bigfoot Sightings in North America"
format: gfm
---

## Background

Bigfoot is one of, if not the most famous North American cryptid. Sasquatch sightings dating back to the 1960s have been monitored on the BFRO's (Bigfoot Field Researchers' Organization) website, BFRO.org. TidyTueday scraped this data and provided it in their weekly Github blog, where I downloaded it. This dataset includes variables exact location (latitude, longitude, county, state) variables, variables that describe the weather conditions at the time of the encounter (such as precipitation type, visibility, and intensity of precipitation), and the BFRO's official classification of the encounter's credibility, with Class A being the best, and Class C being the worst (Class B in the middle).

The data is loaded in here:

```{r}
#| message: false
#| warning: false

# install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2022-09-13')
tuesdata <- tidytuesdayR::tt_load(2022, week = 37)

bigfoot <- tuesdata$bigfoot

# Or read in the data manually

bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-09-13/bigfoot.csv')

## data prep
#| message: false
#| warning: false
# install.packages("leaflet")
# install.packages("tigris")
# install.packages("USABoundaries")
# install.packages("ggeffects")
library(leaflet)
library(readr)
library(dplyr)
library(lubridate)
library(tigris)
library(sf)
library(ggplot2)
library(broom)
library(ggeffects)
library(plotly)

```

## Goals

My goal with this project was to characterize and describe Bigfoot sightings in the U.S. with the use of the variables listed above. I hoped to achieve this by generating a number of visualizations and interpreting them. My three most useful visualizations were as follows; a map of the 48 contiguous United States + Alaska with each state shaded according to how many sightings have occurred in it.

## USA Sightings Density Map

```{r}
states <- states(cb = TRUE)

library(USAboundaries)

states <- us_states() %>%
  filter(name != "Hawaii", name != "Puerto Rico") %>%
    st_crop(xmin = 130, xmax = -60,
            ymin = 0, ymax = 70)

sightings_by_state <- bigfoot %>%
  group_by(state) %>%
  summarize(
    n_sightings = n()
  )

sightings_joined <- full_join(states, sightings_by_state, join_by(name == state))

ggplot(data = sightings_joined) +
  geom_sf(aes(fill = n_sightings)) +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(-170, -60), ylim = c(20, 73)) +
  labs(fill = "Number of Sightings") +
  theme_void()
```

Washington appears to have *by far* the most Bigfoot sightings, clocking in at over 600. What makes Washington different from other states? How dramatic is its lead? For my next visualization, I want to investigate these questions by generating a line graph that depicts the rise of bigfoot sightings over time in the top five states with the most sightings over all. We conveniently already have a date variable in the original dataset, so it's really just a matter of figuring out how to generate a line plot that shows the **cumulative** number of cases at each "point."

## 

```{r}
sightings_over_time <- bigfoot %>%
  filter(!is.na(date)) %>%
  filter(state == "Washington"|
         state == "California" |
         state == "Ohio" |
         state == "Florida" |
         state == "Oregon") %>%
  mutate(
    date = ymd(date)
  ) %>%
  group_by(state, date) %>%
  summarize(
    n_sightings = n()
  ) %>%
  mutate(
    cumulative_sightings = cumsum(n_sightings)
    )


ggplot(sightings_over_time, aes(x = date, 
                                y = cumulative_sightings, 
                                color = state)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Bigfoot Sightings Over Time",
    x = "Date",
    y = "Total Sightings",
    color = "State"
  ) + 
  scale_color_manual(values = c("red","seagreen2","slateblue2","thistle2","yellow")) +
  scale_x_date(limit=c(as.Date("1950-01-01"),as.Date("2026-04-24"))) +
  labs(caption = "*7 sightings have been excluded as they occured before 01-01-1950. Axis scaled for ease of reading.") +
  theme_minimal() 
```

It appears from this visualization that Washington has always more or less led the United States in the number of Bigfoot sightings, with no signs of that rate slowing any time soon. The other interesting thing to note about this visualization is that, around the same time (roughly 2010), both Oregon and California have, somewhat dramatically, plateaued.More research would have to be done into whether this is a significant observation and if so, what caused this plateau. But with a limited dataset, time, and skills, just observing this phenomenon will have to be enough.

Lastly, I performed a linear regression analysis to determine if the BFRO's assessment of a sighting's credibility can be predicted by the climate variables at the time of said sighting.

```{r}
bigfoot_quality <- bigfoot %>%
  filter(state == "Washington") %>%
  mutate(
    quality = case_when(
      classification == "Class A" ~ 1,
      classification ==  "Class B"~ 2,
      classification == "Class C" ~ 3
  ),
    precip_type = factor(case_when(
      precip_type == "rain" ~ "rain",
      precip_type == "snow" ~ "snow",
      is.na(precip_type) ~ "none"
    ))
  ) 

mod_quality <- lm(quality ~ visibility + precip_intensity + precip_type + precip_type:precip_intensity, data = bigfoot_quality)

pred_quality <- ggpredict(mod_quality, terms = c("visibility [fivenum]",
                              "precip_intensity [fivenum]",
                              "precip_type")) %>%
  as.data.frame(terms_to_colnames = TRUE) %>%
  as_tibble()

plot_quality <- ggplot(data = pred_quality, aes(x=visibility, y=predicted))+
  geom_line(aes(color = precip_intensity), linewidth = 1.5) +
  facet_wrap(~precip_type) +
  scale_color_brewer(palette = "Pastel1") +
  theme_dark() +
  geom_point(data = bigfoot_quality, aes(y=quality, 
                                         color = precip_type,
                                         Title = `title`,
                                         County = county,
                                         Classification = classification
                                         ), alpha = 0.5) +
  theme(base_size = 24)

ggplotly(plot_quality, tooltip = c("Title", "County", "Classification"))
```

The major observation from this visualization was that snowy sightings had significantly more credibility than rainy sightings, almost being comparable to clear weather sightings ("1" denotes Class A sightings, while "3" denotes Class C sightings). Interestingly, the heavier the snow, the more credible the encounter, per the BFRO. There is not enough information to determine exactly why this is, but my hypothesis is that snowy footprints might be considered evidence to back a sighting.
