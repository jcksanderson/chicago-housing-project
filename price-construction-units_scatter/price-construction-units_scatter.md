# Housing Price Index, New Construction, and Average Units Scatterplot

## Introduction

A common talking point when discussing housing policy is about
gentrification and development. Specifically, many people argue that new
construction fuels increases in housing prices, as typified by the
“gentrification building.” But basic principles of supply and demand
should suggest that if the supply is high enough, no matter the demand,
the prices shouldn’t increase. So does new construction fuel housing
price increases, or is there more to the story?

The easiest way to find if a pattern exists here is simply to look for
patterns in data. DePaul University’s Institute for Housing Studies
provides data on housing price index changes in Chicago Pubic Use
Microdata Areas (PUMAs) and the Chicago Data Portal provides a list of
building permits issued in the city, both of which are quantifications
of the metrics we want to measure. I also have pulled data from the US
Census about the average units per residential building in Chicago’s
census tracts, to see if there is any differences between the PUMAs in
that regard.

------------------------------------------------------------------------

## Code

The first step was to import packages (and change the knitr root
directory because there’s no easy way to do it in Quarto as of now).

``` r
# packages
library(tidyverse)
library(sf)
library(lubridate)
library(data.table)
library(glue)

# directory setup
library(here)
library(knitr)

# change root directory to Rproj file location;
# that's where all files are pulled from
knitr::opts_knit$set(root.dir = here())
```

### Import Data

I then imported the price index data. I had already cleaned it up a bit
in a Google Sheet, but to get all the PUMA names in full I turned the
/’s and &’s to commas.

``` r
price_index_data <- read_csv(here("data", "CSV_IHS-price-index-data.csv")) %>% 
  gather(2:17, key = "PUMA", value = "Index") %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, "Englewood/West Englewood", "Englewood, West Englewood")) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, "North & South Lawndale", "North Lawndale, South Lawndale")) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, "East & West Garfield Park", "East Garfield Park, West Garfield Park")) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, " & ", ", ")) 
```

Next I imported the new construction permit dataset as a CSV, downloaded
from the Chicago Data Portal (it’s a giant file so it takes a while). To
clean it up, I filtered the permit to be for new construction and turned
the date column into the ISO-8601 standard with Lubridate.

``` r
new_construction_permits <- fread(here("data", "CSV_chicago_building-permits.csv")) %>% 
  select("PERMIT_TYPE", "ISSUE_DATE", "REPORTED_COST", "COMMUNITY_AREA", "XCOORDINATE", "YCOORDINATE", "LATITUDE", "LONGITUDE") %>% 
  filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>%
  filter(!is.na(COMMUNITY_AREA)) %>% 
  mutate(ISSUE_DATE = mdy(.$ISSUE_DATE)) %>% 
  arrange(ISSUE_DATE)
```

### Calculations

After that I calculated the increase in the housing price index from
2012 to 2022. The first step is to filter the price index data to be
2012 onward, after which we can group by the PUMA and subtract the first
(2012) from the last (2022).

This data, though, is still in terms of each PUMA, and the permit data
is in terms of community areas. To aid the future joining of the
dataframes, I added extra columns that listed the community areas within
each PUMA, then used `pivot_longer()` to lengthen the dataframe (see
result below).

``` r
price_index_data_2012 <- price_index_data %>% 
  filter(Date >= "2012-01-01")

price_index_data_diff_2012 <- price_index_data_2012 %>% 
  group_by(PUMA) %>% 
  summarize(diff = last(Index) - first(Index))

# for concrete naming convention during the separate()
comm_area_vector <- c()
for (i in 1:5) {
  comm_area_vector[i] <-glue("community_area_{i}")
}

puma_and_area_diff_2012 <- price_index_data_diff_2012 %>% 
  mutate(community_area = PUMA) %>% 
  separate(., community_area, comm_area_vector, ", ") %>% 
  pivot_longer(3:7) %>% 
  drop_na()

head(puma_and_area_diff_2012)
```

    # A tibble: 6 × 4
      PUMA                                                      diff name      value
      <chr>                                                    <dbl> <chr>     <chr>
    1 Ashburn, Washington Heights, Morgan Park, Beverly         118. communit… Ashb…
    2 Ashburn, Washington Heights, Morgan Park, Beverly         118. communit… Wash…
    3 Ashburn, Washington Heights, Morgan Park, Beverly         118. communit… Morg…
    4 Ashburn, Washington Heights, Morgan Park, Beverly         118. communit… Beve…
    5 Auburn Gresham, Roseland, Chatham, Avalon Park, Burnside  127. communit… Aubu…
    6 Auburn Gresham, Roseland, Chatham, Avalon Park, Burnside  127. communit… Rose…

After completing the price index manipulations, I started working with
the new construction permits. First is filtering the dates to post-2012,
then making a column of 1’s so I could use `cumsum()` and create a
cumulative permits column. Finally, I added the names of the community
areas to the dataframe.

``` r
new_constr_permits_2012 <- new_construction_permits %>% 
  filter(ISSUE_DATE >= "2012-01-01")

new_constr_permits_2012_cumu <- new_constr_permits_2012 %>% 
  mutate(PERMIT_TYPE = "1") %>% 
  group_by(COMMUNITY_AREA) %>% 
  mutate(CUMULATIVE_PERMITS = cumsum(PERMIT_TYPE))

comm_area_cumu_permits_2012 <- new_constr_permits_2012_cumu %>% 
  group_by(COMMUNITY_AREA) %>% 
  summarize(CUMULATIVE_PERMITS = last(CUMULATIVE_PERMITS)) %>% 
  filter(COMMUNITY_AREA != 0)

community_area_names <- read_csv(here("data", "CSV_community_area_names.csv"))
```

    Rows: 77 Columns: 2
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr (1): Name
    dbl (1): Number

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
comm_area_cumu_permits_2012 <- left_join(comm_area_cumu_permits_2012, community_area_names, by = c("COMMUNITY_AREA" = "Number"))
```

And the same steps apply for the 2018 data!

``` r
price_index_data_diff_2018 <- price_index_data %>% 
  filter(Date >= "2018-01-01") %>% 
  group_by(PUMA) %>% 
  summarize(diff = last(Index) - first(Index))

puma_and_area_diff_2018 <- price_index_data_diff_2018 %>% 
  mutate(community_area = PUMA) %>% 
  separate(., community_area, comm_area_vector, ", ") %>% 
  pivot_longer(3:7) %>% 
  drop_na()
```

``` r
new_constr_permits_2018 <- new_constr_permits_2012 %>% 
  filter(ISSUE_DATE >= "2018-01-01")

new_constr_permits_2018_cumu <- new_constr_permits_2018 %>% 
  mutate(PERMIT_TYPE = "1") %>% 
  group_by(COMMUNITY_AREA) %>% 
  mutate(CUMULATIVE_PERMITS = cumsum(PERMIT_TYPE))

comm_area_cumu_permits_2018 <- new_constr_permits_2018_cumu %>% 
  group_by(COMMUNITY_AREA) %>% 
  summarize(CUMULATIVE_PERMITS = last(CUMULATIVE_PERMITS)) %>% 
  filter(COMMUNITY_AREA != 0)

comm_area_cumu_permits_2018 <- left_join(comm_area_cumu_permits_2018, community_area_names, by = c("COMMUNITY_AREA" = "Number"))
```

To get the data that will be used to plot each scatterplot point, all
that must be done is a `left_join()` for each of the years.

``` r
scatterplot_data_2012 <- left_join(puma_and_area_diff_2012, comm_area_cumu_permits_2012, by = c("value" = "Name")) %>% 
  group_by(PUMA) %>% 
  summarize(total_permits = sum(CUMULATIVE_PERMITS), diff = mean(diff))

scatterplot_data_2018 <- left_join(puma_and_area_diff_2018, comm_area_cumu_permits_2018, by = c("value" = "Name")) %>% 
  group_by(PUMA) %>% 
  summarize(total_permits = sum(CUMULATIVE_PERMITS), diff = mean(diff))
```

### Census Data

Now it’s time for the census data. Step one is importing the shapefile
and making the column names understandable. I used a very crude method
for finding the average of each tract, simply by multiplying the number
in each column with the column’s average units (e.g. `[5 + 9] / 2`),
then dividing by the total units. Is it perfect? No. But it does capture
the bigger picture, which works for basic exploratory analysis. I then
added a column which had the center of each tract, for future use.

``` r
tract_housing_units <- read_sf(here("data", "SHP_chicago-tracts_housing-stock"), quiet = TRUE) %>% 
  select(geoid, 
         name, 
         geometry, 
         total_units = B25024001, 
         one_detached = B25024002, 
         one_attached = B25024003, 
         two = B25024004, 
         three_or_four = B25024005, 
         five_to_nine = B25024006, 
         ten_to_nineteen = B25024007, 
         twenty_to_fortynine = B25024008, 
         fifty_plus = B25024009)

tract_housing_centers <- tract_housing_units %>% 
  mutate(average_units = (one_detached +
                          one_attached +
                          (two * 2) +
                          (three_or_four * 3.5) +
                          (five_to_nine * 7) +
                          (ten_to_nineteen * 14.5) +
                          (twenty_to_fortynine * 34.5) +
                          (fifty_plus * 100)) / total_units) %>% 
  mutate(center = st_centroid(geometry), .after = "geometry") %>% 
  select(geoid, name, center, average_units)

head(tract_housing_centers)
```

    Simple feature collection with 6 features and 3 fields
    Active geometry column: geometry
    Geometry type: POLYGON
    Dimension:     XY
    Bounding box:  xmin: -87.68465 ymin: 41.99819 xmax: -87.6508 ymax: 42.02314
    Geodetic CRS:  WGS 84
    # A tibble: 6 × 5
      geoid  name                center average_units                       geometry
      <chr>  <chr>          <POINT [°]>         <dbl>                  <POLYGON [°]>
    1 14000… Cens… (-87.66983 42.02126)          21.3 ((-87.6772 42.02294, -87.6762…
    2 14000… Cens… (-87.68015 42.01601)          20.7 ((-87.68465 42.01948, -87.684…
    3 14000… Cens… (-87.67333 42.01605)          25.7 ((-87.67683 42.01941, -87.676…
    4 14000… Cens… (-87.66654 42.01594)          38.7 ((-87.67133 42.01937, -87.671…
    5 14000… Cens… (-87.65717 42.00544)          34.8 ((-87.66345 42.01283, -87.663…
    6 14000… Cens… (-87.66396 42.00941)          41.4 ((-87.66592 42.01279, -87.665…

I imported the Illinois PUMAs shapefile, limited them to Chicago, fixed
their names, and numbered them (I saved just PUMA IDs and names for
later). Then I used `st_within()` to calculate what census tracts are
inside which PUMAs.

``` r
chicago_pumas <- read_sf(here("data", "SHP_illinois_PUMAs", "tl_2020_17_puma10.shp")) %>% 
  filter(grepl("Chicago City", NAMELSAD10) & !grepl("Cook", NAMELSAD10)) %>% 
  st_set_crs(st_crs(tract_housing_centers)) %>% 
  mutate(NAMELSAD10 = str_match(.$NAMELSAD10, ".*\\-\\-(.*)")[,2]) %>% 
  mutate(NAMELSAD10 = str_remove(.$NAMELSAD10, " PUMA")) %>% 
 mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, "Englewood/West Englewood", "Englewood, West Englewood")) %>% 
  mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, "North & South Lawndale", "North Lawndale, South Lawndale")) %>% 
  mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, "East & West Garfield Park", "East Garfield Park, West Garfield Park")) %>% 
  mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, " & ", ", ")) %>% 
  mutate(STATEFP10 = seq(1:17))

puma_id <- chicago_pumas %>% 
  tibble(.) %>% 
  select(id = STATEFP10, name = NAMELSAD10)

tracts_within <- st_within(tract_housing_centers, chicago_pumas, sparse = FALSE) %>% 
  data.frame(.)
```

The following is probably the most insane code chunk in this file.  
I bound the tract center and tract within data together, then melted it
and selected the rows only when the tract is within the PUMA. Then I
took the mean of the average units per PUMA and added the PUMA names to
their IDs (which is why I kept just the names and IDs).

``` r
#label: PUMA Average Housing Units
puma_average_units <- cbind(tract_housing_centers, tracts_within) %>% 
  pivot_longer(4:20, names_to = "PUMA", values_to = "within") %>% 
  filter(within == TRUE) %>% 
  group_by(PUMA) %>% 
  mutate(average_units = mean(average_units, na.rm = TRUE)) %>% 
  tibble(.) %>% 
  select(id = PUMA, average_units) %>% 
  distinct() %>% 
  mutate(id = as.double(str_remove_all(id, "X"))) %>% 
  left_join(., puma_id, by = c("id" = "id")) %>% 
  select(PUMA = name, average_units) %>% 
  tbl_df()
```

    Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ℹ Please use `tibble::as_tibble()` instead.

``` r
head(puma_average_units)
```

    # A tibble: 6 × 2
      PUMA                                              average_units
      <chr>                                                     <dbl>
    1 Edgewater, Uptown, Rogers Park                            40.9 
    2 West Ridge, Lincoln Square, North Center                  11.7 
    3 Lake View, Lincoln Park                                   30.3 
    4 Near North Side, Loop, Near South Side                    82.2 
    5 Portage Park, Dunning, Jefferson Park                      6.14
    6 Irving Park, Albany Park, Forest Glen, North Park          9.39

### Plots

All of that done, it’s as simple as using a join on the scatterplot data
and average units to finally get the plot!

``` r
avg_units_scatter_2012 <- left_join(scatterplot_data_2012, puma_average_units, by = c("PUMA" = "PUMA"))

ggplot(avg_units_scatter_2012,
       aes(x = total_permits, 
           y = diff)
  ) +
  geom_point(
    aes(size = average_units,
        color = average_units)
  ) +
  stat_smooth(
    geom = "line",
    method = "lm",
    color = "red",
    linewidth = 1,
    alpha = 0.5
  ) +
  labs(
    title = "New Construction Permits vs. Housing Price Increase",
    subtitle = "In Chicago's Public Use Microdata Areas",
    color = "Average Units per Building",
    x = "New Construction Permits Issued",
    y = "Housing Price Index Increase",
    caption = "2012-2022"
  ) +
  scale_color_viridis_c(
    name = "Average Housing Units per Building",
    breaks = c(5, 10, 20, 30, 40)
  ) +
  scale_size_continuous(
    name = "Average Housing Units per Building",
    range = c(1, 10),
    breaks = c(5, 10, 20, 30, 40)
  ) +
  guides(
    color = "legend",
    size = "legend"
  ) +
  theme(
    plot.background = element_rect(fill = "gray10",
                                   color = "gray10"),
    panel.background = element_rect(color = "white",
                                    linewidth = 2,
                                    fill = NA),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(family = "Space Grotesk",
                             color = "white"),
    axis.title = element_text(family = "Space Grotesk Bold",
                              color = "white"),
    plot.title = element_text(family = "Space Grotesk Bold",
                              size = 18,
                              color = "white"),
    plot.subtitle = element_text(family = "Space Grotesk Bold",
                                 size = 13,
                                 color = "gray90"),
    plot.caption = element_text(family = "Space Grotesk Bold",
                                 size = 10,
                                 color = "gray90"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "gray10"),
    legend.title = element_text(family = "Space Grotesk",
                                color = "white",
                                vjust = 0.8),
    legend.text = element_text(family = "Space Grotesk",
                             color = "white"),
    legend.margin = margin(0, 0, 0, 0),
    legend.key = element_blank(),
  ) 
```

    `geom_smooth()` using formula = 'y ~ x'

![](price-construction-units_scatter_files/figure-commonmark/2012%20Scatter-1.png)

And the same goes for 2018 (the code is hidden).

![](price-construction-units_scatter_files/figure-commonmark/2018%20Scatter-1.png)

------------------------------------------------------------------------

## Discussion

So it seems that the amount of new construction permits issued has no
effect or even a negative effect on the increase in housing prices. This
finding is more consistent with the supply and demand idea, as more
units should mean less price gouging.

``` r
summary(lm(diff ~ total_permits*average_units, data = avg_units_scatter_2012))
summary(lm(diff ~ total_permits*average_units, data = avg_units_scatter_2018))

summary(lm(diff ~ total_permits, data = avg_units_scatter_2018))
```

The statistics (hidden) also give somewhat strong explanatory power to
the number of permits and average units. Both 2012 and 2018 having
multiple r-squared values over 0.3, and the p-value on just the permit’s
coefficient in 2018 is a technically significant 0.0467. But in all
likelihood, there are too many variables not controlled when looking at
just these three datasets, so visualizations are likely a better option
for finding trends.

------------------------------------------------------------------------

## Sources

[IHS Price Index](https://price-index.housingstudies.org/)  
[Chicago Building
Permits](https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu)  
[Building Unit Census
Data](https://censusreporter.org/data/table/?table=B25024&geo_ids=16000US1714000,140%7C16000US1714000&primary_geo_id=16000US1714000)

## Bonus

(Here’s a bonus plot of the cumulative permits too.)

``` r
ggplot(new_constr_permits_2012_cumu, aes(x = ISSUE_DATE, 
                                      y = CUMULATIVE_PERMITS,
                                      group = COMMUNITY_AREA)
  ) +
  geom_line(aes(color = CUMULATIVE_PERMITS),
                show.legend = FALSE) + 
  labs(
    title = "Cumulative New Construction Permits Since 2012",
    subtitle = "By Community Area",
    x = "Date",
    y = "Cumulative Permits Issued"
  ) +
  scale_color_viridis_c() +
  theme(
    plot.background = element_rect(fill = "gray10",
                                   color = "gray10"),
    panel.background = element_rect(color = "white",
                                    linewidth = 2,
                                    fill = NA),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(family = "Space Grotesk",
                             color = "white"),
    axis.title = element_text(family = "Space Grotesk Bold",
                              color = "white"),
    plot.title = element_text(family = "Space Grotesk Bold",
                              size = 20,
                              color = "white"),
    plot.subtitle = element_text(family = "Space Grotesk Bold",
                                 size = 14,
                                 color = "gray90")
  ) 
```

![](price-construction-units_scatter_files/figure-commonmark/Cumulative%20New%20Construction%20Permits%20Plot-1.png)
