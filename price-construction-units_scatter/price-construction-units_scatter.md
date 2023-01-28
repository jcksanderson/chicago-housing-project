Housing Price Index, New Construction, and Aver Units Scatterplot
================

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

``` r
price_index_data <- read_csv(here("CSV", "CSV_IHS-price-index-data.csv")) %>% 
  gather(2:17, key = "PUMA", value = "Index") %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, "Englewood/West Englewood", "Englewood, West Englewood")) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, "North & South Lawndale", "North Lawndale, South Lawndale")) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, "East & West Garfield Park", "East Garfield Park, West Garfield Park")) %>% 
  mutate(PUMA = str_replace_all(.$PUMA, " & ", ", ")) 
```

    Rows: 102 Columns: 17
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    dbl  (16): Edgewater, Uptown & Rogers Park, Lake View & Lincoln Park, West R...
    date  (1): Date

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
new_construction_permits <- fread(here("CSV", "CSV_chicago_building-permits.csv")) %>% 
  select("PERMIT_TYPE", "ISSUE_DATE", "REPORTED_COST", "COMMUNITY_AREA", "XCOORDINATE", "YCOORDINATE", "LATITUDE", "LONGITUDE") %>% 
  filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>%
  filter(!is.na(COMMUNITY_AREA)) %>% 
  mutate(ISSUE_DATE = mdy(.$ISSUE_DATE)) %>% 
  arrange(ISSUE_DATE)
```

``` r
price_index_data_2012 <- price_index_data %>% 
  filter(Date >= "2012-01-01")

price_index_data_diff_2012 <- price_index_data_2012 %>% 
  group_by(PUMA) %>% 
  summarize(diff = last(Index) - first(Index))

comm_area_vector <- c()
for (i in 1:5) {
  comm_area_vector[i] <-glue("community_area_{i}")
}

puma_and_area_diff_2012 <- price_index_data_diff_2012 %>% 
  mutate(community_area = PUMA) %>% 
  separate(., community_area, comm_area_vector, ", ") %>% 
  pivot_longer(3:7) %>% 
  drop_na()
```

    Warning: Expected 5 pieces. Missing pieces filled with `NA` in 12 rows [1, 3, 4,
    5, 6, 7, 8, 9, 10, 12, 15, 16].

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

community_area_names <- read_csv(here("CSV", "CSV_community_area_names.csv"))
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

    Warning: Expected 5 pieces. Missing pieces filled with `NA` in 12 rows [1, 3, 4,
    5, 6, 7, 8, 9, 10, 12, 15, 16].

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

``` r
scatterplot_data_2012 <- left_join(puma_and_area_diff_2012, comm_area_cumu_permits_2012, by = c("value" = "Name")) %>% 
  group_by(PUMA) %>% 
  summarize(total_permits = sum(CUMULATIVE_PERMITS), diff = mean(diff))

scatterplot_data_2018 <- left_join(puma_and_area_diff_2018, comm_area_cumu_permits_2018, by = c("value" = "Name")) %>% 
  group_by(PUMA) %>% 
  summarize(total_permits = sum(CUMULATIVE_PERMITS), diff = mean(diff))
```

``` r
tract_housing_units <- read_sf(here("SHP", "SHP_chicago-tracts_housing-stock")) %>% 
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
```

``` r
chicago_pumas <- read_sf(here("SHP", "SHP_illinois_PUMAs", "tl_2020_17_puma10.shp")) %>% 
  filter(grepl("Chicago City", NAMELSAD10) & !grepl("Cook", NAMELSAD10)) %>% 
  st_set_crs(st_crs(tract_housing_centers)) %>% 
  mutate(NAMELSAD10 = str_match(.$NAMELSAD10, ".*\\-\\-(.*)")[,2]) %>% 
  mutate(NAMELSAD10 = str_remove(.$NAMELSAD10, " PUMA")) %>% 
 mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, "Englewood/West Englewood", "Englewood, West Englewood")) %>% 
  mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, "North & South Lawndale", "North Lawndale, South Lawndale")) %>% 
  mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, "East & West Garfield Park", "East Garfield Park, West Garfield Park")) %>% 
  mutate(NAMELSAD10 = str_replace_all(.$NAMELSAD10, " & ", ", ")) %>% 
  mutate(STATEFP10 = seq(1:17))
```

    Warning: st_crs<- : replacing crs does not reproject data; use st_transform for
    that

``` r
puma_id <- chicago_pumas %>% 
  data.frame(.) %>% 
  select(id = STATEFP10, name = NAMELSAD10)

tracts_within <- st_within(tract_housing_centers, chicago_pumas, sparse = FALSE) %>% 
  data.frame(.)

puma_average_units <- cbind(tract_housing_centers, tracts_within) %>% 
  pivot_longer(4:20, names_to = "PUMA", values_to = "within") %>% 
  filter(within == TRUE) %>% 
  select(-within) %>% 
  group_by(PUMA) %>% 
  mutate(average_units = mean(average_units, na.rm = TRUE)) %>% 
  data.frame(.) %>% 
  select(id = PUMA, average_units) %>% 
  distinct() %>% 
  mutate(id = as.double(str_remove_all(id, "X"))) %>% 
  arrange(id) %>% 
  left_join(., puma_id, by = c("id" = "id")) %>% 
  select(PUMA = name, average_units)
```

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
    title = "New Construction Permits vs. Housing Price Index",
    subtitle = "2012-2022, Point Size & Color as Average Units per Building",
    color = "Average Units per Building",
    x = "New Construction Permits Issued",
    y = "Housing Price Index Increase"
  ) +
  scale_color_viridis_b(
    option = "viridis", 
    breaks = c(5, 10, 20, 40)
  ) +
  scale_size(
    guide = "none"
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
                              size = 20,
                              color = "white"),
    plot.subtitle = element_text(family = "Space Grotesk Bold",
                                 size = 14,
                                 color = "gray90"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "gray10"),
    legend.title = element_text(family = "Space Grotesk",
                                color = "white",
                                vjust = 0.8),
    legend.text = element_text(family = "Space Grotesk",
                             color = "white"),
    legend.margin = margin(0, 0, 0, 0)
  ) 
```

    `geom_smooth()` using formula = 'y ~ x'

![](price-construction-units_scatter_files/figure-commonmark/2012%20Scatter-1.png)

``` r
avg_units_scatter_2018 <- left_join(scatterplot_data_2018, puma_average_units, by = c("PUMA" = "PUMA"))

ggplot(
  avg_units_scatter_2018,
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
  coord_cartesian(
    ylim = c(0, 300)
  ) +
  labs(
    title = "New Construction Permits vs. Housing Price Index",
    subtitle = "2018-2022, Point Size & Color as Average Units per Building",
    color = "Average Units per Building",
    x = "New Construction Permits Issued",
    y = "Housing Price Index Increase"
  ) +
  scale_color_viridis_b(
    option = "viridis", 
    breaks = c(5, 10, 20, 40)
  ) +
  scale_size(
    guide = "none"
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
                              size = 20,
                              color = "white"),
    plot.subtitle = element_text(family = "Space Grotesk Bold",
                                 size = 14,
                                 color = "gray90"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "gray10"),
    legend.title = element_text(family = "Space Grotesk",
                                color = "white",
                                vjust = 0.8),
    legend.text = element_text(family = "Space Grotesk",
                             color = "white"),
    legend.margin = margin(0, 0, 0, 0)
  ) 
```

    `geom_smooth()` using formula = 'y ~ x'

![](price-construction-units_scatter_files/figure-commonmark/2018%20Scatter-1.png)

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
