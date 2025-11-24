EDA
================

- [EDA for Household Energy
  Insecurity](#eda-for-household-energy-insecurity)
- [EDA for Census](#eda-for-census)
- [EDA for Census and Household Energy
  Insecurity](#eda-for-census-and-household-energy-insecurity)

# EDA for Household Energy Insecurity

``` r
RECS_EI = read_csv("clean data/RECS_Energy_Insecurity.csv")
```

    ## Rows: 51 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): state
    ## dbl (4): PO_number, PO_proportion, EI_number, EI_proportion
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
RECS_EI_code = read_xlsx("clean data/CodeBook_State_Level_Energy_Insecurity.xlsx")

EI_long = RECS_EI |>
  pivot_longer(cols = c(EI_proportion, PO_proportion),
               names_to = "Type", values_to = "Value")

plot_EI = ggplot(EI_long, aes(x = reorder(state, Value), y = Value, color = Type)) +
  geom_point(size = 3) +
  geom_line(aes(group = state), color = "gray60") +
  coord_flip() +
  labs(title = "Difference Between EI and PO for Each State",
       x = "State",
       y = "Proportion") +
  theme_minimal()

rank_table =
  RECS_EI |>
  mutate(
    EI_rank = dense_rank(desc(EI_proportion)),
    PO_rank = dense_rank(desc(PO_proportion))
  ) |>
  select(state, EI_rank, PO_rank) |>
  arrange(EI_rank)

RECS_EI_range_mean = RECS_EI |>
  summarise(
    mean_EI = mean(EI_proportion),
    range_EI = paste0(round(min(EI_proportion),3), "–", round(max(EI_proportion),3)),
    mean_PO = mean(PO_proportion),
    range_PO = paste0(round(min(PO_proportion),3), "–", round(max(PO_proportion),3))
  )
```

First, I calculated the average and range of both EI and PO proportions
across all 50 states plus the District of Columbia. The average level of
energy insecurity (EI) is about 0.256, with values ranging from
0.15–0.4. For power outage proportion (PO), the average is roughly
0.182, and the range spans from 0.04–0.6.

The distribution plots are shown below.

``` r
p_dist_EI =
  RECS_EI |>
  ggplot(aes(x = EI_proportion)) +
  geom_histogram(bins = 20, fill = "skyblue", alpha = 0.8) +
  labs(title = "Distribution of EI Proportion",
       x = "EI proportion",
       y = "Count of states") +
  theme_minimal()

p_dist_EI
```

![](EDA_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
p_dist_PO =
  RECS_EI |>
  ggplot(aes(x = PO_proportion)) +
  geom_histogram(bins = 20, fill = "orange", alpha = 0.8) +
  labs(title = "Distribution of PO Proportion",
       x = "PO proportion",
       y = "Count of states") +
  theme_minimal()

p_dist_PO
```

![](EDA_files/figure-gfm/unnamed-chunk-2-2.png)<!-- --> Both
distributions are right-skewed and do not appear to be multimodal. This
suggests that most states fall within the low-to-moderate range of EI or
PO, while a small number of states experience disproportionately high
levels of energy insecurity or power outages.

To examine whether the two variables are related, I also put them
together. The plot for both is below.
![](EDA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

I have also computed the ranks of EI and PO for each state, the results
are shown in the table below.

| state                | EI_rank | PO_rank |
|:---------------------|--------:|--------:|
| Mississippi          |       1 |       5 |
| West Virginia        |       2 |       9 |
| Arkansas             |       3 |      12 |
| Alabama              |       4 |       6 |
| Oklahoma             |       4 |       5 |
| Texas                |       4 |      11 |
| Georgia              |       5 |       7 |
| Kentucky             |       5 |      18 |
| Louisiana            |       5 |       1 |
| South Carolina       |       6 |      13 |
| California           |       7 |      14 |
| Indiana              |       8 |      12 |
| Michigan             |       8 |       8 |
| Nevada               |       8 |      24 |
| New York             |       9 |      14 |
| Arizona              |      10 |      26 |
| Connecticut          |      10 |       2 |
| Missouri             |      10 |      18 |
| North Carolina       |      10 |      11 |
| Tennessee            |      10 |      15 |
| Delaware             |      11 |      17 |
| Ohio                 |      11 |      17 |
| Alaska               |      12 |      22 |
| Kansas               |      12 |      20 |
| New Jersey           |      12 |       4 |
| New Mexico           |      12 |      21 |
| Virginia             |      12 |      14 |
| Colorado             |      13 |      23 |
| Hawaii               |      13 |      27 |
| Montana              |      13 |      19 |
| Pennsylvania         |      13 |      14 |
| Florida              |      14 |      16 |
| Illinois             |      14 |      18 |
| Maine                |      14 |       2 |
| Maryland             |      14 |      23 |
| New Hampshire        |      14 |      11 |
| Rhode Island         |      14 |      10 |
| Massachusetts        |      15 |      16 |
| Washington           |      15 |      13 |
| North Dakota         |      16 |      25 |
| Oregon               |      16 |      14 |
| Wyoming              |      16 |      19 |
| South Dakota         |      17 |      24 |
| Wisconsin            |      17 |      21 |
| Idaho                |      18 |      18 |
| Utah                 |      18 |      15 |
| District of Columbia |      19 |      24 |
| Iowa                 |      19 |       3 |
| Minnesota            |      20 |      23 |
| Nebraska             |      20 |      22 |
| Vermont              |      21 |      19 |

Ranks of EI and PO by State

There are 51 states in total, but many states share the same EI or PO
proportion, so we end up with only 21 unique EI ranks and 24 PO ranks.

I expected those with high PO rank will also have high EI rank, but the
results above don’t really show a clear pattern. So, I run a simple
linear regression.

``` r
fit_EI = lm(EI_proportion ~ PO_proportion, data = RECS_EI)
summary(fit_EI)
```

    ## 
    ## Call:
    ## lm(formula = EI_proportion ~ PO_proportion, data = RECS_EI)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.115524 -0.035730 -0.004147  0.028972  0.117642 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    0.22217    0.01386  16.032  < 2e-16 ***
    ## PO_proportion  0.18807    0.06424   2.928  0.00517 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05288 on 49 degrees of freedom
    ## Multiple R-squared:  0.1489, Adjusted R-squared:  0.1315 
    ## F-statistic: 8.571 on 1 and 49 DF,  p-value: 0.005167

``` r
ggplot(RECS_EI, aes(x = PO_proportion, y = EI_proportion)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship Between EI and PO",
       x = "PO proportion",
       y = "EI proportion")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](EDA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> The regression
result isn’t great. The points are pretty scattered, and the R² is only
about 0.149, meaning PO explains only a small part of the differences in
EI across states.

Still, the slope is positive, so if there is a relationship between PO
and EI, it’s a positive one. States with more power outages tend to have
slightly higher energy insecurity, but the effect is weak.

Overall, this suggests that how often people experience power outages
probably isn’t the main reason why a state has more energy-insecure
households. Other factors—like income, housing conditions, or energy
costs—are likely playing a bigger role.

# EDA for Census

``` r
PLACES = read_csv("clean data/PLACES_Census_Tract_Data.csv")
```

    ## New names:
    ## Rows: 70102 Columns: 38
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (2): state_desc, county_name dbl (36): ...1, tract_fips, total_population,
    ## county_fips, access2_crude_pre...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
PLACES_code = read_csv("clean data/Codebook_PLACES.csv")
```

    ## Rows: 36 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Variable Name, Description, Category
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
mental_health_vars = PLACES |>
  select(
    state_desc,
    county_name,
    tract_fips,
    mhlth_crude_prev,
    depression_crude_prev
  )

plot_mhlth = mental_health_vars |>
  ggplot(aes(x = mhlth_crude_prev)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Poor Mental Health (mhlth_crude_prev)",
    x = "Percent of adults with ≥14 days of poor mental health",
    y = "Number of census tracts"
  ) +
  theme_minimal()

plot_depression = mental_health_vars |>
  ggplot(aes(x = depression_crude_prev)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution of Depression Prevalence (depression_crude_prev)",
    x = "Percent of adults ever told they have depression",
    y = "Number of census tracts"
  ) +
  theme_minimal()

mental_summary =
  mental_health_vars |>
  summarise(
    mean_mhlth = mean(mhlth_crude_prev, na.rm = TRUE),
    range_mhlth = paste0(round(min(mhlth_crude_prev, na.rm = TRUE), 3), "–", 
                         round(max(mhlth_crude_prev, na.rm = TRUE), 3)),
    mean_dep = mean(depression_crude_prev, na.rm = TRUE),
    range_dep = paste0(round(min(depression_crude_prev, na.rm = TRUE), 3), "–", 
                       round(max(depression_crude_prev, na.rm = TRUE), 3))
  )
```

Our research focuses on mental health, so I first examined the
distributions of poor mental health and depression prevalence across
census tracts. Both distributions are slightly skewed to the right and
unimodal. The average prevalence of poor mental health is 15.17, ranging
from 6.1–33%. Depression shows a similar pattern, with an average
prevalence of 20.545 and a range of 8.5–36.8%.
![](EDA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](EDA_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

Next, I aggregated the census-tract data to the state level by averaging
mental health measures across counties.

``` r
state_summary =
  mental_health_vars |>
  group_by(state_desc) |>
  summarise(
    mean_mhlth = mean(mhlth_crude_prev, na.rm = TRUE),
    mean_depression = mean(depression_crude_prev, na.rm = TRUE)
  ) |>
  ungroup()

state_summary =
  state_summary |>
  mutate(
    mhlth_rank = dense_rank(desc(mean_mhlth)),
    depression_rank = dense_rank(desc(mean_depression))
  ) |>
  arrange(mhlth_rank)   

kable(
  state_summary,
  caption = "State-Level Averages and Rankings for Mental Health Outcomes",
  digits = 3
)
```

| state_desc           | mean_mhlth | mean_depression | mhlth_rank | depression_rank |
|:---------------------|-----------:|----------------:|-----------:|----------------:|
| West Virginia        |     18.576 |          27.962 |          1 |               1 |
| Kentucky             |     18.496 |          25.883 |          2 |               2 |
| Louisiana            |     18.009 |          24.939 |          3 |               6 |
| Tennessee            |     17.160 |          25.815 |          4 |               3 |
| Oklahoma             |     17.142 |          24.437 |          5 |               8 |
| Ohio                 |     16.963 |          23.841 |          6 |              11 |
| Alabama              |     16.835 |          25.045 |          7 |               5 |
| Arkansas             |     16.834 |          24.413 |          8 |               9 |
| Michigan             |     16.810 |          20.984 |          9 |              25 |
| Nevada               |     16.737 |          19.651 |         10 |              34 |
| Indiana              |     16.255 |          23.335 |         11 |              14 |
| Georgia              |     16.175 |          19.749 |         12 |              32 |
| Missouri             |     16.088 |          23.551 |         13 |              12 |
| Texas                |     15.772 |          21.325 |         14 |              24 |
| Pennsylvania         |     15.632 |          21.767 |         15 |              22 |
| Utah                 |     15.563 |          24.607 |         16 |               7 |
| South Carolina       |     15.469 |          22.231 |         17 |              20 |
| District of Columbia |     15.437 |          20.138 |         18 |              26 |
| Mississippi          |     15.384 |          21.379 |         19 |              23 |
| Arizona              |     15.258 |          20.002 |         20 |              27 |
| Florida              |     15.200 |          18.247 |         21 |              40 |
| Oregon               |     15.162 |          22.823 |         22 |              17 |
| Washington           |     14.971 |          25.681 |         23 |               4 |
| California           |     14.821 |          17.002 |         24 |              47 |
| New Mexico           |     14.639 |          19.653 |         25 |              33 |
| New Hampshire        |     14.636 |          22.253 |         26 |              19 |
| Rhode Island         |     14.540 |          22.933 |         27 |              16 |
| North Carolina       |     14.518 |          22.204 |         28 |              21 |
| Vermont              |     14.443 |          24.283 |         29 |              10 |
| Kansas               |     14.248 |          19.389 |         30 |              35 |
| New York             |     14.206 |          18.412 |         31 |              39 |
| Massachusetts        |     14.115 |          19.290 |         32 |              37 |
| Connecticut          |     14.054 |          19.937 |         33 |              29 |
| Colorado             |     14.030 |          19.903 |         34 |              31 |
| Maine                |     14.025 |          23.398 |         35 |              13 |
| Idaho                |     13.937 |          19.936 |         36 |              30 |
| Maryland             |     13.920 |          17.754 |         37 |              44 |
| Wisconsin            |     13.775 |          19.977 |         38 |              28 |
| Delaware             |     13.748 |          17.729 |         39 |              45 |
| Virginia             |     13.702 |          19.364 |         40 |              36 |
| Iowa                 |     13.684 |          17.918 |         41 |              42 |
| Montana              |     13.564 |          23.081 |         42 |              15 |
| Minnesota            |     13.504 |          22.279 |         43 |              18 |
| Wyoming              |     13.312 |          18.116 |         44 |              41 |
| Alaska               |     13.142 |          17.716 |         45 |              46 |
| Illinois             |     12.721 |          16.107 |         46 |              48 |
| North Dakota         |     12.505 |          18.870 |         47 |              38 |
| Nebraska             |     12.322 |          17.812 |         48 |              43 |
| Hawaii               |     11.968 |          14.347 |         49 |              50 |
| South Dakota         |     11.255 |          16.099 |         50 |              49 |

State-Level Averages and Rankings for Mental Health Outcomes

The census dataset includes 50 states, resulting in 50 unique ranks for
poor mental health and 50 unique ranks for depression.

I did not examine the relationship between depression and poor mental
health, because their association is already well established and
clearly reflected in the data. Instead, I am more interested in how
mental health outcomes relate to energy insecurity.

# EDA for Census and Household Energy Insecurity

I first compared the state-level ranks, and the results are shown in the
table below.

``` r
mh_rank_table =
  state_summary |>
  select(state_desc, mhlth_rank, depression_rank) |>
  rename(state = state_desc)

rank_compare =
  rank_table |>
  left_join(mh_rank_table, by = "state") |>
  arrange(EI_rank)   

kable(
  rank_compare,
  caption = "State-Level Ranks of EI, PO, Poor Mental Health, and Depression",
  digits = 3
)
```

| state                | EI_rank | PO_rank | mhlth_rank | depression_rank |
|:---------------------|--------:|--------:|-----------:|----------------:|
| Mississippi          |       1 |       5 |         19 |              23 |
| West Virginia        |       2 |       9 |          1 |               1 |
| Arkansas             |       3 |      12 |          8 |               9 |
| Alabama              |       4 |       6 |          7 |               5 |
| Oklahoma             |       4 |       5 |          5 |               8 |
| Texas                |       4 |      11 |         14 |              24 |
| Georgia              |       5 |       7 |         12 |              32 |
| Kentucky             |       5 |      18 |          2 |               2 |
| Louisiana            |       5 |       1 |          3 |               6 |
| South Carolina       |       6 |      13 |         17 |              20 |
| California           |       7 |      14 |         24 |              47 |
| Indiana              |       8 |      12 |         11 |              14 |
| Michigan             |       8 |       8 |          9 |              25 |
| Nevada               |       8 |      24 |         10 |              34 |
| New York             |       9 |      14 |         31 |              39 |
| Arizona              |      10 |      26 |         20 |              27 |
| Connecticut          |      10 |       2 |         33 |              29 |
| Missouri             |      10 |      18 |         13 |              12 |
| North Carolina       |      10 |      11 |         28 |              21 |
| Tennessee            |      10 |      15 |          4 |               3 |
| Delaware             |      11 |      17 |         39 |              45 |
| Ohio                 |      11 |      17 |          6 |              11 |
| Alaska               |      12 |      22 |         45 |              46 |
| Kansas               |      12 |      20 |         30 |              35 |
| New Jersey           |      12 |       4 |         NA |              NA |
| New Mexico           |      12 |      21 |         25 |              33 |
| Virginia             |      12 |      14 |         40 |              36 |
| Colorado             |      13 |      23 |         34 |              31 |
| Hawaii               |      13 |      27 |         49 |              50 |
| Montana              |      13 |      19 |         42 |              15 |
| Pennsylvania         |      13 |      14 |         15 |              22 |
| Florida              |      14 |      16 |         21 |              40 |
| Illinois             |      14 |      18 |         46 |              48 |
| Maine                |      14 |       2 |         35 |              13 |
| Maryland             |      14 |      23 |         37 |              44 |
| New Hampshire        |      14 |      11 |         26 |              19 |
| Rhode Island         |      14 |      10 |         27 |              16 |
| Massachusetts        |      15 |      16 |         32 |              37 |
| Washington           |      15 |      13 |         23 |               4 |
| North Dakota         |      16 |      25 |         47 |              38 |
| Oregon               |      16 |      14 |         22 |              17 |
| Wyoming              |      16 |      19 |         44 |              41 |
| South Dakota         |      17 |      24 |         50 |              49 |
| Wisconsin            |      17 |      21 |         38 |              28 |
| Idaho                |      18 |      18 |         36 |              30 |
| Utah                 |      18 |      15 |         16 |               7 |
| District of Columbia |      19 |      24 |         18 |              26 |
| Iowa                 |      19 |       3 |         41 |              42 |
| Minnesota            |      20 |      23 |         43 |              18 |
| Nebraska             |      20 |      22 |         48 |              43 |
| Vermont              |      21 |      19 |         29 |              10 |

State-Level Ranks of EI, PO, Poor Mental Health, and Depression

New Jersey is not included in the census data, likely due to small
sample size or CDC’s small-area suppression rules. As a result, we can
only see its energy insecurity data in the table, but not its mental
health estimates.

Since the table alone is not easy to interpret visually, I created an
additional scatterplot using the poor mental health rank and EI rank for
each state. (New Jersey does not appear in the plot because its mental
health data is missing.)

``` r
rank_EI_mhlth =
  rank_table |>
  left_join(
    state_summary |> 
      select(state_desc, mhlth_rank) |>
      rename(state = state_desc),
    by = "state"
  )

ggplot(rank_EI_mhlth, aes(x = EI_rank, y = mhlth_rank, label = state)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Energy Insecurity Rank and Poor Mental Health Rank",
    x = "EI Rank (1 = Highest Energy Insecurity)",
    y = "Poor Mental Health Rank (1 = Highest Poor Mental Health Prevalence)"
  ) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 row containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: The following aesthetics were dropped during statistical transformation: label.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_point()`).

![](EDA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

The relationship between EI rank and poor mental health rank has some
degree of association in the scatterplot. States with higher energy
insecurity tend to also have worse mental health outcomes.

To examine this relationship further, I conducted a simple linear
regression using the proportions of energy insecurity and poor mental
health, instead of their ranks.

``` r
ei_mh_prop =
  RECS_EI |>
  select(state, EI_proportion) |>
  left_join(
    state_summary |> 
      select(state_desc, mean_mhlth) |> 
      rename(state = state_desc),
    by = "state"
  )


ei_mh_prop_clean = ei_mh_prop |>
  filter(!is.na(mean_mhlth))


fit_prop = lm(mean_mhlth ~ EI_proportion, data = ei_mh_prop_clean)

summary(fit_prop)
```

    ## 
    ## Call:
    ## lm(formula = mean_mhlth ~ EI_proportion, data = ei_mh_prop_clean)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.62099 -0.88085 -0.04676  0.89633  2.19290 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    10.0167     0.8161  12.274  < 2e-16 ***
    ## EI_proportion  19.0512     3.1055   6.135 1.56e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.246 on 48 degrees of freedom
    ## Multiple R-squared:  0.4395, Adjusted R-squared:  0.4278 
    ## F-statistic: 37.63 on 1 and 48 DF,  p-value: 1.561e-07

``` r
ggplot(ei_mh_prop_clean, aes(x = EI_proportion, y = mean_mhlth)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Relationship Between Energy Insecurity and Poor Mental Health (Proportions)",
    x = "Energy Insecurity Proportion",
    y = "Poor Mental Health Proportion"
  ) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](EDA_files/figure-gfm/unnamed-chunk-11-1.png)<!-- --> This simple
regression supports a moderate positive association, which provides
evidence supporting our initial expectation: states with higher energy
insecurity tend to report higher levels of poor mental health, and the
relationship is both statistically significant and practically
meaningful. The model explains about 44% of the variation in poor mental
health across states, suggesting that energy insecurity is a meaningful
but not exclusive predictor.
