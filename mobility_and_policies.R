# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:hydrogen
#     text_representation:
#       extension: .R
#       format_name: hydrogen
#       format_version: '1.3'
#       jupytext_version: 1.9.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %%
rm(list = ls())

library(LalRUtils)
libreq(data.table, magrittr, tidyverse, janitor, huxtable, knitr, patchwork, timeDate, plotly)

# %%
# theme_set(lal_plot_theme(textangle = 90))
theme_set(lal_plot_theme(textangle = 90))
set.seed(42)
options(repr.plot.width = 15, repr.plot.height=12)

# %% [markdown]
# # Mobility Reports Query
# [info](https://console.cloud.google.com/marketplace/details/bigquery-public-datasets/covid19_google_mobility?filter=solution-type%3Adataset&id=db650d71-e53e-4677-b37b-d8bdca3ec4dc&project=spatial-acumen-244921&folder&organizationId)

# %%
libreq(tidyverse, DBI, dbplyr, RSQLite, bigrquery, tictoc)

# %%
projid <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
bq_auth(email = "lal.apoorva@gmail.com",
       path = "~/keys/sandbox.json")

# %%
bq_con <-  dbConnect(
    bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "covid19_google_mobility",
    billing = projid
    )
bq_con
dbListTables(bq_con)

# %%
mob <- tbl(bq_con, "mobility_report")
mob %>% glimpse

# %%
mob %>% filter(country_region_code == "US") %>% 
    collect() -> us_mobility

# %%
tic()
mob %>% collect() ->
    mobility_df
toc()

# %%
mobility_df %<>% setDT
mobility_df %>% glimpse()

# %%
fwrite(mobility_df, "data/google-mobility-reports.csv.gz")
dbDisconnect(bq_con)

# %% [markdown]
# ## Viz

# %%
mobility_df = fread("data/google-mobility-reports.csv.gz")
old = c('retail_and_recreation_percent_change_from_baseline',
  'grocery_and_pharmacy_percent_change_from_baseline',
  'parks_percent_change_from_baseline',
  'transit_stations_percent_change_from_baseline',
  'workplaces_percent_change_from_baseline',
  'residential_percent_change_from_baseline')
new = c("retail", "grocery", "parks", "transit", "work", "home")
setnames(mobility_df, old, new)
mobility_df[, d := lubridate::ymd(date)]
mobility_df[, weekend := isWeekend(d)]
mobility_df %>% glimpse

# %% [markdown]
# ### US Counties

# %%
us_mob = mobility_df[country_region_code == "US"]
us_mob %>% head

# %%
bay   = paste0(c( "San Francisco", "San Mateo", "Santa Clara",
                        "Alameda", "Contra Costa", "Marin"), " County")

socal = paste0(c("Los Angeles", "Orange", "San Bernardino", "Riverside", "Ventura", "Merced"),
               " County")

bay_area = us_mob[sub_region_1 == "California" & sub_region_2 %in% bay ]

la_area = us_mob[sub_region_1 == "California" & sub_region_2 %in% socal ]

# long
bay_area2 = melt(bay_area, id.vars = c("sub_region_2", "d", "weekend"),
        measure = c("retail", "grocery", "parks", "transit", "work", "home"),
        variable.name = "category")
la_area2 = melt(la_area, id.vars = c("sub_region_2", "d", "weekend"),
        measure = c("retail", "grocery", "parks", "transit", "work", "home"),
        variable.name = "category")

bay_area2 %>% head

# %% [markdown]
# Function to plot mobility with sensible faceting + gray bars for weekends.

# %%
mobility_faceted = function(df, cmap = T, leg = T, wknd = F){
    p =  ggplot(df, aes(x = d, y = value, group = category, colour = category))
    if (wknd == T){
        p = p + geom_vline(data = df[weekend == T],
                     mapping = aes(xintercept = d),
                     color = 'gray', size = 2.9, alpha = 0.1)
    }
    p = p + geom_point(size = 1, alpha = 0.5) + geom_smooth(se = F, size = 0.8) +
            geom_hline(yintercept = 0, colour = 'gray', linetype = 'dashed') +
            scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
            facet_wrap(~ sub_region_2, scale = "free")
    if (cmap == T) p = p + scale_colour_brewer(palette = "Set2")
    if (leg == F) p = p + guides(colour = F)
    return(p)
}


mobility_plotter = function(df, col, cmap = T, leg = F){
    p = df %>%
        ggplot(aes(x = d, y = {{ col }}, group = sub_region_2, colour = sub_region_2)) +
            geom_hline(yintercept = 0, colour = 'gray', linetype = 'dashed') +
#             geom_vline(data = df[weekend == T],
#                      mapping = aes(xintercept = d),
#                      color = 'gray', size = 2.9, alpha = 0.1) +
            geom_point() + geom_smooth(se = F) +
            scale_x_date(date_breaks = "1 week", date_labels = "%b %d")
    if (cmap == T) p = p + scale_colour_brewer(palette = "Set2")
    if (leg == F) p = p + guides(colour = F)
    return(p)
}

# %%
options(repr.plot.width = 15, repr.plot.height=20)
mobility_plotter(bay_area, grocery, leg = T) / mobility_plotter(la_area, grocery, leg = T)

# %%
options(repr.plot.width = 25, repr.plot.height=16)
mobility_faceted(bay_area2) + ggtitle("Mobility Trends in the Bay Area")
mobility_faceted(la_area2)  + ggtitle("Mobility Trends in the LA Area")

# %% [markdown]
# ## International

# %% [markdown]
# ### Helper Functions

# %%
melter = function(df, keys = c("country_region", "d", "weekend")){
    df_long = melt(df, id.vars = keys,
        measure = c("retail", "grocery", "parks", "transit", "work", "home"),
        variable.name = "category")
    return(df_long)
}

subregion_faceted = function(df, cmap = T, freescale = F, leg = T, wknd = F){
    p = ggplot(df, aes(x = d, y = value, group = category, colour = category))
    if (wknd == T){
        p = p + geom_vline(data = df[weekend == T], mapping = aes(xintercept = d),
                     color = 'gray', size = 2.9, alpha = 0.1)
    }
    p = p +
        geom_point(size = 1, alpha = 0.5) + geom_smooth(se = F, size = 0.6) +
        geom_hline(yintercept = 0, colour = 'gray', linetype = 'dashed') +
        scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    if (freescale == T) {
        p = p + facet_wrap(~ sub_region_1, scale = "free_y")
    } else{
        p = p + facet_wrap(~ sub_region_1)
    }
    if (cmap == T) p = p + scale_colour_brewer(palette = "Set2")
    if (leg == F) p = p + guides(colour = F)
    return(p)
}

country_faceted = function(df, freescale = F,  cmap = F, leg = F, wknd = F){
    p = ggplot(df, aes(x = d, y = value))
    if (wknd == T){
        p = p + geom_vline(data = df[weekend == T], mapping = aes(xintercept = d),
                     color = 'gray', size = 2.9, alpha = 0.1)
    }
    p = p +
        geom_point(size = 1, alpha = 0.5) + geom_smooth(se = F, size = 0.6) +
        geom_hline(yintercept = 0, colour = 'gray', linetype = 'dashed') +
        scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    if (freescale == T) {
        p = p + facet_wrap(~ category, scale = "free")
    } else{
        p = p + facet_wrap(~ category)
    }
    if (cmap == T) p = p + scale_colour_brewer(palette = "Set2")
    if (leg == F) p = p + guides(colour = F)
    return(p)
}

mob_plotter = function(df, col, grp, leg = T){
    p = df %>%
        ggplot(aes(x = d, y = {{ col }}, group = {{grp}}, colour = {{grp}})) +
            geom_hline(yintercept = 0, colour = 'gray', linetype = 'dashed') +
#             geom_vline(data = df[weekend == T],
#                      mapping = aes(xintercept = d),
#                      color = 'gray', size = 2.9, alpha = 0.3) +
        geom_point() + geom_smooth(se = F) +
        scale_x_date(date_breaks = "1 week", date_labels = "%m-%d")
    if (leg == F) p = p + guides(colour = F)
    return(p)
}

# %%
nep = mobility_df[country_region =="Nepal"]
nep_l = melter(nep)
(nep = country_faceted(nep_l) +
    labs(title = "Mobility Report for Nepal", subtitle = "Based on Google Maps Activity", caption = "Queried on BigQuery"))

# %%
ggsave("nep_mobility.jpg", nep, width = 20, height = 15)

# %%
bih = mobility_df[country_region =="India" & sub_region_1 == "Bihar" & sub_region_2 == ""]
bih %>% head
bih_l = melter(bih)
(bih = country_faceted(bih_l) +
    labs(title = "Mobility Report for Bihar", subtitle = "Based on Google Maps Activity", caption = "Queried on BigQuery"))

# %%
wb= mobility_df[country_region =="India" & sub_region_1 == "West Bengal" & sub_region_2 == ""]
wb_l = melter(wb)
(wb = country_faceted(wb_l) +
    labs(title = "Mobility Report for WB", subtitle = "Based on Google Maps Activity", caption = "Queried on BigQuery"))

# %%
options(repr.plot.width = 25, repr.plot.height=12)
(nep | bih)

# %%
ind = mobility_df[country_region =="India"]
ind_l = melter(ind, c("sub_region_1", "d", "weekend"))
ind_l = ind_l[sub_region_1 != ""][order(d)]
ind %>% head

# %%
options(repr.plot.width = 25, repr.plot.height=12)
(ind_disagg = subregion_faceted(ind_l, freescale = T))
ggsave("ind_mobility.jpg", ind_disagg, width = 30, height = 30)

# %% [markdown]
# Jammu and Kashmir up by 300%+ because the baseline period corresponds with a sever lockdown [for political reasons - abrogation of article 370 etc]. Baselines matter in DiD.

# %%
d = mobility_df[country_region =="South Africa"]
d_l= melter(d, c("sub_region_1", "d", "weekend")) %>% .[sub_region_1 != ""]
(ind_disagg = subregion_faceted(d_l))

# %% [markdown]
# ### Subregion Data Availability

# %%
countries_w_subn = mobility_df[, .(sub = unique(sub_region_1)), by = country_region][sub != ""]
countries_w_subn[, unique(country_region)] %>% print
# %%
countries_w_subn = mobility_df[, .(sub = unique(sub_region_2)), by = country_region][sub != ""]
countries_w_subn[, unique(country_region)] %>% print
# %%
countries_w_metros = mobility_df[, .(sub = unique(metro_area)), by = country_region][sub != ""]
countries_w_metros[, unique(country_region)] %>% print
# %% [markdown]
# # Oxford Govt Policy Tracker

# %%
options(repr.plot.width = 12, repr.plot.height=10)

# %%
projid <- Sys.getenv("GCE_DEFAULT_PROJECT_ID")
bq_auth(email = "lal.apoorva@gmail.com",
       path = "~/keys/sandbox.json")

# %%
bq_con <-  dbConnect(
    bigrquery::bigquery(),
    project = "bigquery-public-data",
    dataset = "covid19_govt_response",
    billing = projid
    )
bq_con
dbListTables(bq_con)

# %%
mob <- tbl(bq_con, "oxford_policy_tracker")
mob %>% glimpse

# %%
tic()
mob %>% collect() ->
    policies
toc()

# %%
policies %<>% setDT

fwrite(policies, "data/ox_policy_tracker.csv.gz")
dbDisconnect(bq_con)

# %% [markdown]
# ### Viz

# %%
policies = fread("data/ox_policy_tracker.csv.gz")
policies[, date := lubridate::ymd(date)]
policies %>% glimpse

# %%
country_obs = policies[, .N, by = .(country_name, alpha_3_code)][order(country_name)]
country_obs

# %%
country_obs[N > median(N)]

# %%
sa = policies[alpha_3_code %in% c("IND", "NPL", "PAK", "BGD", "LKA", "AFG")]

tsplot = function(df, grouper = alpha_3_code){
    p = ggplot(df, aes(x = date, y = stringency_index, group = {{grouper}}, colour = {{grouper}})) +
    geom_line() 
    return(p)
}


# %%
tsplot(sa) + ggtitle("Lockdown / NPI Stringency in South Asia")

# %%
policies[alpha_3_code == "USA"][region_code %in% c("US_CA", "US_NY", "US_FL", "US_TX", "US_IL", "US_GA", "US_WA", "US_AZ")] %>% tsplot(., region_code)
