# ---
# jupyter:
#   jupytext:
#     formats: ipynb,R:hydrogen
#     text_representation:
#       extension: .R
#       format_name: hydrogen
#       format_version: '1.3'
#       jupytext_version: 1.3.3
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %%
rm(list = ls())
library(LalRUtils)
libreq(data.table, tidyverse, anytime, patchwork, plotly, broom, zoo)
theme_set(lal_plot_theme_d())

options(repr.plot.width = 15, repr.plot.height = 12)

# %% [markdown]
# # Covidtracking Project data viz
# https://covidtracking.com/api

# %%
system("rm -f data/daily.csv")
system("curl https://covidtracking.com/api/v1/states/daily.csv > data/daily.csv")

# %%
state_tests = fread('https://api.covidtracking.com/v1/states/daily.csv')
state_tests[, d := anydate(date)]
state_tests[, day := weekdays(d)]
dropcols = c('hash', 'dateChecked')
state_tests[, (dropcols) := NULL]
state_tests %>% glimpse()

# %%
state_tests[, tpr := positive / totalTestResults]
state_tests[, cfr := death / positive ]
state_tests[, tpr_new := positiveIncrease / totalTestResultsIncrease]
(most_affected = 
     state_tests[d == max(state_tests$d)][
    order(-positive)][
    1:10, 
    c("state", "d", "positive", "death", "positiveIncrease", "deathIncrease", 
      'totalTestResults', 'totalTestResultsIncrease', 'cfr',  'tpr', 'tpr_new')]
 )

# %%
t10states = state_tests[positive >= 10 & state %in% most_affected$state]

# %%
setorder(t10states, state, date)

smoothvars = c("positive", "hospitalizedCurrently", "cfr", "death", "totalTestResults", 
              "positiveIncrease", "hospitalizedIncrease", "deathIncrease", "totalTestResultsIncrease",
              'tpr', 'tpr_new')

t10states[, paste0("rm3_", smoothvars) := lapply(.SD, rollmean, k = 3, fill = NA, na.pad = T), 
   by = .(state), .SDcols = smoothvars]

# t10states[, tpr_rm3 := rollmeanr(tpr, 3, fill = NA), by = .(state)]

# %%
t10states %>% glimpse

# %% [markdown] toc-hr-collapsed=true toc-nb-collapsed=true
# # Plots 

# %% [markdown]
# ## Natl Time Series

# %%
vars = c("positive", "positiveIncrease", "death", "deathIncrease", "totalTestResultsIncrease", "totalTestResults", 
              "hospitalizedCurrently", "hospitalizedCumulative")
nat_ts = state_tests[, lapply(.SD, sum, na.rm = T), by = d, 
        .SDcols = vars][, 
    `:=`(cfr = death/positive,
         tpr = positive/totalTestResults,
         tpr_n = positiveIncrease/totalTestResultsIncrease
         )]

smoothvars = c(vars, "tpr_n", "cfr", "tpr" )

nat_ts[, paste0("rm3_", smoothvars) := lapply(.SD, rollmean, k = 3, fill = NA, na.pad = T), .SDcols = smoothvars]
nat_ts[, .(d, positive, positiveIncrease, death, deathIncrease, totalTestResults, totalTestResultsIncrease)] %>% head

# %%
p1 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = positive)) + scale_y_log10() +
    geom_point() + geom_line(aes(y = rm3_positive)) + ggtitle("Agg case count")

p2 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = positiveIncrease)) + geom_smooth(se = F) + 
    geom_point() + ggtitle("New Cases")

p3 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = death)) + scale_y_log10() + geom_line(aes(y = rm3_death)) +
    geom_point() + ggtitle("Agg death count")

p4 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = deathIncrease)) + scale_y_log10() + geom_smooth(se = F) +
    geom_point() + ggtitle("New Deaths")

(p1 | p2)/(p3 | p4)

# %%
ggplotly(p2)

# %%
p1 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = tpr)) +
    geom_point() + geom_smooth(se = F) + ggtitle("TPR over time")

p2 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = tpr_n)) + geom_smooth(se = F) +
    geom_point() + ggtitle("New Tests TPR over time")
p3 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = totalTestResults)) + scale_y_log10() +
    geom_point() + ggtitle("Total Tests over time")

p4 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = totalTestResultsIncrease)) + 
    scale_y_log10() + geom_smooth(se = F) +
    geom_point() + ggtitle("New Tests over time")

p5 = nat_ts[d >= "2020-03-15"] %>% 
    ggplot(aes(x = d, y = cfr)) + geom_smooth(se = F) +
    geom_point() + ggtitle("CFR Estimate over time")
(p2)/(p3 | p4) # | p5

# %%
# generic function to plot time series
plot_ts = function(df, col, t = "Time Series of", logtransform = F, rm = T){
    p = ggplot(df, aes_string(x = 'd', y = col, colour = 'state', group = 'state')) +
        geom_point( size = 0.7) + 
        # plot rolling mean line
        scale_colour_brewer(palette = "Spectral") +
        labs(
            title = t,
            subtitle = paste(unique(df$state), collapse = ", ")
        )
    if (rm == T) {
        p = p + geom_line(aes_string(y = paste0("rm3_", col))) 
    } else{
        p = p + geom_smooth(se = F)
    }
    if (logtransform == T) p = p + scale_y_log10()
    return(p)
}

# %% [markdown]
# ## Cases 

# %%
p1 = plot_ts(t10states, 'positive', "Cumulative Cases Time Series", T)
p2 = plot_ts(t10states, 'positiveIncrease', "New Cases Time Series", T, F)
(p1 | p2)

# %%
ggplotly(p2)

# %%
p1 = plot_ts(t10states, 'death', "Cumulative Deaths Time Series", T, F)
p2 = plot_ts(t10states, 'deathIncrease', "New Deaths Time Series", T, F)
(p1 | p2)

# %%
p1 = plot_ts(t10states, 'hospitalizedCurrently', "Current Hospitalisations", T, F)
p2 = plot_ts(t10states, 'hospitalizedIncrease', "New Hospitalisations", T)

(p1 | p2)

# %%
ggplotly(p1)

# %%
cfp = plot_ts(t10states[d >= "2020-03-15"], "cfr", "Fatality Rate", F, F)
p5 + ylim(c(0, 0.1)) | cfp+ ylim(c(0, 0.1)) 

# %% [markdown]
# ## Tests 

# %%
p = plot_ts(t10states, 'totalTestResults', "Cumulative Tests Time Series", T, F)
p2 = plot_ts(t10states[d >= '2020-03-15'], 'totalTestResultsIncrease', "New Tests Time Series", T, F)

(p | p2)

# %% [markdown]
# # Shares over time  

# %%
t10states[rm3_deathIncrease < 0, rm3_deathIncrease := 0]
t10states[rm3_positiveIncrease< 0, rm3_positiveIncrease := 0]
t10states[rm3_totalTestResultsIncrease< 0, rm3_totalTestResultsIncrease := 0]

t10states[, denom_cases := sum(rm3_positiveIncrease), by = d][, 
            denom_deaths := sum(rm3_deathIncrease), by = d][,
            denom_tests  := sum(rm3_totalTestResultsIncrease), by = d]

t10states[, newcase_share := rm3_positiveIncrease / denom_cases][, 
            newdeath_share := rm3_deathIncrease/denom_deaths][,
            newtests_share := rm3_totalTestResultsIncrease / denom_tests]

# %%
t10states[, stgroup := case_when(
    state %in% c("NY", "NJ", "MA", "CT", "PA") ~ paste0("1_", state), # group northeast
    state %in% c("CA") ~ paste0("2_", state), # group northeast
    state %in% c("TX", "GA", "FL") ~ paste0("3_", state), # group northeast
    TRUE ~ paste0("4_", state)
)]

# %%
p1 = ggplot(t10states[d >= "2020-03-15"], aes(x = d, y = newcase_share, fill = stgroup, colour = stgroup)) +
    geom_area(position="fill") +
    scale_y_continuous(breaks = seq(0, 1, .1))+
    scale_fill_brewer(palette = "Spectral") +
    scale_colour_brewer(palette = "Spectral") +
    ggtitle("New Cases")
p2 = ggplot(t10states[d >= "2020-03-24"], aes(x = d, y = newdeath_share, fill = stgroup, colour = stgroup)) +
    geom_area(position="fill") +
    scale_y_continuous(breaks = seq(0, 1, .1))+
    theme(legend.position = "None") + 
    scale_fill_brewer(palette = "Spectral") +
    scale_colour_brewer(palette = "Spectral") +
    ggtitle("New Deaths")
p3 = ggplot(t10states[d >= "2020-03-15"], aes(x = d, y = newtests_share, fill = stgroup, colour = stgroup)) +
    geom_area(position="fill") +
    theme(legend.position = "None") + 
    scale_y_continuous(breaks = seq(0, 1, .1))+
    scale_fill_brewer(palette = "Spectral") +
    scale_colour_brewer(palette = "Spectral") +
    ggtitle("New Tests")
options(repr.plot.width = 20, repr.plot.height = 12)
p = (p3 | p1 | p2 ) + plot_annotation(title = "Shares of Tests, Cases, and Deaths over time")
p

# %%
ggsave("state_carpet.png", p, width = 20, height = 10)

# %% [markdown] toc-hr-collapsed=true toc-nb-collapsed=true
# ## TPR

# %%
options(repr.plot.width = 20, repr.plot.height = 14)

# %%
p1 = plot_ts(t10states, 'tpr', "Test Positive Rate: Time Series", T, F) +
    scale_y_continuous(breaks = seq(0, 1, .1)) + ylim(c(0, 1))

p2 = plot_ts(t10states, 'tpr_new', "New Test Positive Rate: Time Series", T, F) +
    scale_y_continuous(breaks = seq(0, 1, .1))+ ylim(c(0, 1))

p1 | p2 

# %%
ggplotly(p2)

# %% [markdown]
# # Day-of-week effects 

# %%
dt = t10states[d >= '2020-03-15']
dt[, wknd := ifelse(day %in% c("Saturday", "Sunday"), 1, 0)]
dt[, t := date - 20200315] # time trend 
dt$day2 = as.factor(dt$day) 

# %%
(p = ggplot(dt, aes(d, y = totalTestResultsIncrease)) +
    geom_point(aes(colour = as.factor(wknd))) + 
    geom_smooth(se = F) +
    labs(title = "Day of the week effects in Test Results") +
    facet_wrap(~ state, 2) +
    scale_y_log10()
)


# %%
(p = ggplot(dt, aes(d, y = deathIncrease)) +
    geom_point(aes(colour = as.factor(wknd))) + 
    geom_smooth(se = F) +
    labs(title = "Day of the week effects in Deaths") +
    facet_wrap(~ state, 2) +
    scale_y_log10()
)


# %%
day_of_week_plot = function(df){
    d = df %>% filter(term != "(Intercept)") %>% 
        mutate(day = str_replace(term, "relevel\\(day2, 2\\)", "")) %>% 
        mutate(ord_day = case_when(
            day == "Monday" ~ 1,  day == "Tuesday" ~ 2,  day == "Wednesday" ~ 3,  day == "Thursday" ~ 4, 
            day == "Friday" ~ 5,  day == "Saturday" ~ 6,  day == "Sunday" ~ 7
        ), wknd = as.factor(ifelse(day %in% c("Saturday", "Sunday"), 1, 0))) %>% 
        mutate(ub = estimate + 1.96 * `std.error`, 
               lb = estimate - 1.96 * `std.error`)
    stname = unique(df$state)
    p = ggplot(d, aes(x = ord_day,y = estimate, colour = wknd)) +
        geom_point() + 
        theme(legend.position = "None") +
        geom_pointrange(aes(ymin = lb, ymax = ub)) + 
        labs(title = stname)
}

# %%
test_increase_regs = dt %>% group_by(state) %>% group_map( ~ lm(log(totalTestResultsIncrease+1) ~ relevel(day2, 2), .x) %>% 
                tidy %>% mutate(state = .y[[1]]), .keep = T) 
dow_plots = map(test_increase_regs , day_of_week_plot) %>% wrap_plots(nrow = 2)
dow_plots +  plot_annotation(
    title = 'Day of the week effects in Test Volume',
    subtitle = 'Reference category: Monday',
)


# %%
test_increase_regs = dt %>% group_by(state) %>% 
    mutate(dead = ifelse(deathIncrease < 0 , 0, deathIncrease)) %>% 
    group_map( ~ lm(log1p(dead) ~ relevel(day2, 2), .x) %>% 
                tidy %>% mutate(state = .y[[1]]), .keep = T) 
dow_plots = map(test_increase_regs , day_of_week_plot) %>% wrap_plots(nrow = 2)
dow_plots +  plot_annotation(
    title = 'Day of the week effects in Deaths',
    subtitle = 'Reference category: Monday',
)

