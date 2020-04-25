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
ctracking_url = "http://covidtracking.com/api/states/daily.csv"
state_tests = fread(ctracking_url)
state_tests[, d := anydate(date)]
state_tests[, day := weekdays(d)]
dropcols = c('hash', 'dateChecked')
state_tests[, (dropcols) := NULL]
state_tests %>% glimpse()

# %%
state_tests[, tpr := positive / totalTestResults]
state_tests[, tpr_new := positiveIncrease / totalTestResultsIncrease]
(most_affected = 
     state_tests[d == max(state_tests$d)][
    order(-positive)][
    1:10])

# %%
t10states = state_tests[positive >= 10 & state %in% most_affected$state]

# %%
setorder(t10states, state, date)

smoothvars = c("positive", "hospitalized", "death", "totalTestResults", 
              "positiveIncrease", "hospitalizedIncrease", "deathIncrease", "totalTestResultsIncrease",
              'tpr', 'tpr_new')

t10states[, paste0("rm3_", smoothvars) := lapply(.SD, rollmean, k = 3, fill = NA, na.pad = T), 
   by = .(state), .SDcols = smoothvars]

# t10states[, tpr_rm3 := rollmeanr(tpr, 3, fill = NA), by = .(state)]

# %% [markdown] toc-hr-collapsed=true toc-nb-collapsed=true
# # Plots 

# %%
# generic function to plot time series
plot_ts = function(df, col, t = "Time Series of", logtransform = F){
    p = ggplot(df, aes(x = d, colour = state, group = state)) +
        geom_point(aes_string(y = col), size = 0.7) + geom_line(aes_string(y = paste0("rm3_", col))) +
        scale_colour_brewer(palette = "Spectral") +
        labs(
            title = t,
            subtitle = paste(unique(df$state), collapse = ", ")
        )
    if (logtransform == T) p = p + scale_y_log10()
    return(p)
}

# %% [markdown]
# ## Cases 

# %%
(p = plot_ts(t10states, 'positive', "Cumulative Cases Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %%
(p = plot_ts(t10states, 'positiveIncrease', "New Cases Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %% [markdown]
# ## Tests 

# %%
(p = plot_ts(t10states, 'totalTestResults', "Cumulative Tests Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %%
(p = plot_ts(t10states[d >= '2020-03-15'], 'totalTestResultsIncrease', "New Tests Time Series", T))

# %% [markdown] toc-hr-collapsed=true toc-nb-collapsed=true
# ## TPR

# %%
p1 = plot_ts(t10states, 'tpr', "Test Positive Rate: Time Series", T) +
    scale_y_continuous(breaks = seq(0, 1, .1))
# p2 = plot_ts(t10states, rm3_tpr, "Test Positive Rate (Rolling Mean): Time Series", T) +
#     scale_y_continuous(breaks = seq(0, 1, .1))
p1 

# %% [markdown]
# ### TPR Over time (interactive)

# %%
embed_notebook(ggplotly(p1))

# %% [markdown]
# ## New TPR

# %%
p1 = plot_ts(t10states, 'tpr_new', "Test Positive Rate: Time Series", T) +
    scale_y_continuous(breaks = seq(0, 1, .1))

p1 

# %%
embed_notebook(ggplotly(p2))

# %% [markdown]
# # Day-of-week effects 

# %%
dt = t10states[d >= '2020-03-15']
dt[, wknd := ifelse(day %in% c("Saturday", "Sunday"), 1, 0)]
dt[, t := date - 20200315] # time trend 
dt$day2 = as.factor(dt$day) 
dt %>% head()

# %%
(p = ggplot(dt, aes(d)) +
    geom_point(aes(y = totalTestResultsIncrease, colour = as.factor(wknd))) + 
    geom_line(aes(y = rm3_totalTestResultsIncrease)) +
    labs(title = "Day of the week effects in Test Results") +
    facet_wrap(~ state, 2) +
    lal_plot_theme() +
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
test_increase_regs = dt %>% group_by(state) %>% group_map( ~ lm(log(totalTestResultsIncrease+1) ~ relevel(day2, 2), .x) %>% tidy %>% mutate(state = .y[[1]]), keep = T) 
dow_plots = map(test_increase_regs , day_of_week_plot) %>% wrap_plots(nrow = 2)
dow_plots +  plot_annotation(
    title = 'Day of the week effects in Test Results',
    subtitle = 'Reference category: Monday',
)


# %%
positive_rate = dt %>% group_by(state) %>% group_map( ~ lm(tpr_new ~ relevel(day2, 2), .x) %>% tidy %>% mutate(state = .y[[1]]), keep = T) 
dow_plots = map(positive_rate, day_of_week_plot) %>% wrap_plots(nrow = 2)
dow_plots +  plot_annotation(
    title = 'Day of the week effects in Test Positive Rate',
    subtitle = 'Reference category: Monday',
)

# %%
dths = dt %>% group_by(state) %>% group_map( ~ lm(log(deathIncrease+1) ~ relevel(day2, 2), .x) %>% tidy %>% mutate(state = .y[[1]]), keep = T) 
dow_plots = map(dths, day_of_week_plot) %>% wrap_plots(nrow = 2)
dow_plots +  plot_annotation(
    title = 'Day of the week effects in COVID19 Deaths',
    subtitle = 'Reference category: Monday',
)

# %%
