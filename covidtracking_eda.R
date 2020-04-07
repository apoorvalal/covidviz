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
libreq(data.table, tidyverse, anytime, patchwork, plotly, zoo)
theme_set(lal_plot_theme())

options(repr.plot.width = 15, repr.plot.height = 12)

# %% [markdown]
# # Covidtracking Project data viz

# %%
ctracking_url = "http://covidtracking.com/api/states/daily.csv"
state_tests = fread(ctracking_url)
state_tests[, d := anydate(date)]
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

# %% [markdown]
# # Plots 

# %%
# generic function to plot time series
plot_ts = function(df, col, t = "Time Series of", logtransform = F){
    p = ggplot(df, aes(x = d, y = {{col}}, colour = state, group = state)) +
        geom_point() + geom_line() + 
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
(p = plot_ts(t10states, positive, "Cumulative Cases Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %%
(p = plot_ts(t10states, rm3_positiveIncrease, "New Cases Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %% [markdown]
# # Tests 

# %%
(p = plot_ts(t10states, totalTestResults, "Cumulative Tests Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %%
(p = plot_ts(t10states, rm3_totalTestResultsIncrease, "New Tests Time Series", T))

# %%
embed_notebook(ggplotly(p))

# %% [markdown]
# ## TPR

# %%
p1 = plot_ts(t10states, tpr, "Test Positive Rate: Time Series", T) +
    scale_y_continuous(breaks = seq(0, 1, .1))
p2 = plot_ts(t10states, rm3_tpr, "Test Positive Rate (Rolling Mean): Time Series", T) +
    scale_y_continuous(breaks = seq(0, 1, .1))

p1 / p2

# %% [markdown]
# ### TPR Over time (interactive)

# %%
embed_notebook(ggplotly(p1))

# %%
t10states[state == "CA", .(state, d, positive, totalTestResults, tpr, rm3_tpr)]

# %% [markdown]
# ## New TPR

# %%
p1 = plot_ts(t10states, tpr_new, "Test Positive Rate: Time Series", T) +
    scale_y_continuous(breaks = seq(0, 1, .1))
p2 = plot_ts(t10states, rm3_tpr_new, "New Test Positive Rate: Time Series", T) +
    scale_y_continuous(breaks = seq(0, 1, .1))
(p1 / p2)

# %%
embed_notebook(ggplotly(p2))

# %%
