# KPIsHealth

## Author

Carlos Omar Pardo Gomez (cop2108@columbia.edu)

## Legal Rights

The intellectual property of this code is entirely owned by [Kantar Millward Brown (Mexico)] (http://www.millwardbrown.com/mb-global/get-in-touch/office-locations/mexico) and cannot be exploited for any commercial purpose without their consent. Here it is presented just for educational reasons.

## Overview

Shiny app, useful to monitor the actual trend of a brand's KPIs, when they are estimated from survey data.

Features include:
- Filters by demographics
- Latest change versus last month, last quarter and last year
- Brand Health Map, to visualize KPIs' level and momentum in one plot
- Time series' noise filter
- Automatic structural changes finder
- ARIMA forecast
- Credible intervals and uncertainty measure

## Features

First of all, there is an option to select the brand which is going to be monitored.

![Image of brand](https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/brand.png)

Then, the fun begins...

### Summary Section

This section allows nontechnical decision-makers to have a complete overview of the brand's situation, with regard to a different period.

#### Settings

Comparisson can be done versus the last month, quarter or year. Also, results can be decomposed by demographics!

<img src="https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/summary_settings.png" width="200">

#### Brand Status

This table shows if each one of the KPIs is growing, decreasing or with not enough information to think it is doing one of both, by using statistically significant differences.

![Image of brand status](https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/brand_status.png)

### Analyse Section

This section allows to deepen into each one of the KPI trends, and understand more how the brand is behaving.

#### Filters

All the analysis in this section can be done by filtering by demographics!

<img src="https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/filters.png" width="200">

#### Latest Change

This tab allows the user to visualize the difference's distribution versus a previous period, for every KPI. This shows how close or far was the metric from the significant difference threshold, and how much uncertainty there is about the real value.

![Image of lastest change](https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/latest_change.png)

#### Trend

It shows the time series plot, adding some interesting elements:

- Real value's confidence interval, taking into account the sampling error (gray shadow)
- Structural changes found in the series (vertical lines)
- ARIMA model to forecast the next observations (yellow area)
- Noise filter by _level_ -kalman filter- or _momentum_ -linear regression by stable period- (blue line)

![Image of trend_plot](https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/trend_plot.png)

![Image of momentum](https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/momentum.png)


#### Health Map

Scatter plot with the last _level_ (within the metric's historic data, which quantile represents the last observation) and _momentum_ (how big is the current brand's growing or diminishing).

This grants the modeler to have a robust perspective of the brand's health and quickly identify which metrics should raise some attention.

![Image of health map](https://github.com/opardo/KPIsHealth/blob/master/KPIsHealth/images/brand_health_map.png)


