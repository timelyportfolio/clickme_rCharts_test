#Simple Example of rCharts and clickme Integration

```{r}

require(clickme)
require(PerformanceAnalytics)


set_root_path("c:/users/kent.tleavell_nt/dropbox/development/r")
data(managers)
clickme(data=managers[,c(1,8,9)],ractive="clickme_rCharts_test")
```