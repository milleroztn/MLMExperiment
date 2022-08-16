This is an R Shiny app that presents the difference in residuals for a regression with and without a logarithm transformation applied to the dependent variable as described in [MLMExperiment_analysis.md](https://github.com/milleroztn/MLMExperiment/blob/main/MLMExperiment_analysis.md#heterogeneous-treatment-effects).

The app takes a variable from a list as input and runs one regression using that variable, and another using the logarithim of that variable.
The app then calculates the residuals for each regression and displays a histogram of both sets of residuals as well as Q-Q plots for both sets of residuals.

This app is hosted at https://uncle-pasta.shinyapps.io/log-app/
