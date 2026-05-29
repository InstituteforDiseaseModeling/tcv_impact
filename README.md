# Estimating impact of typhoid conjugate vaccines

## Installation

Run the script `setup.R` to ensure appropriate dependencies are installed.

## Usage and documentation

This repository contains tools to evaluate the population-level impact of typhoid conjugate vaccines from surveillance data. 

The script `tcv-impact.Rmd` was originally developed by the Institute for Disease Modeling to support the TCV Impact Workshop held in Phnom Penh, Cambodia on 27 March 2026. This one-stop-shop script loads in data, performs checks of data quality and formatting, makes exploratory visualizations, and estimates TCV impact using a Poisson pre-post regression which can optionally include adjustments for secular trends, seasonality, and testing volumes for typhoid. Impact can be estimated separately for different age groups and/or time periods since vaccination. Additional documentation is embedded within this script.

The functions in `tcv-impact-functions.R` are required to run `tcv-impact.Rmd`. This includes functions to initialize, fit, and visualize results from the primary pre-post regressions. 

The code in `example-final-models.R` shows how these functions can be used to estimate impact (fit models) to specific age groups and/or time periods. This code will not run on its own, but can optionally be added to `tcv-impact.Rmd`.

The data template `data/DataTemplate_v2.xlsx` describes the required data to perform this analysis. The script can be run using the provided synthetic data `data/FakeData_workshop_v2.xlsx`. The function in `population-interpolation-example.R` may be helpful, but is not required, for formatting the required population data.

## Contributing

If you wish to contribute, please reach out to the repository owners or the organizers of the TCV Impact Workshop. If you wish to use this code for your own analysis, we recommend you create a fork and modify the code as needed in a separate repository.

## Disclaimer

The code in this repository was developed by IDM to support our joint research on typhoid conjugate vaccines. We've made it publicly available under the MIT License to provide others with a better understanding of our research and an opportunity to build upon it for their own work. We make no representations that the code works as intended or that we will provide ongoing development support. You are welcome to create your own fork and modify the code to suit your own modeling needs as permitted under the MIT License.