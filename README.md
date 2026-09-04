# 🌊 Shiny Shores: Marsh Shoreline Change Analysis Simulations 🌊

## Quick Links
* [**Launch the Interactive Web App**](https://sea-aaron-bland.github.io/shiny_shores/)
* **Standalone Scripts**: Look inside the `/scripts` directory to download our raw R scripts.
* **Relevant publications**
  * [**Read the article in Restoration Ecology (Bland et al., 2026)**](https://onlinelibrary.wiley.com/doi/10.1111/rec.70538)
  * [**Read my PhD Dissertation (Chapter 2)**](https://www.proquest.com/openview/1e48fa2143da32021861b1d9829cedea/1?cbl=18750&diss=y&pq-origsite=gscholar)
 

## App Introduction

The purpose of this app is to identify suitable methods for tracking salt marsh shoreline movement given site-specific conditions. Here, "suitable" refers to high confidence in an estimated rate of marsh shoreline movement relative to the magnitude of shoreline movement. This is evaluated by simulating rates of marsh shoreline movement calculated using various user-specified datasets, including relevant sources of error associated with mapping marsh shorelines and performing analyses.

A high-level description of the simulation process:
1. Build an "ideal" shoreline movement dataset (date versus transect-position) based on a regular sampling interval
2. Add simulated measurement errors at each time step, using modeled errors relevant for the tested technology
3. Fit a linear regression to produce the shoreline change rate for the individual transect
4. For scenarios with multiple transects, repeat for each additional transect, and average the rates among all transects
5. Repeat the above for the specified replications (default 100) to obtain a distribution of representative rates
6. Compare the resulting distribution (middle 95%) to a rate of 0 m/yr to determine whether the approach is suitable

In these simulations, measurement and analysis errors are modeled either using (1) standard random variables (e.g. instrument precision using random normal variables, pixel error using uniform random variables), or (2) by sampling from datasets of measurement error collected by myself and collagues (e.g., interoperator variability in RTK GPS shoreline mapping, among-transect variability in movement rates based on a large dataset of marsh shoreline movement in coastal Alabama). See the linked publications for additional details.

This app was built using the `shiny` package, hence, `shiny` Shores!

## App Parameters
*Shoreline data type*

Type of shoreline data used in the simulations. Depending on the selection (RTK/GPS, or the various imagery types), relevant sampling errors will be included based on repeated measurements of (1) in-field RTK surveys or (2) digitizations of shoreline imagery. This selection also toggles the display of relevant parameters below, as well as relevant defaults.

*GPS precision (RMSE or std dev, meters)*

Horizontal precision of the GPS instrument, measured  as a standard deviation of repeated measurements, or equivalently, the RMSE. Default value of 0.15 m corresponds to reported precision of the Emlid Reach RS2 RTK receiver.

*Image resolution (meters/pixel)*

Image resolution or pixel size.

*Image georectification error (RMSE or std dev, meters)*

Horizontal precision of the image georectification, measured as a standard deviation of repeated measurements, or equivalently, the RMSE. Some imagery surveys instead report a confidence for a given percentage. You can estimate the RMSE by using the Z-score for the given percentage. E.g., for a survey with positioning within 5 meters at 90% confidence, the RMSE is `5 / qnorm((1-0.9)/2, lower.tail = FALSE)`

*Include seasonal errors?*

Should simulations include sampling across different seasons? An additional error term is added to each sampling event to represent the magnitude of marsh shoreline movement among seasons based on a dataset from the Grand Bay NERR, MS.

*Shoreline movement rate (m/yr)*
The actual linear rate of shoreline movement being simulated. For exploratory analyses, consider obtaining an estimate of shoreline movement rates based on nearby systems or by taking a few measurements using a timeseries of publicly available imagery, e.g., using Google Earth.

*Total duration of timeseries (years)*

The maximum total duration of the shoreline measurement timeseries. Depending on the input interval, fewer than the maximum number of years may be simulated. E.g., a max of 10 years with a 3 year interval will result in a dataset that spans 9 years.

*Sampling interval (years)*

The sampling interval between successive shoreline measurements, i.e., sampling occurs every X years. Depending on the input interval, fewer than the maximum number of years may be simulated. E.g., a max of 10 years with a 3 year interval will result in a dataset that spans 9 years.

*Transects monitored*

Integer number of synthetic cross-shore transects used to track marsh movement, across a single site. When using more than 1 transect, an additional error term is included to represent among-transect variation in shoreline movement rates, based on a dataset of marsh shoreline movement rates from coastal Alabama. Increasing the number of transects tends to improve the precision of change rate estimates. I recommend using a regular transect spacing interval (e.g., every 10-20 meters along-shore) to determine the number of transects for a particular site, with the total number depending on the site size. I discourage increasing the number of transects by reducing the spacing interval, as this risks pseudoreplication of shoreline measurements.

*Percentile range (0-100)*
The middle percentile of observations used to define the expected range of shoreline change estimates (lower to upper). This value is also used to define which scenarios provide suitable estimates. Suitable estimates are defined as having the lower bound greater than 0. E.g., for a 95% percentile range, if the bottom 2.5% of estimates includes values less than 0 m/yr, then too many simulated shoreline change rates estimate that the shoreline is moving in the wrong direction, therefore, the provided data are not suitable.

*Model replicates*

Number of times the simulation is repeated to generate a range of estimates, i.e., the number of observations in the plotted histogram. Default of 100. This value and the number of transects will determine how long it will take to run the simulations. Avoid values greater than 1000, particularly for a large number of transects (greater than 100).

## App Outputs

*Histogram of slopes*

The displayed histogram will update after each simulation run (including all replicates). This histogram shows the distribution of simulated slopes for the latest model run. The green line represents your input move rate, and the lower and upper bounds of the distribution based on your percentile range are drawn as blue lines. The suitability of this model is indicated as text in the top right based on the position of the lower bound in relation to 0.

*Table of results*

Each simulation run (including all replicates) will add a new row to the table. The table fields include all of the input parameters for the simulation, plus fields indicating the suitability of the simulated dataset and the lower and upper range of the modeled slopes (based on the specified percentile range). The table can be exported as a CSV using the button.
