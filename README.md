# Cryptodata coding challenge

**Reference:** the data was obtained from [**Cryptocompare API**](https://www.cryptocompare.com/api/#). It's about the Bitcoin price against EUR.

**Objectives:** to predict if the price will go up or down,
1. in the next hour;
2. for the next 6 hours, i.e., one prediction per hour;

**Workflow** (main steps): the analysis was performed using the [R](https://cran.r-project.org/) to,
1. fetch histohour data;
2. data preparation;
3. model fitting;
4. forecasting;
5. save workspace.


**Used the following R packages:**
* [base](https://cran.r-project.org/bin/windows/base/old/3.2.3/)
* [stringi](https://cran.r-project.org/web/packages/stringi/index.html)
* [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
* [anytime](https://cran.r-project.org/web/packages/anytime/index.html)
* [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html)
* [forecast](https://cran.r-project.org/web/packages/forecast/index.html)
* [tseries](https://cran.r-project.org/web/packages/tseries/index.html)
