
# healthtrends

<figure>
<img src="https://img.shields.io/badge/lifecycle-development-orange.svg"
alt="Lifecycle: in development" />
<figcaption aria-hidden="true">Lifecycle: In Development</figcaption>
</figure>
<br>
<br>
The healthtrends package has been designed to create reports in the style
of the Health Trends for England report using Quarto. The package uses data
from the Fingertips platform displayed for a selected group of topics and
indicators at different geographical levels. Users can use the package to
create their own versions of the Health Trends for England report using their
own data and style. 

## Installation

### With remotes

You can install the latest version from GitHub using
[remotes](https://CRAN.R-project.org/package=remotes).

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github(
  repo = "dhsc-govuk/healthtrends",
  upgrade = "always"
)
```

## Examples

### Using the package

``` r
# Load the library
library(healthtrends)

# Setup the file structure and get required files
import_healthtrends_files()

# Generate report
generate_reports(
  refresh_data = TRUE,
  config_file = "input/config.yaml")
```

## Vignettes

You can view the vignettes for health trends by running the following code:
``` r
# Load the library
library(healthtrends)

# View the vignettes
vignette("healthtrends")
```

Alternatively, you can view the vignettes in browser with:
``` r
# Load the library
library(healthtrends)

# View the vignettes
browseVignettes("healthtrends")
```

## QA status

QA has been completed.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://dhsc-govuk-internal.github.io/user-guide.github.io/code-of-conduct.html).
By contributing to this project, you agree to abide by its terms.

## Licence

Unless stated otherwise, the codebase is released under the MIT License.
This covers both the codebase and any sample code in the documentation.
The documentation is © Crown copyright and available under the terms of
the [Open Government 3.0
licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

