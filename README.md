# Global TB notifications

## Quick start

To create manuscript figures and tables:

1. Knit `rmd/generate_prediction_data.Rmd`
2. Knit `rmd/generate_manuscript_artefacts.Rmd`

The resulting figures and tables are saved in the `figures` directory.
This is sub-divided into a `main` and `supplement` directory.
The generated tables are embedded in `tables.docx`

## Model selection

Time-series model selection information is included in the
`rmd/model_comparison.Rmd` file.

A more verbose version is available in `rmd/arima_plotting.Rmd`

## Data quality

See `rmd/data_quality.Rmd`.

## Dependencies

```r
devtools::install_github('petedodd/wbmapdata')
suppressMessages(library(tidyverse))
library(here)
library(data.table)
library(metafor)
library(forecast)
library(imputeTS)
library(patchwork)
library(flextable)
library(officer)
library(scales)
library(ggrepel)
library(ggpubr)
library(wbmapdata)
library(sf)
```
