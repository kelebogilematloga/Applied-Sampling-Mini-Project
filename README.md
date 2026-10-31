# Applied Sampling Mini Project

**Project title:** Estimating Mean Monthly Electricity Outage Duration: Comparative Study of Stratified Sampling and Simple Random Sampling Without Replacement in a Simulated Municipality

**Author:** Kelebogile Matloga

## Project Overview

This project investigates the estimation of mean monthly electricity outage duration among residential households in a hypothetical South African municipality.

A simulated finite population of 10,000 households is generated and divided into four settlement-type strata: urban suburbs, townships, rural villages and informal settlements.

The project compares two probability sampling designs:

- Stratified Random Sampling Without Replacement (Stratified SRSWOR).
- Simple Random Sampling Without Replacement (SRSWOR).

The analysis includes sample-size determination, finite population generation, sample selection, non-response adjustments, design-based estimation and a 1,000-repetition Monte Carlo simulation.

The population and household data are simulated and do not represent an actual municipality.

## Repository Files

| File | Description |
|---|---|
| `Applied-Sampling-Mini-Project.Rmd` | Main R Markdown report containing the analysis. |
| `Applied Mini Project RScript.R` | Separate R script containing the project code. |
| `references.bib` | Bibliographic references. |
| `apa.csl` | APA citation style file. |
| `population.csv` | Generated dataset containing the 10,000 simulated households. |
| `sample.csv` | Generated dataset containing the selected stratified sample, including non-respondents. |
| `Final-Report.pdf` | Final knitted project report, if included. |

## Requirements

The analysis is conducted in R using the following packages:

- `survey`
- `dplyr`
- `ggplot2`
- `xtable`
- `knitr`
- `rmarkdown`

Install the packages using:

```r
install.packages(c(
  "survey", "dplyr", "ggplot2",
  "xtable", "knitr", "rmarkdown"
))
```

A working LaTeX installation is also required to generate the PDF report.

## Running the Analysis

1. Download or clone the repository.
2. Open the project folder in RStudio.
3. Open `Applied-Sampling-Mini-Project.Rmd`.
4. Check the working directory in the setup chunk and update the `setwd()` path to match your computer.
5. Click **Knit** in RStudio to run the analysis and generate the PDF report.
6. The analysis automatically generates `population.csv` and `sample.csv` in the working directory.

## Accessing the CSV Files

The generated CSV files can be opened in Excel or imported directly into R.

To import the datasets:

```r
population <- read.csv("population.csv")
sample <- read.csv("sample.csv")
```

The population dataset contains all 10,000 simulated households, while the sample dataset contains the households selected for the realised stratified sample, together with their selection probabilities, weights and response indicators.

## Reproducibility

Random seeds are specified in the R code to support reproducibility. The finite population is generated once and held fixed throughout the realised-sample analysis and the repeated-sampling simulation.