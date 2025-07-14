# Food restriction controls biomass and nutrient fate during animal growth: evidence from a terrestrial consumer

<p align="left">
<a href="#overview">Overview</a><br> •
<a href="#features">Content</a><br> •
<a href="#installation">Installation</a><br> •
<a href="#usage">Usage</a><br> • 
<a href="#citation">Citation</a><br> •
</p>

## Overview

This research compendium contains data and code for the paper: "Food restriction controls biomass and nutrient fate during animal growth: evidence from a terrestrial consumer"

## Content

This repository is structured as follow:

-   [`R/`](https://github.com/samuelcharberet/intake_rate_experiment/tree/master/R):
    contains R functions developed especially for this project
    
-   [`make.R`](https://github.com/samuelcharberet/intake_rate_experiment/tree/master/make.R):
    main R script to run the entire project

-   [`R/`](https://github.com/samuelcharberet/intake_rate_experiment/tree/master/_targets.R):
    contains the pipeline architecture

-   [`1_data/`](https://github.com/samuelcharberet/intake_rate_experiment/tree/master/1_data):
    contains the project data

-   [`1_data/`](https://github.com/samuelcharberet/intake_rate_experiment/tree/master/4_output):
    contains the project figures and tables

    
## Installation

To install this compendium:

-   [Fork](https://docs.github.com/en/get-started/quickstart/contributing-to-projects)
    this repository using the GitHub interface.
-   [Clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
    your fork using `git clone fork-url` (replace `fork-url` by the URL
    of your fork). Alternatively, open [RStudio
    IDE](https://posit.co/products/open-source/rstudio/) and create a
    New Project from Version Control.

## Usage

Launch the
[`make.R`](https://github.com/samuelcharberet/9_caloradapt/tree/master/make.R)
file with:

    source("make.R")

Alternatively, make use of the targets package with:

    targets::tar_make()

**Notes**

-   All required packages listed in the `DESCRIPTION` file will be
    installed (if necessary)
-   All required packages and R functions will be loaded
-   Some analyses listed in the `make.R` might take time

## Citation

Please use the following citation:

> **{{ ADD A CITATION }}**