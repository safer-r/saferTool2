
# saferTool2 <a href="">[<img src="man/figures/saferTool2.png" align="right" height="140" />](https://safer-r.github.io/saferTool2)</a>

<br />

<!-- badges: start -->

[![Codecov test coverage](https://codecov.io/github/safer-r/saferTool2/coverage.svg?branch=master)](https://app.codecov.io/github/safer-r/saferTool2?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/saferTool2)](https://cran.r-project.org/package=saferTool2)
[![downloads](https://cranlogs.r-pkg.org/badges/saferTool2)](https://www.rdocumentation.org/trends)
[![](https://img.shields.io/badge/license-GPL3.0-green.svg)](https://opensource.org/licenses/MITgpl-3-0)
[![rworkflows](https://github.com/safer-r/saferTool2/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/safer-r/saferTool2/actions/workflows/rworkflows.yml)
<!-- badges: end -->

<br />

## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [Acknowledgements](#acknowledgements)

<br />

## Description

Set of R functions for the development of R functions, written according to the [safer project](https://github.com/safer-r) specifications.

<br />

## Content
<br />

| Function | Description |
| :--- | :--- |
| **codon2aa()** | Convert codon to amino acid using standard genetic code indicated in the [DNA and RNA codon tables](https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables). |
| **codon_finder()** | Gives the codon number and position in the codon of nucleotid positions. |
| **permut()** | Reorder the elements of the data1 vector by flipping 2 randomly selected  consecutive positions either: times (when n is precised) or: until the correlation between data1 and data2 decreases down to the cor.limit (0.2 by default). Example of consecutive position flipping: ABCD -> BACD -> BADC, etc. Designed for discrete values, but works also for continuous values. |
| **slide()** | Return a computation made on a vector using a sliding window. |
| **trim()** | Trim and display values from a numeric vector or matrix. Plot 4 graphs: stripchart of values, stripchart of rank of values, histogram and normal QQPlot. Different kinds of intervals are displayed on the top of graphes to facilitate the analysis of the variable and a trimming setting. The trimming interval chosen is displayed on top of graphs.Both trimmed and not trimmed values are returned in a list. |

<br />

Read `vignette("saferTool2")` for more details.

<br />

## Versions

The different *saferTool2* releases are tagged [here](https://github.com/safer-r/saferTool2/tags).

<br />

## Installation

*saferTool2* can be currently be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("https://github.com/safer-r/saferTool2") # or open R as admin and remotes::install_github("https://github.com/safer-r/saferTool2", lib = "C:/Program Files/R/R-4.3.1/library")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/safer-r/saferTool2/tree/", v))
```

<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses or in the Licence.txt attached file.

<br />

## Citation

If you are using functions of *saferTool2*, please cite: 

> Wang H, Han Y, Serizay J, Millot GA (2023). _The R saferTool2 package_.
> <https://github.com/safer-r/saferTool2/>.

<br />

## Credits

[Haiding Wang](https://github.com/Tintin2710), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Yushi Han](https://github.com/yushiHn/), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizay](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## Acknowledgements

The developers & maintainers of the mentioned softwares and packages, including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [Github](https://github.com/)



