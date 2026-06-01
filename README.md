# Shiny-ssTaxSEA

A Shiny frontend for [TaxSEA: Taxon Set Enrichment Analysis](https://github.com/feargalr/TaxSEA), extended with **single-sample TaxSEA (ssTaxSEA)** support and updated to TaxSEA v1.4.0.

> **ssTaxSEA** is available on Bioconductor:
> [Single-sample enrichment vignette](https://bioconductor.org/packages/devel/bioc/vignettes/TaxSEA/inst/doc/single-sample-enrichment.html)

---

## Features

- Interactive Shiny interface for TaxSEA differential abundance enrichment
- Single-sample TaxSEA (ssTaxSEA) analysis
- Upload your own `.csv` or `.xlsx` data
- Dynamic plots that update as you select up to 8 taxa
- Downloadable test data to check expected input format

---

## Quick Start

Open the app and run the built-in **test data** to explore the interface — plots update interactively as you select taxa in the table.

### Input: TaxSEA

Supply a `.csv` or `.xlsx` file with **4 columns in this order**:

| Column | Description | Example |
|--------|-------------|---------|
| 1 | Taxa name | *Bacteroides fragilis* |
| 2 | Rank score | log2 fold change |
| 3 | P value | 0.03 |
| 4 | Padj (FDR) | 0.05 |

> Column headers are ignored — **column order matters**.

### Input: ssTaxSEA

Additionally provide a **count table** (`.csv` or `.xlsx`) with:
- **Rows** = taxa
- **Columns** = samples

---

## Running Locally

Clone the repository:

```bash
git clone https://github.com/Cong34/Shiny-ssTaxSEA
```

Open `Shiny-ssTaxSEA.Rproj` in RStudio, then install dependencies:

```r
install.packages("devtools")
library(devtools)
install_github("feargalr/TaxSEA")

install.packages(c(
  'shiny', 'tidyverse', 'ggrepel',
  'bslib', 'bsicons', 'openxlsx2', 'DT'
))
```

Open `app.R` and click **Run App**.

---

## Acknowledgements

Built upon [Shiny-TaxSEA](https://github.com/feargalr/TaxSEA) by Feargal Ryan.
