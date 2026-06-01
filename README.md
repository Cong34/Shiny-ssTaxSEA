# Shiny-ssTaxSEA

A Shiny frontend for [TaxSEA: Taxon Set Enrichment Analysis](https://github.com/feargalr/TaxSEA), extended with **single-sample TaxSEA (ssTaxSEA)** support and updated to TaxSEA v1.4.0.

> **ssTaxSEA** is available on Bioconductor:
> [Single-sample Taxon Enrichment Analysis](https://bioconductor.org/packages/devel/bioc/vignettes/TaxSEA/inst/doc/single-sample-enrichment.html)

---

## Quick Start

Open the app and run the built-in **test data** to explore the interface — plots update interactively as you select taxa in the table.

### Input: TaxSEA

Supply a `.csv` or `.xlsx` file with **4 columns in this order**:
- Taxa (e.g. species/genus)
- rank (e.g. log2 fold changes)
- P value
- Padj (FDR)

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
