# Shiny-ssTaxSEA
Shiny-TaxSEA is a Shiny frontend to [TaxSEA: Taxon Set Enrichment Analysis](https://github.com/feargalr/TaxSEA)
And Shiny-ssTaxSEA is an updated to Shiny-TaxSEA, with TaxSEA version 1.4.0, inclusion of single sample TaxSEA. 


## Quick Start

Start by analysing test data, the plots update as you select up to 8 taxa in the table.

For Running TaxSEA:
When you're ready to analyse your own data, supply a .csv or .xlsx file with the following columns (column title/header doesn't matter, but order does!): Taxa (e.g. species/genus), rank (e.g. log2 fold changes), P value, Padj (FDR). You can also download test data to see the expected format.

For Running ssTaxSEA:
You will also need a count table, in either .csv or .xlsx file with rows as taxa and columns as samples. 

## Running locally
The project is not packaged nicely yet, so to run it locally clone the repository:
```
git clone https://github.com/Cong34/Shiny-ssTaxSEA
```

Then open the project `Shiny-ssTaxSEA.Rproj` in RStudio and install dependencies:
```{r output}
library(devtools)
install_github("feargalr/TaxSEA")
install.packages(c('shiny', 'tidyverse', 'ggrepel', 'bslib', 'bsicons', 'openxlsx2', 'DT'))
```

Finally, open `app.r` and click 'Run App'.
