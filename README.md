# hoaddash

<!-- badges: start -->
[![build.yaml](https://github.com/subugoe/hoaddash/actions/workflows/build.yaml/badge.svg)](https://github.com/subugoe/hoaddash/actions/workflows/build.yaml)

## About 

Source code repository for building and hosting the Hybrid Open Access Dashboard (HOAD). A [blog post](https://www.coalition-s.org/blog/introducing-the-hybrid-open-access-dashboard-hoad/) introduces the aim of the dashboard and some key functionalities.

The dashboards are built using [Quarto](https://quarto.org/), an open source tool for scientific and technical publishing. Quarto documents contain text and analytical source code, in our case R, and are rendered to HTML. More specifically, we created parameterised reports from which Quarto generates publisher-specific views. The resulting website is published on GitHub pages.

The dashboards are built using GitHub Actions. 

## Templates

This repository contains templates that are used to build specific Quarto documents.

### Transformative Agreement Data

- `_jct_overview.qmd`: - Overview of key indicators for hybrid journals included in the [cOAlition S Transformative Agreement Public Data](https://journalcheckertool.org/transformative-agreements/).
- `_oam_overview.qmd`: - Overview of key indicators for hybrid journals included in consortial transformative agreements in Germany according to the [German Open Access Monitor](https://doi.org/10.26165/JUELICH-DATA/VTQXLM).

### Portfolio-specific dashboards 

- `_portfolio_template.qmd`: - Template for creating portfolio-specific Quarto documents. `create_qmd.R` contains code used to create these Quarto documents. Scripts loads the configuration from `create_qmd.R`. 

## Software dependencies

Underlying data analysis uses the R programming language. [`DESCRIPTION`](DESCRIPTION) gives a detailed overview of R package dependencies. The `R` folder contains a collection of R functions and scripts used to build the dashboards.

## Data

Data is openly available through the R package [hoaddata](https://subugoe.github.io/hoaddata/).

## Funding

HOAD is a scholarly communications analytics project funded by the [German Research Foundation (DFG)](https://gepris.dfg.de/gepris/projekt/416115939) to [support the implementation of consortial transformative agreements in Germany](https://doi.org/10.5282/o-bib/2017H2S87-95). The work is carried out at the [Göttingen State and University Library (SUB Göttingen)](https://www.sub.uni-goettingen.de/sub-aktuell/).

## Contribute

This dashboard has been developed in the open using open tools. There are a number of ways you can help make the dashboard better:

- If you don’t understand something or find a bug, please let us know and submit an [issue](https://github.com/subugoe/hoaddash/issues).
- Do you have an idea of what meaningful data analysis is missing? Please let everyone know and submit an [issue](https://github.com/subugoe/hoaddash/issues).
-  Want to provide more context about your agreement? [Please email us](mailto:najko.jahn@sub.uni-goettingen.de)!
- Let us know if we can help you to provide a detailed view of your consortial transformative agreements. We are not limited to Germany and can extend our dashboards to other countries and consortia. [Please email us](mailto:najko.jahn@sub.uni-goettingen.de)!

## Contributors

SUB Göttingen Team: Dr. Inke Achterberg, Nick Haupka, Dr. Anne Hobert, Najko Jahn, Dr. Birgit Schmidt.

## Contact

Najko Jahn: najko.jahn@sub.uni-goettingen.de

## License

[CC 0](LICENSE.md)


