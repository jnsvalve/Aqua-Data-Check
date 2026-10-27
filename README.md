# Universal Aquaculture Data Harvest & Harmonisation

An in-progress R workflow for comparing aquaculture production statistics from Eurostat and FAO, with a complementary EUMOFA market dataset and separate economic, social, and sales records from the EU Data Collection Framework (EUDCF). The data model and analysis outputs are evolving.

## Sources

- **Eurostat**: The [`fish_aq2a` dataset](https://ec.europa.eu/eurostat/databrowser/view/fish_aq2a/default/table?lang=en) reports annual aquaculture production by country and species, including quantity and value. Eurostat compiles official national statistics; EU Member States report aquaculture statistics under [Regulation (EC) No 762/2008](https://eur-lex.europa.eu/eli/reg/2008/762/oj/eng). The published country coverage can include EEA partners and varies by table and year.
- **FAO**: [Global Aquaculture Production](https://www.fao.org/fishery/en/collection/aquaculture), distributed through FishStatJ and downloadable exports, provides annual production quantity and value across countries or territories, species, and culture environments. FAO says these are primarily official national statistics submitted through its annual questionnaire; it may supplement or estimate missing reports, with FAO estimates flagged in the data. This is an international reporting series, not an EU statistical obligation, and its country coverage and estimates need not match Eurostat's.
- **EUDCF**: The [EU aquaculture data call](https://dcf.ec.europa.eu/data-calls/aquaculture_en) collects economic and sales variables under the EU Data Collection Framework, with data organized by year, variable, species, and sector segment. The framework is established by [Council Regulation (EU) 2017/1004](https://eur-lex.europa.eu/eli/reg/2017/1004/oj/eng). For economic data, [Commission Decision (EU) 2021/1167, point 6.1 and Tables 10–11](https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:32021D1167) specifies annual collection from enterprises whose principal activity is NACE 03.21 (marine aquaculture) or 03.22 (freshwater aquaculture), using the prescribed sector segmentation. This is narrower than a production-wide frame that includes enterprises with aquaculture as a secondary activity; other DCF data types may have their own population rules. EUDCF records are retained separately from production observations.
- **EUMOFA**: The [European Market Observatory for Fisheries and Aquaculture Products](https://fishery-aquaculture-market-observatory.ec.europa.eu/) is a European Commission market-intelligence tool, not a primary statistical reporting obligation equivalent to Eurostat or FAO. Its analyses use market indicators such as first sales, prices, trade, and consumption; this project imports its [yearly aquaculture dataset](https://fishery-aquaculture-market-observatory.ec.europa.eu/en/data/dashboards/aquaculture), which provides volume and value by country, commodity, and species. Treat it as a complementary market-oriented series: its classifications, coverage, and purpose do not define the same frame as Eurostat, FAO, or EUDCF.

These sources do not describe an identical population. Production totals should be interpreted within each source's own country coverage, reporting rules, and categories; differences between totals are not necessarily differences in production alone.

## Workflow

- Source records are normalized into a long observation table while retaining native category identifiers, source values/units, flags/status, and comparison values in tonnes/EUR.
- Published Eurostat `F00` totals remain distinct from species observations. FAO and EUMOFA categories are kept source-native; summaries choose their aggregation rules downstream.
- Freshwater and low-anthropic memberships are derived separately, so observations remain available even when they match no segment.
- EUDCF rows retain their workbook tables, variable codes, segments, values, and units as a separate dataset; sales and economic measures are not included in production comparisons.
- Current summaries and diagnostics are produced from the normalized observations.

## Run

Requires R 4.1 or newer for the native pipe operator.

From the repository root:

```powershell
Rscript main-script.R
```

Eurostat is optional for partial data collection. A complete run refreshes the latest summaries and diagnostics. A partial run saves a separate partial snapshot and leaves the latest outputs unchanged.
The EUDCF workbook is optional and does not affect production coverage status. Place the official workbook in `data/EUDCF/` using the configured filename in `main-script.R`.

## Updating Source Data

Check for new annual releases at [FAO FishStatJ](https://www.fao.org/fishery/en/statistics/software/fishstatj) and the [EUMOFA aquaculture dashboard](https://fishery-aquaculture-market-observatory.ec.europa.eu/en/data/dashboards/aquaculture). No supported public API is currently documented for these exports, so refresh them from the official downloads:

1. Export FAO quantity and value CSVs, plus the yearly EUMOFA CSV. Keep dated copies of the original downloads for reproducibility.
2. Check that the files cover the expected years and that row counts or schema changes are understood.
3. Replace the active files in `data/FAO/` and `data/EUMOFA/`. The importers check required columns and types, including numeric parse failures.
4. From the repository root, run the tests and then `Rscript main-script.R`. Review the source row counts and confirm the complete outputs were refreshed.

For EUDCF, download the workbook from the [EU aquaculture data call](https://dcf.ec.europa.eu/data-calls/aquaculture_en) and retain its original sheet names and columns. The importer keeps economic, social, current sales, and historical sales rows separate; workbook pivot sheets are not imported.

Refresh when a new annual release is published; do not automate dashboard scraping unless the source provides a stable, documented download endpoint.

## Outputs

- `output/normalized/aquaculture_observations.rds` — complete snapshot with production observations, segment memberships, separate EUDCF workbook tables when available, and source/FX manifest.
- `output/normalized/aquaculture_observations_partial.rds` — partial snapshot, when a source is unavailable.
- `output/general/Europe_summary.xlsx`
- `output/general/production_comparisons.xlsx` — all-production, freshwater, and low-anthropic comparisons for the configured years and countries.
- `output/general/comparison_plots/` — quantity and value charts for each comparison segment; partial runs use `_partial.png` without replacing complete-run charts.
- `output/freshwater/Europe_freshwater_summary.xlsx`
- `output/low_anthropic/Europe_low_anthropic_summary.xlsx`
- `output/freshwater/freshwater_consistency.csv`
- `output/diagnostics/` — FAO environment and species-group counts.

Normalized snapshots are excluded from Git. Summary and diagnostic files are regenerated by complete runs.

### Plot Defaults

The default comparison window is the latest 20 completed calendar years (2006–2025 when run in 2026), configured by `years_target` in `main-script.R`. Sources may have shorter coverage or publication delays; missing observations are not filled in. Quantity and value plots use comma-separated y-axis numbers rather than scientific notation.

General comparison plots sum reported production across all available countries for each year and source, separately for each category (for example, `mollusca_all`). The default `comparison_countries = NULL` includes all countries; set it to a vector of country codes to restrict coverage. Source totals reflect each dataset's own country coverage, not a common-country subset. Missing country values are omitted from sums, but groups with no reported values remain missing, not zero. Country-level rows remain in the comparison tables.

To inspect individual countries, call `build_comparison_plot(comparison_table, measure, segment, facet_countries = TRUE)` using a table from `pipeline_result$exports$comparison_tables`; the default is `FALSE` for overview plots.

Low-anthropic overview plots label panels by category, without repeating the strict/broad mode. Both modes remain in the exported tables and retain separate series:

- **Strict**: `mussels_bivalves` and `seaweed_algae`, selected using source species/commodity names; FAO aquatic plants also use the `PLANTAE AQUATICAE` major group.
- **Broad**: `mollusca_all` and `aquatic_plants_all`, using the full FAO `MOLLUSCA` and `PLANTAE AQUATICAE` major groups and matching EUMOFA names. Eurostat reuses the strict matches under these broader category names, so broad coverage is not necessarily wider for every source.
- **Optional broad category**: `crustacea_subset` includes name-matched crustaceans only when `include_crustacea_subset` is enabled (off by default).

Strict and broad categories overlap and should not be added together.

## Tests

```powershell
Rscript -e "testthat::test_dir('tests/testthat')"
```

## R Code Style

The project uses [grkstyle](https://github.com/gadenbuie/grkstyle) as the default style for `styler` add-ins. Install it once from the author's R-universe:

```r
install.packages(
	"grkstyle",
	repos = c(gadenbuie = "https://gadenbuie.r-universe.dev", getOption("repos"))
)
```

The project `.Rprofile` selects the style when opening the repository in RStudio.
