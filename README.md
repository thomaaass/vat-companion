# VAT base-broadening trade-off, 2029–30

Companion page to the report, with the figures, tables and charts behind it.

- **Companion page:** https://thomaaass.github.io/vat-companion/

## Files

| File | What it is |
| --- | --- |
| `vat_companion.html` | The companion page. Self-contained; charts are embedded. |
| `vat_2029.R` | All inputs, assumptions and calculations. Part 9 reproduces every number and chart on the companion page. |

## Reproduce

```sh
Rscript vat_2029.R
```

Needs R with `ggplot2` (and `jsonlite` for `companion_data.json`). It prints the tables and writes the charts as PNG files.

Sources are cited on the page: IFS Green Budget 2025, ONS Family Spending 2024–25, OBR October 2026, HMRC ready reckoner, Resolution Foundation.
