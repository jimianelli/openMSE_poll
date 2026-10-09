# Release checks — 9 October 2026

Product: exploratory MSE working paper and document review. This is an input
and reporting audit; no assessment or MSE was newly fit.

| Check | Outcome | Evidence |
|:--|:--|:--|
| Fresh R input audit | Pass | `R/audit_inputs.R` returned `INPUT_AUDIT_PASS`; hashes and output CSVs stored under `site/data`. |
| Annual model-input acceptance | Pending | `pm_25.dat` requires 62 annual weight rows; observed fishery/spawning rows are 61/63. |
| 2026 update status | Pending | `pm_26.dat` retains the 2024 terminal year; workbook products have different vintages. |
| R source parsing | Pass | Original R sources and the new audit script parsed without errors. |
| Model/graph compilation | Not Tested | No C++ template or model graph compiled; MSEtool, SAMtool, and Slick absent from the inspected environment. |
| New fit gradient/Hessian | Not Tested | No new fit; original saved time-series output lacks these diagnostics. Imported Model 26.0 diagnostics are version-labelled in the audit. |
| Independent Quarto render | Pass | R 4.6.1, Quarto 1.10.18; four current review pages rendered with no unexplained warnings/errors. |
| HTML links/fragments/assets | Pass | `scripts/verify_site.py`; five HTML files including the historical snapshot. |
| Public payload | Pass | Credential and machine-local path scan; raw binaries and caches excluded from tracked files. |
| Browser visual inspection | Pass | Overview, audit tables, 2024–2026 age figure and lightbox, historical notice inspected. |
| HTML source accessibility | Pass for current review | Language, heading hierarchy, semantic tables, chart alt text, caption, and textual summaries inspected. |
| Full accessibility conformance | Not Tested | Screen-reader and full agency/WCAG validation pending; historical figures require an analytical revision. |

The archived January HTML retains its original maturity warnings, progress logs,
and unresolved conditional metric-figure reference. These are recorded findings,
and the visible notice directs readers to the review. The current review pages
contain no unresolved figure/table references.

The public repository preserves the original analytical sources and reports
proposed scientific corrections separately. The original local binary data,
saved fits, duplicate reports, and caches remain in the source folder and are
excluded from Git and the Pages deployment payload.
