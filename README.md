# EBS pollock openMSE working paper

[Published site](https://jimianelli.github.io/openMSE_poll/) ·
[Document evaluation](https://jimianelli.github.io/openMSE_poll/reports/evaluation.html) ·
[Updated inputs](https://jimianelli.github.io/openMSE_poll/reports/updated-inputs.html)

This project preserves the January 2026 SCA-to-OM demonstration and adds an
October 2026 document review, file-level input audit, and development plan.
The demo uses data through 2024. A validated 2026 OM/MSE remains pending.

The review found an incorrect F/FMSY field in the Slick converter, discarded
composition sample sizes, a change in natural mortality during conditioning,
replicated fitted histories, and unresolved annual input dimensions. The
historical rendered walkthrough carries a prominent notice. Scientific changes
are proposed in the evaluation, not silently applied to its original code.

## Repository structure

- `sca_openmse.qmd`, `R/sca_openmse.R`, `R/mse2slick.R`: original analytical sources.
- `R/audit_inputs.R`: explicit input audit, producing `site/data/` summaries.
- `reports/`: Quarto review sources and the marked historical HTML snapshot.
- `site/data/`: aggregate audit tables, fingerprints, public diagnostic snapshots.
- `docs/`: complete rendered website, deployed by the Pages workflow.
- `validation/`: release checks and their evidence.

Raw external assessment inputs and workbooks remain in their owning projects.
Binary input/output files and caches are ignored. The archived R script with a
machine-local path and the older duplicate diagnostic reports are excluded from
the public payload. There is no credential requirement for rendering the review.

## Rebuild the review

Use R 4.6.1 and Quarto 1.10.18 or consult the recorded environment for the actual
rendering versions. Install `dplyr`, `tidyr`, `readxl`, `ggplot2`, `ggthemes`, and
`gt` into an R library appropriate for your machine, then run from this root:

```sh
quarto render
python3 scripts/verify_site.py
```

The review consumes saved audit CSVs. This command executes the report chunks
in an independent R session and does not run SCA or MSE. MSEtool, SAMtool, and
Slick are needed for the original analytical workflow; their installation and
version pinning belong to the next analytical revision.

To refresh the local audit, place the historical demo cache in `data/pm_24.rds`
and supply four explicit paths:

```sh
Rscript --vanilla R/audit_inputs.R /path/to/pm_25.dat /path/to/pm_26.dat /path/to/BTS_Index_agecomps26.xlsx /path/to/weight_at_age.csv
```

The script records malformed input dimensions and sheet-read failures rather
than treating a completed audit as acceptance of those model inputs. For the
Model 26.0 diagnostic snapshot, `site/data/public_source_verification.json`
records source URLs and hashes; the local working diagnostics are kept distinct
in `model26_source_comparison.csv`.

## Publish

Render and verify before committing `docs/`. GitHub Pages uses a GitHub Actions
workflow to validate and deploy only `docs/`; raw data and model computation
are outside its deployment artifact. The default branch is `main`.

The site and source constitute an exploratory working paper. Model names,
proposals, adopted assessment advice, imported diagnostics, and newly executed
checks are identified separately in the review.
