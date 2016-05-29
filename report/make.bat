cd %~dp0
pandoc -s -S --filter pandoc-citeproc report.md --bibliography refs.bibtex --csl style.csl --number-sections -o report.pdf
