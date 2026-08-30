Unsere Wege - GitHub Pages patch

Replace/add these repository files:

Scripts/02_Dashboard.Rmd
.github/workflows/pages.yml

Then:

1. Run Scripts/01_Preprocess.R if the cleaned inputs need updating.
2. Knit Scripts/02_Dashboard.Rmd locally.
   It now writes:
     Data/3_Output/index.html
     Data/3_Output/<dependency folder(s)>
3. Confirm Data/3_Output is NOT excluded by .gitignore.
4. Commit and push the Rmd, workflow, index.html and all dependency files/folders.
5. On GitHub:
     Settings > Pages > Build and deployment > Source > GitHub Actions

After that, every push changing Data/3_Output automatically redeploys the site.

The dashboard is deliberately no longer self-contained. This makes it
more suitable for normal web hosting and mobile Safari than a single
very large embedded HTML file.
