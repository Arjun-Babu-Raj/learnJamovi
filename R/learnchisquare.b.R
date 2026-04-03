
LearnChiSquareClass <- R6::R6Class(
    'LearnChiSquareClass',
    inherit = LearnChiSquareBase,
    private = list(

        .run = function() {

            if (self$options$showTheory)
                self$results$theory$setContent(private$.theoryHtml())

            if (self$options$showInstructions)
                self$results$instructions$setContent(private$.instructionsHtml())

            rows_var <- self$options$rows
            cols_var <- self$options$cols

            if (is.null(rows_var) || is.null(cols_var))
                return()

            data  <- self$data
            rv    <- factor(data[[rows_var]])
            cv    <- factor(data[[cols_var]])
            ok    <- complete.cases(rv, cv)
            rv    <- rv[ok]; cv <- cv[ok]

            if (length(rv) < 2) return()

            tbl   <- table(rv, cv)
            res   <- chisq.test(tbl)
            n     <- sum(tbl)
            k     <- min(nrow(tbl), ncol(tbl))
            v     <- sqrt(res$statistic / (n * (k - 1)))

            self$results$chiSq$addRow(rowKey = 'pearson', values = list(
                test     = 'Pearson',
                value    = res$statistic,
                df       = res$parameter,
                p        = res$p.value,
                cramersV = v))

            row_lvls <- rownames(tbl)
            col_lvls <- colnames(tbl)
            header   <- paste(col_lvls, collapse = ' | ')
            self$results$freq$addRow(rowKey = '__header__', values = list(
                rowLabel = paste0(rows_var, ' \\ ', cols_var),
                counts   = header))
            for (rl in row_lvls) {
                counts_str <- paste(tbl[rl, ], collapse = ' | ')
                self$results$freq$addRow(rowKey = rl, values = list(
                    rowLabel = rl,
                    counts   = counts_str))
            }
        },

        .theoryHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">Chi-Square Tests</h2>

<p>Chi-square (χ²) tests are used to analyse <strong>categorical data</strong>.
They compare observed frequencies to expected frequencies to determine whether
there is a statistically significant association between variables.</p>

<h3 style="color:#2c7bb6;">Two types of chi-square test</h3>

<h4>1. Test of Independence</h4>
<p>Tests whether two categorical variables are associated.
Example: "Is there a relationship between gender and education level?"</p>
<ul>
  <li><strong>H₀:</strong> The two variables are independent (no association).</li>
  <li><strong>H₁:</strong> The two variables are associated.</li>
</ul>

<h4>2. Goodness-of-Fit Test</h4>
<p>Tests whether a single categorical variable follows a hypothesised distribution.
Example: "Are the four grades distributed equally (25 % each)?"</p>
<ul>
  <li><strong>H₀:</strong> The observed distribution matches the expected distribution.</li>
  <li><strong>H₁:</strong> The observed distribution differs from expected.</li>
</ul>

<h3 style="color:#2c7bb6;">The χ² statistic</h3>
<p><strong>χ² = Σ [(O − E)² / E]</strong></p>
<ul>
  <li><strong>O</strong> – Observed count in each cell.</li>
  <li><strong>E</strong> – Expected count if H₀ were true: E = (row total × column total) / N.</li>
</ul>
<p>A large χ² indicates a large discrepancy between observed and expected counts.</p>

<h3 style="color:#2c7bb6;">Degrees of freedom</h3>
<p>For the test of independence: df = (rows − 1) × (columns − 1).</p>

<h3 style="color:#2c7bb6;">Assumptions</h3>
<ul>
  <li><strong>Expected cell counts ≥ 5</strong> in at least 80 % of cells, and no cell
      expected count &lt; 1. If violated, use Fisher\'s exact test (for 2×2 tables)
      or combine categories.</li>
  <li><strong>Independence of observations</strong> – Each participant contributes to
      only one cell.</li>
  <li>Chi-square works with <strong>counts</strong>, not proportions or percentages.</li>
</ul>

<h3 style="color:#2c7bb6;">Effect size – Cramér\'s V</h3>
<p>Cramér\'s V measures the strength of association. Values range from 0 to 1:</p>
<ul>
  <li><strong>Small:</strong> V = .10</li>
  <li><strong>Medium:</strong> V = .30</li>
  <li><strong>Large:</strong> V = .50</li>
</ul>

<h3 style="color:#2c7bb6;">Visualisation</h3>
<p>Use a <strong>stacked bar chart</strong> or <strong>mosaic plot</strong> to
visualise the relationship between two categorical variables. In Jamovi, bar
charts can be produced from <strong>Exploration → Descriptives → Plots</strong>.</p>

</div>'
        },

        .instructionsHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">How to Run a Chi-Square Test in Jamovi</h2>

<h3 style="color:#2c7bb6;">Step 1 – Load your data</h3>
<p>Open your dataset via <strong>File → Open</strong>. For a sample dataset:
<strong>File → Open</strong> and select one of the sample CSV files from the module\'s <code>data/</code> folder. Open <em>survey</em> (test whether gender and education are associated) or
<em>clinical</em> (test whether treatment group and outcome are associated).</p>

<h3 style="color:#2c7bb6;">Step 2 – Check data format</h3>
<p>Both variables must be <strong>categorical</strong> (nominal or ordinal).
In Jamovi, check variable types in the spreadsheet header – they should show
a "↔" (nominal) or "↑↓" (ordinal) icon. If they show a ruler (continuous),
right-click the variable → <em>Variable type</em> → change to Nominal.</p>

<h3 style="color:#2c7bb6;">Step 3 – Open this analysis</h3>
<p>Click <strong>learnJamovi → Chi-Square Tests</strong>.</p>

<h3 style="color:#2c7bb6;">Step 4 – Assign variables</h3>
<ul>
  <li>Drag one categorical variable into <strong>Row variable</strong>
      (e.g., <em>gender</em>).</li>
  <li>Drag the other into <strong>Column variable</strong>
      (e.g., <em>education</em>).</li>
</ul>

<h3 style="color:#2c7bb6;">Step 5 – Jamovi core Frequencies module</h3>
<p>For the complete analysis, use <strong>Frequencies → Independent Samples –
χ² test of association</strong> (or <strong>Goodness of Fit</strong>) in the
Jamovi core. Options include:</p>
<ul>
  <li>Expected counts</li>
  <li>Fisher\'s exact test (for small samples)</li>
  <li>Contingency table with row/column percentages</li>
  <li>Bar charts</li>
</ul>

<h3 style="color:#2c7bb6;">Step 6 – Interpret the output</h3>
<ul>
  <li>Is <strong>p &lt; .05</strong>? If yes, there is a significant association
      between the variables.</li>
  <li>Check that all expected counts ≥ 5. If not, consider Fisher\'s exact test.</li>
  <li>Report <strong>Cramér\'s V</strong> as a measure of association strength.</li>
</ul>

<h3 style="color:#2c7bb6;">Reporting example (APA style)</h3>
<p><em>"A chi-square test of independence revealed a significant association
between gender and education level, χ²(3, N = 30) = 8.42, p = .038,
Cramér\'s V = .53."</em></p>

</div>'
        }
    )
)
