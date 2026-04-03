
LearnCorrelationClass <- R6::R6Class(
    'LearnCorrelationClass',
    inherit = LearnCorrelationBase,
    private = list(

        .run = function() {

            if (self$options$showTheory)
                self$results$theory$setContent(private$.theoryHtml())

            if (self$options$showInstructions)
                self$results$instructions$setContent(private$.instructionsHtml())

            vars <- self$options$vars
            if (length(vars) < 2)
                return()

            data   <- self$data
            method <- self$options$method
            flag   <- self$options$flagSig

            pairs <- combn(vars, 2, simplify = FALSE)

            for (pr in pairs) {
                v1 <- pr[1]; v2 <- pr[2]
                x  <- jmvcore::toNumeric(data[[v1]])
                y  <- jmvcore::toNumeric(data[[v2]])
                ok <- complete.cases(x, y)
                x  <- x[ok]; y <- y[ok]
                n  <- length(x)

                if (n < 3) next

                res <- cor.test(x, y, method = method)
                r   <- res$estimate
                p   <- res$p.value

                if (method == 'pearson' && n >= 4) {
                    ci_l <- res$conf.int[1]
                    ci_u <- res$conf.int[2]
                } else {
                    ci_l <- NaN
                    ci_u <- NaN
                }

                label <- if (flag && !is.na(p)) {
                    if (p < .001)      paste0(v1, ' \u2013 ', v2, ' ***')
                    else if (p < .01)  paste0(v1, ' \u2013 ', v2, ' **')
                    else if (p < .05)  paste0(v1, ' \u2013 ', v2, ' *')
                    else               paste0(v1, ' \u2013 ', v2)
                } else {
                    paste0(v1, ' \u2013 ', v2)
                }

                self$results$matrix$addRow(
                    rowKey = paste0(v1, '__', v2),
                    values = list(
                        v1      = v1,
                        v2      = v2,
                        r       = r,
                        p       = p,
                        n       = n,
                        cilower = ci_l,
                        ciupper = ci_u))
            }
        },

        .theoryHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">Correlation</h2>

<p>A <strong>correlation</strong> measures the strength and direction of the
linear relationship between two continuous variables. It does <em>not</em> imply
causation.</p>

<h3 style="color:#2c7bb6;">The correlation coefficient (r)</h3>
<p>Values range from −1 to +1:</p>
<ul>
  <li><strong>r = +1</strong> – Perfect positive relationship (both variables increase together).</li>
  <li><strong>r = 0</strong> – No linear relationship.</li>
  <li><strong>r = −1</strong> – Perfect negative relationship (one increases as the other decreases).</li>
</ul>

<h3 style="color:#2c7bb6;">Interpreting effect size (Cohen, 1988)</h3>
<ul>
  <li><strong>Small:</strong> |r| = .10</li>
  <li><strong>Medium:</strong> |r| = .30</li>
  <li><strong>Large:</strong> |r| = .50</li>
</ul>

<h3 style="color:#2c7bb6;">Pearson r (parametric)</h3>
<p>The most common correlation. Appropriate when:</p>
<ul>
  <li>Both variables are continuous (interval or ratio scale).</li>
  <li>The relationship is approximately linear (check a scatterplot).</li>
  <li>Both variables are approximately normally distributed.</li>
  <li>There are no influential outliers.</li>
</ul>

<h3 style="color:#2c7bb6;">Spearman ρ (non-parametric)</h3>
<p>Rank-based correlation. Use when:</p>
<ul>
  <li>Data are ordinal, or</li>
  <li>The normality assumption is violated, or</li>
  <li>There are notable outliers.</li>
</ul>
<p>Spearman r is simply Pearson r applied to the ranks of the data.</p>

<h3 style="color:#2c7bb6;">Kendall\'s τ (non-parametric)</h3>
<p>Another rank-based measure. More robust than Spearman for small samples or
many tied ranks. Interpretation of τ is slightly different from r.</p>

<h3 style="color:#2c7bb6;">Coefficient of determination (r²)</h3>
<p>r² is the proportion of variance in one variable explained by the other.
For example, r = .60 means r² = .36, so 36 % of the variance is shared.</p>

<h3 style="color:#2c7bb6;">Correlation ≠ Causation</h3>
<p>A correlation only shows that two variables co-vary. It cannot tell you
<em>which</em> causes <em>which</em>, or whether a third (confounding) variable
drives both. Always think critically about possible causal mechanisms.</p>

</div>'
        },

        .instructionsHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">How to Run Correlation in Jamovi</h2>

<h3 style="color:#2c7bb6;">Step 1 – Load your data</h3>
<p>Open your dataset via <strong>File → Open</strong>. For a sample dataset:
<strong>File → Open</strong> and select one of the sample CSV files from the module's <code>data/</code> folder. Open <em>academic</em> (math_score, reading_score, writing_score, study_hours).</p>

<h3 style="color:#2c7bb6;">Step 2 – Open this analysis</h3>
<p>Click <strong>learnJamovi → Correlation</strong>.</p>

<h3 style="color:#2c7bb6;">Step 3 – Select variables</h3>
<p>Drag two or more continuous numeric variables into the
<strong>Variables</strong> box. All pairwise correlations will be computed.</p>

<h3 style="color:#2c7bb6;">Step 4 – Choose the method</h3>
<ul>
  <li>Use <strong>Pearson</strong> for normally distributed continuous data.</li>
  <li>Use <strong>Spearman</strong> for ordinal data or when normality is violated.</li>
  <li>Use <strong>Kendall\'s τ</strong> for small samples or many ties.</li>
</ul>

<h3 style="color:#2c7bb6;">Step 5 – Jamovi core Regression module</h3>
<p>For a full correlation matrix with scatterplot matrix and correlation plots,
use <strong>Regression → Correlation Matrix</strong> in the Jamovi core.
Options include additional effect sizes, partial correlations, and plots.</p>

<h3 style="color:#2c7bb6;">Step 6 – Interpret the output</h3>
<ul>
  <li>Look at <strong>r</strong> (or ρ / τ): direction and magnitude.</li>
  <li>Check <strong>p</strong>: is the correlation statistically significant?</li>
  <li>Use the <strong>95% CI</strong>: if it includes 0, the result is not significant.</li>
  <li>Correlations marked <strong>*</strong> are significant at p &lt; .05;
      <strong>**</strong> at p &lt; .01; <strong>***</strong> at p &lt; .001.</li>
</ul>

<h3 style="color:#2c7bb6;">Reporting example (APA style)</h3>
<p><em>"There was a strong positive correlation between study hours and math score,
r(28) = .94, p &lt; .001, 95% CI [.87, .97]."</em></p>

</div>'
        }
    )
)
