
LearnANOVAClass <- R6::R6Class(
    'LearnANOVAClass',
    inherit = LearnANOVABase,
    private = list(

        .run = function() {

            if (self$options$showTheory)
                self$results$theory$setContent(private$.theoryHtml())

            if (self$options$showInstructions)
                self$results$instructions$setContent(private$.instructionsHtml())

            dep    <- self$options$dep
            fct    <- self$options$factor

            if (is.null(dep) || is.null(fct))
                return()

            data <- self$data
            y    <- jmvcore::toNumeric(data[[dep]])
            g    <- factor(data[[fct]])
            ok   <- complete.cases(y, g)
            y    <- y[ok]; g <- g[ok]

            lvls <- levels(g)
            if (length(lvls) < 2) {
                stop('The grouping variable must have at least two levels.')
            }

            fit <- aov(y ~ g)
            s   <- summary(fit)[[1]]

            ss_between <- s[['Sum Sq']][1]
            ss_within  <- s[['Sum Sq']][2]
            ss_total   <- ss_between + ss_within
            df_between <- s[['Df']][1]
            df_within  <- s[['Df']][2]
            ms_between <- s[['Mean Sq']][1]
            ms_within  <- s[['Mean Sq']][2]
            f_val      <- s[['F value']][1]
            p_val      <- s[['Pr(>F)']][1]
            eta_sq     <- ss_between / ss_total

            self$results$anova$addRow(rowKey = 'between', values = list(
                source = 'Between groups',
                ss     = ss_between,
                df     = df_between,
                ms     = ms_between,
                f      = f_val,
                p      = p_val,
                etaSq  = eta_sq))

            self$results$anova$addRow(rowKey = 'within', values = list(
                source = 'Within groups (error)',
                ss     = ss_within,
                df     = df_within,
                ms     = ms_within,
                f      = NaN,
                p      = NaN,
                etaSq  = NaN))

            self$results$anova$addRow(rowKey = 'total', values = list(
                source = 'Total',
                ss     = ss_total,
                df     = df_between + df_within,
                ms     = NaN,
                f      = NaN,
                p      = NaN,
                etaSq  = NaN))

            for (lv in lvls) {
                yi <- y[g == lv]
                self$results$desc$addRow(rowKey = lv, values = list(
                    group = lv,
                    n     = length(yi),
                    mean  = mean(yi),
                    sd    = sd(yi),
                    se    = sd(yi) / sqrt(length(yi))))
            }

            if (self$options$postHoc) {
                ph  <- TukeyHSD(fit)$g
                rn  <- rownames(ph)
                for (i in seq_len(nrow(ph))) {
                    self$results$postHocTable$addRow(rowKey = rn[i], values = list(
                        comparison = rn[i],
                        md         = ph[i, 'diff'],
                        cilower    = ph[i, 'lwr'],
                        ciupper    = ph[i, 'upr'],
                        pTukey     = ph[i, 'p adj']))
                }
            }
        },

        .theoryHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">One-Way ANOVA</h2>

<p><strong>Analysis of Variance (ANOVA)</strong> tests whether the means of three
or more groups differ significantly. It is an extension of the independent-samples
t-test to more than two groups.</p>

<h3 style="color:#2c7bb6;">Why not just run multiple t-tests?</h3>
<p>Running multiple pairwise t-tests inflates the Type I error rate. With α = .05
and 3 groups (3 comparisons), the familywise error rate is 1 − (1 − .05)³ ≈ .14.
ANOVA controls this by testing all groups simultaneously.</p>

<h3 style="color:#2c7bb6;">The F-statistic</h3>
<p><strong>F = MS_between / MS_within</strong></p>
<ul>
  <li><strong>MS_between</strong> – Variance due to differences between group means.</li>
  <li><strong>MS_within</strong> – Variance due to differences within each group (error).</li>
</ul>
<p>A large F means between-group variance dominates; groups are more different
than expected by chance. If F exceeds the critical value, reject H₀.</p>

<h3 style="color:#2c7bb6;">Hypotheses</h3>
<ul>
  <li><strong>H₀:</strong> All group means are equal (μ₁ = μ₂ = … = μₖ).</li>
  <li><strong>H₁:</strong> At least one group mean differs.</li>
</ul>

<h3 style="color:#2c7bb6;">Assumptions</h3>
<ul>
  <li><strong>Normality</strong> – Residuals should be approximately normal. Test with
      Shapiro–Wilk or inspect a Q-Q plot.</li>
  <li><strong>Homogeneity of variance</strong> – Groups should have similar variances.
      Test with Levene\'s test. If violated, use Welch\'s ANOVA.</li>
  <li><strong>Independence</strong> – Observations must be independent.</li>
</ul>

<h3 style="color:#2c7bb6;">Effect size – η² (eta squared)</h3>
<p>η² = SS_between / SS_total. Interpretation guidelines:</p>
<ul>
  <li><strong>Small:</strong> η² = .01</li>
  <li><strong>Medium:</strong> η² = .06</li>
  <li><strong>Large:</strong> η² = .14</li>
</ul>

<h3 style="color:#2c7bb6;">Post-hoc tests</h3>
<p>A significant ANOVA only tells you that <em>at least one</em> group differs.
Post-hoc tests compare all pairs of groups while controlling the familywise error
rate. Common options:</p>
<ul>
  <li><strong>Tukey HSD</strong> – Recommended when group sizes are equal or similar.</li>
  <li><strong>Bonferroni</strong> – Conservative; good for a small number of planned comparisons.</li>
  <li><strong>Games-Howell</strong> – Recommended when variances are unequal.</li>
</ul>

</div>'
        },

        .instructionsHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">How to Run a One-Way ANOVA in Jamovi</h2>

<h3 style="color:#2c7bb6;">Step 1 – Load your data</h3>
<p>Open your dataset via <strong>File → Open</strong>. For a sample dataset:
<strong>File → Open</strong> and select one of the sample CSV files from the module's <code>data/</code> folder. Open <em>clinical</em> or <em>academic</em>.</p>

<h3 style="color:#2c7bb6;">Step 2 – Open this analysis</h3>
<p>Click <strong>learnJamovi → One-Way ANOVA</strong>.</p>

<h3 style="color:#2c7bb6;">Step 3 – Assign variables</h3>
<ul>
  <li>Drag your numeric outcome into <strong>Dependent Variable</strong>
      (e.g., <em>math_score</em>).</li>
  <li>Drag your categorical group variable into <strong>Grouping Factor</strong>
      (e.g., <em>grade</em>).</li>
</ul>

<h3 style="color:#2c7bb6;">Step 4 – Optional: Post-hoc comparisons</h3>
<p>Tick <strong>Post-hoc tests (Tukey HSD)</strong> to see which specific pairs
of groups differ.</p>

<h3 style="color:#2c7bb6;">Step 5 – Jamovi core ANOVA module</h3>
<p>For the complete analysis (normality tests, homogeneity check, means plots),
use <strong>ANOVA → One-Way ANOVA</strong> in the Jamovi core. Options include:</p>
<ul>
  <li>Welch\'s and Brown-Forsythe corrections for unequal variances</li>
  <li>η², ω², and partial η² effect sizes</li>
  <li>Descriptive plots and error bar charts</li>
</ul>

<h3 style="color:#2c7bb6;">Step 6 – Interpret the output</h3>
<ul>
  <li>Check the F-statistic and p-value in the ANOVA table.</li>
  <li>If p &lt; .05, at least one group mean is significantly different.</li>
  <li>Examine the post-hoc table to see which specific pairs differ.</li>
  <li>Report η² as a measure of effect size.</li>
</ul>

<h3 style="color:#2c7bb6;">Reporting example (APA style)</h3>
<p><em>"There was a significant effect of grade on math score,
F(3, 26) = 48.7, p &lt; .001, η² = .85. Tukey HSD post-hoc tests revealed
that A students (M = 95.3, SD = 2.7) scored significantly higher than
C students (M = 73.8, SD = 3.6, p &lt; .001)."</em></p>

</div>'
        }
    )
)
