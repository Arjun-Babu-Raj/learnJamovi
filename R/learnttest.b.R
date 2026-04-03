
LearnTTestClass <- R6::R6Class(
    'LearnTTestClass',
    inherit = LearnTTestBase,
    private = list(

        .run = function() {

            if (self$options$showTheory)
                self$results$theory$setContent(private$.theoryHtml())

            if (self$options$showInstructions)
                self$results$instructions$setContent(
                    private$.instructionsHtml(self$options$testType))

            dep  <- self$options$dep
            type <- self$options$testType

            if (is.null(dep))
                return()

            data <- self$data

            if (type == 'independent') {
                grp <- self$options$group
                if (is.null(grp)) return()

                y  <- jmvcore::toNumeric(data[[dep]])
                g  <- data[[grp]]
                lvls <- levels(factor(g))
                if (length(lvls) != 2) {
                    stop('The grouping variable must have exactly two levels for an independent t-test.')
                }

                y1 <- y[g == lvls[1]]
                y2 <- y[g == lvls[2]]
                y1 <- y1[!is.na(y1)]
                y2 <- y2[!is.na(y2)]

                alt <- switch(self$options$hypothesis,
                    twoTailed  = 'two.sided',
                    oneGreater = 'greater',
                    oneLess    = 'less')

                res <- t.test(y1, y2, alternative = alt)
                md  <- res$estimate[1] - res$estimate[2]
                sp  <- sqrt(((length(y1) - 1) * var(y1) + (length(y2) - 1) * var(y2)) /
                                (length(y1) + length(y2) - 2))
                d   <- md / sp

                self$results$ttest$addRow(rowKey = 'result', values = list(
                    var     = paste(dep, ':', lvls[1], 'vs', lvls[2]),
                    t       = res$statistic,
                    df      = res$parameter,
                    p       = res$p.value,
                    md      = md,
                    cilower = res$conf.int[1],
                    ciupper = res$conf.int[2],
                    cohend  = d))

                for (i in seq_along(lvls)) {
                    yi <- y[g == lvls[i]]
                    yi <- yi[!is.na(yi)]
                    self$results$desc$addRow(rowKey = lvls[i], values = list(
                        group = lvls[i],
                        n     = length(yi),
                        mean  = mean(yi),
                        sd    = sd(yi),
                        se    = sd(yi) / sqrt(length(yi))))
                }

            } else if (type == 'paired') {
                dep2 <- self$options$dep2
                if (is.null(dep2)) return()

                y1 <- jmvcore::toNumeric(data[[dep]])
                y2 <- jmvcore::toNumeric(data[[dep2]])
                ok <- complete.cases(y1, y2)
                y1 <- y1[ok]; y2 <- y2[ok]

                alt <- switch(self$options$hypothesis,
                    twoTailed  = 'two.sided',
                    oneGreater = 'greater',
                    oneLess    = 'less')

                res <- t.test(y1, y2, paired = TRUE, alternative = alt)
                d_vals <- y1 - y2
                d  <- mean(d_vals) / sd(d_vals)

                self$results$ttest$addRow(rowKey = 'result', values = list(
                    var     = paste(dep, 'vs', dep2),
                    t       = res$statistic,
                    df      = res$parameter,
                    p       = res$p.value,
                    md      = res$estimate,
                    cilower = res$conf.int[1],
                    ciupper = res$conf.int[2],
                    cohend  = d))

                self$results$desc$addRow(rowKey = dep, values = list(
                    group = dep,
                    n     = length(y1),
                    mean  = mean(y1),
                    sd    = sd(y1),
                    se    = sd(y1) / sqrt(length(y1))))

                self$results$desc$addRow(rowKey = dep2, values = list(
                    group = dep2,
                    n     = length(y2),
                    mean  = mean(y2),
                    sd    = sd(y2),
                    se    = sd(y2) / sqrt(length(y2))))

            } else if (type == 'onesample') {
                y   <- jmvcore::toNumeric(data[[dep]])
                y   <- y[!is.na(y)]
                mu  <- self$options$mu

                alt <- switch(self$options$hypothesis,
                    twoTailed  = 'two.sided',
                    oneGreater = 'greater',
                    oneLess    = 'less')

                res <- t.test(y, mu = mu, alternative = alt)
                d   <- (mean(y) - mu) / sd(y)

                self$results$ttest$addRow(rowKey = 'result', values = list(
                    var     = dep,
                    t       = res$statistic,
                    df      = res$parameter,
                    p       = res$p.value,
                    md      = mean(y) - mu,
                    cilower = res$conf.int[1],
                    ciupper = res$conf.int[2],
                    cohend  = d))

                self$results$desc$addRow(rowKey = dep, values = list(
                    group = dep,
                    n     = length(y),
                    mean  = mean(y),
                    sd    = sd(y),
                    se    = sd(y) / sqrt(length(y))))
            }
        },

        .theoryHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">T-Tests</h2>

<p>A <strong>t-test</strong> is an inferential statistical test used to compare
means. It answers: "Is the difference between means larger than we would expect
by chance?"</p>

<h3 style="color:#2c7bb6;">Hypotheses</h3>
<ul>
  <li><strong>Null hypothesis (H₀)</strong> – There is no difference (e.g., μ₁ = μ₂).</li>
  <li><strong>Alternative hypothesis (H₁)</strong> – There is a difference (two-tailed),
      or the mean is larger/smaller (one-tailed).</li>
</ul>

<h3 style="color:#2c7bb6;">The t-statistic and p-value</h3>
<p>The t-statistic = (observed difference) / (standard error). A large |t| means the
difference is large relative to variability. The <strong>p-value</strong> is the
probability of obtaining a result at least as extreme as the observed one, assuming
H₀ is true. If p &lt; α (usually .05), reject H₀.</p>

<h3 style="color:#2c7bb6;">Three types of t-test</h3>
<ul>
  <li><strong>One-sample t-test</strong> – Compares a sample mean to a known or
      hypothesised value (e.g., "Is the mean IQ of this class = 100?").</li>
  <li><strong>Independent-samples t-test</strong> – Compares the means of two
      independent groups (e.g., treatment vs control).</li>
  <li><strong>Paired-samples t-test</strong> – Compares two related measurements
      from the same participants (e.g., pre-score vs post-score).</li>
</ul>

<h3 style="color:#2c7bb6;">Assumptions</h3>
<ul>
  <li><strong>Normality</strong> – The dependent variable should be approximately
      normally distributed in each group. Check with a Shapiro–Wilk test or
      Q-Q plot. With N &gt; 30, the central limit theorem largely protects you.</li>
  <li><strong>Homogeneity of variance</strong> – For independent t-tests, both groups
      should have similar variances (Levene\'s test). If violated, use Welch\'s
      t-test (the default in Jamovi).</li>
  <li><strong>Independence</strong> – Observations must be independent of each other.</li>
</ul>

<h3 style="color:#2c7bb6;">Effect size – Cohen\'s d</h3>
<p>Cohen\'s d = (mean difference) / (pooled SD). Interpretation guidelines:</p>
<ul>
  <li><strong>Small:</strong> d = 0.2</li>
  <li><strong>Medium:</strong> d = 0.5</li>
  <li><strong>Large:</strong> d = 0.8</li>
</ul>

<h3 style="color:#2c7bb6;">Confidence intervals</h3>
<p>A 95 % CI for the mean difference means: if we repeated the study 100 times,
approximately 95 of the resulting intervals would contain the true difference.
If the CI does not include 0, the difference is statistically significant (p &lt; .05).</p>

</div>'
        },

        .instructionsHtml = function(type) {
            type_label <- switch(type,
                independent = 'Independent Samples',
                paired      = 'Paired Samples',
                onesample   = 'One Sample')

            specific <- switch(type,
                independent = paste0(
                    '<li>Drag your numeric outcome variable into <strong>Dependent Variable</strong>.</li>',
                    '<li>Drag your grouping variable (two levels) into <strong>Grouping Variable</strong>.</li>'),
                paired = paste0(
                    '<li>Drag your first variable into <strong>Dependent Variable</strong>.</li>',
                    '<li>Drag your second variable into <strong>Second Variable (paired)</strong>.</li>'),
                onesample = paste0(
                    '<li>Drag your numeric variable into <strong>Dependent Variable</strong>.</li>',
                    '<li>Enter the hypothesised value in <strong>Test value (H₀ mean)</strong>.</li>'))

            paste0(
'<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">How to Run a ', type_label, ' T-Test in Jamovi</h2>

<h3 style="color:#2c7bb6;">Step 1 – Load your data</h3>
<p>Open your dataset via <strong>File → Open</strong>. To use a sample dataset:
<strong>File → Open</strong> and select one of the sample CSV files from the module's <code>data/</code> folder.</p>

<h3 style="color:#2c7bb6;">Step 2 – Open this analysis</h3>
<p>Click <strong>learnJamovi → T-Tests</strong>, then select
<strong>', type_label, '</strong> from the <em>Type of t-test</em> dropdown.</p>

<h3 style="color:#2c7bb6;">Step 3 – Assign variables</h3>
<ul>
  ', specific, '
</ul>

<h3 style="color:#2c7bb6;">Step 4 – Check assumptions (Jamovi core module)</h3>
<p>For a thorough analysis, use <strong>T-Tests → ', type_label, ' T-Test</strong>
in the Jamovi core. There you can enable:</p>
<ul>
  <li><em>Normality test</em> (Shapiro–Wilk)</li>
  <li><em>Equality of variances</em> (Levene\'s test) for independent t-tests</li>
  <li><em>Descriptive plots</em></li>
  <li><em>Effect size</em> (Cohen\'s d)</li>
</ul>

<h3 style="color:#2c7bb6;">Step 5 – Interpret the output</h3>
<ul>
  <li>Is p &lt; .05? If yes, reject H₀ and conclude there is a significant difference.</li>
  <li>Report: <em>t</em>(df) = value, <em>p</em> = value, Cohen\'s d = value, 95% CI [lower, upper].</li>
</ul>

<h3 style="color:#2c7bb6;">Reporting example (APA style)</h3>
<p><em>"The treatment group (M = 13.2, SD = 2.8) scored significantly lower on
anxiety than the control group (M = 19.6, SD = 3.1), t(28) = −5.43, p &lt; .001,
d = −1.98, 95% CI [−8.9, −3.9]."</em></p>

</div>')
        }
    )
)
