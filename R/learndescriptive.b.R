
LearnDescriptiveClass <- R6::R6Class(
    'LearnDescriptiveClass',
    inherit = LearnDescriptiveBase,
    private = list(

        .run = function() {

            if (self$options$showTheory)
                self$results$theory$setContent(private$.theoryHtml())

            if (self$options$showInstructions)
                self$results$instructions$setContent(private$.instructionsHtml())

            vars <- self$options$vars
            if (length(vars) == 0)
                return()

            data <- self$data

            for (v in vars) {
                x <- jmvcore::toNumeric(data[[v]])
                x <- x[!is.na(x)]
                n <- length(x)
                if (n == 0) next

                m    <- mean(x)
                med  <- median(x)
                s    <- sd(x)
                se   <- s / sqrt(n)
                mn   <- min(x)
                mx   <- max(x)
                rng  <- mx - mn
                skw  <- private$.skewness(x)
                krt  <- private$.kurtosis(x)

                self$results$descriptives$setRow(
                    rowKey = v,
                    values = list(
                        n      = n,
                        mean   = m,
                        median = med,
                        sd     = s,
                        se     = se,
                        min    = mn,
                        max    = mx,
                        range  = rng,
                        skew   = skw,
                        kurt   = krt))
            }
        },

        .skewness = function(x) {
            n <- length(x)
            if (n < 3) return(NA)
            m <- mean(x)
            s <- sd(x)
            if (s == 0) return(NA)
            sum((x - m)^3) / (n * s^3)
        },

        .kurtosis = function(x) {
            n <- length(x)
            if (n < 4) return(NA)
            m <- mean(x)
            s <- sd(x)
            if (s == 0) return(NA)
            (sum((x - m)^4) / (n * s^4)) - 3
        },

        .theoryHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">Descriptive Statistics</h2>

<p>Descriptive statistics summarise and describe the main features of a dataset.
They give you a quick overview of your data before running inferential tests.</p>

<h3 style="color:#2c7bb6;">Types of Data</h3>
<ul>
  <li><strong>Nominal</strong> – Categories with no order (e.g., gender, colour)</li>
  <li><strong>Ordinal</strong> – Ordered categories (e.g., strongly agree … strongly disagree)</li>
  <li><strong>Interval</strong> – Numeric with equal gaps but no true zero (e.g., temperature in °C)</li>
  <li><strong>Ratio</strong> – Numeric with a true zero (e.g., height, reaction time)</li>
</ul>

<h3 style="color:#2c7bb6;">Measures of Central Tendency</h3>
<ul>
  <li><strong>Mean (M)</strong> – The arithmetic average. Best for symmetric, normally distributed data.
      Sensitive to outliers.</li>
  <li><strong>Median</strong> – The middle value when data are sorted. Robust to outliers.
      Preferred for skewed distributions.</li>
  <li><strong>Mode</strong> – The most frequently occurring value. Useful for nominal data.</li>
</ul>

<h3 style="color:#2c7bb6;">Measures of Spread</h3>
<ul>
  <li><strong>Range</strong> – Maximum minus minimum. Simple but influenced by extreme values.</li>
  <li><strong>Variance (s²)</strong> – The average squared deviation from the mean.</li>
  <li><strong>Standard Deviation (SD)</strong> – The square root of variance; in the same units as the data.
      About 68 % of normally distributed values fall within ±1 SD of the mean.</li>
  <li><strong>Standard Error (SE)</strong> – SD divided by √N. Reflects precision of the mean estimate.</li>
  <li><strong>Interquartile Range (IQR)</strong> – The range of the middle 50 % of data (Q3 − Q1).
      Robust to outliers.</li>
</ul>

<h3 style="color:#2c7bb6;">Distribution Shape</h3>
<ul>
  <li><strong>Skewness</strong> – Measures asymmetry. Positive skew = long right tail;
      Negative skew = long left tail. Values between −1 and +1 are generally acceptable.</li>
  <li><strong>Kurtosis (excess)</strong> – Measures "peakedness". Normal distribution has excess
      kurtosis = 0. Positive = sharper peak (leptokurtic); Negative = flatter (platykurtic).</li>
</ul>

<h3 style="color:#2c7bb6;">Sample Datasets Available</h3>
<p>This module includes three sample datasets you can open from
<strong>File → Open</strong> → select a CSV from the module\'s <code>data/</code> folder:</p>
<ul>
  <li><strong>survey</strong> – Survey data with age, gender, education, income, satisfaction, and anxiety scores</li>
  <li><strong>academic</strong> – Student scores, study hours, attendance, and grade</li>
  <li><strong>clinical</strong> – Clinical trial data with treatment groups and outcome measures</li>
</ul>

</div>'
        },

        .instructionsHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">How to Run Descriptive Statistics in Jamovi</h2>

<h3 style="color:#2c7bb6;">Step 1 – Load your data</h3>
<p>Open Jamovi and load your dataset via <strong>File → Open</strong>. You can open
CSV, Excel, SPSS, or Jamovi (.omv) files. To use one of the bundled sample
datasets, choose <strong>File → Open</strong> and select one of the sample CSV files from the module\'s <code>data/</code> folder.</p>

<h3 style="color:#2c7bb6;">Step 2 – Open this analysis</h3>
<p>Click <strong>learnJamovi → Descriptive Statistics</strong> in the menu bar.</p>

<h3 style="color:#2c7bb6;">Step 3 – Select variables</h3>
<p>Drag your continuous numeric variables from the left-hand variable list into
the <strong>Variables</strong> box. For example, drag <em>age</em> and <em>income</em>
if you are using the survey dataset.</p>

<h3 style="color:#2c7bb6;">Step 4 – Optional: Split by group</h3>
<p>To compute descriptives separately for each level of a categorical variable
(e.g., gender), drag that variable into the <strong>Split by</strong> box.</p>

<h3 style="color:#2c7bb6;">Step 5 – Interpret the output</h3>
<ul>
  <li>Check <strong>N</strong> to see how many valid cases were analysed.</li>
  <li>Report <strong>Mean</strong> and <strong>SD</strong> for normally distributed variables.</li>
  <li>Report <strong>Median</strong> and <strong>IQR</strong> for skewed variables.</li>
  <li>Inspect <strong>Skewness</strong> and <strong>Kurtosis</strong> to assess normality:
      values outside ±2 may indicate non-normality.</li>
</ul>

<h3 style="color:#2c7bb6;">Built-in Descriptives module (Jamovi core)</h3>
<p>Jamovi also ships with a full Descriptives module under
<strong>Exploration → Descriptives</strong>. There you can produce histograms,
Q-Q plots, box plots, and frequency tables. Use that for publication-ready output.</p>

<h3 style="color:#2c7bb6;">Reporting example (APA style)</h3>
<p><em>"Participant age (N = 30) had a mean of 39.5 years (SD = 9.8, range 22–65)."</em></p>

</div>'
        }
    )
)
