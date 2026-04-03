
LearnRegressionClass <- R6::R6Class(
    'LearnRegressionClass',
    inherit = LearnRegressionBase,
    private = list(

        .run = function() {

            if (self$options$showTheory)
                self$results$theory$setContent(private$.theoryHtml())

            if (self$options$showInstructions)
                self$results$instructions$setContent(private$.instructionsHtml())

            dep  <- self$options$dep
            covs <- self$options$covs

            if (is.null(dep) || length(covs) == 0)
                return()

            data <- self$data
            keep <- c(dep, covs)
            df   <- data[keep]
            df   <- df[complete.cases(df), ]
            n    <- nrow(df)

            if (n < length(covs) + 2) return()

            fmla <- as.formula(paste(dep, '~', paste(covs, collapse = ' + ')))
            fit  <- lm(fmla, data = df)
            sm   <- summary(fit)
            cf   <- coef(sm)
            ci   <- confint(fit)

            r2    <- sm$r.squared
            r2adj <- sm$adj.r.squared
            r     <- sqrt(r2)
            fstat <- sm$fstatistic
            f     <- fstat['value']
            df1   <- fstat['numdf']
            df2   <- fstat['dendf']
            p     <- pf(f, df1, df2, lower.tail = FALSE)

            self$results$modelFit$addRow(rowKey = 'model', values = list(
                r     = r,
                r2    = r2,
                r2adj = r2adj,
                f     = f,
                df1   = as.integer(df1),
                df2   = as.integer(df2),
                p     = p))

            y_sd <- sd(df[[dep]])
            for (i in seq_len(nrow(cf))) {
                term  <- rownames(cf)[i]
                b     <- cf[i, 'Estimate']
                se    <- cf[i, 'Std. Error']
                t     <- cf[i, 't value']
                pv    <- cf[i, 'Pr(>|t|)']
                ci_l  <- ci[term, 1]
                ci_u  <- ci[term, 2]

                if (term == '(Intercept)') {
                    beta <- NaN
                } else {
                    x_sd <- tryCatch(sd(df[[term]]), error = function(e) NA)
                    beta <- if (!is.na(x_sd) && y_sd > 0) b * x_sd / y_sd else NaN
                }

                self$results$coeff$addRow(rowKey = term, values = list(
                    term    = term,
                    b       = b,
                    se      = se,
                    beta    = beta,
                    t       = t,
                    p       = pv,
                    cilower = ci_l,
                    ciupper = ci_u))
            }
        },

        .theoryHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">Linear Regression</h2>

<p><strong>Linear regression</strong> models the relationship between a continuous
<em>outcome</em> (dependent variable) and one or more <em>predictors</em>
(independent variables). It produces a straight line (or hyperplane) that best
fits the data.</p>

<h3 style="color:#2c7bb6;">The regression equation</h3>
<p><strong>Simple (one predictor):</strong> Y = b₀ + b₁X + ε</p>
<p><strong>Multiple (k predictors):</strong> Y = b₀ + b₁X₁ + b₂X₂ + … + bₖXₖ + ε</p>
<ul>
  <li><strong>b₀ (intercept)</strong> – Predicted Y when all predictors = 0.</li>
  <li><strong>b (unstandardised coefficient)</strong> – Change in Y for a one-unit
      increase in X, holding other predictors constant.</li>
  <li><strong>β (standardised coefficient)</strong> – Change in Y (in SDs) for a
      one-SD increase in X. Allows comparison across predictors.</li>
  <li><strong>ε (residual)</strong> – The unexplained part of each observation.</li>
</ul>

<h3 style="color:#2c7bb6;">Model fit – R²</h3>
<p><strong>R²</strong> is the proportion of variance in Y explained by the model.
R² = .70 means the predictors explain 70 % of the variability in the outcome.</p>
<p><strong>Adjusted R²</strong> penalises for the number of predictors, making it
better for comparing models with different numbers of predictors.</p>

<h3 style="color:#2c7bb6;">The F-test</h3>
<p>Tests whether the model as a whole explains significantly more variance than
no predictors (intercept-only model). A significant p-value means at least one
predictor is useful.</p>

<h3 style="color:#2c7bb6;">Assumptions</h3>
<ul>
  <li><strong>Linearity</strong> – The relationship between predictors and outcome
      is linear. Check with scatterplots or residuals vs fitted plot.</li>
  <li><strong>Independence</strong> – Observations are independent.</li>
  <li><strong>Homoscedasticity</strong> – Residuals have constant variance across
      fitted values. Check with scale-location plot.</li>
  <li><strong>Normality of residuals</strong> – Residuals are approximately normal.
      Check with Q-Q plot of residuals.</li>
  <li><strong>No multicollinearity</strong> – Predictors should not be highly
      correlated with each other (VIF &lt; 10).</li>
</ul>

<h3 style="color:#2c7bb6;">Simple vs multiple regression</h3>
<ul>
  <li><strong>Simple regression</strong> – One predictor. Useful for exploring a
      single relationship.</li>
  <li><strong>Multiple regression</strong> – Two or more predictors. Controls for
      other variables; shows unique contribution of each predictor.</li>
</ul>

</div>'
        },

        .instructionsHtml = function() {
            '<div style="font-family:sans-serif; max-width:700px; line-height:1.6;">

<h2 style="color:#2c7bb6;">How to Run Linear Regression in Jamovi</h2>

<h3 style="color:#2c7bb6;">Step 1 – Load your data</h3>
<p>Open your dataset via <strong>File → Open</strong>. For a sample dataset:
<strong>File → Open</strong> and select one of the sample CSV files from the module's <code>data/</code> folder. Open <em>academic</em> (predict math_score from study_hours and attendance).</p>

<h3 style="color:#2c7bb6;">Step 2 – Open this analysis</h3>
<p>Click <strong>learnJamovi → Linear Regression</strong>.</p>

<h3 style="color:#2c7bb6;">Step 3 – Assign variables</h3>
<ul>
  <li>Drag your numeric outcome variable into <strong>Outcome (dependent) variable</strong>
      (e.g., <em>math_score</em>).</li>
  <li>Drag one or more predictor variables into <strong>Predictor(s)</strong>
      (e.g., <em>study_hours</em>, <em>attendance</em>).</li>
</ul>

<h3 style="color:#2c7bb6;">Step 4 – Jamovi core Regression module</h3>
<p>For publication-quality output including diagnostic plots, use
<strong>Regression → Linear Regression</strong> in the Jamovi core. Options include:</p>
<ul>
  <li>Model comparison (enter method, stepwise)</li>
  <li>Residual plots (Q-Q, scale-location, Cook\'s D)</li>
  <li>VIF for multicollinearity</li>
  <li>Confidence intervals for coefficients</li>
  <li>Predicted values and residuals saved back to the dataset</li>
</ul>

<h3 style="color:#2c7bb6;">Step 5 – Interpret the output</h3>
<ul>
  <li><strong>Model Fit table</strong>: Check R² and whether F is significant.</li>
  <li><strong>Coefficients table</strong>: For each predictor, check t and p.
      Significant predictors (p &lt; .05) contribute unique variance.</li>
  <li>Compare β values to judge the relative importance of predictors.</li>
</ul>

<h3 style="color:#2c7bb6;">Reporting example (APA style)</h3>
<p><em>"Study hours and attendance significantly predicted math score,
F(2, 27) = 156.3, p &lt; .001, R² = .92. Study hours was the strongest
predictor, β = .78, t(27) = 12.5, p &lt; .001."</em></p>

</div>'
        }
    )
)
