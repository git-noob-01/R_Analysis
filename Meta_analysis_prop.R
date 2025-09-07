library(metadat)
library(metafor)
library(meta)

dat <- dat.hannum2020

measure = "PFT"
method_ci = "CP"
method = "REML"

ies <- escalc(xi = xi, ni = ni, data = dat, measure = "PFT")
pes.da <- rma(yi, vi, data = ies, method = method)
pes <- predict(pes.da, transf = transf.ipft.hm, targ = list(ni = dat$ni))

# Add arg 'method' = "Inverse" to use classic logit transform, e.g., when using REML; also required if using "IVhet" for CI under fixed-effects

pes.summary <- metaprop(xi, ni, authorName, data = dat, sm = measure, method.tau = method, method.ci = method_ci, method.random.ci = "HK")

# Generating forest plot
forest(pes.summary,
       common = FALSE,
       print.tau = TRUE,
       print.Q = TRUE,
       print.pval.Q = TRUE,
       print.I2 = TRUE,
       rightcols = FALSE,
       pooled.totals = FALSE,
       prediction = TRUE,
       weight.study = "random",
       leftcols = c("studlab", "event", "n", "effect", "ci"),
       leftlabs = c("Study", "Cases", "Total", "Proportion", "95% C.I"),
       xlab = "Prevelance of Anosmia (%)",
       smlab = "",
       xlim = c(0, 1.5),
       squaresize = 0.5,
       fs.hetstat = 10,
       digits = 2,
       col.square = "navy",
       col.square.lines = "navy",
       col.diamond = "maroon",
       col.diamond.lines = "maroon")

## Diagnostics ##

# Baujat plots
baujat_plot <- baujat(pes.da,
                      symbol = 19,
                      xlab = "Contribution to heterogenity (Q)",
                      ylab = "Influence on Summary Proportion")
df <- baujat_plot[baujat_plot$x >= 3 | baujat_plot$y >= 0.1, ]
text(df$x, df$y, df$slab, pos = 1)

# Influence Metrics
inf <- influence(pes.da)
print(inf, digits = 3)
plot(inf)

# Leave-one out analysis
l1o <- leave1out(pes.da) 
yi <- l1o$estimate
vi <- l1o$se^2 
forest(yi, 
       vi, 
       transf = transf.ipft.hm, 
       targ = list(ni = dat$ni), 
       slab = pes.da$slab, 
       refline = pes$pred, 
       xlab = "Leave-one-out summary proportions")
