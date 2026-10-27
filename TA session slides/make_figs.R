# Figures for the TA session slides. Run from this folder: Rscript make_figs.R
navy <- "#23395B"; red <- "#C8553D"; grey <- "#8D99AE"; light <- "#D9E2EC"
dir.create("figs", showWarnings = FALSE)
op <- function() par(mar = c(4, 3, 2, 1), family = "Helvetica", col.axis = "grey30", col.lab = "grey20")

# 1. CLT: skewed population -> sampling distribution of the mean
set.seed(7)
pdf("figs/clt.pdf", width = 10, height = 3.4)
par(mfrow = c(1, 4), mar = c(4, 1, 3, 1), family = "Helvetica")
x <- seq(0, 5, length.out = 300)
plot(x, dexp(x), type = "l", lwd = 3, col = navy, yaxt = "n", ylab = "", xlab = "y",
     main = "Population: skewed\n(Exponential, mean 1)", cex.main = 1.1)
for (n in c(2, 10, 100)) {
  m <- replicate(10000, mean(rexp(n)))
  hist(m, breaks = 40, col = light, border = "white", yaxt = "n", ylab = "", freq = FALSE,
       xlab = expression(bar(y)), main = paste0("n = ", n), cex.main = 1.1)
  xx <- seq(min(m), max(m), length.out = 200)
  lines(xx, dnorm(xx, 1, 1 / sqrt(n)), col = red, lwd = 2.5)
}
dev.off()

# 2. t vs normal
pdf("figs/t_vs_z.pdf", width = 6.5, height = 3.6)
op(); x <- seq(-4.5, 4.5, length.out = 400)
plot(x, dnorm(x), type = "l", lwd = 3, col = navy, ylab = "", xlab = "", yaxt = "n", ylim = c(0, .42))
lines(x, dt(x, 3), lwd = 2.5, col = red); lines(x, dt(x, 10), lwd = 2.5, col = grey, lty = 2)
legend("topright", c("N(0,1)", expression(t[10]), expression(t[3])), col = c(navy, grey, red),
       lwd = 3, lty = c(1, 2, 1), bty = "n", cex = 1.1)
dev.off()

# 3. CI coverage: 40 intervals
set.seed(5)
pdf("figs/ci_coverage.pdf", width = 6.5, height = 4)
op(); N <- 40; n <- 25; mu <- 50
res <- t(replicate(N, { y <- rnorm(n, mu, 10); m <- mean(y); e <- qt(.975, n - 1) * sd(y) / sqrt(n); c(m - e, m + e) }))
miss <- res[, 1] > mu | res[, 2] < mu
plot(NA, xlim = range(res), ylim = c(1, N), yaxt = "n", ylab = "sample", xlab = "", bty = "l")
segments(res[, 1], 1:N, res[, 2], 1:N, lwd = 2.5, col = ifelse(miss, red, grey))
abline(v = mu, lwd = 2, col = navy)
text(mu, N + 1.2, expression(mu), col = navy, xpd = NA, cex = 1.3)
dev.off()
cat("missed:", sum(miss), "of", N, "\n")

# 4. p-value, two-sided, t = 2.0
pdf("figs/pvalue.pdf", width = 6.5, height = 3.6)
op(); x <- seq(-4, 4, length.out = 500); tt <- 2.0
plot(x, dnorm(x), type = "l", lwd = 3, col = navy, ylab = "", xlab = "t statistic", yaxt = "n")
for (s in c(-1, 1)) { xs <- seq(tt, 4, length.out = 100) * s
  polygon(c(xs[1], xs, xs[100]), c(0, dnorm(xs), 0), col = adjustcolor(red, .6), border = NA) }
abline(v = c(-1.96, 1.96), lty = 2, col = grey, lwd = 2)
segments(c(-tt, tt), 0, c(-tt, tt), dnorm(tt), col = red, lwd = 3)
text(tt + .35, .13, "p/2", col = red, cex = 1.2); text(-tt - .35, .13, "p/2", col = red, cex = 1.2)
dev.off()

# 5. Type I / II errors
pdf("figs/errors.pdf", width = 7, height = 3.6)
op(); x <- seq(-3.5, 7, length.out = 600); cc <- 1.645
plot(x, dnorm(x), type = "l", lwd = 3, col = navy, ylab = "", xlab = "test statistic", yaxt = "n", ylim = c(0, .45))
lines(x, dnorm(x, 3), lwd = 3, col = grey)
xa <- seq(cc, 7, length.out = 100); polygon(c(cc, xa, 7), c(0, dnorm(xa), 0), col = adjustcolor(red, .6), border = NA)
xb <- seq(-3.5, cc, length.out = 100); polygon(c(-3.5, xb, cc), c(0, dnorm(xb, 3), 0), col = adjustcolor(grey, .5), border = NA)
abline(v = cc, lty = 2, lwd = 2)
text(0, .43, expression(H[0]), col = navy, cex = 1.3); text(3, .43, expression(H[A]), col = "grey35", cex = 1.3)
text(2.4, .03, expression(alpha), col = red, cex = 1.3); text(1.1, .05, expression(beta), col = "grey25", cex = 1.3)
dev.off()

# 6. Central 95% of the standard normal (where 1.96 comes from)
pdf("figs/normal95.pdf", width = 6.5, height = 3.4)
op(); x <- seq(-4, 4, length.out = 500)
plot(x, dnorm(x), type = "l", lwd = 3, col = navy, ylab = "", xlab = "standard deviations from the mean", yaxt = "n")
xs <- seq(-1.96, 1.96, length.out = 200); polygon(c(-1.96, xs, 1.96), c(0, dnorm(xs), 0), col = adjustcolor(navy, .18), border = NA)
segments(c(-1.96, 1.96), 0, c(-1.96, 1.96), dnorm(1.96), col = red, lwd = 3)
text(0, .17, "95%", cex = 1.8, col = navy); text(-2.9, .1, "2.5%", col = red, cex = 1.1); text(2.9, .1, "2.5%", col = red, cex = 1.1)
mtext("-1.96", 1, at = -1.96, line = -1.2, col = red, cex = .9); mtext("1.96", 1, at = 1.96, line = -1.2, col = red, cex = .9)
dev.off()

# 7. Population spread vs spread of the sample mean
pdf("figs/se_n.pdf", width = 6.5, height = 3.4)
op(); x <- seq(20, 80, length.out = 500)
plot(x, dnorm(x, 50, 10), type = "l", lwd = 3, col = grey, ylab = "", xlab = "", yaxt = "n", ylim = c(0, .22))
for (i in 1:3) { n <- c(10, 40, 160)[i]; lines(x, dnorm(x, 50, 10 / sqrt(n)), lwd = 3, col = c(light, red, navy)[i]) }
lines(x, dnorm(x, 50, 10 / sqrt(10)), lwd = 3, col = "#6C8EAD")
legend("topright", c("individual y  (sd = 10)", "mean, n = 10", "mean, n = 40", "mean, n = 160"),
       col = c(grey, "#6C8EAD", red, navy), lwd = 3, bty = "n", cex = .9)
dev.off()
