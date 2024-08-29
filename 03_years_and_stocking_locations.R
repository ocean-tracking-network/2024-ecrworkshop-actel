## OTN SYMPOSIUM 2024
## Hugo Flávio (hugoflavio.com)
## 2024-07-28
## Script 3: Survival as a function of year and release site.

# What's going on here?
#
# In this script, we'll start conducting statistical analyses. In the previous
# script, we determined that we didn't need to apply any changes to our dataset.
# knowing this is important, as it means I can safely load only script 01 and
# proceed from there directly. Once we've run the survival analysis, we'll
# proceed to making a nice plot of the results.
# Finally, we'll take the chance to calculate survival and mortality rates, and
# we'll discuss the differences between finite and instantaneous rates.

# 3.1) load needed packages/scripts -----------------------
  source("01_load_data.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/convert_link.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/colorblind.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/expand_line.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/simple_legend.R")
  library("multcomp")

# 3.2) model success --------------------------------------
  m1 <- glm(Succeeded ~ Length.mm + Release.site * Year,
    family = binomial(link = "logit"),
    data = wilds)

  anova(m1)

  m2 <- glm(Succeeded ~ Year,
    family = binomial(link = "logit"),
    data = wilds)

  anova(m2)

  summary(glht(m2, mcp("Year" = "Tukey")))  

# 3.3) plot success model ---------------------------------

  m2_fit <- data.frame(Year = levels(wilds$Year))

  # Get predicted values and 95% CIs.
  aux <- predict(m2,
                 newdata = m2_fit,
                 type = "link",
                 se = TRUE)

  m2_fit <- cbind(m2_fit, convert_link(aux, "logit"))

  aux <- split(wilds, list(wilds$Year, wilds$Release.site))
  output <- lapply(aux, function(x) {
    data.frame(
      Year = x$Year[1],
      Release.site = x$Release.site[1],
      prop = sum(x$Succeeded)/nrow(x))
  })
  year_points <- do.call(rbind, output)

  year_points

  p <- ggplot(data = m2_fit, aes(x = Year))

  p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)
  p <- p + geom_errorbar(aes(ymin = fit, ymax = fit), width = 0.2)

  p <- p + geom_point(data = year_points,
    aes(y = prop, colour = Release.site), size = 2)

  # significance
  p <- p |> geom_signif(x1 = 1, x2 = 2, y1 = 0.85, drop = 0.01, label = "**")
  p <- p |> geom_signif(x1 = 1, x2 = 3, y1 = 0.9, drop = 0.01, label = "***")

  # visuals
  p <- p + theme_bw()
  p <- p + scale_color_manual(values = unname(cb[1:2]))
  p <- p + labs(y = "Survival probability",x = "Year", colour = "Release site")
  # p <- p + simple_legend(h = 0.05, v = 0.05, borderwidth = 0.2)
  p <- p + simple_legend(h = 0.95, v = 0.85, borderwidth = 0.2)
  p

  dev.size()
  ggsave("plot_03-1_survival.png", width = 3, height = 4)

# 3.4) Calculate instantaneous mortality rates ------------

  imr <- function(s, d) {
    - log(1 / s) / d
  }

  fsr <- function(s, d) {
    s^(1 / d)
  }

  wilds$river_success <- FALSE
  wilds$river_success[wilds$Status != "Disap. in River"] <- TRUE
 
  surv <- aggregate(wilds$river_success, 
      list(Release.site = wilds$Release.site, 
           Year = wilds$Year), sum)
  
  surv$n <- aggregate(wilds$river_success, 
      list(Release.site = wilds$Release.site, 
           Year = wilds$Year), length)$x
  
  surv$prop <- surv$x / surv$n
 
  surv$km <- 3.161
  surv$km[surv$Release.site == "Cross"] <- 24.43
  
  surv$pct_km <- (1-surv$prop) / surv$km * 100

  surv$IMR <- imr(s = surv$prop, d = surv$km)
  surv$FSR <- fsr(s = surv$prop, d = surv$km)

# 3.5) Plot instantaneous mortality rates -----------------

  p <- ggplot(surv, aes(x = Release.site, y = IMR, fill = as.factor(Year)))
  p <- p + geom_bar(stat = "identity", position = "dodge")
  p <- p + theme_bw()
  p <- p + labs(x = "Release site", y = "Instantaneous mortality rate", fill = "Year")
  p <- p + scale_fill_manual(values = unname(cb[1:3]))
  p <- p + scale_y_reverse(expand = c(0, 0), limits = c(0, -0.3)) 
  p <- p + simple_legend(h = 0.05, v = 0.95, borderwidth = 0.2)
  p

  ggsave("plot_03-2_IMR.png", width = 4, height = 4)

