## OTN SYMPOSIUM 2024
## Hugo Flávio (hugoflavio.com)
## 2024-07-28
## Script 2: Preliminary data checks

# What's going on here?
#
# In this script, we'll conduct a preliminary check on our variables of
# interest. This step is very important, as it might reveal errors in the data
# (e.g. a length that is far off because of a misplaced decimal delimiter).
# This is also where we'll start _seeing_ the data, so we can start gathering
# some information on the relationships between the variables, and start
# having an idea of what our results might look like in the end.

# 2.1) load needed packages/scripts -----------------------
  source("01_load_data.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/plot_vars.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/plot_collinearity.R")
  library("patchwork")

# 2.2) Preliminary analysis -------------------------------
  # 1) Subset the variables you want to examine. Put your Y variable first.
    to_test <- wilds[,c("Succeeded","Release.site","Year","Length.mm")]

  # 2) Verify data atributes
    head(to_test)
    str(to_test)
    # Apply any necessary corrections:

  # 3) Data structuring
    plot_vars(to_test)

    # If you want to test it for all variables:
    plot_vars(dplyr::relocate(wilds, "Succeeded"))

    # Check:
    # - outliers
    # - factor balance
    # - homoscedascity (for continuous Ys)
    
  # 4) Collinearity and Relationships
    # Both represent the same phenomenom, but the first is for X on X and the
    # second is for Y on X. Essentially, Collinearity is bad and Relationships
    # are good. The code will determine which variables are continouous and 
    # which are factorial on its own.
    plot_collinearity(to_test)

  # 5) Zeros
    # FOR CONTINUOUS Y's: 
    #  If the data has a high ammount of zeros, 
    #  it will require a model which accounts 
    #  for zero inflation.
    # FOR CATEGORICAL Y's:
    #  Ensure a good balance of each level's occurences.
    if (is.numeric(to_test[, 1])) {
      message(round(sum(to_test[, 1] == 0) / nrow(to_test) * 100, 2), "% of zeros")
    } else {
      table(to_test[, 1])
    }
    
# 2.3) quick example check --------------------------------
  with(wilds, table(Year, Release.site))

  # mean comparisons by year
  aggregate(wilds$Length.mm, list(wilds$Year), mean)
  aggregate(wilds$Length.mm, list(wilds$Year), sd)

  # mean comparisons by year×release
  aggregate(wilds$Length.mm, list(wilds$Year, wilds$Release.site), mean)
  aggregate(wilds$Length.mm, list(wilds$Year, wilds$Release.site), sd)

  # alternatively, you can use the mean_table
  # from my online repository
  means <- mean_table(wilds$Length.mm, 
                      list(Year = wilds$Year, Release.site = wilds$Release.site), 
                      ci = "sd", digits = 1)
  means

# 2.4) plot mean lengths ----------------------------------
  p <- ggplot(data = wilds, aes(x = Year, colour = Release.site))
  p <- p + geom_point(aes(y = Length.mm), 
                      position = position_jitterdodge(jitter.width = 0.2,
                                                      jitter.height = 0,
                                                      dodge.width = 0.4),
                      alpha = 0.4)
  # STYLE ONE:
  # p <- p + geom_pointrange(data = means, aes(y = mean, ymin = lower, ymax = upper))
  # STYLE TWO:
  p <- p + geom_errorbar(data = means, aes(ymin = lower, ymax = upper), 
                         position = position_dodge(width = 0.4), width = 0.1)
  p <- p + geom_errorbar(data = means, aes(ymin = mean, ymax = mean),
                         position = position_dodge(width = 0.4), width = 0.2)
  # --
  p
