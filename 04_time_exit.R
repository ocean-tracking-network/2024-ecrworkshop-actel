## OTN SYMPOSIUM 2024
## Hugo Flávio (hugoflavio.com)
## 2024-07-28
## Script 4: Time of river exit

# What's going on here?
#
# In this script, we'll look into the specific time at which the fish left the
# river, determine if it happened during the day or night, and finally test if
# leaving during the night makes fish more likely to succeed.
# We'll also take the chance to make a nice circular plot of the lost/surviving
# fish as a function of their departure time.

# 4.1) load needed packages/scripts -----------------------
  source("01_load_data.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/plot_vars.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/plot_collinearity.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/convert_link.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/colorblind.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/expand_line.R")
  source("https://raw.githubusercontent.com/hugomflavio/effective-fiesta/main/Random_R_Functions/simple_legend.R")
  library("circular")

# 4.2) trim data ------------------------------------------
  # select fish whose last river detection was at the last river station
  from_14 <- wilds$Year == 2014 & wilds$Last.station.River == "St.3"
  from_17 <- wilds$Year == 2017 & wilds$Last.station.River == "St.6"
  from_18 <- wilds$Year == 2018 & wilds$Last.station.River == "St.4"

  sea <- wilds[which(from_14 | from_17 | from_18), ]

  # sea$Succeeded[sea$Transmitter == "R64K-2849"] <- FALSE

  sea$hour_left_river <- format(sea$Last.left.River, "%H:%M:%S")
  sea$nhour_left_river <- as.difftime(sea$hour_left_river, units = "hours")
  sea$nhour_left_river <- as.numeric(sea$nhour_left_river)
  sea$day_left_river <- format(sea$Last.left.River, "%Y-%m-%d")

  EV <- read.csv("dataset/bush_envir_data.csv")
  head(EV)
  EV$nSunrise <- as.numeric(as.difftime(paste0(EV$Sunrise, ":00"), units = "hours"))
  EV$nSunset <- as.numeric(as.difftime(paste0(EV$Sunset, ":00"), units = "hours"))

  link <- match(sea$day_left_river, EV$Timestamp)

  before_sunrise <- sea$nhour_left_river < EV$nSunrise[link]
  after_sunset <- sea$nhour_left_river > EV$nSunset[link]

  sea$left_river_night <- before_sunrise | after_sunset

# 4.3) quick preliminary check ----------------------------
  # 1) Subset the variables you want to examine. Put your Y variable first.
    to_test <- sea[,c("Succeeded", "left_river_night", "Year")]

  # 2) Verify data atributes
    head(to_test)
    str(to_test)
    # Apply any necessary corrections:

  # 3) Data structuring
    plot_vars(to_test)

    # Check:
    # - outliers
    # - factor balance
    # - homoscedascity (for continuous Ys)
    
  # 5) Collinearity and Relationships
    # Both represent the same phenomenom, but the first is for X on X and the
    # second is for Y on X. Essentially, Collinearity is bad and Relationships
    # are good. The code will determine which variables are continouous and 
    # which are factorial on its own.
    plot_collinearity(to_test)

  # 4) Zeros
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

# 4.4) model success chance -------------------------------

  m1 <- glm(Succeeded ~ left_river_night + Year,
    family = binomial(link = "logit"),
    data = sea)

  anova(m1)

  m2 <- glm(Succeeded ~ left_river_night,
    family = binomial(link = "logit"),
    data = sea)

  anova(m2)

# 4.5) plot success model ---------------------------------

  m2_fit <- data.frame(left_river_night = unique(sea$left_river_night))

  # Get predicted values and 95% CIs.
  aux <- predict(m2,
                 newdata = m2_fit,
                 type = "link",
                 se = TRUE)

  m2_fit <- cbind(m2_fit, convert_link(aux, "logit"))

  aux <- split(sea, list(sea$Year, sea$left_river_night))
  output <- lapply(aux, function(x) {
    data.frame(
      Year = x$Year[1],
      left_river_night = x$left_river_night[1],
      prop = sum(x$Succeeded)/nrow(x))
  })
  year_points <- do.call(rbind, output)

  year_points

  p <- ggplot(data = m2_fit, aes(x = left_river_night))

  p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1)
  p <- p + geom_errorbar(aes(ymin = fit, ymax = fit), width = 0.2)

  p <- p + geom_point(data = year_points,
    aes(y = prop, colour = Year), size = 2)

  # significance
  p <- p |> geom_signif(x1 = 1, x2 = 2, y1 = 1.02, drop = 0.005, label = "*")
  # p <- p |> geom_signif(x1 = 1, x2 = 3, y1 = 0.9, drop = 0.01, label = "***")

  # visuals
  p <- p + theme_bw()
  p <- p + scale_color_manual(values = unname(cb[1:3]))
  p <- p + scale_x_discrete(labels=c("TRUE" = "Night", "FALSE" = "Day"))
  p <- p + labs(y = "Survival probability",x = "", colour = "Year")
  p <- p + simple_legend(h = 0.85, v = 0.05, borderwidth = 0.2)
  p

  dev.size()
  ggsave("plot_04-1_exit_ggplot.png", width = 2.5, height = 4)

# 4.6) prepare circular data ------------------------------

  aux <- sea[, c("Signal", "Succeeded", "Last.left.River")]
  circular_time <- timesToCircular(aux, by.group = TRUE)

  watson.wheeler.test(circular_time) # Difference found

  plotTimes(circular_time, alpha = 0.5)


  jday_range <- range(as.numeric(format(as.Date(sea$day_left_river),"%j")))

  aux <- EV[EV$Julian %in% jday_range, ]
  mean(aux$nSunrise)
  mean(aux$nSunset)

  plotTimes(circular_time, alpha = 0.5, night = c("21:20", "05:30"))

  names(circular_time)
  names(circular_time) <- c("Lost", "Succeeded")

  plotTimes(circular_time, alpha = 0.5,
            night = c("21:20", "05:30"),
            file = "plot_04-2_circular_plot.pdf")

# That is the end of our exercises for this workshop.
# I hope you enjoyed it! :) 
#
# Is there something you'd like to see in a future workshop?
# Let me know, and I can consider it for the next symposium!