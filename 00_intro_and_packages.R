## OTN SYMPOSIUM 2024
## Hugo Flávio (hugoflavio.com)
## 2024-07-28
## Script 0: Initial instructions

# NOTE: --------------------------
# The data you're being provided with is part of a published study. This data is
# _NOT_ to be reused for any publication (scientific or otherwise) without my 
# express permission.
# --------------------------------

# Welcome! In this workshop, we will be going through some common steps of
# performing (simple) analyses related to fish migration data collected
# using acoustic telemetry.

# Specifically, we are going to retrace some of the steps I had to go through
# while I was working on one of the papers from my PhD. You should have received
# a copy of the manuscript as part of the course materials; I highly recommend
# that you give it at least a glance before we start. If you don't have a copy
# on hand, grab one here: 
# https://hugoflavio.com/assets/pdf/Flavio_et_al_2020_Marine_mortality_in_the_river.pdf

# Inside the dataset folder, you will find the data we will be using, already
# separated by year. You will also find there a shape file and a transition
# layer. No need to worry about those, R will know what to do with them. At the
# start of the workshop, I will go through the details of the study area.

# The remaining R scripts in this folder have been organized in a chapter
# fashion. We will go through them together during the workshop.

# Please install the packages below before the start of the workshop, to save
# us some time on the day:

install.packages("actel")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("multcomp")
install.packages("circular")

# Finally, at the start of the workshop, please make sure that your laptop is 
# connected to the internet, as some of the functions we'll be using are stored
# in an online GitHub repository.
