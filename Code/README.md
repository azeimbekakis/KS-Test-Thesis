---
title: "Code supplement to On Misuses of the Kolmogorov–Smirnov Test for One-Sample Goodness-of-Fit"
author: "Blinded"
date: "April 24, 2024"
---

This folder contains R code for reproducing the analysis in the paper.

# Implementation of the Proposed KS Test

+ ksfitted.R: Function to perform one-sample K-S test with fitted parameters and serial dependence
+ tacvfARMA.R: a bug fix to ltsa::tacvfARMA() that is used in ksfitted.R for serially dependent data

# Simulation study

+ sim.R: R script that carries out the reported simulation studies on the sizes of the tests
+ plot.R: R script that generates the Q-Q plots in the paper
+ power.R: R script that performs the power study of the tests
+ power-sum.R: R script that generates the power table in the paper.

