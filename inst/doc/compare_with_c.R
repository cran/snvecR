## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8, fig.height = 4
)
## modern_r <- getRversion() >= "4.1.0"
pth <- withr::local_tempdir(pattern = "snvecR")
withr::local_options(list(snvecR.cachedir = pth))

## ----setup--------------------------------------------------------------------
library(tibble)  # nice dataframes
library(ggplot2) # nice plots
library(snvecR)  # this package

## ----snvec--------------------------------------------------------------------
dat <- snvec(-1e5, 1, 1, astronomical_solution = "full-ZB18a")

## ----pt-----------------------------------------------------------------------
pt <- get_solution("PT-ZB18a(1,1)")

## ----prec---------------------------------------------------------------------
pl <- ggplot(dat, aes(x = time / 1000, y = cp)) +
  labs(x = "Time (Myr)",
       y = "Climatic precession") +
  geom_line(aes(colour = "snvecR ZB18a(1,1)")) +
  geom_line(aes(colour = "snvec  ZB18a(1,1)"),
            data = pt) +
  # add eccentricity
  geom_line(aes(y = ee, colour = "ZB18a eccentricity"),
            linetype = "solid",
            data = get_solution("full-ZB18a")) +
  labs(colour = "")
pl + xlim(-60, -59)

## ----obl----------------------------------------------------------------------
plo <- ggplot(dat, aes(x = time / 1000, y = epl)) +
  labs(x = "Time (Myr)",
       y = "Obliquity (rad)") +
  geom_line(aes(colour = "snvecR ZB18a(1,1)")) +
  geom_line(aes(colour = "snvec  ZB18a(1,1)"), data = pt) +
  labs(colour = "")
plo + xlim(-60, -59)

## ----prec2--------------------------------------------------------------------
pl + xlim(-100, -99)

## ----obl2---------------------------------------------------------------------
plo + xlim(-100, -99)

