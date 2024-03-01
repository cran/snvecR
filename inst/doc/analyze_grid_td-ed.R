## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12, fig.height = 8
)

## ----setup--------------------------------------------------------------------
library(tibble)  # nice dataframes
library(dplyr)   # mutate/select/filter/glimpse
library(purrr)   # pmap
library(tidyr)   # unnest
library(ggplot2) # nice plots
library(snvecR)  # this package

## ----make-grid----------------------------------------------------------------
biggrid <- expand.grid(Td = c(0, 0.5, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2),
              Ed = c(1.000, 0.998, 1.005, 1.012)) |>
    as_tibble()
    # that's 32 rows
biggrid

## ----update-grid--------------------------------------------------------------
biggrid <- biggrid |>
    # for now only for 1000 years at very high tolerance so it's fast
    mutate(atol = 1e-4, tend = -1e3)
    # this would be the real deal, the full 100--0 Myr results at medium
    # tolerance.
    ## mutate(tol = 1e-7, tend = -1e5)

## ----snvec-tail---------------------------------------------------------------
snvec_tail <- function(..., n = 100) {
  # do the fit with the parameters in ...
  snvec(...) |>
    # save only the last n values, that's where the differences are greatest
    tail(n = n)
}

## ----massive-compute----------------------------------------------------------
biggrid <- biggrid |>
    # apply our new function!
    mutate(sol = pmap(list(td = Td, ed = Ed, tend = tend, atol = atol),
                      .f = snvec_tail,
                      # additional parameters to snvec_tail can go after!
                      quiet = TRUE, output = "nice", n = 100,
                      # I would strongly recommend against increasing the
                      # resolution too much, but for speed/illustration we
                      # prefer to do it here
                      tres = -5,
                      # interactively this makes a nice progress bar
                      .progress = "snvec on a grid")) #|>

    # normally we would save the results to file, because these take quite a
    # long time to calculate and we don't want to accidentally delete them.
    ## write_rds("out/2023-04-05_biggrid.rds")

## ----read-old, eval=FALSE-----------------------------------------------------
#  biggrid <- readr::read_rds("out/2023-04-05_biggrid.rds")

## ----check--------------------------------------------------------------------
glimpse(biggrid)

## ----unnest-------------------------------------------------------------------
expanded <- biggrid |>
  unnest(sol)
expanded

## ----plot---------------------------------------------------------------------
expanded |>
  ggplot(aes(x = time, y = cp,
             colour = factor(Td),
             linetype = factor(Ed))) +
  labs(x = "Time (kyr)",
       y = "Climatic precession",
       colour = "Tidal dissipation",
       linetype = "Dynamical ellipticity") +
  # make panels of plots
  facet_grid(rows = vars(Td)) +
  geom_line() +
  # add eccentricity
  geom_line(aes(y = ee),
            linetype = "solid",
            colour = "black",
            data = get_solution() |>
              filter(time > -1000) |>
              filter(time < -500))

## ----add-filenames------------------------------------------------------------
biggrid <- biggrid |>
  # get rid of sol column
  select(-sol) |>
  # add a filename that's easy to break into relevant parameters later
  # I write to tempdir here, but you might want to write to something like out/
  mutate(file = glue::glue("{tempdir()}/2023-04-13_biggrid_{Td}_{Ed}_{atol}_{tend}.rds"))
biggrid

## ----snvec-save---------------------------------------------------------------
snvec_save <- function(..., file) {
  snvec(...) |>
    readr::write_rds(file)
  cli::cli_inform("Wrote file {.file {file}}.")
}

## ----run-pwalk----------------------------------------------------------------
biggrid |>
  # in this case we make sure that column names are identical to argument names
  # so that the list (in this case tibble/data.frame) is matched to the correct
  # arguments
  rename(td = Td, ed = Ed, atol = atol) |>
  purrr::pwalk(.f = snvec_save,
               # additional parameters can go after!
               quiet = TRUE, output = "nice", tres = -5,
               # show progress bar
               .progress = "snvec to file")

## ----read-files---------------------------------------------------------------
biggrid |> # limit to a few experiments
  ## slice(c(1, 15, 32)) |>
  # in this case that's not necessary because we limited it to a very
  # low-resolution (tres) and short time period (tend)
  # read them in to list-column
  mutate(fullsol = map(file, readr::read_rds)) |>
  # unfold the list column
  unnest(fullsol) |>
  # plot the obliquity
  ggplot(aes(x = time, y = epl,
             colour = factor(Td),
             linetype = factor(Ed))) +
  labs(x = "Time (kyr)", y = "Obliquity",
       colour = "Tidal dissipation",
       linetype = "Dynamical ellipticity") +
  ## facet_grid(rows = vars(Td)) +
  geom_line()

