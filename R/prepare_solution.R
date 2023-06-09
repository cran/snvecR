#' Prepare Orbital Solution
#'
#' Calculates helper columns from an orbital solution input.
#'
#' @param data The output of [get_solution()].
#' It needs to contain columns:
#'
#' * `t` Time \eqn{t} (days).
#' * `lph` Longitude of perihelion \eqn{\varpi} (degrees).
#' * `lan` Longitude of the ascending node \eqn{\Omega} (degrees).
#' * `inc` Inclination \eqn{I} (degrees).
# inherit quiet
#' @inheritParams get_ZB18a
#' @returns A [tibble][tibble::tibble-package] with the new columns added.
#' @seealso [get_ZB18a()] [get_solution()]
#'
#' @details
#' New columns include:
#'
#' * `lphu` Unwrapped longitude of perihelion \eqn{\varpi} (degrees without
#'   jumps).
#'
#' * `lanu` Unwrapped longitude of the ascending node \eqn{\Omega} (degrees
#'   without jumps).
#'
#' * `hh` Variable: \eqn{e\sin(\varpi)}{ee * sin(lph / R2D)}.
#'
#' * `kk` Variable: \eqn{e\cos(\varpi)}{ee * cos(lph / R2D)}.
#'
#' * `pp` Variable: \eqn{2\sin(0.5I)\sin(\Omega)}{2 * sin(0.5 * inc / R2D) * n
#'   R2D)}.
#'
#' * `qq` Variable: \eqn{2\sin(0.5I)\cos(\Omega)}{2 * sin(0.5 * inc / R2D) *
#'   n  R2D)}.
#'
#' * `cc` Helper: \eqn{\cos(I)}{cos(inc / R2D)}.
#'
#' * `dd` Helper: \eqn{\cos(I)/2}{cos(inc / R2D / 2)}.
#'
#' * `nnx`, `nny`, `nnz` The \eqn{x}, \eqn{y}, and \eqn{z}-components of the
#'   Earth's orbit unit normal vector \eqn{\vec{n}}{n}, normal to Earth's
#'   instantaneous orbital plane.
# HCI = heliocentric inertial
#  \item{npx, npy, npz}{The \eqn{x}, \eqn{y}, and \eqn{z}-components of the unit
#   normal vector \eqn{\vec{n}'}{n'}, relative to ECLIPJ2000.}
# IOP = instantaneous orbit plane
prepare_solution <- function(data, quiet = FALSE) {
  if (!all(c("lph", "lan", "t", "inc") %in% colnames(data))) {
    cli::cli_abort("Column names 't', 'lph', 'lan', 'inc' must be in data.")
  }

  if (!quiet) cli::cli_alert_info("Calculating helper columns.")
  data |>
    dplyr::mutate(
      lphu = unwrap(.data$lph),
      lanu = unwrap(.data$lan)
    ) |>
    dplyr::mutate(age = -.data$t / KY2D, .after = "t") |>
    dplyr::mutate(
      hh = .data$ee * sin(.data$lph / R2D),
      kk = .data$ee * cos(.data$lph / R2D),
      pp = 2 * sin(0.5 * .data$inc / R2D) * sin(.data$lan / R2D),
      qq = 2 * sin(0.5 * .data$inc / R2D) * cos(.data$lan / R2D),
      cc = cos(.data$inc / R2D),
      dd = cos(.data$inc / R2D / 2),
      ## /* nn <- nvec(t): normal to orbit */
      nnx = sin(.data$inc / R2D) * sin(.data$lan / R2D),
      nny = -sin(.data$inc / R2D) * cos(.data$lan / R2D),
      nnz = cos(.data$inc / R2D)
    )
}
