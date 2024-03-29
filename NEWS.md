# snvecR 3.9.1

# snvecR 3.9.0
* Refactor `age` in ka to `time` in kyr throughout.
* Rename default astronomical solution from PT-ZB18a to full-ZB18a.
* For the OS, convert time to negative for consistency.
* Changed some default output! Now returns time (kyr), no longer both t and t_kyr.

# snvecR 3.8.0
* Fix snapshot tests for CI (overwrite the cache dir, which is unique to each test).
* Make it possible for the user to specify a custom orbital solution as a
  data.frame. It should either have the same column names as the output of
  get_ZB18a() or as the output of [orbitN](https://github.com/rezeebe/orbitN);
  we automatically convert the column names to snvec columns names for
  convenience.
* Rename orbital solution to astronomical solution throughout.
* Migrate get_ZB18a to get_ZB so that the user can easily get all the Zeebe
  solutions from the website (and cache them as well).
* Added support for reading all Zeebe astronomical solutions to function
  get_solution(). Also a added a wrapper for `astrochron::getLaskar()` if the
  user provides a supported Laskar solution name.

# snvecR 3.7.7
* Fix CRAN issues by revising the logic for checking the cache.

# snvecR 3.7.6
* Remove cache directory after last example (even though it's in a donttest environment).

# snvecR 3.7.5
* Added a `NEWS.md` file to track changes to the package.
* Note: version number is consistent with snvec C package.
* Implemented first public version based on C-code in [Zeebe and Lourens
  2022](https://doi.org/10.1029/2021PA004349), as explained .
* Added `snvec()` function that does all the work.
* Added `ZB18a` dataset with orbital solution.
* Added tests using `testthat`.
* Added `snvec()` `output` parameter with choice to select `"ode"`, `"all"`, or `"nice"` (default).
* Added a vignette with a grid of variations on Td and Ed.
* Added a bookdown website rendering.
* Released a version to Zenodo and assigned a doi.
* Removed `ZB18a` dataset from the package because it was too large.
* Added functions get_solution, get_ZB18a, prepare_solution.
* Added caching code for the orbital solution.
* Remove the cache directory after running tests, so that reproducible environments remain clean.
