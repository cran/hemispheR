# hemispheR 1.1.3
- Fixed a bug on reading metags in `binarize_fisheye()`
# hemispheR 1.1.2
- The display of `import_fisheye()` reports the selected circular mask centre.
- Additionally, a warning is displayed if the selected circular mask falls outside the image area;

# hemispheR 1.1.1
- Azimuth segments from `gapfrac_fisheye()` are now ordered following the North-clockwise convention, instead of the East-counterclockwise one (i.e., using `atan2(x,y)` instead of `atan2(y,x)`).

# hemispheR 1.1.0
- Fixed a bug in naming `GF` cols in `gapfrac_fisheye()` function & display segments

# hemispheR 1.0.1
- There is an issue with a extdata/fullframe image, fixed
- Read metadata in gapfrac_fisheye()

# hemispheR 1.0.0

Following R-spatial evolution (https://r-spatial.org/r/2023/05/15/evolution4.html), functions have been re-structured as follows:\

* Migration from `raster` to `terra` package
* Migration from `sp` to `sf` package
* Added references to readme

# hemispheR 0.2.0

* Minor changes in readme (rewritten math and added a link to panorama-to-spherical tool)
* use the \doi{} tag in Rd files and https://doi.org/ in readme

# hemispheR 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Avoid options(warn=-1) in some functions
* Add references describing the methods in DESCRIPTION
