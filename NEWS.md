
# CropRotationViz 1.1.3

# CropRotationViz 1.1.2

* Sped up the "Plot Sequence" and "Plot Crop Specific Sequence" tabs in the
  visualization app by rewriting `transform_rotation_data()` (used by both
  tabs) with `data.table` instead of `dplyr`/`tidyr`: the row-wise crop
  filters and the wide-to-long reshape were the main cost on large datasets.
  An initial version of this rewrite still converted the whole (potentially
  very large) `distribution_df` to a `data.table` up front, which cost about
  as much as it saved and left the tabs about as slow as before; the fix
  avoids that conversion entirely and filters directly on the source
  data frame's columns. Measured ~2.4x faster at ~900k fields, with
  identical output verified against the original implementation.
* Fixed unstable color scaling on the crop-specific area distribution map
  (Crop-Specific Analysis tab): the color scale used to be recalculated from
  whatever district/catchment/year happened to be on screen, so the same
  color could mean different area percentages depending on what was
  currently displayed. The scale is now fixed once per crop selection, using
  the global min/max area percentage across every district and catchment,
  every year, and their per-region averages, so colors stay comparable
  across views.
* Added a "Download Map (Shapefile)" export for the crop-specific area
  distribution map (Crop-Specific Analysis tab). Regardless of whether a
  single year or the complete average is currently displayed, the export
  always includes every year as its own column (`pct_<year>`) plus the
  average (`pct_avg`); the geometry (districts vs catchments) still follows
  whichever map type is currently selected.
* Added a "Download Map (Shapefile)" export for the Dominant Rotation Map on the
  Rotation Ranking tab, including the soil potential (`BS_mean`) column when
  available, plus a companion settings file recording the analysis parameters
  (rotation length, year range, filters) used to generate the export.
* Documented previously undocumented app features in `README.md`: the
  field-level exploration application, crop transition heatmap, rotation
  ranking, rotation analysis (monoculture detection and clustering), the
  Shannon-Wiener diversity index, AOI comparison mode, CSV/Excel/PNG export
  options, and bilingual (English/German) crop names.
* Fixed a typo in `README.md` referencing a nonexistent `dummy_filed_generator`
  function; corrected to `dummy_field_creator`.
* Synced the version number across the standalone bundle files (`DESCRIPTION`,
  installer scripts, `CropRotationViz.bat`, `User_Guide.txt`,
  `Technical_Notes.txt`, and the installer wizard icon label).

# CropRotationViz 1.1.1

* Fixed version mismatch between the package (`DESCRIPTION`) and the standalone
  Windows installer scripts, which were still pinned at 1.0.0.
* Corrected `check_versions.R` to point at the actual installer script location
  (`CropRotationViz_Standalones/installer/create_installer.iss`), so version
  consistency checks no longer silently skip the installer file.
* Synced `create_installer.iss` and `CropRotationViz_Simple.iss` to 1.1.1.

# CropRotationViz 0.1.0

* Initial CRAN submission.
