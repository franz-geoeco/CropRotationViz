
# CropRotationViz 1.1.4

* Replaced the drag-and-drop crop aggregation editor (Processing app, "Crop
  Code Aggregation Editor") with a searchable, filterable table and
  bulk-assign controls. The previous editor used one `sortable::bucket_list`
  per aggregation class sharing a single drag group; moving one crop forced
  every class box to re-render, which caused visible back-and-forth/flicker
  once a dataset had many crop codes and many classes. The table now holds
  crop-to-class assignment as a single data frame, edits are patched into the
  existing table via `DT::replaceData()` instead of re-rendering it, and
  crops are reassigned by ticking rows and clicking "Assign Checked Rows to
  Selected Class" or "Mark Checked Rows as Ignored" rather than dragging one
  at a time. Classes can still be created, renamed, and removed; the default
  crop-code aggregation (Code mode) still pre-populates automatically.
* Set the crop assignment table to show 100 rows by default (was 15), and
  fixed its search box, per-column filter boxes, length picker, and
  pagination controls being unreadable against the app's dark theme.
* Fixed the crop-specific area distribution map's color scale (Crop-Specific
  Analysis tab) being stretched flat by a single outlier: `compute_crop_
  percentage_range()` now caps the upper bound at the 95th percentile of area
  percentages across all districts/catchments/years instead of the true
  maximum, so one unusually high district-year no longer washes out the color
  differences across the rest of the map. Falls back to the true maximum if
  the 95th percentile collapses onto the minimum (e.g. a rare crop where
  almost all values are the same low number). Values above the 95th-percentile
  cap are now clamped to the domain's edge before coloring, so the
  districts/catchments with the highest percentages render as the darkest
  color instead of turning transparent (`leaflet::colorNumeric()` maps any
  value outside its domain to `na.color`, which was "transparent" here); the
  popup for each district/catchment still shows its true, unclamped
  percentage.

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
