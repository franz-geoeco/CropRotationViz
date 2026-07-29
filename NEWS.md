
# CropRotationViz 1.1.2

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
