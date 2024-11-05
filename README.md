# CropRotationViz 🌾 :corn:

An interactive web application for visualizing and analyzing crop rotation patterns across agricultural landscapes. Built with R Shiny, this tool helps agricultural stakeholders understand rotation patterns, assess risks, and make data-driven decisions for sustainable farming practices.

<div align="center">
  <img src="files/example.png" alt="Descriptive Alt Text" width="100%">
</div>

## Table of Contents
- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Running the Application](#running-the-application)
- [Data Structure](#data-structure)
- [Usage Examples](#usage-examples)
  - [Basic Rotation Analysis](#basic-rotation-analysis)
  - [Risk Assessment](#risk-assessment)
- [Interface Overview](#interface-overview)
- [Contributing](#contributing)
  - [Development Setup](#development-setup)
- [Dependencies](#dependencies)
- [License](#license)
- [Target Users](#target-users)
- [Citation](#citation)
- [Acknowledgments](#acknowledgments)
- [Contact](#contact)
- [Updates & Versions](#updates--versions)

## Features

- **Interactive Rotation Visualization**
  - Dynamic Sankey diagrams showing crop sequences
  - Spatial distribution maps of rotation patterns
  - Temporal analysis of rotation changes

- **Risk Assessment Tools**
  - Disease pressure analysis based on rotation patterns
  - Weed risk evaluation
  - Hotspot identification for management optimization

- **Diversity Analysis**
  - Structural and functional diversity metrics
  - Temporal diversity trends
  - Regional comparison tools

## Getting Started

### Prerequisites

- R (>= 4.1.0)
- RStudio (recommended for development)
- Required R packages listed in the Dependencies section

### Installation

1. Clone the repository:
```bash
git clone https://github.com/franz-geoeco/CropRotationViz.git
cd CropRotationViz
```

2. Install dependencies:
```r
source("install_dependencies.R")
```

3. Configure your environment:
```r
# Copy example configuration file
cp config.example.R config.R
# Edit config.R with your settings
```

### Running the Application

```r
shiny::runApp()
```

## Data Structure

The application expects input data in the following format:

```r
data_structure <- data.frame(
  field_id = character(),      # Unique identifier for each field
  year = numeric(),            # Year of observation (2017-2023)
  crop = character(),          # Crop type
  area_ha = numeric(),         # Field size in hectares
  geometry = sf::st_geometry() # Spatial data
)
```

## Usage Examples

### Basic Rotation Analysis
```r
# Load example data
data <- read_rotation_data("path/to/data")

# Generate rotation visualization
plot_rotation_sankey(data)
```

### Risk Assessment
```r
# Calculate disease risk based on rotation patterns
risk_analysis <- calculate_disease_risk(rotation_data)

# Visualize risk hotspots
plot_risk_map(risk_analysis)
```

## Interface Overview

The application is organized into several main sections:

1. **Dashboard**
   - Overview statistics
   - Key performance indicators
   - Quick navigation

2. **Rotation Analysis**
   - Temporal patterns
   - Spatial distribution
   - Sequence analysis

3. **Risk Assessment**
   - Disease pressure maps
   - Weed risk evaluation
   - Management recommendations

4. **Reports**
   - Custom report generation
   - Data export options
   - Summary statistics

## Contributing

Contributions are welcome! Please read our [Contributing Guidelines](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

### Development Setup

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## Dependencies

This project relies on the following R packages:

- [shiny](https://github.com/rstudio/shiny) - Web application framework for R
- [shinythemes](https://github.com/rstudio/shinythemes) - Themes for Shiny
- [tidyverse](https://github.com/tidyverse/tidyverse) - Collection of R packages for data science
- [leaflet](https://github.com/rstudio/leaflet) - Create interactive web maps
- [ggalluvial](https://github.com/corybrunson/ggalluvial) - Alluvial diagrams in ggplot2
- [lattice](https://github.com/cran/lattice) - Data visualization system
- [colorspace](https://github.com/cran/colorspace) - Color space manipulations
- [shinyWidgets](https://github.com/dreamRs/shinyWidgets) - Custom widgets for Shiny
- [randomcoloR](https://github.com/ronammar/randomcoloR) - Random color generation
- [plotly](https://github.com/plotly/plotly.R) - Interactive web-based graphs
- [shinyBS](https://github.com/ebailey78/shinyBS) - Bootstrap components for Shiny
- [bslib](https://github.com/rstudio/bslib) - Custom Bootstrap themes for Shiny
- [DT](https://github.com/rstudio/DT) - R Interface to the DataTables library
- [sf](https://github.com/r-spatial/sf) - Simple Features for R
- [leaflet.minicharts](https://github.com/rte-antares-rpackage/leaflet.minicharts) - Add small charts on leaflet maps
- [shinycssloaders](https://github.com/daattali/shinycssloaders) - Add loading animations to Shiny outputs
- [shinyalert](https://github.com/daattali/shinyalert) - Easily create pretty popup messages in Shiny

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Target Users

- Agricultural Policy Makers
- Researchers & Scientists
- Farm Advisors & Consultants
- Farmers & Farm Managers
- Agricultural Education Institutions
- Environmental Organizations

## Citation

If you use this tool in your research, please cite:

```bibtex
@software{croprotationviz2024,
  author = {Schulze, Franz and Pöhlitz, Julia and Conrad, Christopher},
  title = {CropRotationViz: Interactive Analysis of Agricultural Rotation Patterns},
  year = {2024},
  publisher = {GitHub},
  url = {https://github.com/franz-geoeco/CropRotationViz}
}
```

## Acknowledgments

- Ministry for Climate Protection, Agriculture, Rural Areas and Environment of MWP for providing IACS data
- R Shiny community for excellent documentation and support
- Contributors and beta testers

## Contact

- **Project Lead**: Franz Schulze franz.schulze@geo.uni-halle.de
- **Project Website**: [https://croprotationviz.example.com](https://croprotationviz.example.com)

## Updates & Versions

See [CHANGELOG.md](CHANGELOG.md) for a list of changes and version updates.
