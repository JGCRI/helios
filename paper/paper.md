---
title: 'helios: An R package to process heating and cooling degrees for GCAM'
tags:
  - R
  - heating and cooling degree-hours and degree-days
authors:
  - name: Mengqi Zhao
    orcid: 0000-0001-5385-2758
    affiliation: 1
  - name: Zarrar Khan
    orcid: 0000-0002-8147-8553
    affiliation: 2
  - name: Kalyn Dorheim
    orcid: 0000-0001-8093-8397
    affiliation: 2
  - name: Chris Vernon
    orcid: 0000-0002-3406-6214
    affiliation: 1
affiliations:
 - name: Pacific Northwest National Laboratory, Richland, WA, USA
   index: 1
 - name: Joint Global Change Research Institute, Pacific Northwest National Laboratory, College Park, MD, USA
   index: 2
date: July 2023
bibliography: paper.bib
---

# Summary

`helios` is an open-source R package that estimates population-weighted heating and cooling degree-hours (HDH and CDH) and degree-days (HDD and CDD) at various temporal (e.g., energy dispatch segments, monthly, yearly) and spatial scales (e.g., U.S. states, global political regions, countries). The degree hour and degree day outputs from `helios` are used to inform electricity demand load in the Global Change Analysis Model (GCAM) [@calvin2019gcam] as well as in GCAM-USA (which is the version of GCAM with U.S. state-level details) [@binsted2022gcam-usa]. `helios` uses a workflow with four steps: processing raw data; calculating heating and cooling degrees; visualizing performance diagnostics; and outputing results in various formats. There are two sources of widely-used climate data compatible with `helios`: (1) hourly climate data with 12-km resolution that are dynamically downscaled with the Weather Research and Forecasting (WRF) model and projected using a thermal global warming (TGW) approach [@jones2022im3]; and (2) daily climate data with 0.5-degree resolution from the Coupled Model Intercomparison Project (CMIP) that is bias-adjusted and statistical downscaled by the Inter-Sectoral Impact Model Intercomparison Project (ISIMIP). In summary, `helios` is a model that standardizes methodology of heating and cooling degrees-hours and degree-days using publicly available data and advances the understanding of the impact of spatial and temporal temperature variability on building energy services.

# Statement of Need

`helios` was developed to meet the increasing research interests to explore the spatial and temporal heterogeneity of climate impacts on sub-annual electricity demand from buildings. @ciscar2014integrated pointed out most integrated modeling systems designed to link human-Earth systems are unable to take advantage of the publicly available high resolution data to account for the impact of seasonal temperature change on energy system. To better fill in this gap, researchers have developed GCAM versions (e.g., GCAM-USA) to include power sector details at sub-annual and sub-national level [@wise2019representing]. For example, @khan2021impacts used GCAM-USA to show that the temperature-induced heating and cooling demands can significantly affect sub-annual electricity demand profiles and peak electricity loads. Understanding the seasonal dynamics of electricity demand and capacity within multi-sector dynamics models is of importance to support future infrastructure planning [@binsted2022electrified]. We develop `helios` to bridge the gap between high resolution data and global scale models by facilitating the workflow in estimating population-weighted heating and cooling degrees. `helios` serves as a pre-processing tool of GCAM for researchers to capture the impact of sub-annual variation of different climate and socioeconomic scenarios on building energy demand.

# Statement of Field

Heating and cooling degree days are commonly used as meteorological indices in the energy system to measure the temperature deviations from the reference temperature over time. These indices are widely calculated at point scale rather than spatial scale. With the increasing availability of spatially distributed climate data, few tools are developed to access and post-process the raw climate data format into tabular format, such as `Climate4R` [@iturbide2019]. However, there are rarely well-documented and open-source tools that streamline the calculation of population-weighted HDD and CDD at user-defined spatiotemporal resolutions or electricity dispatch sectors defined in GCAM, directly using gridded climate and population data. `helios` is developed to integrate these workflows and standardize the output for easy usage within and beyond GCAM applications.

# Design and Functionality

`helios` is designed to provide heating and cooling degrees to GCAM (or GCAM-USA) at two spatiotemporal scales: (1) HDH and CDH for the U.S. States for dispatch segments by building thermal service (for GCAM-USA); (2) annual HDD and CDD for GCAM's 32 geopolitical regions at 5-year time step by building thermal service (for GCAM-Regions). Beyond providing information for GCAM, `helios` can serve as a general tool to calculate heating and cooling degrees at a monthly time step and various spatial scales (e.g., country, basin). The use of `helios` requires users to provide information about the input files, such as the climate model source for climate data and the variable name for temperature. For example, the ISIMIP-CMIP data uses "tas" for the variable name for temperature while the WRF data uses "T2". Figure 1 shows the workflow for both GCAM-USA and GCAM-Regions. More details can be accessed in the `helios` [documentation](https://jgcri.github.io/helios/index.html) page on Github.

![An example of the helios workflow using two types of input datasets (e.g., global data from CMIP6 and CONUS data from WRF). This demonstration showcases helios' capability to generate heating and cooling degrees by GCAM region or U.S. States, among other spatiotemporal scales. \label{fig:1}](Fig1_helios_workflow.jpg)

Working with climate data can pose challenges, given large data sizes and diverse formats, spatiotemporal resolutions, data structures, and dimensions involved. The `helios` package provides functionalities that make it more convenient for users to manipulate climate data. `helios` provides easier access to various climate data types in a simplified format, facilitates the calculation of heating and cooling degrees using a standardized methodology, and ensures quality control through detailed diagnostics. There are five main functions provided by `helios`:

(1) `helios::read_ncdf` processes complex climate data (e.g., NetCDF) and converts to tabular data with latitude and longitude.
(2) `helios::read_population` processes population data and converts to the same resolution as the climate data if needed.
(3) `helios::hdcd` calculates heating and cooling degree-hours and degree-days at various spatial and temporal scales.
(4) `helios::diagnostics` visualizes the outputs and compares with observation data if available.
(5) `helios::save_xml` converts outputs to XML files, which is a format required by GCAM to calculate building energy demand.

# Acknowledgements

This research was supported by the U.S. Department of Energy, Office of Science, as part of research in MultiSector Dynamics, Earth and Environmental System Modeling Program.

# References
