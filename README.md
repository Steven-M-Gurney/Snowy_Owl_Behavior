# Snowy owl migratory behavior and management at DTW

### [Steven Gurney](https://linktr.ee/stevenmgurney), [Austin Messing](), [Nick Scobal](), [Selena Creed]()

### Manuscript: Airfield Operations – Wildlife Division, Special Publication No. 25-02. Wayne County Airport Authority, Detroit, MI, USA.

### Manuscript available here: [![DOI](https://zenodo.org/badge/678437269.svg)](https://doi.org/10.5281/zenodo.17839774)

### Data: Most data are sensitive and housed with the Wildlife Division's special-publication repository.

#### Please contact the first author for questions about the code or data: Steven M. Gurney (stevenmgurney@gmail.com)
__________________________________________________________________________________________________________________________________________

## Summary
At Detroit Metropolitan Wayne County Airport (DTW), we remain committed to upholding the highest standards of aviation safety, security, and operational efficiency. Consistent with 14 CFR § 139.337 and DTW’s Wildlife Hazard Management Plan, we recognize that proactive identification and mitigation of wildlife hazards are essential to reducing strike risk and protecting both airfield operations and the traveling public. Snowy owls (*Bubo scandiacus*) represent a uniquely challenging hazard at DTW due to their large body size, low-altitude hunting behavior, frequent use of airfield structures, and high public interest. Recognizing the importance of this issue, the Wayne County Airport Authority’s Airfield Operations – Wildlife Division conducted a comprehensive assessment of snowy owl activity, movement patterns, airfield use, and historical management practices. We applied an integrated suite of analytical methods, including temporal and spatial analyses of activity records, migration-timing evaluation, capture-method assessment, banding and recapture analysis, and strike metric interpretation. This work provided the first airport-specific evaluation of snowy owl behavior at DTW, identified operational periods and locations of elevated risk, and highlighted opportunities to refine existing management strategies. Our findings underscored the importance of rapid detection and response, informed translocation practices, and coordinated communication across airport departments. This assessment also supported the development of consistent, centralized data systems and established critical baseline metrics that will guide adaptive management in future migration seasons. Collectively, these efforts strengthen DTW’s commitment to science-driven, humane, and effective wildlife hazard management, ensuring safer airfield operations while contributing to the conservation of a Vulnerable Arctic species.

## Repository Directory

### [Capture_Method_Assessment.R](./Capture_Method_Assessment.R): Code to analyze use of snowy owl capture methods, including summarizing and visualizing results.

### [Data_Prep.R](./Data_Prep.R): Code to wrangle, merge, and prepare WCAA snowy owl data for analyses.

### [Time_of_Day.R](./Time_of_Day.R): Code to process, analyze, and visualize time-of-day results, including snowy owl observations and capture.

### [Time_of_Year.R](./Time_of_Year.R): Code to assign snowy owl migration periods, evaluate seasonal activity patterns, and produce plots and summary table.

### [Migration_Timing.R](./Migration_Timing.R): Code to visualize migration timing by migration period; and estimate overall average migration start and end dates.

### [Aircraft_Interactions.R](./Aircraft_Interactions.R): Code to summarize strike counts and damage by airport and migration period.

### [Bird_Bands.R](./Bird_Bands.R): Code to summarize WCAA-USDA snowy owl banding records; and estimate recapture rates of banded birds.

## Supplemental Information

### [SNOW_Presentation.pdf](./SNOW_Presentation.pdf): Relative presentation given at DTW's Wildlife Hazard Management Committee, Novemember 2025.
