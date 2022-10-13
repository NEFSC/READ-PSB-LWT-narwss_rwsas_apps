---
title: 'Shiny tools for management rules: interactive applications that aid in the conservation
  strategies for North Atlantic right whales'
authors:
- name: Leah M. Crowe
  orcid: 0000-0001-9133-8522
  affiliation: 1
- name: Tim V. N. Cole
  affiliation: 2
- name: Danielle M. Cholewiak
  affiliation: 2
date: "26 September 2022"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
bibliography: paper.bib
affiliations:
- name: Integrated Statistics under contract to the Northeast Fisheries Science Center,
    National Marine Fisheries Service, National Oceanic and Atmospheric Administration,
    USA
  index: 1
- name: Northeast Fisheries Science Center, National Marine Fisheries Service, National
    Oceanic and Atmospheric Administration, USA
  index: 2
tags:
- R
- right whales
- trigger analysis
- conservation
- management

---

# Summary

Conservation strategies aimed at protecting North Atlantic right whales (*Eubalaena glacialis*) in the United States currently include dynamic and static measures aimed to mitigate vessel strike of this critically endangered species [@NMFS:2013]. Seasonal Management Areas (SMAs) are static geographic areas where vessels larger than 65 ft (19.8 m) are required to limit their speed to 10 kts or less at prescribed, annual periods [@NMFS:2008; @NMFS:2013]. Dynamic Right Whale Slow Zones are declared when a threshold density of right whales is detected visually (a.k.a. Dynamic Management Areas, DMAs) [@NMFS:2008; @NMFS:2013] or acoustically outside of active SMAs [@NOAAFisheries:2020], and within these zones, the vessel speed restriction is voluntary[^1]. The boundaries of dynamic Slow zones are determined based on where the detections occur, and for visual detections, the number of right whales sighted factors into the calculated size of the area [@NMFS:2008; @NMFS:2013; @NOAAFisheries:2020].  

The vessel speed restriction ruling was first put into place in 2008 [@NMFS:2008], but beginning around 2010, a range-wide distribution shift occurred for this species [@davis2017long], and right whales were increasingly sighted outside of SMAs. This change in habitat use has been attributed to a climate-driven shift in right whale prey which has put pressure on dynamic measures to provide some level of protection for the whales [@Meyer-Gutbrod:2021] (Fig. 1). The increase in visual detections outside of federally mandated SMAs also called for technological innovations to bolster detections of right whales due to this regime shift [@NOAAFisheries:2020]. Passive acoustic detections of right whale vocalizations that could be reported in near real-time became an increasingly important tool to supplement visual sightings [@baumgartner2019persistent]. In late 2020, a program was introduced that expanded dynamic management by triggering Slow Zones in response to acoustic detections of right whale vocalizations [@NOAAFisheries:2020; @van2021noaa]. 

The expansion of the dynamic program, as well as the increase in detection of right whales outside of seasonal protection zones, called for tools to 1) streamline the process of handling multiple streams of right whale detection data, 2) refine and modernize the process of determining if the trigger criteria for dynamic measures were met, and 3) quickly and accurately calculate the boundaries of a proposed Slow Zones and communicate this information to federal managers in near real-time.

# Statement of need

Vessel strikes and entanglement in fishing gear are the leading causes of right whale mortality and constrain the recovery of this species [@corkeron2018recovery]. North Atlantic right whales predominately live along the eastern seaboard of the United States and Canada where some of the busiest shipping ports in North America exist. The North Atlantic Right Whale Sighting Survey (NARWSS) data processing tool, and the Right Whale Sighting Advisory System (RWSAS) trigger analysis tool improve the workflow between data processing and the determination of potential Right Whale Slow Zones (Fig. 2). These tools provide a platform that allows for efficient survey data processing to help eliminate sources of human error, integrates currently active management zones, determines if trigger criteria for a new dynamic protection zone has been met, and calculates the boundaries of proposed Slow Zones. 

The tools described here have been developed to be specifically used by scientists at the Northeast Fisheries Science Center (NEFSC) in their role of providing data on right whales to federal managers; however, the data processing and reporting procedure will be of interest to survey and management teams with similar objectives. Additionally, these publicly available tools provide transparency and reproducibility of the process of declaring dynamic protection zones. 

# 'Aerial Survey Data Processing'

Current survey data collection software used by the NEFSC renders three relevant files at the end of an aerial survey flight: a Global Positioning System (GPS) file that logs time and position at a determined sampling rate, an effort file that holds recorded changes in weather conditions and survey mode, and a sightings file that includes information on animal sighting details including species, group size, and position. The 'Aerial Survey Data Processing' tool provides a platform that merges these files together, and gives the user an interface to edit and visualize the survey data via interactive maps. Finally, this tool executes the repeated processes that compile the data into the suitable form for storage in a long-term database. The final stages of this application writes a csv file of the final form of the data, uploads right whale sightings to the RWSAS database, and renders a PDF or HTML summary report for the survey day. 

# 'Trigger Analysis'

Both visual and acoustic detections come from several sources, including governmental, educational, military, commercial, and non-profit institutions in addition to the general public. The 'Trigger Analysis' tool accesses internal and external databases where these detection data are amalgamated. Visual and acoustic detection data can be evaluated on a daily scale. Per the criteria of the Slow Zone program, either three or more upcalls (an acoustic vocalization that is common for this species and is known to be made by all ages and sex classes, e.g. @parks2011sound; @van2021noaa) are acoustically detected within a 15 minute period or sightings of three of more whales with a minimum density of 4 whales per 100 nm$^2$ are observed outside of SMAs or existing dynamics zones are required to trigger the creation of extension of a dynamic protection zone (see Fig. 3 for the decision making process built into the code). When a dynamic Slow Zone is triggered, this application will then determine the boundaries of the zone and name it relative to the closest port or landmark. Both a letter to federal managers proposing the zone, as well as a report describing the potential zone, are generated as PDFs, and the data describing the zone are uploaded to an internal database. 

# Conclusion

These tools have been part of the NEFSC Right Whale team workflow since 2017, and have been greatly refined and improved. Other applications, including one used for dynamic management in eastern Canada, have been modeled off of the processes in the 'Trigger Analysis' tool, and other teams have since created tools similar to the 'Aerial Survey Data Processing' application to process survey data and compile reports. Due to the dire status of the North Atlantic right whale, new conservation strategies are being discussed to improve their ability to recover, including a proposed ruling to make speed restrictions within dynamic Slow Zones mandatory and applicable to smaller vessels (87 FR 46921). To decrease sources of human error and to increase the efficiency of determining when these kinds of management schemes are triggered, tools to help streamline the process in a reproducible way are vital to timely implementation of protection measures. 

# Acknowledgements

We acknowledge thoughtful testing and critiques from Allison Henry, Christin Khan, Pete Duley, and Heather Foley. We additionally thank Hansen Johnson, Genevieve Davis, Beth Josephson, Peter Corkeron, the Shiny people at the Northeast Fisheries Science Center (particularly Andy Beet, Kim Bastille, Josh Hatch, Dave Hiltz, and Alicia Miller), and Brigid McKenna/Center for Coastal Studies right whale aerial survey team.

# Figures

![The number of dynamic Slow Zones that have been triggered for each year that the vessel speed rule has been in place. Dynamic Management Areas (DMAs) are triggered by visual detections, and Acoustic Slow Zones, triggered by the detection of right whale upcall vocalizations, were added to the program in late 2020. *Data for 2022 are through September.\label{fig:figure1}](Fig1.png){width=70%}

![The stages that each application addresses in the process of right whale detections informing conservation actions.\label{fig:figure2}](Fig2.png){width=90%}

![a. Decision chart for determining if the trigger criteria has been met to request dynamic Slow Zones in the United States, and b. an example trigger analysis for a survey day when both static (SMAs) and dynamic Slow Zones were active, and visual sightings were observed both within and outside active protection zones. \label{fig.figure3}](Fig3ab.png){width=90%}

[^1]: On 1 August 2022, the US National Marine Fisheries Service proposed amendments to the North Atlantic Vessel Strike Reduction Rule that would make dynamic speed restriction zones mandatory (87 FR 46921)

# References
