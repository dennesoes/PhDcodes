PhD Codes

Chapter 3: General measures
	• PT convenience score
		○ ConvenienceBus.R - Compute convenience scores based on bus stops
		○ ConvenienceTrain.R - Compute convenience scores based on railway stations and PT in general
	• Attitude clustering
		○ Attitudes_segmentation.Rmd - Create and explore attitudinal clusters

Chapter 4:
	• Affect_Regression.R
		○ Logistic regression models for the affective rating of free associations
	• Analysis_function.R
		○ Function to analyse differences in the content of free associations (Chi-square tests and correspondence analysis with moonplots)
	• AssociationAnalysis.R
		○ Process questionnaire data and do analyses related to free associations
	• Predef_Barriers.R
		○ Analyse predefined barrier ratings (Kruskal Wallis and Post-hoc Dunn tests, Logistic regression with odds ratio plots, boxplots for each barrier rating)
	• Predef_Barriers_Behaviour_Correlation.R
		○ Compute correlation values between each barrier and travel behaviour (mode use frequency)
	• Predef_Barriers_Corrplot.R
		○ Correlation plot for all predefined barrier ratings

Chapter 5:
	• Interactive_mapping.R 
		○ Cleaning, plotting and editing the location data of each user on a map
		○ Uses functions:
			§ TrackDistTime.R: compute measures between location points, such as distance and time
			§ Data_cleaning.R: reduce noise in the data
			§ Do_kalman.R: smoothening the tracking data using a Kalman filter
			§ Create_map.R: plot the data on an interactive map 
	• Process_trips.R
		○ Process the manually created trip tables and compute measures such as trip distance and time
		○ Uses functions:
			§ Trip_distance_time.R
	• Trip_list.R
		○ Combine all processed trip data files and enrich with additional information
	• Trip_analysis_PT.R
		○ Analyses on walks to/from PT
	• Walks_linkplace.R
		○ Define the link-place walkability for each walk
	• Planner_publictransport.R
		○ Plan all car trips by public transport
	• Car_timecompute.R
		○ Simulate travel time for car trips
	• Trip_analysis_car
		○ Analyses on potential walks to/from PT (current car trips)
