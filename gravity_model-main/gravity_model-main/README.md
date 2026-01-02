# Analyzing Global Bilateral Trade Flows in Transportation Services Using the Gravity Model

###### Completion date (DD/MM/YYYY): 06/11/2024

### 1. Project Background
The Gravity Model of international trade is a widely used framework to analyze bilateral trade flows, emphasizing factors such as economic size, geographical distance, and cultural or institutional similarities between trading partners. This project applies the Gravity Model to examine the trade in Transportation Services (TSP) in 2015, focusing on how variables like GDP, distance, contiguity, common language, and OECD membership influence trade patterns. By analyzing descriptive statistics, graphical correlations, and regression models, the project explores the determinants of trade in TSP and evaluates the effectiveness of the Gravity Model in explaining these flows. The study also investigates the role of policy variables, such as OECD membership, and assesses whether the model can provide causal insights or merely correlational patterns. This analysis contributes to understanding the dynamics of trade in services, particularly in the transportation sector, and highlights the importance of institutional and geographical factors in shaping global trade networks.

#### 1.1 Key Questions Addressed
- What are the descriptive characteristics of trade in Transportation Services (TSP) in 2015, and what proportion of countries engaged in non-zero trade in this sector?

- How do geographical distance and combined GDP influence bilateral trade in Transportation Services, and do factors like contiguity and common language enhance trade relationships?

- What is the impact of distance, GDP, contiguity, and common language on bilateral trade in Transportation Services, as estimated by the intuitive Gravity Model?

- How does OECD membership influence bilateral trade in Transportation Services, and does the augmented Gravity Model provide additional explanatory power compared to the intuitive model?

- What are the effects of distance and OECD membership on trade in Transportation Services when using a structural Gravity Model with importer and exporter fixed effects, and how do these results compare to the augmented model?
- What challenges arise when estimating the intuitive or structural Gravity Model, particularly regarding the policy variable OECD, and can these models provide causal insights into the determinants of trade in Transportation Services? 

### 2. Methodology

#### The Software package used for this project is "R"

This project follows Shepherd et al., (2019) user guide to the Gravity Model of international trade. The guide relies on a dataset on bilateral trade in services for the year 2015, which was compiled by Francois et al., (2009) and categorized into sector definitions by the Global Trade Analysis Project (GTAP). While the examples in the guide utilize total (aggregate) services trade, identified as sector SER, for this project, the  focus will be on Transportation Services (TSP). The data for the project is available at  https://www.unescap.org/resources/gravity-model-international-trade-user-guide-r-version. Full details of the models used can be found in the project report available at [tps_report.pdf](resources/gravitymodel_tsp_report.pdf) and the .html output file of  the analysis from R available at [tps_output.html](resources/tps.html). 

### 3. Results

###### _The results can be found in the report attached: [FRE 502 Assignment.pdf](https://github.com/user-attachments/files/24406344/FRE.502.Assignment.pdf)



### 4. References 

Shepherd, B., Doytchinova, H. S. & Kravchenko, A.. (2019). The gravity model of international trade: a user guide [R version]. Bangkok: United Nations ESCAP. Available at: https://www.unescap.org/resources/gravity-model-international-trade-user-guide-r-version

### 5. Appendix

- The data used for this project is available at https://www.unescap.org/resources/gravity-model-international-trade-user-guide-r-version
- The R script, which includes step-by-step codes and comments for running all the analysis for the project, is available [here](resources/tps_bello.R)
- The output of the analysis on R in ```.html``` is available [here](resources/tps.html)

