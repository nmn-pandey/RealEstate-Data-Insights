# Real Estate Data Insights and Modelling using R
This repository contains an R Markdown file, housing dataset and HTML output showcasing a comprehensive analysis of property prices and furnishing status, ideal for real estate insights. The analysis encompasses data cleaning, exploratory data analysis, regression modeling for price prediction, and binary logistic regression for furnishing classification. Originally created as academic coursework for Quantitative Data Analysis, this project analyzes a housing dataset to predict property prices and determine furnishing status. Users can leverage the code and analysis for their own real estate data analytics and modeling.

## Dataset Information
The dataset used for this analysis consists of various features associated with property listings:

- `id`: Numerical identifier for each property.
- `price`: Numerical variable indicating the price of the property.
- `mq`: Numerical variable representing the size of the property in square meters.
- `floor`: Discrete numerical variable indicating the floor on which the property is located.
- `n_rooms`: Discrete numerical variable representing the number of rooms in the property.
- `n_bathrooms`: Discrete numerical variable indicating the number of bathrooms in the property.
- `has_terrace`: Binary variable indicating the presence (1) or absence (0) of a terrace.
- `has_alarm`: Binary variable indicating the presence (1) or absence (0) of an alarm system.
- `heating`: Binary variable indicating the presence (1) or absence (0) of a heating system.
- `has_air_conditioning`: Binary variable indicating the presence (1) or absence (0) of air conditioning.
- `has_parking`: Binary variable indicating the presence (1) or absence (0) of a parking space.
- `is_furnished`: Binary variable indicating whether the property is furnished (1) or not (0).

### 1. Data Quality Analysis and Cleaning
The dataset underwent a rigorous quality check involving:
- Verification against provided metadata.
- Confirmation of data dimensions, variable names, and data types.
- Review of data summaries and unique values in each column.
- Cleaning the data to remove/impute incorrect values and outliers.

### 2. Exploratory Data Analysis
The EDA involved individual analysis of each variable, including numerical summaries, skewness checks, and data visualizations. Correlations between numerical variables were also evaluated.

### 3. Modelling
A regression model was developed for house price prediction, critiqued using various diagnostics, and enhanced based on the evaluations.

### 4. Extension Work
A logistic regression model was built to predict the likelihood of a property being furnished, utilizing all variables to determine the model’s maximum likelihood estimates.

## Dependencies
Ensure R and RStudio are installed. Install the required R packages with the following commands:
```r
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
# Add other package installation commands here
```

## Usage
1. Clone this repository.
2. Open `CS5801_2246041.rmd` in RStudio.
3. Install the required dependencies.
4. Run the RMD file for analysis, or open `CS5801_2246041.html` to view the precompiled output.

## License
This project is under the MIT License - see [LICENSE.md](LICENSE.md) for details.
