
# HarvardX Dengue Data Analysis and Modeling

Welcome to the **HarvardX Dengue Data Analysis and Modeling** repository! This project is the culmination of the **HarvardX Data Science Professional Certificate** capstone, where we leveraged machine learning and statistical techniques to predict and analyze dengue fever outbreaks.

## Overview
Dengue fever poses a significant global health challenge, especially in tropical and subtropical regions. This project examines the relationship between environmental, climatic, and temporal factors to predict the number of dengue cases in two cities: **San Juan (SJ)** and **Iquitos (IQ)**. 

Here is the link to download the dataset: https://www.kaggle.com/datasets/ahmadpk/dengue-dataset

The dataset includes features such as:
- Temperature (minimum, maximum, and average)
- Precipitation
- Vegetation indices (NDVI)
- Sea-level pressure
- Relative humidity

Machine learning models like **Random Forest (RF)** and **XGBoost** were employed to predict dengue outbreaks effectively.

## Key Features
1. **Exploratory Data Analysis (EDA):**
   - Uncovered patterns and relationships between features and dengue outbreaks.
   - Visualized data distributions for better insights.

2. **Data Preprocessing:**
   - Handled missing values using mean imputation.
   - Scaled and normalized features for better model performance.
   - Created new features such as average temperatures and vegetation indices.

3. **Feature Engineering:**
   - Split data by city for independent analysis.
   - Applied one-hot encoding for temporal data.

4. **Modeling:**
   - Implemented multiple machine learning models with a focus on handling imbalanced datasets.
   - Evaluated models using **Mean Absolute Error (MAE)**.

5. **Visualization:**
   - Correlation heatmaps to identify key predictors.
   - Scatter plots to understand relationships between dengue cases and environmental factors.

## Dataset
The dataset used for this project includes: 
- **Dengue Features Train:** Contains 24 columns of environmental and climatic data.
- **Dengue Labels Train:** Includes the total dengue cases recorded.

The dataset was preprocessed to ensure integrity and suitability for machine learning.

## Installation
To run this project locally, you'll need R and the following R packages installed:

```r
install.packages(c("tidyverse", "caret", "xgboost", "randomForest", "ggcorrplot", "kableExtra"))
```

## Usage
1. Clone the repository:
   ```bash
   git clone https://github.com/Jencheng1/harvardx-dengue.git
   ```

2. Open the R scripts in your preferred IDE (e.g., RStudio).
3. Follow the provided scripts to preprocess data, perform EDA, and run the models.
4. Use the included visualizations to interpret the results.

## Project Structure
- **`data/`**: Contains the dataset files (dengue features and labels).
- **`scripts/`**: R scripts for data preprocessing, modeling, and visualization.
- **`results/`**: Output from models and visualizations.

## Results
The project highlights:
- Strong predictors of dengue outbreaks, including climatic factors like temperature and humidity.
- Visual insights that emphasize the disparity in dengue cases between San Juan and Iquitos.
- Model performance metrics, with **Random Forest** and **XGBoost** achieving robust predictions.

## Contributing
Feel free to contribute to this project by submitting issues or pull requests.

## License
This project is licensed under the MIT License. See the LICENSE file for details.

## Acknowledgments
- HarvardX Data Science Professional Certificate program for the dataset and guidance.
- R community for the amazing libraries that made this project possible.

---
Thank you for exploring this repository. If you find this project helpful, please give it a ⭐ on [GitHub](https://github.com/Jencheng1/harvardx-dengue)!
