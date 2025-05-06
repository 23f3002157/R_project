## Overview

This project connects R to a MySQL database to analyze a cardiovascular health dataset. It includes preprocessing, SQL querying, statistical summaries, and visualizations to explore health factors affecting heart disease.

## Prerequisites

**MySQL**: Version 8.0.21
**Install R Packages :** 

```
install.packages("pacman") 
pacman::p_load(pacman,dplyr,ggplot2,GGally,ggthemes,ggvis,httr,DBI,
               lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr, ggcorrplot)
```

## Setup Instructions

### 1. Create MySQL Schema
To create a new schema (`rdataset`) in MySQL Workbench:

1. Open **MySQL Workbench**.    
2. Click the **"Create a new schema in the connected server"** icon.
3. Name the schema `rdataset`.
4. Click **Apply**, then **Finish**.

### 2. Configure Environment

Set the MySQL root password as an environment variable `DB_PASSWORD` in your system or R session.

```r
Sys.setenv(DB_PASSWORD = "your_mysql_password")
```

## Code Overview

1. **Package Loading & Database Connection**:
	- Loading the necessary packages.
	- The connection to a MySQL database is established using `dbConnect()`, where the password is fetched from an environment variable, and the connection is made to a MySQL database (`rdataset`).
    
2. **Data Import & Preprocessing**:
	- The dataset (`cardio_data_processed.csv`) is imported using `rio::import()`.
	- Unnecessary columns like `age` and `bp_category` are dropped using `select()`. 
	- Basic summary statistics and structure of the data are displayed using `summary()` and `str()`.
	
 3. **Outlier Removal**:
	- Outliers are identified and removed based on the `bmi` column using the Interquartile Range (IQR) method.
	- A histogram is plotted to visualize the distribution of `bmi` after outlier removal.
    
4. **Data Transformation**:
	- The `id` column is updated to be sequential.
	- The `smoke`, `alco`, `active`, and `cardio` columns are converted to logical values (`TRUE/FALSE`).
	- The dataset is then loaded into the MySQL database as a new table (`cardioVascular`).
    
 5. **Database Operations**:
	- Queries are performed to fetch and manipulate data from the MySQL database. 
	- Key queries include:
	    - View sample rows.
	    - Add and update a new `bmi_category` column.
	    - Get statistics (e.g., highest BMI, blood pressure extremes).
	    - Group patients by age, smoking, cholesterol, etc.
	- The queries are then executed using `dbGetQuery()` and results are stored in data frames.
	
6. **Statistical Analysis**:
	- **Correlation Matrix**: A correlation matrix of selected numerical variables is calculated and visualized using `ggcorrplot()`.
	- **Logistic Regression**: A logistic regression model is built to predict the likelihood of cardiovascular disease (`cardio`) based on several predictors like height, weight, blood pressure, cholesterol, etc.
    
7. **Cleanup**:
	- The table is dropped from the database (if necessary) using `dbExecute()`.
	- The database connection is closed with `dbDisconnect()`.

## Key Points:

- **Data Preprocessing**: Outlier removal and type conversions ensure clean data before analysis.
- **Database Operations**: SQL queries are used extensively to interact with the MySQL database, retrieve and manipulate data, and perform specific analysis tasks.
- **Visualization**: `ggplot2` are used for creating various visualizations such as bar plots and correlation matrices.
- **Statistical Modeling**: A logistic regression model is used to predict cardiovascular disease based on different features.
- **EDA**: Various EDA steps help understand the relationship between features (e.g., smoking, cholesterol, BMI) and cardiovascular disease.
- To reset the table: uncomment the `DROP TABLE` command near the end.
- Make sure the MySQL ODBC 8.0 Unicode Driver is installed and available.