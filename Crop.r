# Importing relevant libraries
# dplyr: for data manipulation tasks like filtering, selecting, mutating, and summarizing data.
library(dplyr)
# plotly: for creating interactive plots and visualizations.
library(plotly)
# tidyr: for data tidying tasks like reshaping data frames, gathering, and spreading columns.
library(tidyr)
# ggplot2: for creating static plots and visualizations based on the grammar of graphics.
library(ggplot2)
# reshape2: simplifies data reshaping and manipulation tasks in R. (melt function)
library(reshape2)
# stringr: used for string manipulation tasks.
library(stringr)
# lubridate: used for handling date-time objects and operations.
library(lubridate)
# gridExtra: used to make extra grids to combine the plots side by side
library(gridExtra)
# scales: transforming data scales, such as rescaling, centering, and standardizing variables, making it easier to visualize and interpret data in graphs and plots.
library(scales)


# Import the datatset
crop <- read.csv("C://Users//verma//Downloads//Crop Production data.csv")

# First 5 Rows
head(crop)

# Last 5 Rows
tail(crop)

# Dimension of Dataset
dim(crop)

# Generating Statistical Summary of Each of the columns of dataset
summary(crop)

# Datatype of Each of the Column
str(crop)

# Count the number of duplicate values
sum(duplicated(crop))                   
#There are no duplicate values in the dataset

# Count the number of null values
colSums(is.na(crop))


# We'll not remove the null values because of the project's objectives is to "find important insights highlighting key indicators and metrics that influence crop production." 
# By retaining the null values in the "Production" column, we can analyze and identify patterns or factors that may contribute to declines or fluctuations in crop production over the years.



# Check for unexpected characters
crop$Production <- as.numeric(gsub("[^0-9.-]", "", crop$Production))

# Inspect specific rows
rows_with_na <- crop[is.na(crop$Production), ]
View(rows_with_na)
rows_with_na

# Performing treatment because if we'll remove the data it will lead of inconsistency of the dataset
# We are using median imputation for missing values
crop$Production[is.na(crop$Production)] <- median(crop$Production, na.rm = TRUE)

# Verify if there are any NA values remaining
sum(is.na(crop))


# Identify the columns with blanks
# So first, identify the columns that contain empty or blank values
cols_blanks <- sapply(crop, function(x) any(x == ""))
cols_blanks <- names(cols_blanks)[cols_blanks]

cols_blanks
# So, there are no columns with blanks


# Now we are changing the column notation as numeric because it is in scientific format
#crop$Production <- format(crop$Production, scientific = FALSE)
#crop$Production <- as.numeric(crop$Production)


# Now we'll add one column name "Yield" as it gives information about production per unit area for different crops.
crop$Yield <- crop$Production / crop$Area


# Identify columns that are numerical
num_col <- sapply(crop, is.numeric)

# Identify columns that are categorical
catg_col <- names(crop)[!num_col]

# Name of the columns that are numerical and categorical
num_col <- names(crop)[num_col]
num_col
catg_col


# Check the outliers
before_plot <- ggplot(melt(crop[, num_col]), aes(y = value, x = variable)) +
  geom_boxplot(aes(fill = variable), notch = TRUE) +
  labs(title = "Before Outlier Treatment", x = "Columns", y = "Value") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)  # Displaying numeric values without scientific notation
before_plot


# Create a box plot to analyze outliers in the production column
ggplot(crop, aes(x = State_Name, y = Production, fill = Season)) +
  geom_boxplot() +
  labs(x = "State", y = "Production", title = "Production Distribution by State and Season") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma) +  # Displaying numeric values without scientific notation
  scale_fill_manual(values = c("Spring" = "lightblue", "Summer" = "lightgreen", 
                               "Autumn" = "lightyellow", "Winter" = "lightpink"))

# We are using the variables State_Name and Season to analyze outliers in the Production column because:
# State_Name: It helps us identify if there are any specific states that consistently have higher or lower production, which could indicate potential outliers or anomalies.
# Season: It allows us to explore if there are any seasonal patterns in production that might contribute to outliers. Different crops may have different growing seasons, and seasonal factors can influence production levels.


# So, we'll not remove the outliers but we'll treat it using IQR method beacuse is it in less number of columns so, we'll use the IQR methoid for the treatment.
# Second, if we'll remove the outliers it can distort the data output of the dataset but specially of "Andhra Pradesh", "Kerela" and "Tamil Nadu".

# Treating the outliers using the IQR method
# Calculate IQR
Q1 <- quantile(crop$Production, 0.25)
Q3 <- quantile(crop$Production, 0.75)
IQR <- Q3 - Q1

# Determine upper and lower bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Replace outliers with the nearest non-outlier value within bounds
crop$Production[crop$Production < lower_bound] <- lower_bound
crop$Production[crop$Production > upper_bound] <- upper_bound


# After treatment boxplot
after_plot1 <- ggplot(melt(crop[, num_col]), aes(y = value, x = variable)) +
  geom_boxplot(aes(fill = variable), notch = TRUE) +
  labs(title = "After  Phase 1 Outlier Treatment", x = "Columns", y = "Value") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)  # Displaying numeric values without scientific notation
after_plot1

# Combine plots side by side (Treatment Comparison)
grid.arrange(before_plot, after_plot1, nrow = 1)



# Identify outliers based on z-scores (e.g., z-score > 3 or < -3)
outliers <- crop %>%
  filter(scale(crop$Area) > 3)
outliers


# Box plot of 'Area'
boxplot(crop$Area, main = "Boxplot of Crop Area") +
  scale_y_continuous(labels = scales::comma)
# Histogram of 'Area' 
hist(crop$Area, main = "Histogram of Crop Area (Before Outlier Removal)") +
  scale_y_continuous(labels = scales::comma)





# Treating the outliers using the IQR method
# Calculate IQR
Q1 <- quantile(crop$Area, 0.25)
Q3 <- quantile(crop$Area, 0.75)
IQR <- Q3 - Q1

# Determine upper and lower bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Replace outliers with the nearest non-outlier value within bounds
crop$Area[crop$Area < lower_bound] <- lower_bound
crop$Area[crop$Area > upper_bound] <- upper_bound


# After treatment boxplot
after_plot2 <- ggplot(melt(crop[, num_col]), aes(y = value, x = variable)) +
  geom_boxplot(aes(fill = variable), notch = TRUE) +
  labs(title = "After Phase 2 Outlier Treatment", x = "Columns", y = "Value") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)  # Displaying numeric values without scientific notation
after_plot2

# Combine plots side by side (Treatment Comparison)
grid.arrange(after_plot1, after_plot2, nrow = 1)




# Treating the outliers using the IQR method
# Calculate IQR
Q1 <- quantile(crop$Yield, 0.25)
Q3 <- quantile(crop$Yield, 0.75)
IQR <- Q3 - Q1

# Determine upper and lower bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Replace outliers with the nearest non-outlier value within bounds
crop$Yield[crop$Yield < lower_bound] <- lower_bound
crop$Yield[crop$Yield > upper_bound] <- upper_bound


# After treatment boxplot
after_plot3 <- ggplot(melt(crop[, num_col]), aes(y = value, x = variable)) +
  geom_boxplot(aes(fill = variable), notch = TRUE) +
  labs(title = "After Phase 3 Outlier Treatment", x = "Columns", y = "Value") +
  theme_bw() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)  # Displaying numeric values without scientific notation
after_plot3

# Combine plots side by side (Treatment Comparison)
grid.arrange(before_plot, after_plot1, after_plot2, after_plot3, nrow = 2)






# Define a function to map states to regions
map_state_to_region <- function(states) {
  region_map <- list(
    "North" = c("Uttar Pradesh", "Himachal Pradesh", "Punjab", "Uttarakhand", "Haryana"),
    "South" = c("Telangana ", "Andhra Pradesh", "Telangana", "Karnataka", "Tamil Nadu", "Kerala"),
    "East" = c("Odisha", "West Bengal", "Jharkhand", "Bihar"),
    "West" = c("Rajasthan", "Gujarat", "Goa", "Maharashtra"),
    "Central" = c("Madhya Pradesh", "Chhattisgarh"),
    "Northeast" = c("Assam", "Sikkim", "Nagaland", "Manipur", "Mizoram", "Tripura", "Meghalaya", "Arunachal Pradesh"),
    "Union Territories" = c("Andaman and Nicobar Islands", "Dadra and Nagar Haveli", "Ladakh", "Jammu and Kashmir ", "Puducherry", "Lakshadweep", "Delhi", "Chandigarh")
  )
  
  regions <- character(length(states))  # Initialize regions variable
  
  # Loop through each state and find the corresponding region
  for (i in seq_along(states)) {
    state <- states[i]
    region <- NULL  # Initialize region variable
    
    # Loop through region_map to find the matching region
    for (reg in names(region_map)) {
      if (state %in% region_map[[reg]]) {
        region <- reg
        break  # Once a match is found, exit the loop
      }
    }
    
    # Handle missing states
    if (is.null(region)) {
      region <- "Other"  # If no match is found, mark it as "Other"
    }
    
    regions[i] <- region  # Store the region for this state
  }
  
  return(regions)
}

# Add a new column for region based on state
crop <- crop %>%
  mutate(Region = map_state_to_region(State_Name))




# Univariate Analysis of numerical columns

# Analysing the distribution of area for crop farming
ggplot(crop, aes(x = Area)) +
  geom_histogram(fill = 'lightblue', color = 'black' , bins = 30) +
  labs(title = "Distribution of Area for Farming", x = "Area", y = "Frequency") +
  theme_minimal()
# The distribution of area for farming shows a highly skewed pattern, with most farms having relatively small areas, and a few outliers with very large areas. This could indicate the presence of both small-scale and large-scale farming operations in the dataset.



# Analysing the Production Distribution
ggplot(crop, aes(x = Production)) +
  geom_histogram(fill = 'darkgreen', color = 'black', bins = 30) +
  labs(title = "Distribution of Crop Production ", x = "Production" , y = "Frequency") +
  theme_minimal()
# The distribution of crop production is also highly skewed, with a few areas contributing to a significant portion of the total production. This could be due to factors like favorable climate, soil conditions, irrigation facilities, or the use of advanced agricultural practices in those areas.


# Analysing the Yield Distribution
ggplot(crop, aes(x = Yield)) +
  geom_histogram(fill = 'pink', color = 'black', bins = 30) +
  labs(title = "Distribution of Yield", x = "Yield", y = "Frequency")+
  theme_minimal()
# The distribution of yield shows a more normal distribution, with most areas having moderate yields, and fewer areas with exceptionally high or low yields. This could suggest that yield is influenced by a combination of factors, rather than being heavily skewed by a single factor.






# Univariate Analysis of categorical Columns

#There are total 33 States and Union Territories. So to get complete information we're taking first top 17 and bottom 16 so that we have complete analysis of all the states.
# 1. States Analysis 
# Group by State_Name and calculate counts
state <- crop %>%
  group_by(State_Name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Get the top 17 states
top_17 <- state %>% slice(1:17)

# Get the bottom 16 states
bottom_16 <- state %>% slice((n() - 15):n())

# Plot the top 17 states
ggplot(top_17, aes(x = reorder(State_Name, count), y = count, label = count)) +
  geom_col(color = 'blue', fill = 'skyblue') +
  geom_text(vjust = -0.7, size = 3) +  # Add labels above points
  labs(title = "Top 17 States & Union Territories", x = "States & Union Territories", y = "Frequency") +
  theme_minimal()

# The top 17 states and union territories account for a significant portion of the total crop production, indicating the importance of these regions in the overall agricultural landscape of India.


# Plot the bottom 16 states
ggplot(bottom_16, aes(x = reorder(State_Name, count), y = count, label = count)) +
  geom_col(color = 'blue', fill = 'skyblue') +
  geom_text(vjust = -0.7, size = 3) +  # Add labels above points
  labs(title = "Bottom 16 States & Union Territories", x = "States & Union Territories", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()
# The bottom 16 states and union territories contribute a relatively smaller portion of the total crop production, potentially due to factors like climate, soil quality, irrigation facilities, or other regional challenges.


# 2. Season Analysis
# Group by Season and calculate counts
seasonal <- crop %>%
  group_by(Season) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Calculate percentages
seasonal <- seasonal %>%
  mutate(percentage = count / sum(count) * 100)

# Create pie chart
ggplot(seasonal, aes(x = "", y = count, fill = Season)) +
  geom_col(color = 'black', width = 1) +  # Use width to make it a pie chart
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Season", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "right")
# Seasonal Distribution:
#- The majority of crop production occurs during the Autumn season (39%), followed by Winter (23%) and Summer (27%).
#- This indicates that the climatic conditions and availability of resources during these seasons are favorable for crop cultivation in India.



# 3. Crop Analysis

# Create a frequency table for the Crop column
crop_freq <- crop %>%
  count(Crop) %>%
  arrange(desc(n))

# Extract top 10 and bottom 10 crops
top_10 <- head(crop_freq, 10)
bottom_10 <- tail(crop_freq, 10)

# Visualize top 10 crops with a bar chart
ggplot(top_10, aes(x = reorder(Crop, n), y = n, fill = Crop)) +
  geom_col(color = 'black', stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  labs(title = "Top 10 Crops by Frequency", x = "Crop", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Crop Frequency:
#- Rice, Maize, and Wheat are among the most frequently cultivated crops, suggesting high demand and production of these staple grains.


# Visualize bottom 10 crops with a bar chart
ggplot(bottom_10, aes(x = reorder(Crop, n), y = n, fill = Crop)) +
  geom_col(color = 'black', stat = "identity") +
  geom_text(aes(label = n), vjust = - 0.4) +
  labs(title = "Bottom 10 Crops by Frequency", x = "Crop", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Crop Frequency:
#- Crops like Apple, Coffee, Litchi, and Peach have relatively low frequencies, indicating potential opportunities for diversification or specialization in these niche crops.







# Bivariate Analysis 


# Analysis for Crop Year

# 1. Crop Production Over the Years
# Group by Crop_Year and summarize the production for each year
yearly_production <- crop %>%
  group_by(Crop_Year) %>%
  summarise(total_production = sum(Production))

# Convert Crop_Year to numeric for continuous scale
yearly_production$Crop_Year <- as.numeric(yearly_production$Crop_Year)

# Create the line chart with proper x-axis breaks and filled area
ggplot(yearly_production, aes(x = Crop_Year, y = total_production)) +
  geom_area(fill = "skyblue", color = "blue", alpha = 0.5) +  # Fill area under the line with skyblue color
  geom_line(color = "blue", size = 0.5) +  # Line chart with blue line and thickness
  geom_point(color = "blue", size = 2) +  # Add points with blue color and larger size
  geom_text(aes(label = total_production), vjust = -1.4, hjust = 0.5, color = "black", size = 3) +  # Display value of each year
  labs(title = "Crop Production Over the Years", x = "Crop Year", y = "Total Production") +  # Title and axis labels
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  scale_x_continuous(breaks = yearly_production$Crop_Year, labels = yearly_production$Crop_Year) +  # Specify x-axis breaks and labels
  theme_minimal()  # Minimal theme
# Crop Production Over the Years:
#- Crop production in India has experienced significant fluctuations over the years, with peaks in certain years and dips in others.
#- The highest production peak was observed around 2003, followed by a decline in subsequent years, indicating the potential impact of factors like weather conditions, policy changes, or resource availability.
#- The most recent years (2013-2015) show an upward trend, suggesting improved agricultural practices or favorable conditions.


# 2.Crop Area Over the Years
# Group by Crop_Year and summarize the area for each year
yearly_area <- crop %>%
  group_by(Crop_Year) %>%
  summarise(total_area = sum(Area))

# Convert Crop_Year to numeric for continuous scale
yearly_area$Crop_Year <- as.numeric(yearly_area$Crop_Year)

# Create the line chart with filled area
ggplot(yearly_area, aes(x = Crop_Year, y = total_area)) +
  geom_area(fill = "skyblue", color = "blue", alpha = 0.5) +  # Fill area under the line with skyblue color
  geom_line(color = "blue", size = 0.5) +  # Line chart with blue line and thickness
  geom_point(color = "blue", size = 2) +  # Add points with blue color and larger size
  geom_text(aes(label = total_area), vjust = -1.4, hjust = 0.5, color = "black", size = 3) +  # Display value of each year
  labs(title = "Crop Area Over the Years", x = "Crop Year", y = "Total Area") +  # Title and axis labels
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  scale_x_continuous(breaks = yearly_area$Crop_Year, labels = yearly_area$Crop_Year) +  # Specify x-axis breaks and labels
  theme_minimal()  # Minimal theme
# Crop Area Over the Years:
#- The total area under cultivation has also fluctuated over the years, with a peak around 2003-2004 and a gradual decline afterward.
#- The declining trend in crop area could be attributed to urbanization, industrialization, or other factors leading to land-use changes.
#- However, the recent years (2013-2015) show an increase in crop area, indicating potential efforts towards expanding agricultural land or implementing more efficient land-use practices.


# 3. Crop Yield Over the Years
# Group by Crop_Year and summarize the yield for each year
yearly_yield <- crop %>%
  group_by(Crop_Year) %>%
  summarise(total_yield = sum(Yield))

# Convert Crop_Year to numeric for continuous scale
yearly_yield$Crop_Year <- as.numeric(yearly_yield$Crop_Year)

# Create the line chart with filled area
ggplot(yearly_yield, aes(x = Crop_Year, y = total_yield)) +
  geom_area(fill = "skyblue", color = "blue", alpha = 0.5) +  # Fill area under the line with skyblue color
  geom_line(color = "blue", size = 0.5) +  # Line chart with blue line and thickness
  geom_point(color = "blue", size = 2) +  # Add points with blue color and larger size
  geom_text(aes(label = total_yield), vjust = -1.4, hjust = 0.5, color = "black", size = 3) +  # Display value of each year
  labs(title = "Crop Yield Over the Years", x = "Crop Year", y = "Total Yield") +  # Title and axis labels
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  scale_x_continuous(breaks = yearly_yield$Crop_Year, labels = yearly_yield$Crop_Year) +  # Specify x-axis breaks and labels
  theme_minimal()  # Minimal theme
# We can observe that the crop yield over the years has shown a significant increase, with some fluctuations. The yield reached its peak in the year 2003 and then declined until 2007. 
# After that, it started increasing again, indicating an overall positive trend in crop production over the years.



# 4. Region vs. Production

# Group by Region and summarize the production for each region
region_production <- crop %>%
  group_by(Region) %>%
  summarise(total_production = sum(Production)) %>%
  arrange(desc(total_production))  # Arrange regions in descending order of total production

# Calculate average production across all regions
average_production <- mean(region_production$total_production)


# Create the bar chart to compare production levels across different regions
ggplot(region_production, aes(x = total_production, y = reorder(Region, -total_production), fill = Region)) +
  geom_bar(stat = "identity", color = "black") +
  geom_vline(xintercept = average_production, color = "darkblue", linetype = "dashed", size = 0.5) +  # Add average line
  labs(title = "Region vs. Production", x = "Total Production", y = "Region") +
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Rotate y-axis labels for better readability



# 5. Region vs. Area
# Group by Region and summarize the area for each region
region_area <- crop %>%
  group_by(Region) %>%
  summarise(total_area = sum(Area)) %>%
  arrange(desc(total_area))  # Arrange regions in descending order of total area

# Create the bar chart to compare crop area across different regions
ggplot(region_area, aes(x = total_area, y = reorder(Region, -total_area), fill = Region)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Region vs. Area", x = "Total Area", y = "Region") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Rotate y-axis labels for better readability
# Both provide insights into the regional distribution of crop production and cultivated area. 
# The South region appears to have the highest production, followed by the North and Northeast regions. 
# However, the South region also has the largest cultivated area, which could be a contributing factor to its high production.



# 6. Region vs. Yield
# Group by Region and summarize the yield for each region
region_yield <- crop %>%
  group_by(Region) %>%
  summarise(total_yield = sum(Yield)) %>%
  arrange(desc(total_yield))  # Arrange regions in descending order of total yield

# Create the line chart to visualize the trend of yield over time for each region
ggplot(region_yield, aes(x = Region, y = total_yield, group = 1)) +
  geom_line(color = "blue", size = 0.5) +  # Line color and thickness
  geom_point(color = "blue", size = 2) +  # Point color and size
  geom_text(aes(label = total_yield), vjust = -0.9, hjust = 0.5, color = "black", size = 3) +  # Display value of each region
  labs(title = "Region vs. Yield", x = "Region", y = "Total Yield") +  # Title and axis labels
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# The Union Territories region exhibits the highest yield per unit area, followed by the Northeast and Central regions. This could indicate factors such as favorable climatic conditions, advanced agricultural practices, or the cultivation of high-yielding crop varieties in these regions.


# 7.Area vs. Yield
# Create a violin plot to explore the distribution of yield across different crop areas
ggplot(crop, aes(x = Area, y = Yield)) +
  geom_violin(fill = "skyblue" , color = "blue", alpha = 0.7) +  # Violin plot with skyblue fill color
  labs(title = "Area vs. Yield", x = "Area", y = "Yield") +  # Title and axis labels
  theme_minimal()  # Minimal theme
# The curve shows a distinct pattern, indicating that as the cultivated area increases, the yield initially rises, reaches a peak, and then starts declining. This could be attributed to factors such as diminishing returns, resource constraints, or environmental factors affecting larger cultivation areas.



# 8.Area vs. Production
# Create the violin plot to explore the relationship between area and production
ggplot(crop, aes(x = Area, y = Production)) +
  geom_violin(fill = "skyblue", color = "blue", alpha = 0.7) +  # Violin plot with skyblue fill and blue border
  labs(title = "Area vs. Production", x = "Area", y = "Production") +  # Title and axis labels
  theme_minimal()  # Minimal theme
# Area vs. Production Curve (Image 1):
#- The curve shows that crop production is maximized within a specific range of cultivated area, beyond which production decreases.
#- This could indicate factors like diminishing returns, resource constraints, or potential over-cultivation at larger areas.
#- Understanding the optimal area for maximizing production can help in efficient land utilization and resource allocation.



# 9. Season vs. Area: line plot to analyze the distribution of crop area across different seasons.

crop_summary <- crop %>%
  group_by(Season) %>%
  summarise(Total_Area = sum(Area)) %>%
  arrange(desc(Total_Area))

ggplot(crop_summary, aes(x = Season, y = Total_Area, color = Season)) +
  geom_line(aes(group = 1), size = 0.8) +  # Line size 1.5, set group to 1
  geom_point(size = 2) +  # Point size 3
  geom_text(aes(label = scales::comma(Total_Area)), hjust = -0.2, vjust = 0.5, size = 3) +  # Adjust text size and position
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = "Crop Area by Season", x = "Season", y = "Area") +  # Adjust labels
  theme_minimal()
# Crop Area by Season:
#- The highest crop area is during the Kharif (monsoon) season, followed by Rabi (winter) and Autumn seasons.
#- This aligns with the typical agricultural patterns in India, where major crops are cultivated during the monsoon and winter seasons.
#- Identifying the peak cultivation seasons can aid in planning logistics, labor requirements, and resource management.





# 10. Season vs. Production: line plot to analyze the distribution of production across different seasons.
crop_summary <- crop %>%
  group_by(Season) %>%
  summarise(Total_Production = sum(Production)) %>%
  arrange(desc(Total_Production))

ggplot(crop_summary, aes(x = factor(Season, levels = Season), y = Total_Production, color = Season)) +
  geom_line(aes(group = 1), size = 0.8) +  # Line size 1.5, set group to 1
  geom_point(size = 2) +  # Point size 3
  geom_text(aes(label = scales::comma(Total_Production)), vjust = -0.5, size = 3) +  # Adjust text size and position
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  labs(title = "Production by Season", x = "Season", y = "Production") +  # Adjust labels
  theme_minimal()
# Production by Season:
#- Production follows a similar trend as crop area, with the highest production during the Kharif season and the lowest during Autumn.
#- This highlights the importance of monsoon rainfall and favorable climatic conditions for crop yields.
#- Understanding seasonal variations can help in forecasting demand, storage requirements, and potential supply gaps.






# 11. State vs Production
# Top 10 States by Production
top_states <- crop %>%
  group_by(State_Name) %>%
  summarise(total_production = sum(Production)) %>%
  top_n(10, total_production) %>%
  arrange(desc(total_production))

# Bar chart for top 10 states by production
ggplot(top_states, aes(x = reorder(State_Name, total_production), y = total_production, fill = State_Name)) +
  geom_col(color = "black") +
  labs(title = "Top 10 States by Production", x = "State", y = "Total Production") +
  geom_text(aes(label = scales::comma(total_production)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Top 10 States by Production:
#- Uttar Pradesh, Andhra Pradesh, and Assam are the top three states in terms of crop production.
#- These states may have favorable climatic conditions, better agricultural practices, or more extensive cultivation areas.
#- Identifying high-producing states can guide investment decisions, infrastructure development, and targeted agricultural policies.


  

# Bottom 10 States by Production
bottom_states <- crop %>%
  group_by(State_Name) %>%
  summarise(total_production = sum(Production)) %>%
  top_n(-10, total_production) %>%
  arrange(total_production)

# Bar chart for bottom 10 states by production
ggplot(bottom_states, aes(x = reorder(State_Name, total_production), y = total_production, fill = State_Name)) +
  geom_col(color = "black") +
  labs(title = "Bottom 10 States by Production", x = "State", y = "Total Production") +
  geom_text(aes(label = scales::comma(total_production)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# Bottom 10 States by Production:
#- Tripura, Andaman and Nicobar Islands, and Chandigarh have the lowest crop production among the states/UTs.
#- These regions may face challenges like limited arable land, unfavorable climatic conditions, or resource constraints.
#- Providing targeted support, introducing advanced agricultural techniques, or focusing on alternative industries could be explored for these regions.





# 12. District vs Production

# Top 10 Districts by Production
top_districts <- crop %>%
  group_by(District_Name) %>%
  summarise(total_production = sum(Production)) %>%
  top_n(10, total_production) %>%
  arrange(desc(total_production))

# Bar chart for top 10 districts by production
ggplot(top_districts, aes(x = reorder(District_Name, total_production), y = total_production, fill = District_Name)) +
  geom_col(color = "black") +
  labs(title = "Top 10 Districts by Production", x = "District", y = "Total Production") +
  geom_text(aes(label = scales::comma(total_production)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Top 10 Districts by Production:
#1. Kornool district in Andhra Pradesh has the highest production among the districts, followed by Belgaum in Karnataka and Bellary in Karnataka.
#2. The top 3 districts account for a significant portion of the total production, highlighting their importance in the agricultural landscape.
#3. Several districts from Uttar Pradesh, like Ghaziabad and Gulbarga, are also among the top producers.
#4. The production levels vary significantly among the top 10 districts, suggesting differences in factors such as land area, climatic conditions, and agricultural practices at the district level.


# Bottom 10 Districts by Production
bottom_districts <- crop %>%
  group_by(District_Name) %>%
  summarise(total_production = sum(Production)) %>%
  top_n(-10, total_production) %>%
  arrange(total_production)

# Bar chart for bottom 10 districts by production
ggplot(bottom_districts, aes(x = reorder(District_Name, total_production), y = total_production, fill = District_Name)) +
  geom_col(color = "black") +
  labs(title = "Bottom 10 Districts by Production", x = "District", y = "Total Production") +
  geom_text(aes(label = scales::comma(total_production)), vjust = -0.5) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Bottom 10 Districts by Production:
#1. Prachint is the district with the lowest production, followed by Kargil and Mumbri.
#2. Several districts from the state of Jammu and Kashmir, like Kargil and Kishtwar, are among the bottom 10 producers.
#3. Districts like Hyderabad and Namsai also have relatively low production levels.
#4. The low production levels in these districts could be due to factors like limited arable land, challenging terrain, or a focus on other economic activities within the district.





# Multivariate Analysis


# 1. Correlation Analysis :- Area, Production, Yield
# Select numerical columns
num_col <- crop[, c("Area", "Production", "Yield")]

# Calculate correlation matrix
correlation_matrix <- cor(num_col)

# Melt correlation matrix for visualization
correlation_data <- melt(correlation_matrix)


# Create heatmap of correlation matrix with values
ggplot(correlation_data, aes(Var1, Var2, fill = value, label = round(value, 2))) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "skyblue", high = "darkblue", 
                       midpoint = 0, limits = c(-1,1), 
                       name = "Correlation") +
  geom_text(color = "white", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Numerical Columns ", y = "Numerical Columns")




# 2. Seasonal Analysis by Region :- Production, Season, State_Name Seasonal Analysis by Region:

# Seasonal Analysis by Region

# Group data by State_Name, Season, and Region and calculate total production
seasonal_analysis <- crop %>%
  group_by(State_Name, Season, Region) %>%
  summarise(total_production = sum(Production))


# Bar chart to visualize the distribution of production for each season within different states and regions
ggplot(seasonal_analysis, aes(x = State_Name, y = total_production, fill = Season)) +
  geom_col(position = "dodge", color = "black") +
  facet_wrap(~ Region, scales = "free") +
  labs(title = "Seasonal Analysis by Region", x = "State", y = "Total Production") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3. Time-Series Analysis:- Crop_Year, Production, Area

# Group data by Crop_Year and calculate the mean values for Production, Area, and Yield
yearly_data <- crop %>%
  group_by(Crop_Year) %>%
  summarise(mean_Production = mean(Production),
            mean_Area = mean(Area))

# Line plot for time-series analysis with filled area under the lines
ggplot(yearly_data, aes(x = Crop_Year)) +
  geom_line(aes(y = mean_Production, color = "Production"), size = 0.5) +
  geom_line(aes(y = mean_Area, color = "Area"), size = 0.5) +
  geom_point(aes(y = mean_Production, color = "Production"), size = 1.5) +
  geom_point(aes(y = mean_Area, color = "Area"), size = 1.5) +
  geom_ribbon(aes(ymin = mean_Production, ymax = 0, fill = "Production"), alpha = 0.3) +
  geom_ribbon(aes(ymin = mean_Area, ymax = 0, fill = "Area"), alpha = 0.3) +
  geom_text(aes(label = round(mean_Production, 2), y = mean_Production + 0.2), size = 3, vjust = -0.5, hjust = -0.2) +
  geom_text(aes(label = round(mean_Area, 2), y = mean_Area + 0.2), size = 3, vjust = -0.5, hjust = -0.2) +
  labs(title = "Time-Series Analysis of Production, Area, and Yield",
       x = "Crop Year",
       y = "Mean Value") +
  scale_color_manual(values = c("Production" = "blue", "Area" = "skyblue"),
                     labels = c("Production", "Area")) +
  scale_fill_manual(values = c("Production" = "blue", "Area" = "skyblue")) +
  theme_minimal() +
  scale_x_continuous(breaks = yearly_data$Crop_Year)



# 4. Interaction Plots :- Crop, Season, Production

# Filter out crops with zero production
crop_filtered <- crop %>%
  filter(Production > 0)

# Calculate total production for each crop
crop_summary <- crop_filtered %>%
  group_by(Crop) %>%
  summarise(total_production = sum(Production))

# Select top 10 crops by production
top_crops <- crop_summary %>%
  top_n(10, total_production)

# Select bottom crops excluding those with zero production
bottom_crops <- crop_summary %>%
  filter(total_production > 0) %>%
  top_n(-10, total_production)

# These are the crops with 0 total production, so we're leaving these and going with the bottom crop according to production leaving zero.
#Crop               total_production
#1 Apple                             0
#2 Ash Gourd                         0
#3 Beet Root                         0
#4 Ber                               0
#5 Cucumber                          0
#6 Lab-Lab                           0
#7 Litchi                            0
#8 Other Citrus Fruit                0
#9 Other Dry Fruit                   0
#10 Peach                            0
#11 Pear                             0
#12 Peas  (vegetable)                0
#13 Plums                            0
#14 Ribed Guard                      0
#15 Snak Guard                       0
#16 Water Melon                      0
#17 Yam                              0
#18 other fibres                     0

# Filter data for top and bottom crops
top_crop_data <- crop %>%
  filter(Crop %in% top_crops$Crop)

bottom_crop_data <- crop %>%
  filter(Crop %in% bottom_crops$Crop)

# Calculate mean production for each crop and season combination
top_crop_season_production <- top_crop_data %>%
  group_by(Crop, Season) %>%
  summarise(mean_production = mean(Production))

bottom_crop_season_production <- bottom_crop_data %>%
  group_by(Crop, Season) %>%
  summarise(mean_production = mean(Production))


# For top crops
ggplot(top_crop_season_production, aes(x = Season, y = mean_production, group = Crop, color = Crop)) +
  geom_line(size = 0.5) + # Line size 0.5
  geom_point(size = 1.5) + # Point size 1.5
  geom_text(aes(label = round(mean_production, 2)), size = 2.7, vjust = -0.1) + # Decrease text size to 3
  labs(title = "Top 10 Crops by Production",
       x = "Season", y = "Mean Production",
       color = "Crop") +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~ Crop, scales = "free_y")

# For bottom crops
ggplot(bottom_crop_season_production, aes(x = Season, y = mean_production, group = Crop, color = Crop)) +
  geom_line(size = 0.5) + # Line size 0.5
  geom_point(size = 1.5) + # Point size 1.5
  geom_text(aes(label = round(mean_production, 2)), size = 2.7, vjust = -0.1) + # Decrease text size to 3
  labs(title = "Bottom 10 Crops by Production",
       x = "Season", y = "Mean Production",
       color = "Crop") +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(~ Crop, scales = "free_y")








