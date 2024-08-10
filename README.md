## Project Overview

This project involves analyzing sales data for a company that specializes in programming books. As a data analyst, you'll use R programming to clean, transform, and analyze the data to determine which book titles are the most profitable.

## Objectives
Data Cleaning: Address missing values, standardize labels, and convert data types.
Data Analysis: Utilize R programming concepts like control flow, loops, and functions to develop an efficient analysis workflow.
Insights: Identify the most profitable book titles and provide actionable recommendations based on your analysis.
Reporting: Document your findings in a structured report to showcase your analytical and communication skills.

## Key Skills Utilized
Data Cleaning and Transformation
Control Flow and Loops in R
Data Analysis and Visualization
Report Writing and Presentation

## Set 1: Working Directory

setwd("path/to/your/directory")


# Sample vectors
book <- c("Book A", "Book B", "Book C")
review <- c("Good", "Excellent", "Fair")
state <- c("CA", "NY", "TX")
price <- c(10.99, 15.50, 7.75)

# Check the length of the price vector
length(price)


# Create the data frame
books <- data.frame(book = book,
                    review = review,
                    state = state,
                    price = price)

# Print the data frame
print(books)


# Write data frame to CSV
write.csv(books, file = "books.csv", row.names = FALSE)

# List files in the directory
list.files()


# Inspect the structure of the data
str(books)

# Initialize an index to track rows without missing values
index <- rep(TRUE, nrow(books))
print(index)

# Loop through each column to identify and remove missing data
for (col in names(books)) {
    missing_rows <- is.na(books[[col]])
    index <- index & !missing_rows
}
print(index)

# Create cleaned data frame
clean_books <- books[index, ]
print("\nCleaned data frame (missing values removed):")
print(clean_books)



# Install dplyr package if not already installed
install.packages("dplyr")

# Load the dplyr package
library(dplyr)

# Assuming clean_books is your dataset and state column needs standardization
clean_books <- clean_books %>%
    mutate(
        standardized_state = case_when(
            state %in% c("CA", "NY", "TX", "FL") ~ state,
            state == "California" ~ "CA",
            state == "New York" ~ "NY",
            state == "Texas" ~ "TX",
            state == "Florida" ~ "FL",
            TRUE ~ NA_character_  # Handle any other cases not matched
        )
    )





# Create review_num column
clean_books <- clean_books %>%
    mutate(
        review_num = case_when(
            review == "Poor" ~ 1,
            review == "Fair" ~ 2,
            review == "Good" ~ 3,
            review == "Great" ~ 4,
            review == "Excellent" ~ 5,
            TRUE ~ NA_integer_  # Handle any unexpected values
        )
    )



# Create is_high_review column
clean_books <- clean_books %>%
    mutate(
        is_high_review = review_num >= 4
    )
print(clean_books)


##Commands for Analyzing the data 
## 1. Total revenue
total_revenue <- clean_books %>%
     group_by(book) %>%
     summarise(total_revenue = sum(price, na.rm = TRUE)) %>%
     arrange(desc(total_revenue))
     
 View(total_revenue)
 
## 2. Number of purchases per book
num_purchases <- clean_books %>%
     group_by(book) %>%
    summarise(num_purchases = n()) %>%
    arrange(desc(num_purchases))
    
 View(num_purchases)

 ## 3. Merge the results for profitability Analysis  
 profitability_analysis <- merge(total_revenue, num_purchases, by = "book", all = TRUE)
 
 View(profitability_analysis)
