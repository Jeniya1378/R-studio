# Step 1: Read the data into R
data <- read.csv("I.csv")

# Check the first few rows of the data to understand its structure
head(data)

# Step 2: Calculate delta Ct for untreated and treated conditions
# delta_Ct = Ct(p53) - Ct(GAPDH)
data$delta_Ct_untreated <- data$untreated_p53 - data$untreated_GAPDH
data$delta_Ct_treated <- data$treated_p53 - data$treated_GAPDH

# Step 3: Calculate delta-delta Ct
# We use the untreated condition as the reference (average delta Ct of untreated)
delta_Ct_ref <- mean(data$delta_Ct_untreated)
data$delta_delta_Ct <- data$delta_Ct_treated - delta_Ct_ref

# Step 4: Convert delta-delta Ct to relative expression (using 2^-delta-delta Ct)
data$relative_expression <- 2^(-data$delta_delta_Ct)

# Step 5: Calculate average relative expression for untreated and treated groups
mean_untreated_expression <- mean(2^(-data$delta_Ct_untreated))
mean_treated_expression <- mean(data$relative_expression)

# Step 6: Plot the bar chart for relative expression
bar_values <- c(mean_untreated_expression, mean_treated_expression)
bar_labels <- c("Control", "Test")

barplot(bar_values, names.arg = bar_labels, col = c("skyblue", "salmon"),
        main = "Gene Expression Comparison (Relative Expression)", ylab = "Relative Expression")

# Step 7: Run a t-test to compare delta Ct values between untreated and treated groups
t_test_result <- t.test(data$delta_Ct_untreated, data$delta_Ct_treated)

# Display t-test result
print(t_test_result)

