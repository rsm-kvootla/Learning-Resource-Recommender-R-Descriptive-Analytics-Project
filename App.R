# Load required libraries
library(shiny)
library(visNetwork)
library(dplyr)
library(DT)
library(shinyjs)
library(plotly)

# Define the conversion function
convert_to_minutes <- function(duration) {
  parts <- unlist(strsplit(duration, ':'))
  parts <- as.numeric(parts)
  if (length(parts) == 3) {
    return(parts[[1]] * 60 + parts[[2]] + parts[[3]] / 60)
  } else if (length(parts) == 2) {
    return(parts[[1]] + parts[[2]] / 60)
  } else {
    return(parts[[1]]) # Assuming single entry is in minutes
  }
}

# Load your CSV data (replace these paths with your actual file paths)
setwd("/Users/apple/Desktop/Visualization/gp app")
data <- read.csv("Data Gathering.csv")
job_skills_data <- read.csv("google_job_cat_skills_final.csv")
courses_data <- read.csv("courses_data.csv")
user_data <- read.csv("Data Gathering.csv")
job_skill <- read.csv("google_job_cat_skills_final.csv")
job_skill[,1] <- NULL
skill_video <- read.csv("courses_data.csv")

# Define UI with sub-tabs and a checkbox for skills
ui <- navbarPage("TechJ.SB", id = "navbar",
                 tabPanel("User Profile",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("education", "Select Your Education Level:", unique(data$Education)),
                              selectInput("specialisation", "Select Your Tech Specialisation:", unique(data$Specialisation)),
                              selectInput("category", "Select a Job Category you are most interested in:", unique(job_skills_data$Category)),
                              checkboxGroupInput("skills_proficient", "Programming Skills You Are Proficient In:",
                                                 choices = c("JavaScript/Java", "R", "Python", "C/C#/C++", 
                                                             "HTML/CSS", "SQL", "Typescript", "Bash/shell", 
                                                             "Go", "Ruby", "Swift"))
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Competitiveness", plotOutput("pieChart"), plotOutput("barChart1"), plotOutput("barChart2")),
                                tabPanel("Skills Needed", 
                                         actionButton("show_all", "Display All Job Categories", style = "background-color: #3498db; color: white;"),
                                         visNetworkOutput("networkChart")),
                                tabPanel("Timeline", 
                                         selectInput("daily_free_time", "Select Your Daily Free Time (in minutes):", 
                                                     choices = c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90", "90-105")),
                                         dateInput("start_date", "Select Start Date:", value = Sys.Date()),
                                         numericInput("study_days_per_week", "Days per week you plan to study:", 
                                                      value = 7, min = 1, max = 7),
                                         textOutput("required_days"),
                                         textOutput("estimated_end_date")
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Video Recommendations",
                          fluidRow(
                            column(12, actionButton("toggleSidebar", "Video Duration Analysis")),
                            column(12, DTOutput("videoTable"))
                          )
                 )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the selected job category
  selected_category <- reactiveVal(unique(job_skills_data$Category)[1])
  
  # Add this at the beginning of your server function
  show_all_categories <- reactiveVal(FALSE)
  
  # Observe changes in the dropdown and update the selected category
  observe({
    selected_category(input$category)
    # Reset show_all_categories to FALSE when the category is changed
    show_all_categories(FALSE)
  })
  
  #__________________________________________USER PROFILE TAB__________________________________________#  
  # ==== Competitiveness ====
  # Pie Chart rendering logic
  output$pieChart <- renderPlot({
    req(selected_category())
    job_percentage <- job_skills_data %>%
      group_by(Category) %>%
      summarize(Count = n()) %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      ungroup() %>%
      arrange(desc(Percentage))
    
    selected_category_exists <- selected_category() %in% job_percentage$Category
    
    labels <- ifelse(job_percentage$Category == selected_category() & selected_category_exists,
                     paste(job_percentage$Category, sprintf("(%.2f%%)", job_percentage$Percentage[job_percentage$Category == selected_category()])),
                     as.character(job_percentage$Category))
    colors <- ifelse(job_percentage$Category == selected_category(), "lightblue", "gray")
    
    # Draw the pie chart
    if (nrow(job_percentage) > 0) {
      pie(job_percentage$Percentage, labels = labels, col = colors, 
          main = "Job Category Percentage")
    }
  })
  
  # bar Chart1 rendering logic
  output$barChart1 <- renderPlot({
    # Link job category to users' Dream.Tech.job
    unique(user_data$dream.Tech.job)
    unique(job_skill$Category)
    job_skill1 <- job_skill
    # Replace "IT & Data Management" and "Technical Infrastructure" with "IT Services"
    job_skill1$Category <- ifelse(job_skill1$Category == "IT & Data Management" | job_skill1$Category == "Technical Infrastructure", 
                                  "IT Services", 
                                  job_skill1$Category)
    # Replace specified categories with "Business Analytics"
    job_skill1$Category <- ifelse(job_skill1$Category %in% c("Product & Customer Support", 
                                                             "Developer Relations", 
                                                             "Program Management", 
                                                             "Technical Solutions", 
                                                             "Technical Writing"), 
                                  "Business Analytics", 
                                  job_skill1$Category)
    # Replace "Hardware Engineering" with "Emerging Technologies"
    job_skill1$Category <- ifelse(job_skill1$Category == "Hardware Engineering", 
                                  "Emerging Technologies", 
                                  job_skill1$Category)
    # Replace specified categories with "Telecommunications and Networks"
    job_skill1$Category <- ifelse(job_skill1$Category %in% c("Network Engineering", 
                                                             "Data Center & Network"), 
                                  "Telecommunications and Networks", 
                                  job_skill1$Category)
    # Replace specified categories with "Software and Applications"
    job_skill1$Category <- ifelse(job_skill1$Category %in% c("Software Engineering", 
                                                             "User Experience & Design"), 
                                  "Software and Applications", 
                                  job_skill1$Category)
    unique(job_skill1$Category)
    
    # users' Dream.Tech.job + skills they need
    job_skill2 <- job_skill1
    # Check for duplicates within each category
    job_skill2_cleaned <- job_skill2[!duplicated(job_skill2[c("Category", "combined_qual1")]), ]
    # 'job_skill2_cleaned' will now have unique 'combined_qual1' values for each category
    
    # skills users already have
    user_data1 <- user_data
    # Specify the range of columns (columns 8 to 18)
    columns_to_replace <- 8:18
    # Replace "no" with 1 and "yes" with 0 in specified columns
    user_data1[, columns_to_replace] <- lapply(user_data1[, columns_to_replace], function(x) ifelse(x == "yes", 1, ifelse(x == "no", 0, x)))
    # Specify the range of columns (columns 8 to 18)
    columns_to_convert <- 8:18
    # Convert columns to numeric
    user_data1[, columns_to_convert] <- lapply(user_data1[, columns_to_convert], as.numeric)
    # Specify the range of columns (columns 8 to 18)
    columns_to_sum <- 8:18
    # Calculate the sum for each category
    sum_by_category <- aggregate(user_data1[, columns_to_sum], by = list(Category = user_data1$dream.Tech.job), sum)
    # Print the result
    print(sum_by_category)
    # Calculate the number of rows for each category
    category_counts <- table(user_data1$dream.Tech.job)
    # Convert to data frame
    category_counts_df <- data.frame(Category = names(category_counts), Count = as.numeric(category_counts))
    # Print the result
    print(category_counts_df)
    
    # Merge the data frames based on the common column "Category"
    merged_data <- merge(category_counts_df, job_skill2_cleaned, by = "Category")
    # Calculate the total number of people for each skill
    skill_counts <- aggregate(Count ~ combined_qual1, data = merged_data, sum)
    # Print the result
    print(skill_counts)
    
    # Step 1
    step_1_rows <- user_data1[user_data1$dream.Tech.job %in% c("IT Services", "Software and Applications", "Business Analytics"), ]
    step_1_sums <- c(sum(step_1_rows[, 8]), sum(step_1_rows[, 10]), sum(step_1_rows[, 11]), sum(step_1_rows[, 16]))
    # Step 2
    step_2_rows <- user_data1[user_data1$dream.Tech.job %in% c("IT Services", "Software and Applications"), ]
    step_2_sum_12 <- sum(step_2_rows[, 12])
    # Step 3
    step_3_rows <- user_data1[user_data1$dream.Tech.job == "Business Analytics", ]
    step_3_sum_13 <- sum(step_3_rows[, 13])
    # Step 4
    step_4_rows <- user_data1[user_data1$dream.Tech.job %in% c("Emerging Technologies", "Software and Applications", "IT Services"), ]
    step_4_sum_15 <- sum(step_4_rows[, 15])
    # Step 5
    step_5_rows <- user_data1[user_data1$dream.Tech.job == "Business Analytics", ]
    step_5_sum_17 <- sum(step_5_rows[, 17])
    # Step 6
    result_df <- data.frame(
      Step_1_Sum_8th_Column = step_1_sums[1],
      Step_1_Sum_10th_Column = step_1_sums[2],
      Step_1_Sum_11th_Column = step_1_sums[3],
      Step_1_Sum_16th_Column = step_1_sums[4],
      Step_2_Sum_12th_Column = step_2_sum_12,
      Step_3_Sum_13th_Column = step_3_sum_13,
      Step_4_Sum_15th_Column = step_4_sum_15,
      Step_5_Sum_17th_Column = step_5_sum_17
    )
    
    result_df1 <- result_df
    # Rename columns in result_df1
    colnames(result_df1)[1] <- colnames(user_data1)[8]
    colnames(result_df1)[2] <- colnames(user_data1)[10]
    colnames(result_df1)[3] <- colnames(user_data1)[11]
    colnames(result_df1)[4] <- colnames(user_data1)[16]
    colnames(result_df1)[5] <- colnames(user_data1)[12]
    colnames(result_df1)[6] <- colnames(user_data1)[13]
    colnames(result_df1)[7] <- colnames(user_data1)[15]
    colnames(result_df1)[8] <- colnames(user_data1)[17]
    
    result_df2 <- result_df1
    
    # Select columns 8 to 18 and calculate their sum
    sum_result <- colSums(result_df2[, 1:8])
    # Convert the result to a data frame
    sum_df <- data.frame(Sum = sum_result)
    # Extract row names into a new column named "Skill"
    sum_df$Skill <- colnames(result_df2)
    # Reset row names of the data frame
    rownames(sum_df) <- NULL
    # Reorder columns to move "Sum" to the right of "Skill"
    sum_df1 <- sum_df[, c("Skill", "Sum", setdiff(names(sum_df), c("Skill", "Sum")))]
    sum_df2 <- sum_df1
    # Remove "proficiency." and "Proficiency." from the "Skill" column
    sum_df2$Skill <- gsub("proficiency\\.|Proficiency\\.", "", sum_df1$Skill)
    print(sum_df2)
    sum_df3 <- sum_df2
    # Check if the Skill column contains exactly one "."
    contains_one_dot <- grepl("^\\w+\\.\\w+$", sum_df3$Skill)
    # Create a new data frame to store the modified rows
    new_rows <- data.frame(
      Skill = unlist(strsplit(sum_df3$Skill[contains_one_dot], "\\.")),
      Sum = rep(sum_df3$Sum[contains_one_dot] / 2, sapply(strsplit(sum_df3$Skill[contains_one_dot], "\\."), length))
    )
    # Combine the modified rows with the original rows
    sum_df3 <- rbind(sum_df3[!contains_one_dot, ], new_rows)
    # Reset row names (optional)
    rownames(sum_df3) <- NULL
    sum_df4 <- sum_df3
    # Define the row to split and the new rows
    row_to_split <- "C.C..C.."
    new_skills <- c("C", "C#", "C++")
    division_factor <- 3  # Divide the Sum by 3
    # Find the row to split
    split_row <- sum_df4[sum_df4$Skill == row_to_split,]
    # Create new rows with modified skills and Sum values
    new_rows <- data.frame(
      Skill = new_skills,
      Sum = rep(split_row$Sum / division_factor, length(new_skills))
    )
    # Remove the original row to split from the data frame
    sum_df4 <- sum_df4[sum_df4$Skill != row_to_split,]
    # Combine the modified rows with the original rows
    sum_df5 <- rbind(sum_df4, new_rows)
    # Reset row names (optional)
    rownames(sum_df5) <- NULL
    sum_df6 <- sum_df5
    # Convert the "Sum" column to integers
    sum_df6$Sum <- as.integer(sum_df6$Sum)
    sum_df7 <- sum_df6
    sum_df7$Skill <- sub("shell", "shell programming", sum_df7$Skill)
    
    skill_counts1 <- skill_counts
    library(dplyr)
    # Join the two data frames based on the condition
    skill_counts2 <- skill_counts1 %>%
      left_join(sum_df7, by = c("combined_qual1" = "Skill")) %>%
      mutate(Result = Count - Sum)
    # The skill_counts2 now contains the "Result" column which is the Count - Sum as per your requirement.
    
    skill_counts3 <- skill_counts2
    # Replace 'Count' with 'Result' where 'Result' is not NA
    skill_counts3$Count <- ifelse(!is.na(skill_counts3$Result), skill_counts3$Result, skill_counts3$Count)
    
    skill_counts4 <- skill_counts3
    skill_counts4 <- subset(skill_counts4, select = -c(Sum, Result))
    
    # Merge the data frames based on 'Category' and 'combined_qual1'
    skill_video1 <- merge(skill_video, skill_counts4, by.x = "Category", by.y = "combined_qual1", all.x = TRUE)
    
    # Extract the columns "Category" and "Count"
    video_views <- skill_video1[c("Title", "Count")]
    # Rename the data frame to "video_views"
    names(video_views) <- c("Video", "Views")
    
    # Aggregate 'Views' by 'Video'
    video_views1 <- aggregate(Views ~ Video, data = video_views, sum)
    # Remove duplicated rows in case there are any remaining
    video_views1 <- unique(video_views1)
    video_views2 <- video_views1
    # Delete rows 197 and 299
    video_views2 <- video_views2[-c(197, 299, 324, 536), , drop = FALSE]
    
    # Load the ggplot2 package
    library(ggplot2)
    # Sort the data by Views in descending order and select the top 20 videos
    top_13_videos <- head(video_views2[order(-video_views2$Views),], 13)
    # Create a horizontal bar chart with labels and adjusted x-axis label size
    ggplot(top_13_videos, aes(y = reorder(Video, Views), x = Views, label = Views)) +
      geom_bar(stat = "identity", fill = "cornflowerblue") +
      geom_text(size = 3, hjust = 1.2) +
      labs(title = "Top 13 Most Popular Videos",
           y = "Video",
           x = "Views") +
      theme(axis.text.y = element_text(size = rel(0.7)))
  })
  
  # bar Chart2 rendering logic
  output$barChart2 <- renderPlot({
    library(ggplot2)
    # Calculate the number of rows for each category
    category_counts <- table(user_data$dream.Tech.job)
    # Convert to data frame
    category_counts_df <- data.frame(Category = names(category_counts), Count = as.numeric(category_counts))
    # Create a bar plot with count annotations and custom x-axis labels
    ggplot(category_counts_df, aes(x = Category, y = Count, fill = Category)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Count), vjust = -0.5) +  # Add count annotations above the bars
      labs(title = "Popularity of Dream Technical Jobs",
           x = "Job Categories",
           y = "Number of People") +
      theme_minimal() +
      scale_x_discrete(labels = c("BA", "ET", "IT", "SA", "TN"))
  })
  
  
  # ==== Skills Needed ====
  # Network Chart rendering logic
  output$networkChart <- renderVisNetwork({
    req(selected_category())
    if (show_all_categories()) {
      nodes <- data.frame(
        id = unique(c(job_skills_data$Category, job_skills_data$combined_qual1)),
        label = unique(c(job_skills_data$Category, job_skills_data$combined_qual1)),
        group = ifelse(unique(c(job_skills_data$Category, job_skills_data$combined_qual1)) %in% job_skills_data$Category, "job", "skill")
      )
      
      edges <- data.frame(
        from = job_skills_data$Category,
        to = job_skills_data$combined_qual1
      )
    } else {
      subset_data <- job_skills_data[job_skills_data$Category == selected_category(), ]
      
      nodes <- data.frame(
        id = unique(c(subset_data$Category, subset_data$combined_qual1)),
        label = unique(c(subset_data$Category, subset_data$combined_qual1)),
        group = ifelse(unique(c(subset_data$Category, subset_data$combined_qual1)) %in% subset_data$Category, "job", "skill")
      )
      
      edges <- subset_data %>%
        select(from = Category, to = combined_qual1)
    }
    
    visNetwork(nodes, edges) %>%
      visNodes(color = list(background = c("job" = "yellow", "skill" = "lightblue")[nodes$group]), shape = "dot") %>%
      visEdges(color = list(color = "black")) %>%
      visLayout(randomSeed = 123) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
  })
  
  # "Show All Categories" button logic
  observeEvent(input$show_all, {
    show_all_categories(TRUE)
  })
  
  
  # ==== Timeline ====
  # Add a reactive expression to get the related courses for the selected job category
  related_courses <- reactive({
    req(input$category)
    skills <- job_skills_data %>%
      filter(Category == input$category) %>%
      pull(combined_qual1)
    courses_data %>%
      filter(Category %in% skills)
  })
  
  # Calculate the required days
  output$required_days <- renderText({
    req(input$daily_free_time, related_courses(), input$start_date)
    
    # Calculate the average daily free time in minutes
    daily_minutes_range <- as.numeric(unlist(strsplit(input$daily_free_time, "-")))
    daily_minutes <- mean(daily_minutes_range)
    
    # Get the user's non-proficient skills
    user_skills_proficient <- input$skills_proficient
    
    # Define a list of skills to exclude for each option
    skills_to_exclude <- list(
      "JavaScript/Java" = c("JavaScript", "Java"),
      "C/C#/C++" = c("C", "C#", "C++"),
      "HTML/CSS" = c("HTML", "CSS"),
      "Bash/shell" = c("Bash", "shell"),
      "R" = "R",
      "Python" = "Python",
      "SQL" = "SQL",
      "Typescript" = "Typescript",
      "Go" = "Go",
      "Ruby" = "Ruby",
      "Swift" = "Swift"
    )
    
    # Initialize an empty vector to store skills to exclude
    skills_to_exclude_final <- character(0)
    
    # Loop through selected options and add the corresponding skills to exclude
    for (option in user_skills_proficient) {
      if (option %in% names(skills_to_exclude)) {
        skills_to_exclude_final <- c(skills_to_exclude_final, skills_to_exclude[[option]])
      }
    }
    
    # Filter the related courses to exclude the ones where the user is proficient
    filtered_courses <- related_courses() %>%
      filter(!(Category %in% skills_to_exclude_final))
    
    # Calculate total time required to watch the videos
    total_time <- sum(sapply(filtered_courses$Readable_Duration, convert_to_minutes))
    
    # Calculation for days required based on study days per week
    days_required <- ceiling(total_time / daily_minutes)
    
    # Adjust the calculation for weeks required
    full_weeks_required <- days_required %/% 7
    remaining_days <- days_required %% 7
    fraction_of_week <- remaining_days / input$study_days_per_week
    weeks_required <- full_weeks_required + fraction_of_week
    
    # Round the weeks to two decimal places
    weeks_required <- round(weeks_required, 2)
    
    # Calculate the end date based on the selected start date
    start_date <- as.Date(input$start_date)
    end_date <- start_date + days_required - 1  # Adjust for inclusive counting
    
    # Create a message that includes both the days and the equivalent in weeks
    if (weeks_required > 0) {
      message <- paste("You will need approximately", days_required, "days to complete all videos.",
                       "This is roughly", weeks_required, "weeks.",
                       "Your estimated end date is:", format(end_date, "%Y-%m-%d"))
    } else {
      message <- paste("You will need approximately", days_required, "days to complete all videos.",
                       "This is less than a week.",
                       "Your estimated end date is:", format(end_date, "%Y-%m-%d"))
    }
  })
  
  #__________________________________________VIDEO RECOMMENDATIONS TAB__________________________________________#
  # Video Recommendations logic
  output$videoTable <- renderDT({
    req(selected_category())
    
    # Get the user's non-proficient skills
    user_skills_proficient <- input$skills_proficient
    all_skills <- c("JavaScript/Java", "R", "Python", "C/C#/C++", "HTML/CSS", "SQL", "Typescript", "Bash/shell", "Go", "Ruby", "Swift")
    
    # Define a list of skills to exclude for each option
    skills_to_exclude <- list(
      "JavaScript/Java" = c("JavaScript", "Java"),
      "C/C#/C++" = c("C", "C#", "C++"),
      "HTML/CSS" = c("HTML", "CSS"),
      "Bash/shell" = c("Bash", "shell"),
      "R" = "R",
      "Python" = "Python",
      "SQL" = "SQL",
      "Typescript" = "Typescript",
      "Go" = "Go",
      "Ruby" = "Ruby",
      "Swift" = "Swift"
    )
    
    # Initialize an empty vector to store skills to exclude
    skills_to_exclude_final <- character(0)
    
    # Loop through selected options and add the corresponding skills to exclude
    for (option in user_skills_proficient) {
      if (option %in% names(skills_to_exclude)) {
        skills_to_exclude_final <- c(skills_to_exclude_final, skills_to_exclude[[option]])
      }
    }
    
    # Filter the job_skills_data for the selected category to get the related skills
    related_skills <- job_skills_data %>%
      filter(Category == selected_category()) %>%
      pull(combined_qual1)
    
    # Filter out proficient skills from the related_skills
    related_skills <- setdiff(related_skills, skills_to_exclude_final)
    
    # Filter the courses_data to only include videos related to the skills the user is not proficient in and within the selected category
    recommended_videos <- courses_data %>%
      filter(Category %in% related_skills) %>%
      select(Category, Title, Readable_Duration, Video_URL) %>%
      mutate(Video_URL = sprintf('<a href="%s" target="_blank">Link</a>', Video_URL))
    
    datatable(recommended_videos, escape = FALSE, options = list(pageLength = 5))
  })
  
  # Modal dialog for plotly graph
  observeEvent(input$toggleSidebar, {
    related_skills <- job_skills_data %>%
      filter(Category == selected_category()) %>%
      pull(combined_qual1)
    
    filtered_videos <- courses_data %>%
      filter(Category %in% related_skills) %>%
      mutate(TotalTime = sapply(Readable_Duration, convert_to_minutes)) %>%
      group_by(Category) %>%
      summarise(TotalTime = sum(TotalTime, na.rm = TRUE)) %>%
      arrange(desc(TotalTime))
    
    output$modalPlot <- renderPlotly({
      p <- ggplot(filtered_videos, aes(x = reorder(Category, -TotalTime), y = TotalTime, text = sprintf('Total Time: %.2f minutes', TotalTime))) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(title = "Total Estimated Watch Time by Skill", x = "", y = "Total Time (minutes)") +
        coord_flip()
      
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    showModal(modalDialog(
      title = "Video Duration Analysis",
      plotlyOutput("modalPlot", width = "100%"),
      size = "l"
    ))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)