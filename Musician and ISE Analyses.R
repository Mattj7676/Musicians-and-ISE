packages = c("dplyr", "stringr", "tidyr", "car", "ggplot2", "lsr", "data.table")

package.check <- lapply( ## Downloads packages if needed
  packages,
  FUN = function(n) {
    if (!require(n, character.only = TRUE)) {
      install.packages(n, dependencies = TRUE)
      library(n, character.only = TRUE)
    }
  }
)

### Calls necessary libraries within installed packages
L <- c("dplyr", "stringr", "tidyr", "ggplot2", "car", "lsr", "data.table")

lapply(L, require, character.only = TRUE)

### Let's keep all data, R converts 
### data input after 50 rows of NA as NA
### MAKE SURE YOUR WORKING DIRECTORY IS SET TO THE LOCATION OF THE DATA FILE
x <- read.csv("Musician-ISE-all-data (4).csv", na.strings = c("NA"))

names(x)[names(x) == "openLabId"] <- "ID"

### This is getting rid of duplicate IDs
### Some participants did the study twice
### We are only keeping the first attempt

# Step 1: Add row number and identify new session starts (non-empty signature)
x <- x %>%
  mutate(row_index = row_number(),
         is_new_session = !is.na(signature) & signature != "")

# Step 2: Create a cumulative session number per ID
x <- x %>%
  group_by(ID) %>%
  mutate(session_num = cumsum(is_new_session)) %>%
  ungroup()

# Step 3: Keep only rows from the **first session (session_num == 1)** per ID
x <- x %>%
  filter(session_num == 1) %>%
  select(-row_index, -is_new_session, -session_num)

row_counts <- x %>%
  count(ID, name = "n_rows")

### Some participants did not finish
### We are only going to keep those that finished

high_participants <- row_counts$ID[row_counts$n_rows >= 1800]

low <- setdiff(unique(x$ID), high_participants)

length(low)

filtered_x <- x %>%
  filter(ID %in% high_participants)

x <- filtered_x

# Subset columns of interest
subset_data <- x %>%
  select(ID, 50, 51, 53, 54, 61, 62) %>%
  # Keep rows where there is data in any of these columns
  filter(
    if_any(2:5, ~ . != "") |   # Y/N columns (50,51,54,55)
      if_any(6:7, ~ !is.na(.))   # Numeric columns (61,62)
  )

###Create a subset of data of participants that answered
###yes on criteria questions or <= 2 on interest
subset_data <- subset_data %>%
  group_by(ID = subset_data[[1]]) %>%       # group by first column
  summarise(across(everything(), ~ first(na.omit(.)))) %>%
  ungroup()

subset_filtered <- subset_data %>%
  filter(
    # Condition 1: at least one "Y" in columns 2–5
    rowSums(across(2:5, ~ . == "Y")) > 0 |
      
      # Condition 2: at least one value <= 2 in columns 6–7
      rowSums(across(6:7, ~ . <= 2)) > 0
  )

remove_ids <- subset_filtered$ID
x <- x %>%
  filter(!ID %in% remove_ids)

# Category A: Y in column 2 or 3
catA_ids <- subset_filtered %>%
  filter(.[[2]] == "Y" | .[[3]] == "Y") %>%
  pull(ID)

# Category B: not already in A, but Y in column 4 or 5
catB_ids <- subset_filtered %>%
  filter(
    !ID %in% catA_ids &
      (.[[4]] == "Y" | .[[5]] == "Y")
  ) %>%
  pull(ID)

catC_ids <- subset_filtered %>%
  filter(
    !ID %in% c(catA_ids, catB_ids) &
      (.[[6]] <= 2 | .[[7]] <= 2)
  ) %>%
  pull(ID) %>%
  unique()

n_catA <- length(unique(catA_ids))
n_catB <- length(unique(catB_ids))
n_catC <- length(unique(catC_ids))

n_catA
n_catB
n_catC

# extract gmsi scores
GMSI <- x[,c(17, 67:107)]
GMSI <- na.omit(GMSI)

#Fix the reverse scoring
reverse_items <- c("AE_06", "EM_02", "MT_03", "MT_07",
                   "PA_03", "PA_05", "PA_08",
                   "SA_04", "SA_05")

GMSI[reverse_items] <- 8 - GMSI[reverse_items]

#wrote a CSV file to upload the raw scores onto the scoring app online
write.csv(GMSI, "GMSI raw scores 03:7.csv", row.names = FALSE)

#took scores from scoring app and turned them into a dataframe
#CSV file from scoring app needed here (import into environment)
GMSI_sum <- scored_data_30_ [, grepl("sum", names(scored_data_30_))]
GMSI_sum <- cbind(ID = GMSI$ID, GMSI_sum)

hist(GMSI_sum$GM_sum, xlab = "General Musicianship Score",
                      main = "Histogram of General Musicianship")

install.packages("e1071")
library(e1071)

skewness(GMSI_sum$GM_sum)

#Extract all MET data from the original dataset
MET <- x[,c(28, 36, 17)]
MET <- MET[!is.na(MET$correct), ]
MET <- MET[!grepl("The", MET$correct), ]
MET <- MET[!grepl("[0-9]", MET$response), ]

#Get rid of pratice results for each participants
# Total number of rows
n <- nrow(MET)

# Number of rows per participant (2 blocks of 158)
rows_per_participant <- 316

# How many participants are in the dataset
num_participants <- n / rows_per_participant

# Indices to remove
rows_to_remove <- c()

for (i in 0:(num_participants - 1)) {
  start <- i * rows_per_participant
  rows_to_remove <- c(rows_to_remove,
                      start + 1,    # row 1 of block 1
                      start + 2,    # row 2 of block 1
                      start + 159,  # row 1 of block 2
                      start + 160)  # row 2 of block 2
}

# Remove those rows
MET <- MET[-rows_to_remove, ]

#Fill in timed out responses with false 
#because a timed out response counts as incorrect

# Ensure 'correct' is character and standardized
MET$correct <- as.character(MET$correct)

# Number of rows
n <- nrow(MET)

# We'll process in non-overlapping windows of 3 rows
for (i in seq(1, n, by = 3)) {
  # Make sure we have a full group of 3
  if (i + 2 <= n) {
    group <- MET$correct[i:(i + 2)]
    
    # Check if all 3 values are the same and either "Same" or "Different"
    if (!any(is.na(group)) && length(unique(group)) == 1 && group[1] %in% c("Same", "Different")) {
      MET$correct[i + 1] <- "FALSE"  # Change middle one only
    }
  }
}

# Remove rows where response is NA and correct is "same" or "different"
MET <- MET[!(is.na(MET$response) & MET$correct %in% c("Same", "Different")), ]

# Number of participants
#Break down the number of participants
#by the melody and rhythm trials (52 per test = 104 total)
n_participants <- nrow(MET) / 104

# Initialize two empty data frames to create
# a dataset for each MET test
#r=rhythm
#m=melody
r <- data.frame()
m <- data.frame()

# Loop through participants
for (i in 0:(n_participants - 1)) {
  start_row <- i * 104 + 1
  end_row <- start_row + 103
  
  # Get the full block per participant per loop
  participant_block <-MET[start_row:end_row, ]
  
  # Split into halves
  #first half is rhythm scores
  #second half is melody scores
  first_half <- participant_block[1:52, ]
  second_half <- participant_block[53:104, ]
  
  # Append to new datasets
  r <- rbind(r, first_half)
  m <- rbind(m, second_half)
}

#Count the number of correct responses 
#in the rhythm (r) and melody (m) 
Rhythm <- r$correct == "true"
Melody <- m$correct == "true"

# Sum correct responses per participant (without adding to MET)
Rhythm <- aggregate(Rhythm ~ ID, data = r, FUN = sum)
Melody <- aggregate(Melody ~ ID, data = m, FUN = sum)

MET_total <- left_join(Melody, Rhythm, by = "ID")

check <- MET[!is.na(MET$correct) & MET$correct != "", ]

check$response <- trimws(check$response)         # remove extra spaces
check$response[is.na(check$response)] <- ""      # convert NA to blank

participants_with_6plus_blank <- check %>%
  group_by(ID) %>%
  summarise(
    max_blank_run = max(
      rle(response == "")$lengths[rle(response == "")$values],
      na.rm = TRUE
    )
  ) %>%
  filter(max_blank_run >= 12) %>%
  pull(ID)

check_6plus <- check %>%
  filter(ID %in% participants_with_6plus_blank)

id_list <- data.frame(unique(check_6plus$ID))

time_outs <- id_list$unique.check_6plus.ID.
MET_total <- MET_total %>%
  filter(!ID %in% time_outs)

#Combine the GMSI and MET scores
Total <- inner_join(GMSI_sum, MET_total, by = "ID")

#Get only the rows containing ISE and Span responses
mem <- x[grepl("Recall", x$sender, ignore.case = TRUE), ]

#Get columns pertaining to only memory measures
mem <- mem[,c(17,42,43,46,47)]

#Change ISE states for clarity
mem$state_stim[mem$state_stim == "Q"] <- "silent"
mem$state_stim[mem$state_stim == "a"] <- "steady"
mem$state_stim[mem$state_stim == "Random"] <- "changing"

mem <- mem %>%
  group_by(ID) %>%
  mutate(row_within_ID = row_number()) %>%
  ungroup()

# Separate ISE results (rows 1 to 63 per participant)
ISE <- mem %>%
  filter(row_within_ID <= 63) %>%
  select(-row_within_ID)

#Score ISE
#Do not need length
ISE <- ISE[,-2]

ISE_scored <- ISE %>%
  rowwise() %>%
  mutate(
    # Split strings into character vectors
    span_digits = str_split(span, "", simplify = TRUE),
    response_digits = str_split(responses, "", simplify = TRUE),
    
    # Get number of digits to compare
    n_to_compare = min(length(span_digits), length(response_digits)),
    
    # Compare digits one by one (up to shortest length)
    n_correct = sum(span_digits[1:n_to_compare] == response_digits[1:n_to_compare]),
    
    # Proportion correct = correct / length of span
    proportion_correct = n_correct / length(span_digits)
  ) %>%
  ungroup() %>%
  select(-span_digits, -response_digits)

#Remove unecessary columns
ISE_scored <- ISE_scored[,-c(5:6)]

#average scores for each participant
ISE_summary <- ISE_scored %>%
  group_by(ID) %>%
  summarise(ISE = mean(proportion_correct, na.rm = TRUE), .groups = "drop")

#Join it to our existing data
Total <- Total %>%
  inner_join(ISE_summary, by = "ID")

#Average across ISE conditions
condition <- ISE_scored %>%
  group_by(state_stim) %>%
  summarise(mean_proportion_correct = mean(proportion_correct, na.rm = TRUE), .groups = "drop")

#Average for participants in each condition
state <- ISE_scored %>%
  group_by(ID, state_stim) %>%
  summarise(mean_proportion_correct = mean(proportion_correct, na.rm = TRUE), .groups = "drop")


# Separate Span results (rows 64 to 87 per participant)
Span <- mem %>%
  filter(row_within_ID >= 64 & row_within_ID <= 87) %>%
  select(-row_within_ID)

#Delete the first three row for every participant
#those were the practice trials
Span <- Span %>%
  group_by(ID) %>%
  mutate(row_within_span = row_number()) %>%
  filter(row_within_span > 3) %>%
  ungroup() %>%
  select(-row_within_span)

Span <- Span[,-3]

#Score the span task
Span_scored <- Span %>%
  rowwise() %>%
  mutate(
    # Store vectors as list-columns
    span_digits = list(str_split(span, "", simplify = FALSE)[[1]]),
    response_digits = list(str_split(responses, "", simplify = FALSE)[[1]]),
    
    # Get the number of digits in the correct span
    n_digits = length(span_digits),
    
    # Only compare up to the length of the shorter string
    n_compare = min(length(span_digits), length(response_digits)),
    
    # Count number of matching digits in correct positions
    n_correct = sum(span_digits[1:n_compare] == response_digits[1:n_compare]),
    
    # Proportion correct is based on length of the correct span
    proportion_correct = n_correct / n_digits
  ) %>%
  ungroup() %>%
  select(-span_digits, -response_digits)

#Remove unecessary columns
Span_scored <- Span_scored[,-c(5:7)]

#Average across participants
span_participant <- Span_scored %>%
  group_by(ID) %>%
  summarise(span = mean(proportion_correct, na.rm = TRUE), .groups = "drop")

#Average across list lengths
span_length <- Span_scored %>%
  group_by(span_length) %>%
  summarise(mean = mean(proportion_correct, na.rm = TRUE), .groups = "drop")

#Average across participants for each list length
span_participant_length <- Span_scored %>%
  group_by(ID, span_length) %>%
  summarise(mean_score = mean(proportion_correct, na.rm = TRUE), .groups = "drop")

#Join with data
Total <- Total %>%
  inner_join(span_participant, by = "ID")

#Add scores for each length for each participant
span_wide <- span_participant_length %>%
  pivot_wider(
    names_from = span_length,
    values_from = mean_score,
    names_prefix = "span_"
  )

Total <- Total %>%
  left_join(span_wide, by = "ID")

# Calculate group mean (average across participants for each span length)
span_group_means <- span_participant_length %>%
  group_by(span_length) %>%
  summarise(mean_score = mean(mean_score, na.rm = TRUE), .groups = "drop")

# Plot
ggplot() +
  # Participant lines
  geom_line(
    data = span_participant_length,
    aes(x = span_length, y = mean_score, group = ID),
    color = "gray70", alpha = 0.6
  ) +
  # Bold average line
  geom_line(
    data = span_group_means,
    aes(x = span_length, y = mean_score),
    color = "black", size = 1.5
  ) +
  scale_x_continuous(breaks = 3:9) +
  labs(
    x = "List Length (Span Length)",
    y = "Proportion Correct",
    title = "Span Task Performance Across List Lengths"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Calculate the integer scores
#Step 1: Count number of fully correct trials per participant and span length
span_correct_counts <- Span_scored %>%
  mutate(correct_trial = proportion_correct == 1) %>%
  group_by(ID, span_length) %>%
  summarise(n_correct = sum(correct_trial, na.rm = TRUE), .groups = "drop")

#Step 2: Determine the highest span_length with at least 1 correct trial
integer_scores <- Span_scored %>%
  mutate(correct_trial = proportion_correct == 1) %>%
  group_by(ID, span_length) %>%
  summarise(n_correct = sum(correct_trial, na.rm = TRUE), .groups = "drop") %>%
  group_by(ID) %>%
  summarise(
    integer = if (any(n_correct >= 1)) max(span_length[n_correct >= 1]) else 0,
    .groups = "drop"
  )


Total <- Total %>%
  inner_join(integer_scores, by = "ID")

#Calculate the cumulative score
#Step 1: Summarize correct trials per participant and span length
span_summary <- Span_scored %>%
  mutate(correct = proportion_correct == 1) %>%
  group_by(ID, span_length) %>%
  summarise(n_correct = sum(correct, na.rm = TRUE), .groups = "drop")

#Step 2: Calculate base score (highest span_length with 3/3 correct)
base_scores <- span_summary %>%
  filter(n_correct == 3) %>%
  group_by(ID) %>%
  summarise(base = max(span_length), .groups = "drop")

#Step 3: Add missing IDs with base = 0
all_IDs <- unique(Span_scored$ID)
base_scores_complete <- tibble(ID = all_IDs) %>%
  left_join(base_scores, by = "ID") %>%
  mutate(base = ifelse(is.na(base), 0, base))

#Step 4: Calculate bonus (correct trials after base, or all if base=0)
bonus_scores <- span_summary %>%
  right_join(base_scores_complete, by = "ID") %>%
  filter(span_length > base | base == 0) %>%   # if base=0, all span lengths count
  group_by(ID) %>%
  summarise(bonus = sum(n_correct, na.rm = TRUE) * 0.33, .groups = "drop")

# Step 5: Combine base and bonus to get cumulative_score
cumulative_scores <- base_scores_complete %>%
  left_join(bonus_scores, by = "ID") %>%
  mutate(
    bonus = ifelse(is.na(bonus), 0, bonus),
    cumulative_score = base + bonus
  ) %>%
  select(ID, base, bonus, cumulative_score)

Total <- Total %>%
  inner_join(cumulative_scores, by = "ID")

# Compute participant-level span/ISE (take first value per participant)
scores <- Total %>%
  group_by(ID) %>%
  summarize(
    span = first(span),
    ISE  = first(ISE),
    .groups = "drop"
  ) %>%
  # Compute z-scores
  mutate(
    span_z = (span - mean(span, na.rm = TRUE)) / sd(span, na.rm = TRUE),
    ISE_z  = (ISE  - mean(ISE,  na.rm = TRUE)) / sd(ISE,  na.rm = TRUE),
    remove = span_z < -3 | ISE_z < -3
  )

# Keep IDs that pass the cutoff
keep_ids <- scores %>% filter(!remove) %>% pull(ID)

outlier <- scores %>%
  filter(remove) %>%
  pull(ID)

length(outlier)

# Filter the full long dataset
clean_total <- Total %>% filter(ID %in% keep_ids)


#compare ISE conditions
ise_condition <- ISE_scored %>%
  group_by(ID, state_stim) %>%
  summarise(mean_score = mean(proportion_correct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = state_stim,
    values_from = mean_score,
    names_prefix = "ISE_"
  )

clean_total <- clean_total %>%
  inner_join(ise_condition, by = "ID")

clean_total <- clean_total %>%
  select(-c(10,11), everything()) 

ise_long <- ise_condition %>%
  pivot_longer(
    cols = starts_with("ISE_"),
    names_to = "condition",
    names_prefix = "ISE_",
    values_to = "proportion_correct"
  )


names(clean_total)[names(clean_total) == "ISE"] <- "ISE_overall"

clean_total <- clean_total %>%
  mutate(
    ISE = ISE_silent - ISE_changing,   # silent - changing
    CSE = ISE_steady - ISE_changing,   # steady - changing
    SSE = ISE_silent - ISE_steady      # silent - steady
  )

filtered_demo <- filtered_x %>%
  group_by(ID) %>%
  summarise(
    age = first(na.omit(participant_age)),
    gender = first(na.omit(participant_gender)),
    .groups = "drop"
  )


clean_total_updated <- clean_total %>%
  left_join(filtered_demo, by = "ID")

clean_total_updated <- clean_total_updated %>%
  mutate(gender = case_when(
    gender == 1 ~ "Male",
    gender %in% c(2, 12) ~ "Female",
    gender == 3 ~ "Non-binary",
    TRUE ~ NA_character_
  ))

table(clean_total_updated$gender)
mean(clean_total_updated$age)
sd(clean_total_updated$age)


ISE_final <- subset(clean_total_updated, 
                    select = c(ID,
                               ISE_changing,
                               ISE_silent,
                               ISE_steady))

write_csv(ISE_final, "ISE scores.csv")

library(RColorBrewer)

ggplot(ise_long, aes(x = condition, y = proportion_correct, fill = condition)) +
  # Violin plot with black outline
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  
  # Jittered participant points
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6, color = "black") +
  
  # Boxplot
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", color = "black") +
  
  # Mean red dot
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  
  # ColorBrewer palette
  scale_fill_brewer(palette = "Dark2") +
  
  # Labels
  labs(
    title = "ISE Condition Scores",
    x = "Condition",
    y = "Proportion Correct"
  ) +
  
  # Minimal theme, no legend
  theme_minimal() +
  theme(legend.position = "none")

summary_stats <- data.frame(
  Mean = colMeans(ise_condition[, 2:4], na.rm = TRUE),
  SD = sapply(ise_condition[, 2:4], sd, na.rm = TRUE)
)

summary_stats

ise <- aov(proportion_correct ~ condition, data = ise_long)
summary(ise)

TukeyHSD(ise)
etaSquared(ise)

outcome <- pivot_longer(
  clean_total_updated,
  cols = c(ISE, CSE, SSE),
  names_to = "Condition",
  values_to = "Value"
)

ISE_outcome <- aov(Value ~ Condition, data = outcome)
summary(ISE_outcome)

TukeyHSD(ISE_outcome)
etaSquared(ISE_outcome)

ggplot(ISE_outcome, aes(x = factor(Condition, levels = c("SSE", "CSE", "ISE")), 
                    y = Value, 
                    fill = Condition)) +
  # Violin plot with black outline
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  
  # Jittered participant points
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6, color = "black") +
  
  # Boxplot
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white", color = "black") +
  
  # Mean red dot
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  
  # ColorBrewer palette
  scale_fill_brewer(palette = "Dark2") +
  
  # Labels
  labs(
    title = "ISE Measures",
    x = "Condition",
    y = "Scores"
  ) +
  
  # Minimal theme, no legend
  theme_minimal() +
  theme(legend.position = "none")

aggregate(
  Value ~ Condition,
  data = outcome,
  FUN = mean
)

aggregate(
  Value ~ Condition,
  data = outcome,
  FUN = sd
)

ISEmod <- lm(ISE ~ MT_sum, data = clean_total)
summary(ISEmod)

ISEmod2 <- lm(ISE ~ MT_sum + Melody + Rhythm, data = clean_total)
summary(ISEmod2)

install.packages("lm.beta")  # run once
library(lm.beta)

lm.beta(ISEmod)
lm.beta(ISEmod2)

CSEmod <- lm(CSE ~ MT_sum, data = clean_total)
summary(CSEmod)

CSEmod2 <- lm(CSE ~ MT_sum + Melody + Rhythm, data = clean_total)
summary(CSEmod2)

lm.beta(CSEmod)
lm.beta(CSEmod2)

SSEmod <- lm(SSE ~ MT_sum, data = clean_total)
summary(SSEmod)

lm.beta(SSEmod)

SSEmod2 <- lm(SSE ~ MT_sum + Melody + Rhythm, data = clean_total)
summary(SSEmod2)

lm.beta(SSEmod2)

###adding age and gender to the data
participant_info <- filtered_x %>%
  group_by(ID) %>%
  summarise(
    participant_age = first(na.omit(participant_age)),
    participant_gender = first(na.omit(participant_gender)),
    .groups = "drop"
  )

clean_total <- clean_total %>%
  left_join(participant_info, by = "ID")

clean_total <- clean_total %>%
  mutate(
    participant_gender = case_when(
      participant_gender == 1 ~ "Male",
      participant_gender == 2 ~ "Female",
      TRUE ~ "Non-binary"
    )
  )

mean(clean_total$participant_age)
sd(clean_total$participant_age)

clean_total %>%
  count(participant_gender)

write.csv(clean_total, "Musician ISE March:2026 Data.csv", row.names = FALSE)

install.packages("apaTables")
library(apaTables)
corr <- clean_total[,c(2:9, 20, 26:28)]
apa.cor.table(
  corr,
  show.conf.interval = FALSE
)

conditioncorr <- clean_total[,c(21:23)]
apa.cor.table(
  conditioncorr,
  show.conf.interval = FALSE
)

GMSI_subset <- GMSI[GMSI$ID %in% clean_total$ID, ]

hist(GMSI_subset$MT_01, xlab = "MT_01",
     main = "Histogram of General Musicianship")

# Select the columns you want to plot
install.packages("ggh4x")
library(ggh4x)

GMSI_selected <- GMSI_subset %>%
  select(MT_01, MT_02, MT_03, MT_04, MT_05, MT_06, MT_07)

# Give each histogram its own x-axis label
x_labels <- c(
  MT_01 = "Practice Years",
  MT_02 = "Practice Hours",
  MT_03 = "Compliments",
  MT_04 = "Music Theory",
  MT_05 = "Formal Training",
  MT_06 = "Instruments",
  MT_07 = "Musician Identity"
)

GMSI_selected %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Score"
  ) %>%
  ggplot(aes(x = Score)) +
  geom_bar(
    width = 0.4,
    fill = "grey75",
    color = "black"
  ) +
  facet_wrap2(
    ~ Variable,
    scales = "free_y",
    ncol = 2,
    labeller = as_labeller(x_labels),
    axes = "all"
  ) +
  scale_x_continuous(
    breaks = 1:7,
    labels = function(x) ifelse(x %in% c(2, 4, 6), x, ""),
    limits = c(0.5, 7.5)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_blank()
  ) +
  labs(y = "Frequency")

# Cohen's f^2 for each model
f2_ise <- summary(ISEmod2)$r.squared / (1 - summary(ISEmod2)$r.squared)
f2_cse <- summary(CSEmod2)$r.squared / (1 - summary(CSEmod2)$r.squared)
f2_sse <- summary(SSEmod2)$r.squared / (1 - summary(SSEmod2)$r.squared)

# Print the results
f2_ise
f2_cse
f2_sse

t.test(clean_total_updated$SSE, mu = 0)

###plotting SSE and CSE correlation
plot(clean_total_updated$CSE, clean_total_updated$SSE,
     ylab = "SSE",
     xlab = "CSE")
abline(lm(SSE~CSE, data = clean_total_updated), col = "red")
