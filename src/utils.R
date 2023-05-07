library(stringi)
library(stringr)
library(dplyr)
library(purrr)
library(ggplot2)

get_data <- function(raw_text) {
  pattern <- "\\d{1,2}/\\d{1,2}/\\d{2,4},\\s\\d{1,2}:\\d{2}\\s-\\s"
  date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{2,4},\\s\\d{1,2}:\\d{2}"
  message_pattern <- "([\\w\\W]+?):\\s"
  user_pattern <- "([\\w\\W]+?)"
  
  #raw_user_message <- stri_split(raw_text, regex = pattern)
  user_message <- sapply(stri_split(raw_text, regex = pattern), function(x){x[2]})
  
  #raw_message <- stri_split(user_message, regex = message_pattern)
  message <- sapply(stri_split(user_message, regex = message_pattern), function(x){x[2]})
  
  user <- gsub(": ", "", stri_extract(user_message, regex = message_pattern))
  #user <- stri_extract(raw_user, regex = user_pattern)
  
  date <- stri_extract(
    raw_text, 
    regex = date_pattern
    )
  
  df_raw <- data.frame(
    "date" = date, 
    "user" = user, 
    "message" = message
    )
  return(na.omit(df_raw))
}

total_image <- function(df_raw){
  image_hindi <- sum(df_raw$message == "<?????????????????? ?????? ????????????>
                     ", na.rm = TRUE)
  image_hindi_encode <- sum(df_raw$message == "<<U+092E><U+0940><U+0921><U+093F><U+092F><U+093E> <U+0915><U+0947> <U+092C><U+093F><U+0928><U+093E>>")
  image_english <- sum(df_raw$message == "<Media omitted>", 
                       na.rm = TRUE)
  return (max(image_english, image_hindi, image_hindi_encode))
}

user_message_count <- function(data_frame) {
  row.names(data_frame[!(data_frame$user == "group_notification"), ]) <- NULL
  user_data <- data_frame %>% 
    group_by(user) %>% 
    summarise(total_message_count = n())
  options(repr.plot.width = 8, repr.plot.height =10)
  ggplot(
    user_data,
    #data_frame %>% 
     # map(~summarise(group_by(., user), total_message_count = n())),
    aes(x=user, y=total_message_count)) +
    geom_bar(stat="identity", size = 5) +
    labs(x = "Number of messages", 
         y = "User")
}


most_used_word <- function(data_frame){
  all_words = ''
  #print(data_frame$message())
  print("all_word start")
  for(msg in data_frame$message){
    words <- tolower(msg)
    all_words <- paste(all_words, words, sep = " ")
  }
  print("all_word end")

  common_word <- c('hai', 'ke', 'ho', 'ka', 'to', 'me', 'se', 'bhi', 'ko', 'nhi', 'nahi', 'hi', 'ki', 'h', 'rhe', 
                   'tha', 'ye', 'the', 'tm', 'aa', 'do', 'be', 'rha', 'tum', 'kar', 'ha', 'hm', 'kr', 'kya','toh',
                   '<media', 'omitted>', '(file', 'attached)', '<<U+092E><U+0940><U+0921><U+093F><U+092F><U+093E>',
                   '<U+092C><U+093F><U+0928><U+093E>>', '<U+0915><U+0947>', '<???????????????', '??????', '????????????>')
  list_of_word <- strsplit(all_words, " ")[[1]]
  print("list_of word end")
  count <- sort(table(list_of_word), decreasing = TRUE)
  print("counting end")
  filtered_count <- subset(count, !names(count) %in% common_word)
  print("filtered")
  
  top_filtered_count <- as.data.frame(head(filtered_count, 15))
  #(top_filtered_count)
  
  ggplot(top_filtered_count)+
    aes(x = list_of_word, y = Freq) +
    geom_bar(stat="identity", size = 5) 
  
}

most_active_time <- function(df) {
  
  #df$date <- as.POSIXct(df$date, format = "%m/%d/%y, %H:%M")
  df$hour <- format(df$date, "%H")
  df_hour_count <- df %>% 
    group_by(hour) %>% 
    summarize(messages = n()) %>% 
    arrange(hour)
  #print(head(df), 10)
  
  ggplot(df_hour_count, aes(x = hour, y = messages)) +
    geom_bar(stat = "identity") +
    xlab("Time") +
    ylab("Messages") +
    ggtitle("Messages by Hour")
}


most_active_date_of_month <- function(df){
  #df$date <- as.POSIXct(df$date, format = "%m/%d/%y, %H:%M")
  df$month_date <- format(df$date, "%d")
  df_date_count <- df %>% 
    group_by(month_date) %>% 
    summarize(messages = n()) %>% 
    arrange(month_date)
  #print(head(df), 10)
  
  ggplot(df_date_count, aes(x = month_date, y = messages)) +
    geom_bar(stat = "identity") +
    xlab("Date") +
    ylab("Messages") +
    ggtitle("Most active date of month")
}

most_active_day_of_week_original <- function(df){
  df <- df %>% mutate(day = as.Date(date))
  
  date <- df %>% 
    group_by(day) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    slice_head(n = 10)
  
  ggplot(data = date, aes(x = day, y = n)) +
    geom_bar(stat = "identity") +
    ggtitle("most active day") +
    theme_minimal()
}

most_active_day_of_week <- function(df){
  df$weekday <- as.numeric(format(as.Date(df$date), "%u"))
  weekday <- df %>% 
    group_by(weekday) %>% 
    summarise(n = n()) %>% 
    arrange(weekday) %>%
    mutate(weekday_str = case_when(
      weekday == 1 ~ "Mon",
      weekday == 2 ~ "Tue",
      weekday == 3 ~ "Wed",
      weekday == 4 ~ "Thu",
      weekday == 5 ~ "Fri",
      weekday == 6 ~ "Sat",
      weekday == 7 ~ "Sun",
      TRUE ~ "UNKNOWN"
    ))
  
  weekday$weekday_str <- factor(weekday$weekday_str, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  
  print(head(weekday, 5))
  
  ggplot(weekday, aes(x = weekday_str, y = n)) +
    geom_bar(stat = "identity") +
    ggtitle("most active day of week") +
    labs(x = "Weekday", y = "Count") +
    theme_minimal()
}


