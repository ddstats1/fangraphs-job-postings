
library(tidyverse)
library(here)
library(rvest)
library(lubridate)
library(mlbgameday)

# base url: 

# will wanna have some way to make the max page dynamic (maybe keep trying
# url's until it returns some mostly-empty html object)

urls <- 1:33 %>% 
  map_chr(~str_c("https://blogs.fangraphs.com/category/job-postings/page/", .x, "/"))

pg1_html <- xml2::read_html(urls[1])

# get links to full postings
pg1_full_post_links <- pg1_html %>% 
  html_elements(".more-link") %>% 
  html_attr("href")

post1_html <- xml2::read_html(pg1_full_post_links[1])

# get title of the posting (includes team name)
title <- post1_html %>% 
  html_elements(".instagraphstitle") %>% 
  html_text2() %>% 
  str_remove_all("Job Posting: ")

# get team name
mlb_team_names <- Lahman::Teams %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(yearID == 2020) %>%
  select(name) %>% 
  # so will recognize indians or guardians in job title
  rbind(c("Cleveland Guardians")) %>% 
  pull(name)
# will save this in my database (mlb.team) at some point

# team who posted the listing
team_index <- title %>% 
  str_locate(paste(mlb_team_names, collapse = "|"))

team <- str_sub(title, team_index[, 1], team_index[, 2])

# number of h3 headings is the number of distinct jobs in this posting
n_jobs <- post1_html %>% 
  html_elements("h3") %>%
  length()

# then get the position(s) name(s)
position <- post1_html %>% 
  html_elements("h3") %>% 
  html_text2() %>% 
  str_remove_all("Position: ")

meta <- post1_html %>% 
  html_elements(".postmeta") %>% 
  html_text2() 

# find location of "\n" -- then, the rest of the string is the date
slash_n_locs_df <- meta %>% str_locate("\n")
slash_n_locs <- slash_n_locs_df[, 1]

date <- str_sub(meta, 
                slash_n_locs_df[, 1] + 1, 
                str_length(meta))

# subheadings within each job (e.g. "Reports To", "Department") are bolded, and
# under/next to each subheading is a <p> or <li>
subheadings <- post1_html %>% 
  html_elements("strong") %>% 
  html_text2()

# remove "To Apply:" subheading
subheadings <- subheadings[1:length(subheadings) - 1]

# so need a total of length(subheadings) - 1 total <p> and <ul>'s

post1_html %>% 
  html_elements("p") %>% 
  html_text2()

post1_html %>% 
  html_elements("ul") %>% 
  html_text2()
# each bullet is separated in this string by \n

# maybe pull entire post text, then grab & clean info between subheadings i 
# already found
full_post <- post1_html %>% 
  html_elements(".post") %>% 
  html_text2()

subheading_locs <- post1_html %>% 
  html_elements(".post") %>% 
  html_text2() %>% 
  str_locate_all(paste(subheadings, collapse = "|"))

subheading_locs_df <- tibble(subheading = subheadings,
                             subheading_start = subheading_locs[[1]][, 1],
                             subheading_end = subheading_locs[[1]][, 2],
                             text_start = subheading_end + 1,
                             text_end = lead(subheading_start, 1) - 1) %>% 
  # remove "To Apply:"
  slice(1:nrow(.) - 1)

text_starts <- subheading_locs_df$text_start
text_ends <- subheading_locs_df$text_end

text_list <- map2(.x = text_starts,
                  .y = text_ends,
                  .f = ~str_sub(full_post, .x, .y)) %>% 
  map(~str_split(., "\n"))

names(text_list) <- subheadings[1:length(subheadings) - 1]

text_list %>% 
  #flatten() %>% 
  # remove empty strings
  discard(~str_length(.) < 3)

# get this into a nice df somehow, where subheadings & texts are paired up


# if have \n's within one subheading, that means is a list! and will need to break up bullets
# EHHH actually, if there's multiple paragraphs that'll have a \n...hmm. treat a paragraph or bullet
# point as a "block" of text, maybe? 

full_post[1]


# Put together final dataset ----------------------------------------------

tibble(date_posted = date,
       team = team,
       listing_title = title,
       position = position) %>% View()

# maybe one row for each text block?

# will wanna concat position with department, probably? "Intern, Sports Science"


