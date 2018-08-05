library(stringr)
library(tm)
library(dplyr)
library(fuzzywuzzyR)
comp_list=c('Customer Centricity','Developing Self and Others','Driving Execution','Leading by Example','Leading Change','Motivating self and others')
comp_list1=tolower(comp_list)
comp_list1=gsub('[[:punct:] ]+',' ',comp_list1)
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
comp_list1 = stringr::str_replace_all(comp_list1, stopwords_regex, '')
test_string='developing self others customer centricity'

#### Trying Fuzzy wuzzy R

word=test_string
choices=comp_list1
init_proc=FuzzUtils$new()
PROC = init_proc$Full_process
PROC1= tolower
init_scor = FuzzMatcher$new()
SCOR = init_scor$Partial_token_sort_ratio  
init <- FuzzExtract$new()        
init$Extract(string = word, sequence_strings = choices, processor = PROC1, scorer = SCOR)
init$ExtractBests(string = word, sequence_strings = choices, processor = PROC1,scorer = SCOR, score_cutoff = 0L, limit = 2L)



s1 = "Atlanta Falcons"

s2 = "New York Jets"

init = FuzzMatcher$new()        #  initialization of FuzzMatcher class

init$Partial_token_set_ratio(string1 = s1, string2 = s2, force_ascii = TRUE, full_process = TRUE)

