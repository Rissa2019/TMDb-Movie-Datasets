library(tidyverse)

movies_data = read.csv("njson_movie_data.csv")
cast_data = read.csv("njson_cast_data.csv")


#Renaming ID column to match in both datasets
colnames(cast_data)[colnames(cast_data)=="movie_id"] <- "id"
#Removing unnecessary columns
cols_to_delete = c('X','title','crew')

cast_data = cast_data %>% 
  select(-cols_to_delete)

#Merging both datasets
data = merge(movies_data, cast_data, by = 'id')

dim(data)
str(data)
head(data)


#Removing unnecessary columns: column “X” only counts the rows
data = data %>% 
  select(-X)

#Fixing formatting of json columns
data$genres = gsub("\\[|\\]|\\'", "", data$genres)
data$cast = gsub("\\[|\\]|\\'", "", data$cast)
data$keywords = gsub("\\[|\\]|\\'", "", data$keywords)
data$production_countries = gsub("\\[|\\]|\\'", "", data$production_countries)
data$production_companies = gsub("\\[|\\]|\\'", "", data$production_companies)
data$spoken_languages = gsub("\\[|\\]|\\'", "", data$spoken_languages)

#Fixing special characters in spoken_languages by translating to english
library(textclean)
data$spoken_languages = mgsub(data$spoken_languages, c("Français", "Español", "Italiano", "Deutsch", "ελληνικά", "Türkçe", "普通话", "ภาษาไทย", "Pусский", "Íslenska", "svenska", "Română", "日本語", "हिन्दी", "Português", "العربية", "اردو", "Český", "广州话 / 廣州話", "한국어/조선말", "Norsk", "فارسی", "தமிழ்", "עִבְרִית", "Gaeilge", "suomi", "български език", "Tiếng Việt", "Magyar", "Український", "Nederlands", "Polski", "ਪੰਜਾਬੀ", "shqip", "Srpski", "Bosanski", "Hrvatski", "Eesti", "қазақ", "తెలుగు", "Dansk", "Cymraeg", "Kiswahili", "isiZulu", "پښتو", "ქართული", "বাংলা", "Slovenčina", "Català", "Bahasa indonesia"), c("French", "Spanish", "Italian", "German", "Greek", "Turkish", "Mandarin", "Thai", "Russian", "Icelandic", "Swedish", "Romanian", "Japanese", "Hindi", "Portuguese", "Arabic", "Urdu", "Czech", "Guangzhou", "Korean", "Norwegian", "Farsi", "Tamil", "Hebrew", "Irish", "Finnish", "Bulgarian", "Vietnamese", "Hungarian", "Ukrainian", "Dutch", "Polish", "Punjabi", "Albanian", "Serbian", "Bosnian", "Croatian", "Estonian", "Kazakh", "Telugu", "Danish", "Welsh", "Swahili", "Zulu", "Pashto", "Georgian", "Bengali", "Slovak", "Catalan", "Indonesian"))

#Fixing data types in columns
data$budget = as.numeric(data$budget)
data$genres = as.character(data$genres)
data$homepage = as.character(data$homepage)
data$keywords = as.character(data$keywords)
data$original_title = as.character(data$original_title)
data$overview = as.character(data$overview)
data$production_companies = as.character(data$production_companies)
data$production_countries = as.character(data$production_countries)
data$release_date = as.Date(data$release_date)
data$spoken_languages = as.character(data$spoken_languages)
data$tagline = as.character(data$tagline)
data$title = as.character(data$title)
data$vote_count = as.numeric(data$vote_count)


#count null data, include count of blank value and NA value
na_count = sapply(data, function(y) sum(length(which(as.logical(nchar(as.character(y))==0))))+sum(length(which(is.na(y)))))
na_count

#Impute missing values in the runtime column using the median (2 rows in total)
median_runtime= median(data$runtime,   na.rm = T)
mask_NAs_runtime = is.na(data$runtime)
data[mask_NAs_runtime, 'runtime'] = median_runtime
sum(is.na(data$runtime)) == 0
median(data$runtime, na.rm = T) == median_runtime

#Manually impute missing value in release date (1 row affected)
data[is.na(data$release_date),'id']
data$release_date[data$id == 380097] = "2015-03-01"

#Replace missing production company fields with "Unknown"
missing_pc = data %>% 
  select(original_title, overview, production_companies) %>% 
  filter(production_companies == '') 

data$production_companies = ifelse(data$production_companies == '',"unknown", data$production_companies)


#Imputing missing values in production countries using original language of the movie
missing_countries = data %>% 
  select(original_language,production_countries) %>% 
  filter(production_countries == '')

summary(missing_countries$original_language)

data$production_countries = ifelse(data$production_countries == '' & data$original_language == 'en', "United States of America",
                                   ifelse(data$production_countries == '' & data$original_language == 'de', "Germany",
                                          ifelse(data$production_countries == '' & data$original_language == 'es', "Spain",
                                                 ifelse(data$production_countries == '' & data$original_language == 'fr', "France",
                                                        ifelse(data$production_countries == '' & data$original_language == 'vi', "Vietnam",
                                                               ifelse(data$production_countries == '' & data$original_language == 'hi', "India",
                                                                      ifelse(data$production_countries == '' & data$original_language == 'zh', "China", data$production_countries)))))))

#Imputing missing values in spoken language using the original language of the movie
missing_spl = data %>% 
  select(original_language,spoken_languages) %>% 
  filter(spoken_languages == '')

summary(missing_spl$original_language)

data$spoken_languages = ifelse(data$spoken_languages == '' & data$original_language == 'en', "English",
                               ifelse(data$spoken_languages == '' & data$original_language == 'es', "Spanish", data$spoken_languages))

## Manually adding missing genres from the original data source where available
data$genres[data$id == 191229] = c('Drama, Horror, Thriller')
data$genres[data$id == 346081] = ''
data$genres[data$id == 371085] = c('Comedy, Drama, Mystery, Thriller, Romance')
data$genres[data$id == 48382] = ''
data$genres[data$id == 325140] = ''
data$genres[data$id == 357834] = c('Action, Thriller')
data$genres[data$id == 137955] = ''
data$genres[data$id == 206412] = ''
data$genres[data$id == 219716] = ''
data$genres[data$id == 279759] = c('Drama')
data$genres[data$id == 331493] = ''
data$genres[data$id == 294550] = c('Documentary')
data$genres[data$id == 380097] = ''
data$genres[data$id == 297100] = ''
data$genres[data$id == 325579] = ''
data$genres[data$id == 198370] = c('Comedy, Drama')
data$genres[data$id == 328307] = ''
data$genres[data$id == 281189] = ''
data$genres[data$id == 162396] = ''
data$genres[data$id == 365052] = ''
data$genres[data$id == 300327] = ''
data$genres[data$id == 320435] = ''
data$genres[data$id == 194588] = ''
data$genres[data$id == 176074] = ''
data$genres[data$id == 282128] = ''
data$genres[data$id == 126186] = c('Romance, Comedy, Drama')


#Visually examining variables
graph_budget = ggplot(data, aes(x = "", y = budget)) + 
  geom_boxplot() 
graph_popularity = ggplot(data, aes(x = "", y = popularity)) + 
  geom_boxplot() 
graph_revenue = ggplot(data, aes(x = "", y = revenue)) + 
  geom_boxplot() 
graph_runtime = ggplot(data, aes(x = "", y = runtime)) + 
  geom_boxplot() 
graph_vote_average = ggplot(data, aes(x = "", y = vote_average)) + 
  geom_boxplot() 
graph_vote_count = ggplot(data, aes(x = "", y = vote_count)) + 
  geom_boxplot() 

#Group Graphs Together
library(gridExtra)
chart=grid.arrange(graph_budget,graph_popularity,graph_revenue,graph_runtime,graph_vote_average,graph_vote_count)

#Removing outliers
summary(data$popularity)
ggplot(data,aes(x=popularity))+
  geom_histogram()
sum(data$popularity > 100)
data = data %>% 
  filter(data$popularity <= 100 | is.na(data$popularity)) ## based on the boxplot, we’re removing all titles with popularity over 100
summary(data$popularity)

## Clipping production countries (<10 observations = "Other") and converting the column to a factor variable
data$production_countries = factor(data$production_countries)
levels(data$production_countries) = c(levels(data$production_countries), "Other")
table(data$production_countries)
lvl_pcomp = levels(data$production_countries)
table_pcomp = table(data$production_countries)
for (i in lvl_pcomp) {
  if (table_pcomp[i] < 10){
    data$production_countries[data$production_countries == i] = "Other"
  }
} 
data$production_countries = droplevels(data$production_countries)
summary(data$production_countries)

#Creating a subset with numerical value and look for correlation
numeric_data=select_if(data, is.numeric)
numeric_data = numeric_data %>% 
  select(-id) #removing ID column

str(numeric_data)

library(tidyr); library(dplyr); library(ggplot2)
corMatrix = as.data.frame(cor(numeric_data[,-7]))
corMatrix$var1 = rownames(corMatrix)
corMatrix %>%
  gather(key=var2,value=r,1:6)%>%
  ggplot(aes(x=var1,y=var2,fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradient2(low = 'red',high='green',mid = 'white')+
  theme(axis.text.x=element_text(angle=90))


# Feature engineering
##Homepage and tagline have missing data, homepage missing 3091, and tagline missing 844
# For homepage, converting the column to a binary variable 
data$homepage = as.numeric(!data$homepage=="")
summary(data$homepage)
data$homepage = factor(data$homepage, labels=c('no homepage','with homepage'))
# Creating a “with tagline” column and converting it to a binary variable
data$with_tagline = as.numeric(!data$tagline=="")
summary(data$with_tagline)
data$with_tagline = factor(data$with_tagline, labels=c('no tagline','with tagline'))

# Creating a “with overview” variable and converting it to a binary variable
data$with_overview = as.numeric(!data$overview=="")
data$with_overview = factor(data$with_overview, labels=c('no overview','with overview'))

# Creating a variable for the number of keywords
kw_split = strsplit(data$keywords, ",")
num_kw = c()
for (i in 1:length(kw_split)){
  num_kw[i] = length(kw_split[[i]])
}
data$num_keywords = num_kw

# Creating a binary variable for movie containing one or more top paid actors/actresses
top_paid_actors = c("Mark Wahlberg", "Dwayne Johnson", "Vin Diesel", "Adam Sandler", "Jackie Chan", "Robert Downey, Jr.", "Tom Cruise", "Shah Rukh Khan", "Salman Khan", "Akshay Kumar", "Chris Hemsworth", "Tom Hanks", "Samuel L. Jackson", "Ryan Gosling", "Emma Stone", "Jennifer Aniston", "Jennifer Lawrence", "Ryan Reynolds", "Matt Damon", "Jeremy Renner", "Chris Evans", "Melissa McCarthy", "Chris Pratt", "Mila Kunis", "Emma Watson", "Charlize Theron", "Mark Ruffalo", "Cate Blanchett", "Julia Roberts", "Amy Adams")
data$top_paid_actor = as.numeric(sapply(data$cast, function(x) any(sapply(top_paid_actors, str_detect, string = x))))
data$top_paid_actor = factor(data$top_paid_actor,labels=c("no_tp_actor","with_tp_actor"))

#Saving clean data
write.csv(data, 'clean_movie_data_final.csv',row.names = F)

clean_data = read.csv("clean_movie_data_final.csv")

