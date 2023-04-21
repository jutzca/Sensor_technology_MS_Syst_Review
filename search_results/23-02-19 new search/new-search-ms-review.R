library(dplyr)
library(data.table)

data = fread("new-search-unique-doi.csv", data.table=F)
data$DOI = tolower(data$DOI)
table(duplicated(data$DOI))

data = data[!data$`already in old search` == "yes",]

table(TW=data$`TW decision`=="", LB=data$`LB Include/exclude`=="")
table(data[(data$`TW decision` != "" & data$`LB Include/exclude` == "") | (data$`TW decision` == "" & data$`LB Include/exclude` != ""), "consensus"])

data$abstract_screening_result = ifelse(data$consensus == "exclude" | (data$`TW decision` != "" & data$`LB Include/exclude` != ""), "exclude", "keep")
table(data$abstract_screening_result)

data = data[data$abstract_screening_result == "keep",] # 100 kept

table(data$`exclusion in full text phase`)

data = data[data$`exclusion in full text phase` == "",] # 85 kept

data["First author and year"] = paste0(sub(",? .*", "", data$Authors), " et al, ", data$Year)

data = data[,c("Authors", "Year", "First author and year", "Title", "DOI")]

extracted_data = fread("../../ShinyApp/extracted_data_first_search.csv", data.table=F)
extracted_data = bind_rows(extracted_data, data)

write.csv(extracted_data, "extracted_data_new.csv", row.names = F, na="")
