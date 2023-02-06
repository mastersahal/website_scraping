### Calling in required libraries ====
require(rvest)
require(tidyverse)
require(Hmisc)
require(plyr)

#'Creating a data of 165 webpages with the data to be scraped
#' for the total companies
page1 <- "https://www.yelu.in/category/ironsteel"


websites <- data.frame(page = rep(page1, times = 165))

websites <- websites %>% mutate(slash = "/",
                    number = as.character(row_number()))

websites$pages <- paste(websites$page,websites$slash,websites$number)

websites[1,4] <- websites[1,1] 

website <- as.list(websites$pages)
website <- as.list(gsub(" ", "", website))

#collecting results in a list of 165 elements====
rslt <- list()


# Running a for loop to collect the required data====
for (i in seq_along(website)) {
  
rslt[[i]] <-  read_html(website[[i]])
  

rslt[[i]]$company_name <- rslt[[i]] %>% 
    html_nodes("h4 a") %>%
    html_text() 
  
  rslt[[i]]$address <- rslt[[i]] %>%
    html_nodes(".address") %>%
    html_text()
  
  rslt[[i]]$company_site <- rslt[[i]] %>%
    html_nodes("h4 a") %>%
    html_attr("href")

}

# Bringing out required data from the result list for rows binding====
rslt2 <- replicate(165, list())

for (i in 1:165){
  
  rslt2[[i]]$company <- data.frame(rslt[[i]][3])
  rslt2[[i]]$address <- data.frame(rslt[[i]][4])
  rslt2[[i]]$website <- data.frame(rslt[[i]][5])
  
}

### Binding all data together ==== 
data <- ldply(rslt2, data.frame) %>%
  mutate(yelu = "https://www.yelu.in",
         site = gsub(" ","",paste(yelu,company_site))) %>%
  select(-3,-4)


### Saving data to local drive ====
write.csv(data,"data.csv")




### Getting individual data from the companies

raw <- read.csv('data.csv')[,-1]

pages <- as.list(raw[,3])

rslt <- list()

# Running a for loop to collect the required data====
for (i in seq_along(pages)) {
  
  rslt[[i]] <-  read_html(pages[[i]])
  
}


### Getting data from the individual websites====
rslt2 <- replicate(4950, list())

for (i in 1 : 4950) { 
  
  rslt2[[i]]$company_name <- rslt[[i]] %>%
    html_nodes("#company_name") %>%
    html_text()
  
  rslt2[[i]]$location <- rslt[[i]] %>%
    html_nodes(".location") %>%
    html_text()
  
  rslt2[[i]]$phone <- rslt[[i]] %>%
    html_nodes(".phone") %>%
    html_text()
  
  rslt2[[i]]$mob_phone <- rslt[[i]] %>%
    html_nodes(".info:nth-child(6)") %>%
    html_text()
  
  rslt2[[i]]$website <- rslt[[i]] %>%
    html_nodes(".weblinks") %>%
    html_text()
  
  rslt2[[i]]$establishment_year <- rslt[[i]] %>%
    html_nodes(".r_3px+ .info") %>%
    html_text()
  
  rslt2[[i]]$employee_number <- rslt[[i]] %>%
    html_nodes(".info:nth-child(11)") %>%
    html_text()
  
  rslt2[[i]]$registration_code <- rslt[[i]] %>%
    html_nodes(".info:nth-child(12)") %>%
    html_text()
  
  rslt2[[i]]$vat_registration <- rslt[[i]] %>%
    html_nodes(".info:nth-child(13)") %>%
    html_text()
  
  rslt2[[i]]$manager <- rslt[[i]] %>%
    html_nodes(".info:nth-child(14)") %>%
    html_text()
  
  rslt2[[i]]$product_name <- rslt[[i]] %>%
    html_nodes(".product_name a") %>%
    html_text()
  
  }

### Binding all data together ==== 

# Bringing out required data from the result list for rows binding====
rslt3 <- replicate(4950, list())

for (i in 1:4950){
  
  rslt3[[i]]$company_name <- data.frame(rslt[[i]][1])
  rslt3[[i]]$location <- data.frame(rslt[[i]][2])
  rslt3[[i]]$phone <- data.frame(rslt[[i]][3])
  rslt3[[i]]$mob_phone <- data.frame(rslt[[i]][4])
  rslt3[[i]]$website <- data.frame(rslt[[i]][5])
  rslt3[[i]]$establishment_year <- data.frame(rslt[[i]][6])
  rslt3[[i]]$employee_number <- data.frame(rslt[[i]][7])
  rslt3[[i]]$registration_number <- data.frame(rslt[[i]][8])
  rslt3[[i]]$manager <- data.frame(rslt[[i]][9])
  rslt3[[i]]$vat_registration <- data.frame(rslt[[i]][10])
  rslt3[[i]]$product_name <- data.frame(rslt[[i]][11])
  
   
}


company_name
location
phone
mob_phone
website
establishment_year
employee_number
registration_number
manager
vat_registration
product_name

data <- ldply(rslt2, data.frame) %>%
  mutate(yelu = "https://www.yelu.in",
         site = gsub(" ","",paste(yelu,company_site))) %>%
  select(-3,-4)


