### Setting working dictionary ====
setwd("C:/Sahal/Kush's Webscraping")

### Calling in required libraries ====
require(rvest)
require(tidyverse)
require(Hmisc)

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

trial <- read_html("https://www.yelu.in/company/572819/indigo-metalloys-private-limited")

trial %>%
  html_nodes(".product_name a") %>%
  html_text()
