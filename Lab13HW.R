install.packages("rvest")

library(rvest)
library(ggplot2)

url = "https://www.tripadvisor.com/Hotel_Review-g42159-d90019-Reviews-East_Lansing_Marriott_at_University_Place-East_Lansing_Ingham_County_Michigan.html"

#get first 5 reviews
reviews <- url %>%
  read_html() %>%
  html_nodes(".hotels-community-tab-common-Card__section--4r93H")
reviews=reviews[1:5]
Reviewer_ID <- reviews %>%
  html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC") %>%
  html_text()
Title <- reviews %>%
  html_node(".location-review-review-list-parts-ReviewTitle__reviewTitle--2GO9Z") %>%
  html_node("span") %>%
  html_text()
date <- reviews %>%
  html_node(".location-review-review-list-parts-EventDate__event_date--1epHa")%>%
  html_text() %>%
  sub(pattern="Date of stay: ",replacement="")
date=unlist(strsplit(date,split=" "))
date=matrix(date,ncol=2,byrow=TRUE)
colnames(date)=c("month","year")
month=date[,1]
year=date[,2]
review_text <- reviews %>%
  html_node(".location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE") %>%
  html_node("span") %>%
  html_text()
userpage <- reviews %>%
  html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC")%>%
  html_attr("href")
#obtain bubble rating
bubbles=reviews %>%
  html_node(".location-review-review-list-parts-RatingLine__bubbles--GcJvM") 

rating=numeric(5)
for (j in 1:5){
  if (length(bubbles[j]%>%html_nodes(".bubble_50"))==1){rating[j]=5
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_40"))==1){rating[j]=4
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_30"))==1){rating[j]=3
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_20"))==1){rating[j]=2
  next}
  rating[j]=1
}

data=data.frame(Reviewer_ID, Title, year, month, review_text, userpage,stringsAsFactors = FALSE)
rating = data.frame(rating)
data = cbind(data,rating)

#get other 15 reviews
for(i in 1:3){
Sys.sleep(3)
url=paste("https://www.tripadvisor.com/Hotel_Review-g42159-d90019-Reviews-or",i*5,"-Marriott_East_Lansing_at_University_Place-East_Lansing_Ingham_County_Michigan.html#REVIEWS",sep="")
reviews <- url %>%
  read_html() %>%
  html_nodes(".hotels-community-tab-common-Card__section--4r93H")
reviews=reviews[1:5]
Reviewer_ID <- reviews %>%
  html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC") %>%
  html_text()
Title <- reviews %>%
  html_node(".location-review-review-list-parts-ReviewTitle__reviewTitle--2GO9Z") %>%
  html_node("span") %>%
  html_text()
date <- reviews %>%
  html_node(".location-review-review-list-parts-EventDate__event_date--1epHa")%>%
  html_text() %>%
  sub(pattern="Date of stay: ",replacement="")
date=unlist(strsplit(date,split=" "))
date=matrix(date,ncol=2,byrow=TRUE)
colnames(date)=c("month","year")
month=date[,1]
year=date[,2]
review_text <- reviews %>%
  html_node(".location-review-review-list-parts-ExpandableReview__containerStyles--1G0AE") %>%
  html_node("span") %>%
  html_text()
userpage <- reviews %>%
  html_node(".social-member-event-MemberEventOnObjectBlock__member--35-jC")%>%
  html_attr("href")
#obtain bubble rating
bubbles=reviews %>%
  html_node(".location-review-review-list-parts-RatingLine__bubbles--GcJvM") 

rating=numeric(5)
for (j in 1:5){
  if (length(bubbles[j]%>%html_nodes(".bubble_50"))==1){rating[j]=5
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_40"))==1){rating[j]=4
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_30"))==1){rating[j]=3
  next}
  if (length(bubbles[j]%>%html_nodes(".bubble_20"))==1){rating[j]=2
  next}
  rating[j]=1
}
data=rbind(data,data.frame(Reviewer_ID, Title, year, month, review_text,userpage, rating,stringsAsFactors = FALSE))

}
View(data)
ggplot(data=data, aes(x=rating, y = count(rating))) + 
  geom_bar(aes(y = ..count.., group = 1), fill="Steelblue") + ylab("Count of rating") + 
  theme_minimal() 
