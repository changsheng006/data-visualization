library("altair")
getwd()
setwd("/Users/chen/Desktop/Course Documents KU Leuven/kul_course_2020/data visualization/airbnb data")

airbnb <- read.csv('belgium_airbnbData.csv',header = T)
table(airbnb$city)
#subset data
data_bru <- subset(airbnb, city=='Brussels') 
data_ant <- subset(airbnb, city=='Antwerpen')
data_ghe <- subset(airbnb, city=='Ghent')
data_total <- subset(airbnb, city=='Brussels'|city=='Antwerpen'|city=='Ghent')
table(data_total$city)
#two variables: minimum_nights & neighbourhood_cleansed
#check variable
table(data_bru$neighbourhood_cleansed)
table(data_ant$neighbourhood_cleansed)
table(data_ghe$neighbourhood_cleansed)


#Brussels
chart1_bru = alt$Chart(data_bru)$
  mark_circle()$
  encode(
    x = alt$X("neighbourhood_cleansed:N",title = 'neighbourhood'),
    y = alt$Y("minimum_nights:Q",scale=alt$Scale(domain=c(0,50))),
    size='count(neighbourhood_cleansed):Q',
    color="neighbourhood_cleansed:N"
  )$
  properties(
    height=200,
    width=400,
    title='Brussels'
  )

htmlwidgets::saveWidget(vegawidget(chart1_bru),'chart1_bru.html')
vegawidget::vw_examine(chart1_bru, mode = "code")
#Antwerpen
chart1_ant = alt$Chart(data_ant)$
  mark_circle()$
  encode(
    x = alt$X("neighbourhood_cleansed:N",title = 'neighbourhood'),
    y = alt$Y("minimum_nights:Q",scale=alt$Scale(domain=c(0,50))),
    size='count(neighbourhood_cleansed):Q',
    color="neighbourhood_cleansed:N"
  )$
  properties(
    height=200,
    width=800,
    title='Antwerpen'
  )

htmlwidgets::saveWidget(vegawidget(chart1_ant),'chart1_ant.html')
vegawidget::vw_examine(chart1_ant, mode = "code")

#Ghent
chart1_ghe = alt$Chart(data_ghe)$
  mark_circle()$
  encode(
    x = alt$X("neighbourhood_cleansed:N",title = 'neighbourhood'),
    y = alt$Y("minimum_nights:Q",scale=alt$Scale(domain=c(0,50))),
    size='count(neighbourhood_cleansed):Q',
    color="neighbourhood_cleansed:N"
  )$
  properties(
    height=200,
    width=400,
    title='Ghent'
  )
htmlwidgets::saveWidget(vegawidget(chart1_ghe),'chart1_ghe.html')
vegawidget::vw_examine(chart1_ghe, mode = "code")


#average price
#subset dataset and export
mydata <- subset(airbnb, select = c(place, room_type, price,
                                    neighbourhood_cleansed,
                                    minimum_nights))
write.csv(mydata, "airbnb_small.csv")
airbnb2 <- read.csv('airbnb_small.csv',header = T)

#place, room_type, price
library(plyr)
df_price <- ddply(airbnb, c("place", "room_type"), summarise,
                mean_price=mean(price))
class(airbnb2$price)
airbnb2$price <- as.numeric(airbnb2$price)
#price should be transfromed into numeric variable
data_source = read.csv(url("https://raw.githubusercontent.com/changsheng006/data/master/airbnb_small.csv")) # original data
chart2_price = alt$Chart(data_source)$
  mark_bar(
  size=40,
  cornerRadiusTopLeft=3,
  cornerRadiusTopRight=3
  )$
  encode(
  x=alt$X('place:N',title='City'),
  y=alt$Y('mean(price):Q',title='Average Price'),
  color='room_type:O'
  )$
  properties(
    height=200,
    width=400,
    title='Average price of different room types in three cities'
  )
htmlwidgets::saveWidget(vegawidget(chart2_price),'chart2_price.html')

#interactive plot
#step 1
entities = list("Brussels","Antwerpen", "Ghent")
#step 2
chart_a = alt$Chart(data_total)$
  mark_circle()$
  encode(
    x = alt$X("neighbourhood_cleansed:N",title = 'neighbourhood'),
    y = alt$Y("minimum_nights:Q",scale=alt$Scale(domain=c(0,50))),
    size='count(neighbourhood_cleansed):Q',
    color="neighbourhood_cleansed:N",
    tooltip = c("neighbourhood_cleansed:N","minimum_nights:Q",
    'count(neighbourhood_cleansed):Q')
  )$
  properties(
    height = 300,
    width = 700
  )

# step 3
entities_select = alt$selection_single(
  fields = list("city"), 
  bind = alt$binding_radio(options = entities),
  name = "Filter"
)

# step 4
chart_filter_entities = chart_a$
  add_selection(entities_select)$
  transform_filter(
    alt$FieldOneOfPredicate(field = "city", oneOf = (list("Brussels","Antwerpen", "Ghent")))
  )$
  encode(color = alt$condition(entities_select, "city:N", alt$value("white")))


vegawidget::vw_examine(chart_filter_entities, mode = "code")
htmlwidgets::saveWidget(vegawidget(chart_filter_entities),'chart_interactive.html')

#sorting data and uploading into github
getwd()
setwd("/Users/chen/Desktop/Course Documents KU Leuven/kul_course_2020/data visualization/airbnb data")

airbnb_revise <- read.csv('belgium_airbnb_revised20200511.csv',header = T)
table(airbnb_revise$place)

mean(airbnb_revise$price)
class(airbnb_revise$price)


#subset data
bru <- subset(airbnb_revise, place=='Brussels') 
ant <- subset(airbnb_revise, place=='Antwerp')
ghe <- subset(airbnb_revise, place=='Ghent')
write.csv(bru,'airbnb_brussel.csv')
write.csv(ant,'airbnb_antwerpen.csv')
write.csv(ghe,'airbnb_ghent.csv')
