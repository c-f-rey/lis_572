# Load checkout dataset

spl_checkouts <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_572/main/spl_checkouts_by_material_type.csv" , stringsAsFactors = FALSE)

# load plotly and tidyverse

library(tidyverse)
library(plotly)

options(scipen=999)

# For adding charts to Medium post

Sys.setenv("plotly_username"="crey")
Sys.setenv("plotly_api_key"="PNteeLvIksrNZehsEyfl")

# Total checkouts

## Filter data to summarize total materials checked out each year, then graph and upload.

total_checkouts <- spl_checkouts %>% 
  group_by(CheckoutYear) %>% 
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>% 
  filter(CheckoutYear %in% c(2005 : 2022)) #originally here to filter out 2005 and 2022 since they are not complete years, decided to keep and specify since I think they share relevant information

total_checkouts_graph <- ggplot(data = total_checkouts) +
  geom_line(aes(x = CheckoutYear,
                y = TotalCheckouts),
            color = "#0046AD") +
  geom_point(aes(x = CheckoutYear ,
                 y = TotalCheckouts , 
                 text = paste("</br> Year:", CheckoutYear ,
                               "</br> Total Checkouts:", TotalCheckouts)) ,
             color = "#0046AD") +
  labs(title = "SPL Checkouts from April 2005 to October 2022" , x = "Checkout Year" , y = "Total Checkouts")

total_checkouts_plotly <- ggplotly(total_checkouts_graph , tooltip = "text")

print(total_checkouts_plotly)

api_create(total_checkouts_plotly , filename = "total_checkouts_plotly")

# graph library budget to investigate correlation with circulation

#load budget data, graph

spl_budgets <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_572/main/spl_budget.csv" , stringsAsFactors = FALSE)

spl_budgets_graph <- ggplot(data = spl_budgets) +
  geom_line(aes(x = year , 
                y = budget_mil),
            color = "#009E73") +
  geom_point(aes(x = year , 
                y = budget_mil,
                text = paste("</br> Year:", year ,
                              "</br> Budget (in millions):", budget_mil)),
            color = "#009E73") + 
  labs(title = "SPL Budget 2009 - 2022" , x = "Year" , y = "Budget (in millions)")

spl_budgets_plotly <- ggplotly(spl_budgets_graph , tooltip = "text")

api_create(spl_budgets_plotly , filename = "spl_budgets_plotly")

# combine checkout and budget data to graph together??
# tutorial for my reference : https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

spl_checkouts_budget <- left_join(spl_budgets , total_checkouts , by = c("year" = "CheckoutYear"))

coeff <- 100000 #set coefficient to scale data into one graph

spl_checkouts_budget_graph <- ggplot(data = spl_checkouts_budget , aes(x = year)) +
  geom_line(aes(y = budget_mil) , 
            color = "#009E73") +
  geom_point(aes(y = budget_mil ,
                text = paste("</br> Year:", year ,
                             "</br> Budget (in millions):", budget_mil)) , 
            color = "#009E73") +
  geom_line(aes(y = TotalCheckouts / coeff) , #divide by 100000 to get same range for both
            color = "#0046AD") + 
  geom_point(aes(y = TotalCheckouts / coeff , 
                text = paste("</br> Year:", year ,
                             "</br> Total Checkouts:", TotalCheckouts)) , 
            color = "#0046AD") + 
  scale_y_continuous(
    name = "Budget (in millions)" , 
    sec.axis = sec_axis(~.*coeff , name = "Total Checkouts")) +
  labs(title = "SPL Total Checkouts and Annual Budget 2005 - 2022" , x = "Year") +
  theme(
    axis.title.y = element_text(color = "#009E73"),
    axis.title.y.right = element_text(color = "#0046AD"),
    legend.text = element_text()
    ) 

print(spl_checkouts_budget_graph)

spl_checkouts_budget_plotly <- ggplotly(spl_checkouts_budget_graph, tooltip = "text") %>% layout(yaxis2 = ay)

spl_checkouts_budget_plotly

# Group data by material type to see which have been checked out most since 2005, slice top 5, graph

# Color code top 5 material types to be consistent throughout visualizations
spl_colors <- c("#E69F00" , "#009E73" , "#CC79A7" , "#D55E00" , "#0072B2") # colorblind friendly palette according to http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

total_checkouts_by_type <- spl_checkouts %>% 
  group_by(MaterialType) %>% 
  filter(MaterialType != "TOTAL") %>% #remove the sum obvi
  summarize(TotalCheckouts = sum(Checkouts, na.rm = TRUE)) %>% 
  slice_max(n = 5 , order_by = TotalCheckouts) #filter for top 5 checked out materials

checkouts_by_type_barplot <- ggplot(data = total_checkouts_by_type) +
  geom_col(mapping = aes(
    x = TotalCheckouts, 
    y = reorder(MaterialType , TotalCheckouts) ,
    fill = MaterialType ,
    text = paste("</br>" , "Medium:", MaterialType ,
                 "</br>", "Total Checkouts:" , TotalCheckouts)
  )) + 
  labs(title = "SPL Top Media Total Checkouts from April 2005 to October 2022" , 
          x = "Total Checkouts" , 
          y = "Medium" , 
          fill = "Medium") +
  scale_fill_manual(values = spl_colors)

checkouts_by_type_bar_plotly <- ggplotly(checkouts_by_type_barplot , tooltip = "text")

checkouts_by_type_bar_plotly

api_create(checkouts_by_type_bar_plotly)

# Filter data to display yearly checkouts by material types for the top 5 material types, then graph and upload

checkouts_per_year_top_types <- spl_checkouts %>% 
  filter(MaterialType %in% c("AUDIOBOOK" , "BOOK", "EBOOK" , "VIDEODISC" , "SOUNDDISC")) %>% 
  filter(CheckoutYear %in% c(2005 : 2022))

checkouts_by_type_graph <- ggplot(data = checkouts_per_year_top_types) +
  geom_line(aes(x = CheckoutYear ,
                y = Checkouts ,
                color = MaterialType)) +
  geom_point(aes(x = CheckoutYear ,
                y = Checkouts ,
                color = MaterialType , 
                text = paste("</br> Year:", CheckoutYear , 
                             "</br> Medium:" , MaterialType ,
                             "</br> Total Checkouts:", Checkouts))) +
  scale_color_manual(values = spl_colors) +
  labs(title = "SPL Checkouts from April 2005 to October 2022" , x = "Checkout Year" , y = "Total Checkouts" , color = "Medium")

checkouts_by_type_plotly <- ggplotly(checkouts_by_type_graph , tooltip = "text")

api_create(checkouts_by_type_plotly , filename = "checkouts_by_type_plotly")

print(checkouts_by_type_plotly)

# further filter to focus on yearly checkouts of Books vs. eBooks vs. audiobooks, then graph and upload.

book_checkouts <- spl_checkouts %>% 
  filter(MaterialType %in% c("AUDIOBOOK" , "BOOK", "EBOOK")) %>% 
  filter(CheckoutYear %in% c(2005 : 2022))

books_checkouts_graph <- ggplot(data = book_checkouts) +
  geom_line(aes(x = CheckoutYear,
                y = Checkouts,
                color = MaterialType)) +
  geom_point(aes(x = CheckoutYear,
                 y = Checkouts,
                 color = MaterialType , 
                 text = paste("</br> Year:", CheckoutYear , 
                              "</br> Medium:" , MaterialType ,
                              "</br> Total Checkouts:", Checkouts))) +
  scale_color_manual(values = spl_colors) +
  labs(title = "SPL Book Checkouts from April 2005 to October 2022" , x = "Checkout Year" , y = "Total Checkouts" , color = "Medium")

books_checkouts_plotly <- ggplotly(books_checkouts_graph , tooltip = "text")

api_create(books_checkouts_plotly , filename = "books_checkouts_plotly")

# isolate videodisc checkouts

videodisc_checkouts <- spl_checkouts %>% 
  filter(MaterialType == "VIDEODISC") %>% 
  filter(CheckoutYear %in% c(2005 : 2022))

#upload Netflix subscriber dataset

netflix_subscribers <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_572/main/netflix_subscribers.csv" , stringsAsFactors = FALSE)

netflix_subscribers <- netflix_subscribers %>% 
  filter(year %in% 2005 :2021)

#videodisc checkouts graph

videodisc_checkouts_graph <- ggplot(data = videodisc_checkouts) +
  geom_line(aes(x = CheckoutYear,
                y = Checkouts) ,
            color = "#0072B2") +
  geom_point(aes(x = CheckoutYear,
                 y = Checkouts , 
                 text = paste("</br> Year:", CheckoutYear ,
                              "</br> Total Checkouts:", Checkouts)) , 
             color = "#0072B2") +
  labs(title = "SPL Videodisc Checkouts from April 2005 to October 2022" , x = "Checkout Year" , y = "Total Checkouts" )

videodisc_checkouts_plotly <- ggplotly(videodisc_checkouts_graph , tooltip = "text")

api_create(videodisc_checkouts_plotly , filename = "videodisc_checkouts_plotly")

#netlfix subscribers graph

netflix_subscribers_graph <- ggplot(data = netflix_subscribers) +
  geom_line(aes(x = year,
                y = subscribers) , 
            color = "#E50914" ,
            show.legend = FALSE) +
  geom_point(aes(x = year,
                 y = subscribers , 
                 text = paste("</br> Year:", year ,
                              "</br> Subscribers:", subscribers)) ,
             color = "#E50914" ,
             show.legend = FALSE) + 
  labs(title = "Netflix Global Subscribers from 2005 - 2022" , x = "Year" , y = "Subscribers" , caption = "Before 2011, these data include an unclear combination of mail-in subscribers and streaming subscribers. Note that Netflix did not launch their streaming service until 2007. From 2011 on, Netlflix only shares streaming subscription numbers. I would love to see the breakup of mail-in subscribers vs. streaming subscribers, but Netflix did not care to share.")

netflix_subscribers_plotly <- ggplotly(netflix_subscribers_graph , tooltip = "text")

api_create(netflix_subscribers_plotly , filename = "netflix_subscribers_plotly")

# combine videodisc and netflix data

videodisc_v_netflix <- left_join(netflix_subscribers , 
                                 videodisc_checkouts , 
                                 by = c("year" = "CheckoutYear"))
# plot dual y-axis graph for videodisc vs netflix subscribers

coeff_2 <- 100

videodisc_v_netflix_graph <- ggplot(data = videodisc_v_netflix , aes(x = year)) +
  geom_line(aes(y = Checkouts) , 
            color = "#0072B2") +
  geom_line(aes(y = subscribers / coeff_2) , #divide by 100 to get same range for both
            color = "#E50914") + 
  scale_y_continuous(
    name = "SPL Videodisc Checkouts" , 
    sec.axis = sec_axis(~.*coeff_2 , name = "Global Netflix Subscribers")) +
  labs(title = "SPL Videodisc Checkouts vs. Global Netflix Subscribers 2005 - 2021" , x = "Year") +
  theme(
    axis.title.y = element_text(color = "#0072B2"),
    axis.title.y.right = element_text(color = "#E50914"))

print(videodisc_v_netflix_graph)

# Isolate sound disc checkouts data, then graph and upload. 

sounddisc_checkouts <- spl_checkouts %>% 
  filter(MaterialType == "SOUNDDISC") %>% 
  filter(CheckoutYear %in% c(2005 : 2022))

sounddisc_checkouts_graph <- ggplot(data = sounddisc_checkouts) +
  geom_line(aes(x = CheckoutYear ,
                y = Checkouts) ,
            color = "#D55E00") +
  geom_point(aes(x = CheckoutYear ,
                 y = Checkouts , 
                 text = paste("</br> Year:", CheckoutYear ,
                              "</br> Total Checkouts:", Checkouts)) ,
             color = "#D55E00") +
  labs(title = "SPL Sounddisc Checkouts from April 2005 to October 2022" , x = "Checkout Year" , y = "Total Checkouts")

sounddisc_checkouts_plotly <- ggplotly(sounddisc_checkouts_graph , tooltip = "text")

api_create(sounddisc_checkouts_plotly , filename = "sounddisc_checkouts_plotly")

#uplaod Spotify subscribers data, then graph and upload.

spotify_subscribers <- read.csv("https://raw.githubusercontent.com/c-f-rey/lis_572/main/statistic_id244995_spotifys-premium-subscribers-2015-2022.csv" , stringsAsFactors = FALSE)

spotify_subscribers_graph <- ggplot(data = spotify_subscribers) +
  geom_line(aes(x = year , 
                y = subscribers) , 
            color = "#1DB954" , 
            show.legend = FALSE) +
  geom_point(aes(x = year , 
                y = subscribers , 
                text = paste("</br> Year:", year ,
                             "</br> Subscribers:", subscribers)) ,
             color = "#1DB954" ,
             show.legend = FALSE)  +
  labs(title = "Spotify Premium Global Subscribers from 2015 - 2022" , x = "Year" , y = "Subscribers")

spotify_subscribers_plotly <- ggplotly(spotify_subscribers_graph , tooltip = "text")

api_create(spotify_subscribers_plotly , filename = "spotify_subscribers_plotly")

# join spotify data to sounddisc data

sounddisc_v_spotify <- left_join(sounddisc_checkouts ,
                                 spotify_subscribers ,
                                 by = c("CheckoutYear" = "year"))

#graph dual y-axis plot of spotify data and sounddisc data, profit.

sounddisc_v_spotify_graph <- ggplot(data = sounddisc_v_spotify , aes(x = CheckoutYear)) +
  geom_line(aes(y = Checkouts) , 
            color = "#D55E00") +
  geom_line(aes(y = subscribers / coeff_2) , #divide by 100 to get same range for both
            color = "#1DB954") + 
  scale_y_continuous(
    name = "SPL Sounddisc Checkouts" , 
    sec.axis = sec_axis(~.*coeff_2 , name = "Global Spotify Premium Subscribers")) +
  labs(title = "SPL Sounddisc Checkouts vs. Global Spotify Premium Subscribers 2005 - 2021" , x = "Year") +
  theme(
    axis.title.y = element_text(color = "#D55E00"),
    axis.title.y.right = element_text(color = "#1DB954"))

print(sounddisc_v_spotify_graph)

# Group material types by physical and digital, then graph and upload.

physical_checkouts <- checkouts_per_year_top_types %>% 
  filter(CheckoutYear %in% c(2005 : 2022)) %>% 
  filter(MaterialType %in% c("BOOK" , "VIDEODISC" , "SOUNDDISC")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) 

physical_checkouts <- physical_checkouts %>% 
  add_column(MaterialType = "Physical")

digital_checkouts <- checkouts_per_year_top_types %>% 
  filter(CheckoutYear %in% c(2005 : 2022)) %>% 
  filter(MaterialType %in% c("EBOOK" , "AUDIOBOOK")) %>% 
  group_by(CheckoutYear) %>% 
  summarize(Checkouts = sum(Checkouts)) 

digital_checkouts <- digital_checkouts %>% 
  add_column(MaterialType = "Digital")

physical_v_digital_checkouts <- bind_rows(physical_checkouts , digital_checkouts)

physical_v_digital_checkouts_graph <- ggplot(data = physical_v_digital_checkouts) +
  geom_line(aes(x = CheckoutYear ,
                y = Checkouts ,
                color = MaterialType)) +
  geom_point(aes(x = CheckoutYear ,
                 y = Checkouts ,
                 color = MaterialType , 
                 text = paste("</br> Year:", CheckoutYear , 
                              "</br> Medium:" , MaterialType ,
                              "</br> Total Checkouts:", Checkouts))) +
  scale_color_manual(values = spl_colors) +
  labs(title = "SPL Physical vs. Digital Checkouts from April 2005 to October 2022" , x = "Checkout Year" , y = "Total Checkouts" , color = "Medium")

physical_v_digital_checkouts_plotly <- ggplotly(physical_v_digital_checkouts_graph , tooltip = "text")

api_create(physical_v_digital_checkouts_plotly , filename = "physical_v_digital_checkouts_plotly")
