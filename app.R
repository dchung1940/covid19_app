library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(usmap)
library(ggplot2)
library(shinyWidgets)
library(scales)
library(rnaturalearthdata)
library(rnaturalearth)
library(rworldmap)
library(Cairo)
library(RColorBrewer)
library(leaflet.extras)
library(dplyr)
library(readr)
library(ggmap)
library(purrr)
library(geosphere)
library(vembedr)
library(ECharts2Shiny)
library(treemap)

NYTimes_US_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
NYTimes_US_States_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
NYTimes_US_Counties_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
COVID_Tracking_Project_US_Historical_Data <- read_csv("https://covidtracking.com/data/download/national-history.csv")
#importing international COVID data for comparison with domestic
WHO_COVID_19_Situation_Report_Data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv")
#up-to-date global covid19 data from WHO
WHO_Global_Historical_Data = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
#estimates of mask usage by county from a nationwide survey
NYTIMES_US_mask_use_by_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")

#sorting the data to see new cases and new deaths in the data
Champaign_data <- NYTimes_US_Counties_Historical_Data %>% 
  filter(county == "Champaign" & state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, new_cases = cases - lag(cases,default = 0), new_deaths = deaths - lag(deaths,default = 0)) %>%
  select(-c(state,fips))

#Check the COVID situation in Illinois, including new cases, new deaths and the increasing percentage
Illinois_data <- NYTimes_US_States_Historical_Data %>% 
  filter(state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, cases_increase_percentage = 0, deaths_increase_percentage = 0,
         new_cases = cases - lag(cases,default = 0), 
         new_deaths = deaths - lag(deaths,default = 0),
         cases_increase_percentage = (cases - lag(cases,default = 0))/lag(cases,default = 0)*100,
         deaths_increase_percentage = (deaths - lag(deaths,default = 0))/lag(deaths,default = 0)*100) %>%
  select(-c(fips))



ui <- navbarPage("UIUC Covid-19 App", 
                 collapsible = TRUE,
                 theme = shinytheme("flatly"),
                 
#HOME
                 tabPanel("Home", tags$head(tags$style(HTML(".tab-content {margin: 20px}"))),
                          
                          #Adding favicon for web browser
                          tags$head(tags$link(rel="virus icon",href="https://sn56.scholastic.com/content/dam/classroom-magazines/sn56/issues/2019-20/031620/coronavirus/16-SN56-20200316-VirusOutbreak-PO-2.png")),
                          
                          #Adding an image
                          img(src = "https://p10cdn4static.sharpschool.com/UserFiles/Servers/Server_415374/Image/News/2020/09/covid19-hero-image.jpg"),
          
                          
                          checkboxGroupInput("Symptoms", h3("Symptoms of COVID-19"),
                                             
                                             choices = list("Fever or chills",
                                                            "Cough", 
                                                            "Shortness of breath or difficulty breathing",
                                                            "Fatigue",
                                                            "Muscle or body aches",
                                                            "Headache",
                                                            "New loss of taste or smell",
                                                            "Sore throat",
                                                            "Congestion or runny nose",
                                                            "Nausea or vomiting",
                                                            "Diarrhea")
                          ),
                          htmlOutput("symptom_choice"),
                          #tags$a(href = "https://landing.google.com/screener/covid19?source=google", "Do a self-assessment here"),
                          actionButton(inputId = "selfAssess", label = "Do a self-assessment here", icon = icon("th"), 
                                       onclick ="window.open('https://landing.google.com/screener/covid19?source=google', '_blank')"),
                          h4("or"),
                          #tags$a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "or find your testing site here"),
                          actionButton(inputId = "findTest", label = "Find your testing site here", icon = icon("th"),
                                       onclick ="window.open('https://my.castlighthealth.com/corona-virus-testing-sites/', '_blank')"),
                          h4("or"),
                          #tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention", "or distinguish whether flu or COVID here"),
                          actionButton(inputId = "distinguishFlu", label = "Distinguish whether flu or COVID-19 here", icon = icon("th"),
                                       onclick ="window.open('https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention', '_blank')"),
                          h4("CDC Information about Symptoms:"),
                          tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/social/covid19-symptoms-fb.png", height = 400, width=600),
                          # Add Quarantine vs Isolation Infographic as a Visual Aid for the site
                          
                          h3("CDC Information about Quarantine vs Isolation:"),
                          tags$img(src="https://www.co.lincoln.or.us/sites/default/files/styles/gallery500/public/imageattachments/hhs/page/7501/covid-19-quarantine-vs-isolation.png?itok=yDWeXaEg", height= 600, width=600),
                          
                          #What to do if you have tested positive for COVID-19
                          h3("What to do if you have tested positive for COVID-19"),
                          img(src = "https://uchealth-wp-uploads.s3.amazonaws.com/wp-content/uploads/sites/6/2020/03/16112257/10Things.jpg", height = 750, width = 500),
                          tags$br(),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html", "Click here to read the CDC guidelines on isolation and how to keep those around you safe."),
                          
                          #Steps to follow if tested positive for COVID-19
                          h3("A Short Video about some QnA of Self-Quarantine"),
                          embed_youtube('7PQxmK8IvTU',
                                        width = NULL,
                                        height = 300,
                                        ratio = c("16by9", "4by3"),
                                        frameborder = 0,
                                        allowfullscreen = TRUE),
                          
                          #Learn about hidden-carriers of Covid-19
                          h3("A video acknowldeging possible hidden-carriers of Covid-19"),
                          embed_youtube('ti3lNFtd5pk',
                                        width = NULL,
                                        height = 300,
                                        ratio = c("16by9", "4by3"),
                                        frameborder = 0,
                                        allowfullscreen = TRUE),
                          
                          
                          h3("I wear a mask because..."), embed_youtube('KpXZkChOXwI', width = NULL, height = 400, ratio =c("16by9","4by3"),frameborder =0, allowfullscreen = TRUE),
                          #Other credible inter-governmental agencies sources
                          tags$div(
                            h3("Other Inter-governmental Agencies' Sources"), 
                            "This list contains internet URLs for various official inter-governmental agencies' websites",
                            tags$li(a(href = "https://covid19.who.int", "WHO Coronavirus Disease (COVID-19) Overview.")),
                            tags$li(a(href = "https://covid19.who.int/table", "WHO Coronavirus Disease (COVID-19) Data Table.")),
                            tags$li(a(href = "https://www.who.int/health-topics/coronavirus#tab=tab_1", "Coronavirus disease (COVID-19).")),
                            tags$li(a(href = "https://www.who.int/csr/don/en/", "Disease Outbreak News (DONs).")),
                            tags$li(a(href = "https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases", "COVID-19 Data, News, and Reports from the European Center for Disease Prevention and Control")),
                            tags$li(a(href = "https://www.worldometers.info/coronavirus/", "COVID-19 Data, News, and Reports from the WorldoMeters.")),
                            tags$li(a(href = "https://covid.ourworldindata.org/data/ecdc/total_cases.csv", "COVID-19 Data, News, and Reports from OurWorldinData"))),
                          
                          #Local Champaign-Urbana Resources
                          tags$div(
                            h3("In need of help in the Champaign-Urbana Area?"), 
                            "These URL's will redirect you to resources/services that are locally obtainable in the CU Area",
                            tags$li(a(href = "https://www.champaigncounty.org/covid-19-resources", "List of Champaign based organizations assisting with matters related to business to health and various others.")),
                            tags$li(a(href = "https://ccrpc.org/covid-19-resources/", "List of Champaign resources that range from offerring assistances to families or immigrants or more.")),
                            tags$li(a(href = "https://www.urbanaillinois.us/COVID-19", "Community Support During the Pandemic & Emergency Situations")),
                            tags$li(a(href = "https://champaignil.gov/cuhelp/", "Champaign-Urbana resource assistance with food or living situation matters and more")),
                          ),
                          #Highly-rated nonprofits providing relief to those impacted by the pandemic
                          tags$div(
                            h3("Looking to Help?"), 
                            "This list contains internet URLs of highly rated nonprofit organizations providing relief and recovery to communities impacted by the pandemic",
                            tags$li(a(href = "https://www.hearttoheart.org/", "Heart to Heart International: Improving global health through humanitarian initiatives that connect people and resources to a world in need.")),
                            tags$li(a(href = "https://icfdn.org/", "International Community Foundation: Seeks to increase charitable giving and volunteerism to benefit communities and nonprofit organizations.")),
                            tags$li(a(href = "https://www.healthcorps.org/", "HealthCorps: Strengthening communities with the most innovative approaches to health and wellness.")),
                            tags$li(a(href = "https://www.crisisaid.org/", "Crisis Aid International: Bringing necessary foods, materials, and medicines to people in times of crisis.")),
                            tags$li(a(href = "https://www.savethechildren.org/us/what-we-do/emergency-response/coronavirus-outbreak", "Save The Children: Providing health, education, and protection to children growing up during a deadly pandemic.")),
                          ),
                          
                          tags$div(
                            h3("FAQs"),
                            "This list contains internet URLS of Frequently Asked Questions regarding COVID-19",
                            tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/faq.html", "Click here to see CDC's Frequently Asked Questions and Answers regarding COVID-19")),
                          )
                 ),
#UIUC
                tabPanel("UIUC", tags$head(tags$style(HTML(".tab-content {margin: 20px;}"))),
                         
                         #add UIUC covid-19 dashboard
                         tags$h3("Covid Data in Champaign"),
                         
                         fluidRow(
                           align = "center",
                           column(3,
                                  dateRangeInput(inputId = "date_range_covid",
                                                 label = "Date Range",
                                                 start = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                 end =  as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                 min = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                 max = as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                 separator = 'to')),
                           
                           column(3,
                                  selectInput(inputId = "Graph_Type", 
                                              label = "Types", 
                                              c("New Cases", "Total Cases","New Deaths","Total Deaths")))
                         ),
                         
                         plotOutput("lol"),
                         
                         h1("Closest Testing Site Near You"),
                         h4("Enter the longitude and latitude of your address using the link below."),
                         uiOutput("tab"),
                         numericInput(inputId = "long", label = "Enter Longitude", value = 0),
                         numericInput(inputId = "lat", label = "Enter the Latitude", value = 0),
                         textOutput("testing_site"),
                         
                         # Add UIUC Testing Sites on Map
                         h1("UIUC Testing Sites on Google Maps", align = "left"),
                         
                         tags$a(href = "https://www.google.com/maps/d/embed?mid=1Bb6Q24_7pzcZOtrz_ZalaUiUdtxf_pOl&hl=en","UIUC Testing Sites on Google Maps"),
                         
                         
                         #add the Growth Rate of Cases plot in Champaign County
                         h1("Daily Growth Rate of Cases in Champaign County"),
                         sliderInput(
                           "champaign_growth_date",
                           "Select the range of date from the first case appeared",
                           min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                             NYTimes_US_Counties_Historical_Data$county == "Champaign",
                             NYTimes_US_Counties_Historical_Data$state == "Illinois"
                           ),]$date[1], "%Y-%m-%d"),
                           max = as.Date(
                             tail(NYTimes_US_Counties_Historical_Data[which(
                               NYTimes_US_Counties_Historical_Data$county == "Champaign",
                               NYTimes_US_Counties_Historical_Data$state == "Illinois"
                             ),]$date, 1),
                             "%Y-%m-%d"
                           ),
                           value = as.Date("2020-04-22", "%Y-%m-%d")
                         ),
                         
                         plotOutput("champaign_growth"),
                         
                         # add precise search for cases around Champaign
                         h1("Search for the number of cases"),
                         dateInput(
                           "champaignCasesSearch_Input",
                           "Please select a date",
                           value = as.Date(NYTimes_US_Counties_Historical_Data[which(
                             NYTimes_US_Counties_Historical_Data$county == "Champaign",
                             NYTimes_US_Counties_Historical_Data$state == "Illinois"
                           ), ]$date[1], "%Y-%m-%d"),
                           min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                             NYTimes_US_Counties_Historical_Data$county == "Champaign",
                             NYTimes_US_Counties_Historical_Data$state == "Illinois"
                           ), ]$date[1], "%Y-%m-%d"),
                           max = as.Date(
                             tail(NYTimes_US_Counties_Historical_Data[which(
                               NYTimes_US_Counties_Historical_Data$county == "Champaign",
                               NYTimes_US_Counties_Historical_Data$state == "Illinois"
                             ), ]$date, 1),
                             "%Y-%m-%d"
                           )
                         ),
                         
                         textOutput("champaign_cases_search"),
                         
                         tags$br(),
                         tags$a(href = "https://covid19.illinois.edu/updates/", "Click here for the latest UIUC Covid-19 updates."),
                         tags$br(),
                         tags$a(href = "https://covid19.illinois.edu/", "Click here for the UIUC Covid-19 main website."),
                         tags$br(),
                         tags$a(href = "https://mckinley.illinois.edu/covid-19", "Click here for UIUC McKinley Health Center Covid-19 information."),
                         tags$br(),
                         tags$a(href = "https://housing.illinois.edu/News/Coronavirus", "Click here for UIUC University Housing Coronavirus news."),
                         tags$br(),
                         tags$a(href = "https://grad.illinois.edu/covid-19/updates", "Click here for UIUC Graduate College Covid-19 updates."),
                         tags$br(),
                         h3("A Short News Report about UIUC's Innovative COVID-19 Saliva Test"),
                         embed_youtube('V9SV5NaDiiI',
                                       width = NULL,
                                       height = 300,
                                       ratio = c("16by9", "4by3"),
                                       frameborder = 0,
                                       allowfullscreen = TRUE,),
                         tags$br(),
                         h3("Information Regarding A Study Based On UIUc's Testing Methods"),
                         h5("Due to frequent testing and fast results, the National Institutes of Health (NIH) has choosen the U. of I.
                                                    viral dynamics study to take part in its Rapid Acceleration of Diagnostics initiative. The U. of I. study aims to see the correlation between results 
                                                      from different testing methods and to determine the point in time at which an infected person becomes infectious.
                                                      Those who test postive or are cautioned for being exposed to a positive case are allowed to partake in the study."),
                         tags$br(),
                         tags$a(href = "https://news.illinois.edu/view/6367/46027586", "Click here to see the full article")
                         
                ),

# Mental Health Resources
              tabPanel("Mental Health Resources",
                       
                       # Add Stress and Anxiety Resources (colep3)
                       h1("Resources for Coping with Anxiety and Stress", align = "center"),
                       
                       tags$a(href = "https://adaa.org/tips","ADAA Tips to Manage Anxiety and Stress"),
                       tags$br(),
                       tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/daily-life-coping/managing-stress-anxiety.html","CDC Recommendations"),
                       tags$br(),
                       tags$a(href = "https://www.mentalhealth.org.uk/publications/how-manage-and-reduce-stress","UK Mental Health Foundation Recommendations"),
                       tags$br(),
                       
                       tags$hr(),
                       h1("Coping with COVID-19 disruptions"), embed_youtube('IFTx7VaHtTI', width = NULL, height = 400, ratio = c("16by9", "4by3"), frameborder = 0, allowfullscreen = TRUE),
                       
                       
                       
                       # Add suicide Hotline
                       h1("Suicide Prevention Resources", align = "center"),
                       
                       tags$a(href = "https://suicidepreventionlifeline.org/", "Suicide Prevention Lifeline"),  
                       tags$br(),
                       p("Suicide Prevention Hotline: 1-800-273-8255"),
                       
                       tags$hr(),
                       
                       # Add university counseling resources.
                       h1("UIUC Counseling Center", align = "center"),
                       
                       p("The Counseling Center is committed to providing a range of services intended to help students develop improved coping skills in order to address emotional, interpersonal, and academic concerns. The Counseling Center provides individual, couples, and group counseling. All of these services are paid for through the health services fee. The Counseling Center offers primarily short-term counseling, but we do provide referrals to the community when students could benefit from longer term services."),
                       
                       tags$a(href = "https://counselingcenter.illinois.edu/screenings", "Mindwise Mental Health Screenings"),
                       tags$br(),
                       
                       tags$a(href = "https://counselingcenter.illinois.edu/counseling/services/group-counseling", "Group Counseling"),
                       tags$br(),
                       
                       tags$hr(),
                       
                       #Add champaign area resources
                       h1("Champaign Area Resource",align = "center"),
                       
                       tags$a(href = "https://champaigncountyil.recovers.org/", "Champaign County IL Relief & Recovery"),  
                       tags$br(),
                       
                       tags$a(href = "https://socialwork.illinois.edu/c-u-community-resource-guide/?fbclid=IwAR0JA6W90dyKMS8YolsVTzltGCD2M_miXQsJ_x8nJtzcNUpIPGM5zjeSUkY&doing_wp_cron=1607479086.8919169902801513671875", "School of Social Work's Community Resource Guide"), 
                       tags$br(),
                       
                       tags$hr(),
                       
                       #Add Chicago area resources
                       h1("Chicago Area Resources",align = "center"),
                       
                       tags$a(href = "https://www.chicago.gov/city/en/depts/cdph/supp_info/behavioral-health/mental-health-and-coping-during-covid-19.html", "Mental Health and Coping During COVID-19"),  
                       tags$br(),
                       
                       tags$a(href = "https://www.cookcountyil.gov/service/covid-19-community-recovery-initiative", "Community Recovery Initiative"), 
                       tags$br(),
                       
                       tags$hr(),
                       
                       h1("National Crisis Resources",align = "center"),
                       
                       h5("National Suicide Prevention Lifeline: 1-800-273-TALK (8255); www.suicidepreventionlifeline.org"),
                       h5("National Domestic Violence Hotline: 1-800-799-7233") ,
                       h5("GLBT Hotline: 1-888-843-4564"),
                       h5("National Alliance on Mental Illness (NAMI) Healthline: 1-800-950-NAMI (6264)"),
                       h5("Substance Abuse and Mental Health Services Administration's Treatment Helpline 1-877-726-4727"),
                       
                       #add link to NAMI support website
                       tags$a(href = "https://www.nami.org/home", "More Resources from NAMI"), 
                       tags$br(),
                       
                       #Add Positive Imagery
                       img(src="https://www.nami.org/NAMI/media/NAMI-Media/Images/awareness%20events/yana2020-mhm-Facebook-post.png", height = 500, width = 500),
                       
              ),

# Legal Aid
              tabPanel("Legal Aid",
                       
                       fluidPage(fluidRow(column(4,align="center",tags$img(src="https://odos.illinois.edu/sls/images/lockup-sls-print.png",width="100%"))),
                                 
                                 #Rent/Living situation aid
                                 tags$br(),
                                 tags$a(href = "https://odos.illinois.edu/sls/", "University Student Legal Services for Tenants"),
                                 tags$br(),
                                 tags$br(),
                                 tags$a(href = "https://www.illinoislegalaid.org/legal-information/housing-coronavirus-and-law#pr", "Illinois Legal Aid information detailing COVID-19 impact on Housing"),
                                 tags$br(),
                                 tags$br(),
                                 tags$a(href = "http://www.cutenantunion.org", "Champaign-Urbana Tenant Union"),       
                       ),
                       
                       
                       tags$br(),
                       tags$br(),
                       
                       fluidPage(fluidRow(column(8,align="left",tags$img(src="https://www.winnetworkdetroit.org/wp/wp-content/uploads/2019/02/National-Domestic-Violence-Hotline-800.799.SAFE-7233-National-Sexual-Assault-Hotline-800.656.4673.png",width="60%"))),
                                 
                                 #Domestic Violence resources in Illinois and National 
                                 tags$br(),
                                 tags$a(href = "https://www.thehotline.org", "National Domestic Violence Hotline and related information: Call 800-799-7233"),
                                 tags$br(),
                                 tags$br(),
                                 tags$a(href = "https://www.rainn.org/resources", "RAINN Sexual Assault Hotline: Call 800-656-4673"),
                                 tags$br(),
                                 tags$br(),
                                 tags$a(href = "https://www.illinoislegalaid.org/legal-information/i-need-help-domestic-violence-during-pandemic", "Illinois Legal Aid: Help with Domestic Violence during Pandemic"),
                       ),
                       
                       tags$br(),
                       tags$br(),
                       
                       fluidPage(fluidRow(column(8,align="left",tags$img(src="https://bostonglobe-prod.cdn.arcpublishing.com/resizer/j6PGGK_Hicl2yIBBgTVQ94chEX4=/1024x0/arc-anglerfish-arc2-prod-bostonglobe.s3.amazonaws.com/public/RHVG6GOVP3N2D6WT5TSQEPFQHI.jpg",width="45%"))),
                                 
                                 
                       ),
                       h3("Unemployment and Poverty during the Pandemic Resources"),
                       tags$br(),
                       tags$a(href="https://www.dol.gov/coronavirus/unemployment-insurance","US Department of Labor information on Unemployment"),
                       tags$br(),
                       tags$br(),
                       tags$a(href = "https://nlihc.org/coronavirus-and-housing-homelessness", "Loss of Housing Due to the Pandemic"),
                       tags$br(),
                       tags$br(),
                       tags$a(href = "https://justiceinaging.org/covid-19-resources-for-advocates/", "Resources for (Financially) Impacted Older Individuals"),
                       tags$br(),
                       tags$br(),
                       tags$a(href = "http://abe.illinois.gov/", "Application for Benefits Eligibility in Illinois"),
                       tags$br(),
                       tags$a(href = "https://www.povertylaw.org/article/covid-19-resources-for-individuals-and-families-in-illinois/#covid", "Information and Resources to Different Communities During COVID-19"),
                       tags$br(),
                       h3("Covid 19's Effect on Crime"),
                       tags$br(),
                       tags$a(href="https://www.youtube.com/watch?list=PLFH18CjbZNsG4xERZvEQV4iIKTlBVLDf7&v=jQE2dZK3iVI&feature=emb_logo","How the Coronavirus is Affecting Crime"),
                       tags$br(),
                       tags$br(),
                       tags$a(href="https://www.ftc.gov/system/files/attachments/coronavirus-covid-19-pandemic-ftc-action/keep_calm_infographic_en_letter_508.pdf","How to avoid scams in Covid-19"),
                       tags$br(),
                       tags$br(),
                       tags$a(href="https://www2.illinois.gov/idoc/programs/pages/victimservices.aspx","Victim service in Illinois"),
                       tags$br(),
                       tags$br(),
              ),

# How To Help
              tabPanel("HOW TO HELP",
                       
                       fluidPage(fluidRow(column(12,align="center",tags$img(src="https://pbs.twimg.com/media/EUdWR4qXkAEwlJ2.jpg",width="50%"))),
                                 fluidRow(column(4,tags$br(),align="center",tags$img(src="https://www.charities.org/sites/default/files/styles/large/public/AmericasCharities_COVID19-Fund_300pxAd_4-2-20.jpg?itok=f4MozTVN",width="51%")),
                                          column(4,tags$br(),align="center",tags$img(src="https://covid19responsefund.org/assets/share-social.png",width="75%")),
                                          column(4,tags$br(),align="center",tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/hhs/Donate-Plasma-Navy.jpg", width="75%"))),
                                 fluidRow(column(4,tags$br(),
                                                 "Nonprofits all across America are playing a vital role in supporting the people and communities that have been impacted by the COVID-19 pandemic",
                                                 tags$br(),
                                                 h4("Find Out How You Can Help"),
                                                 tags$a(href = "https://www.charities.org/coronavirus-covid-19-donations-funds-and-resources?gclid=CjwKCAiA_Kz-BRAJEiwAhJNY71h4gb5n8UlD5eDwUK_WhK1JSVdwvWiFsqnVZNoTkcthCBpSdMQwfRoCT3QQAvD_BwE","List of Nonprofits and Charities"),
                                                 tags$br(),
                                                 tags$a(href = "https://www.feedingamerica.org/take-action/coronavirus","Feeding America's Coronavirus Response Fund"),
                                                 tags$br(),
                                                 tags$a(href = "https://www.powerof.org/volunteer","Sign Up to Become a Volunteer!"),
                                                 tags$br(),
                                                 tags$a(href = "https://www.consumerreports.org/charitable-donations/how-to-help-your-community-during-coronavirus-crisis/", "Help your community during the COVID-19 Crisis"),
                                                 tags$br(),
                                                 img(src="data:image/png;base64,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",height=150,width=258),
                                                 tags$br(),
                                                 h4("Learn more about UNICEF"),
                                                 tags$br(),
                                                 tags$a(href="https://www.unicefusa.org/about","Click here to learn more about UNICEF SATFF"),
                                                 tags$br(),
                                                 tags$a(href="https://www.unicefusa.org/about","Click here to join UNICEF volunteers"),
                                                 tags$br(),
                                                 tags$a(href="https://www.unicefusa.org/about/contact","Click here to contact UNICEF"),
                                                 tags$br(),
                                                 img(src="data:image/png;base64,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",height=150,width=258),
                                                 tags$br(),
                                                 
                                 ),
                                 column(4,tags$br(),
                                        h4("World Health Organization"),
                                        "The World Health Organization is leading and coordinating the global effort to combat the COVID-19 pandemic by helping countries prevent, detect, and respond to the virus.",
                                        tags$br(),
                                        tags$a(href = "https://www.fda.gov/emergency-preparedness-and-response/coronavirus-disease-2019-covid-19/donate-covid-19-plasma",
                                               "Click here to learn about Donate COVID-19 Plasma."),
                                        tags$br(),
                                        tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/donate",
                                               "Click Here to Learn More About the COVID-19 Solidarity Response Fund."),
                                        tags$br(),
                                        tags$a(href = "https://www.unicefusa.org/?utm_content=corona3responsive_E2001&ms=cpc_dig_2020_Emergencies_20200402_google_corona3responsive_delve_E2001&initialms=cpc_dig_2020_Emergencies_20200402_google_corona3responsive_delve_E2001&gclid=EAIaIQobChMIycvruda97QIVEj2tBh1wmgHzEAAYASAAEgJuOvD_BwE",
                                               "Click here to Donate to the COVID-19 with UNICEF USA Official Site."),
                                        tags$br(),
                                        tags$a(href = "https://covid19responsefund.org/en/",
                                               "Click here to Donate to the COVID-19 Solidarity Response Fund."),
                                        h4("National Voluntary Organizations Active in Disaster"),
                                        img(src="http://volunteer.volunteerleon.org/content/volunteer.volunteerleon.org/agency/21484.jpg?1400597910?area=agency", height=150, width=258),
                                        "While the WHO is spearheading the international effort, the NVOAD is helping to assist in the US's response to COVID-19.",
                                        tags$br(),
                                        tags$a(href = "https://www.nvoad.org/covid-19-response/", "Click here to Donate to the National Voluntary Organizations Active in Disaster.")
                                 ),
                                 column(4,tags$br(),
                                        "Because you fought the infection, your plasma now contains COVID-19 antibodies. These antibodies provided one way for your immune system to fight the virus when you were sick, so your plasma may be able to be used to help others fight off the disease.",
                                        tags$h4("Plasma Donations"),
                                        tags$a(href = "https://www.fda.gov/emergency-preparedness-and-response/coronavirus-disease-2019-covid-19/donate-covid-19-plasma",
                                               "Click Here to Learn More About how Donating Plasma Saves Lives."),
                                        tags$br(),
                                        tags$a(href = "https://thefightisinus.org/en-US/home#home",
                                               "Click Here if You Are Interested in Donating Plasma."),
                                        tags$h4("Blood Donations"),
                                        tags$a(href = "https://www.redcrossblood.org/", "Click Here if You Are Interested in Donating Blood.")   
                                 ),
                                 column(4, tags$br(),
                                        img(src="https://imagesvc.meredithcorp.io/v3/mm/image?q=85&c=sc&poi=face&w=1920&h=1005&url=https%3A%2F%2Fstatic.onecms.io%2Fwp-content%2Fuploads%2Fsites%2F20%2F2020%2F04%2F09%2Fgfm20_covid19.jpg", height=150, width=258),
                                        h4("Donating to Global COVID-19 Causes"), 
                                        "Here are links to GoFundMe pages for COVID-19 ravaged third-world contries. These countries don't have as much funding to effectively aid all citizens from this pandemic.",
                                        tags$br(),
                                        tags$a(href="https://www.globalgiving.org/projects/help-families-affected-by-covid-19-in-colombia/donate/", "Click here to donate to Colombia"), 
                                        tags$br(),
                                        tags$a(href="https://www.ethiopiatrustfund.org/covid-19-donation/", "Click here to donate to Ethopia"), 
                                        tags$br(), 
                                        tags$a(href="https://www.globalgiving.org/fundraisers/coronavirus-relief-fund-bangladesh/", "Click here to donate to Bangladesh"),
                                        tags$br(), 
                                        tags$a(href="https://www.globalgiving.org/projects/prepare-clinics-in-haiti-for-covid-19/", "Click here to donate to Haiti"),
                                        tags$br(), 
                                        tags$a(href="https://www.globalgiving.org/projects/covid-19-save-life-and-global-well-being-cameroon/", "Click here to donate to Cameroon"),
                                        tags$br(), 
                                        tags$a(href="https://www.savethechildren.org/us/where-we-work/yemen", "Click here donate to Yemen"),
                                        tags$br(), 
                                        tags$a(href="https://www.globalgiving.org/projects/supporting-1000-vulnerable-families-in-nigeria/donate/", "Click here to donate to Nigeria"),
                                        tags$br(),
                                        img(src="https://cdn2.atlantamagazine.com/wp-content/uploads/sites/4/2020/03/donate_getty.jpg", height=150, width=258))
                                 #add an image that shows that we must all support the world 
                                 
                                 )
                                 
                       )
              ),

#Covid-19 Recent News
                  tabPanel("COVID-19 Recent News",
                           #add images, and links
                           h1("Center of Disease Control", align = "center"),
                           tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/whats-new-all.html","What's New | CDC"),
                           tags$br(),
                           tags$a(href = "https://tools.cdc.gov/campaignproxyservice/subscriptions.aspx?topic_id=USCDC_2067", "CDC Email Newsletter"),  
                           tags$br(),
                           tags$a(href = "https://covid.cdc.gov/covid-data-tracker/#trends_dailytrendscases", "CDC US Daily Cases"),  
                           tags$br(),
                           tags$hr(),
                           
                           h1("World Health Organization", align = "center"),
                           tags$a(href = "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports","Updates | WHO"),
                           tags$br(),
                           tags$a(href = "https://www.who.int/news-room/newsletters", "WHO Email Newsletters"),  
                           tags$br(),
                           tags$a(href = "https://worldhealthorg.shinyapps.io/covid/", "WHO Daily Cases Data"),  
                           tags$br(),
                           tags$hr(),
                           
                           h1("Illinois Department of Public Health", align = "center"),
                           tags$a(href = "https://dph.illinois.gov/news-pressrelease","Daily News | IDPH"),
                           tags$br(),
                           tags$a(href = "https://www.dph.illinois.gov/news", "IDPH Daily Cases"),  
                           tags$br(),
                           tags$hr(),
                           
                           h1("Champaign-Urbana Public Health District", align = "center"),
                           tags$a(href = "https://www.c-uphd.org/champaign-urbana-illinois-coronavirus-information.html","Recent Info | CUPHD"),
                           tags$br(),
                           tags$a(href = "https://public.tableau.com/views/Master2COVIDTableau/Dashboard1?:embed=y&:showVizHome=no&:host_url=https%3A%2F%2Fpublic.tableau.com%2F&:embed_code_version=3&:tabs=no&:toolbar=no&:animate_transition=yes&:display_static_image=no&:display_spinner=no&:display_overlay=yes&:display_count=yes&:language=en&publish=yes&:loadOrderID=0", "CUPHD Daily COVID Case Tracker"),  
                           tags$br(),
                           tags$hr(),
                           
                           h1("Other Resources", align = "center"),
                           
                           h3("Chicago Tribune - Daily Updates"),
                           tags$a(href = "https://www.chicagotribune.com/coronavirus/","Coronavirus Updates | Chicago Tribune"),
                           tags$br(),
                           
                           h3("New York Times - Daily Updates"),
                           tags$a(href = "https://www.nytimes.com/news-event/coronavirus?name=styln-coronavirus&region=TOP_BANNER&block=storyline_menu_recirc&action=click&pgtype=LegacyCollection&impression_id=a0a836e0-3b3b-11eb-b184-ff23d8f1fa7b&variant=1_Show","COVID-19 Live Updates | NYT"),
                           tags$br(),
                           
                           h3("Johns Hopkins Coronavirus Resource Center"),
                           tags$a(href = "https://coronavirus.jhu.edu/","JHU Covid Updates"),
                           tags$br(),
                           
                           h1("Coronavirus pandemic on BBC"),
                           tags$a(href = "https://www.bbc.com/news/coronavirus","Click here to see the site of BBC News"),
                           tags$br(),
                           
                           h1("FDA Daily Updates"),
                           tags$a(href = "https://www.fda.gov/news-events/press-announcements/coronavirus-covid-19-update-december-10-2020","Click here to see the updates"),
                           tags$br(),
                           
                           #add covid picture
                           img(src="https://i.ytimg.com/vi/F70BzSFAZfw/maxresdefault.jpg", height= 300, width=500),
                           tags$br(),
                           
                           img(src="https://www.fda.gov/themes/custom/preview/img/FDA-Social-Graphic.png",height= 300, width=500),
                           tags$br()
                           
                  ),

#Vaccine
                  tabPanel("Vaccine",
                           #Add a picture about vaccines to guide the web content 
                           img(src="https://images.theconversation.com/files/341551/original/file-20200612-153812-ws3rqu.jpg?ixlib=rb-1.1.0&q=45&auto=format&w=754&fit=clip", height = 400, width = 700),
                           tags$br(),
                           #add link about information on COVID-19 vaccine for CDC
                           tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/index.html", "Click here for more information on COVID-19 Vaccine for CDC."),
                           tags$br(),
                           tags$a(href = "https://www.who.int/news-room/q-a-detail/coronavirus-disease-(covid-19)-vaccines?adgroupsurvey={adgroupsurvey}&gclid=Cj0KCQiA5bz-BRD-ARIsABjT4nhYVbcBsg0jkC_m7SJ6vGxslyV2uHtBS82mEqE_HUiii4rmcA6HoJAaAtZ8EALw_wcB", "Click here for more information on COVID-19 Vaccine for WHO."),
                           tags$br(),
                           #add link about information on COVID-19 vaccine
                           tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/vaccines/index.html", "Click here for more information on COVID-19 Vaccine."),
                           tags$br(),
                           #add first person in uk to receive the vaccine
                           h1("Vaccine is already in action"),
                           img(src="https://cnet1.cbsistatic.com/img/S8P2GXkwkXsYfC-v9BR-hP6uw0Y=/940x0/2020/12/08/4659ec1c-12e1-462e-a1e7-65eb52bd4c68/gettyimages-1230005171.jpg"),
                           tags$br(),
                           tags$a(href = "https://www.bbc.com/news/uk-55227325", "Click here to read about Margarets journey."),
                           tags$br(),
                           #add link about information on COVID-19 vaccination timeline
                           tags$a(href = "https://www.nytimes.com/interactive/2020/12/03/opinion/covid-19-vaccine-timeline.html", "Click here to fill out a survey to find your place in the COVID-19 Vaccine Line"),
                           tags$br(), 
                           tags$a(href = "https://www.businessinsider.com/when-can-i-get-a-coronavirus-vaccine-timeline-2020-11", "Click here to learn the COVID-19 vaccination timeline"),
                           tags$br(),
                           #add link about information related to vaccine tracking
                           tags$a(href = "https://www.raps.org/news-and-articles/news-articles/2020/3/covid-19-vaccine-tracker", "Click here to access a COVID-19 Vaccine Tracker"),
                           tags$br(),
                           #add link to access live updates of vaccine related news
                           tags$a(href = "https://www.wsj.com/articles/covid-19-vaccines-mass-distribution-supply-chain-11607874181?mod=searchresults_pos1&page=1", "Click here to access live news updates of the COVID-19 Vaccine (Distribution Under Way)"),
                           tags$br(),
                           tags$img(src="https://media.gatesnotes.com/-/media/Images/Articles/Health/What-you-need-to-know-about-the-COVID-19-vaccine/what-you-need-to-know_2020_inline-graph_desktop-03.ashx", height = 500, width=500),
                           
                           #link from WHO that gives info on Vaccine and answers questions
                           tags$a(href = "https://www.who.int/news-room/q-a-detail/coronavirus-disease-(covid-19)-vaccines?adgroupsurvey={adgroupsurvey}&gclid=CjwKCAiAiML-BRAAEiwAuWVgggLpGj6VTEYCx5tAsUHBdo9SkhMO9pnpAyqysVA9mNOs8oMLduhLXhoCPDAQAvD_BwE", "Click Here for Vaccine Questions answered by WHO"),
                           tags$br(),
                           
                           #Link to register for updates on when you may qualify for the COVID vaccine in Champaign County
                           tags$a(href = "https://www.urbanacitizen.com/news/78679/sign-up-for-covid-vaccine-alerts-with-new-link", "Click Here for updates on when you can get the vaccine"),
                           tags$br(),
                           
                           h2("Vaccines in Progress(US)",align = "left"),
                           shinyApp(
                             shinyUI(
                               fluidPage(
                                 dataTableOutput('PM_output')
                               )
                             ),
                             
                             shinyServer(function(input, output, session) {
                               require(DT)
                               dat = data.frame(Manufacturer=c("AstraZeneca","Janssen Pharmaceutical Companies","Moderna","Novavax","Pfizer"),`Vaccine Platform` = c("Non-Replicating Viral Vector ","Non-Replicating Viral Vector ","RNA","Protein Subunit","RNA"), 
                                                Websites = c("https://www.astrazeneca.com/media-centre/press-releases/2020/azd1222hlr.html","https://www.janssen.com/us/covid19-patient-resources","https://www.modernatx.com/modernas-work-potential-vaccine-against-covid-19","https://ir.novavax.com/news-releases/news-release-details/novavax-announces-covid-19-vaccine-clinical-development-progress","https://www.wsj.com/articles/fda-set-to-release-analyses-of-the-pfizer-biontech-covid-19-vaccine-11607423403"))
                               dat$Websites <- sapply(dat$Websites, function(x) 
                                 toString(tags$a(href=paste0("http://", x), x)))
                               
                               output$PM_output <- renderDataTable(expr = datatable(dat, escape=FALSE),
                                                                   options = list(autoWidth = T))
                             })
                           ),
                           h3("British AstraZeneca Vaccine"),
                           tags$img(src="https://api.time.com/wp-content/uploads/2020/11/GettyImages-1229753927.jpg?w=800&quality=85", height = 200, width=300),
                           h5("AstraZeneca plc is a British-Swedish multinational pharmaceutical and biopharmaceutical company with its headquarters in Cambridge, England.
                              In June 2020, the National Institute of Allergy and Infectious Diseases (NIAID) confirmed that the third phase of testing for potential vaccines
                              developed by Oxford University and AstraZeneca would begin in July 2020. One of them, AZD1222, reached phase III trials."),
                           tags$a(href = "https://www.astrazeneca.com/covid-19.html", "Click here if you want to know any info about AstraZeneca's vaccine progress"),
                           
                           h3("Germany BioNTech & US Pfizer Vaccine"),
                           tags$img(src="https://cached.imagescaler.hbpl.co.uk/resize/scaleWidth/800/cached.offlinehbpl.hbpl.co.uk/news/PGH/GettyImages-1229653900.jpg", height = 200, width=300),
                           h5("BNT162b2 is a COVID-19 vaccine developed by BioNTech and Pfizer and given by intramuscular injection.
                                It is an RNA vaccine composed of nucleoside-modified mRNA encoding a mutated form of the spike protein of SARS-CoV-2,
                                and is encapsulated in lipid nanoparticles.In November 2020, interim analysis of the trial examined research participants who had been diagnosed with COVID-19 and received the vaccine candidate,
                                showing that BNT162b2 may have an efficacy of over 90% in preventing infection within seven days of a second dose."),
                           tags$a(href = "https://www.pfizer.com/", "Click here if you want to know any info about BNT162b2 progress"),
                           
                           h3("Sinopharm vaccine"),
                           tags$img(src="https://news.cgtn.com/news/2020-11-25/Sinopharm-applies-to-bring-its-COVID-19-vaccine-to-market--VIiimRueZy/img/971c31ce7cea4b6aaa0015354221c537/971c31ce7cea4b6aaa0015354221c537.jpeg", height = 200, width=300),
                           h5("On October 15, the Beijing Institute of Biological Products published results of its Phase I (192 adults) and Phase II (448 adults)
                               clinical studies for the BBIBP-CorV vaccine in The Lancet. BBIBP-CorV, was safe and well tolerated at all tested doses in two age groups.
                               Antibodies were elicited against SARS-CoV-2 in all vaccine recipients on day 42. The report noted that the inactivated COVID-19 vaccine had a low rate of
                               adverse reactions and demonstrated immunogenicity, but longer-term assessment of safety and efficacy would require phase III trials."),
                           tags$a(href = "http://www.sinopharm.com/en/1156.html","Click here if you want to know any info about Sinopharm vaccine progress"), 
                           
                           h3("Sputnik V Vaccine"), 
                           tags$img(src="https://s.france24.com/media/display/836ad246-dbc7-11ea-822f-005056a98db9/w:1280/p:16x9/Russia%20coronavirus%20vaccine.webp", height=200, width=300), 
                           h5("Released this August, Sputnik V was officially the first COVID-19 vaccine. Though it is still in the midst of trials to check that it's safe and actually works
                               the company has promised a 91.4% accurate rate. As of today, around 100,000 people have been dosed with it. Because there is little to no data backing
                               the vaccine there are only a limited amount of people that have taken it. The general public believes that because Russia is being hit with the virus 
                               very hard (roughly 30,000 new cases a day), the release of the vaccine was rushed and not safe to give to a wider population."), 
                           tags$a(href="https://www.bbc.com/news/world-europe-55221785", "Click here if you want to learn about Russia's Sputnik V Vaccine"),
                           
                           h3("Novavax Vaccine"), 
                           tags$img(src="https://static.toiimg.com/thumb/msid-79132422,imgsize-70644,width-400,resizemode-4/79132422.jpg", height=200, width=300), 
                           h5("Novavax has created a vaccine candidate named NVX-CoV2373. This vaccine has shown that it efficiently binds with human receptors targeted by the virus in preclinical studies,
                                                        which is a critical aspect for a vaccine to be effective. As of November 30th, their vaccine candidate has completed enrollment for its Phase 3 trials in the United Kingdom and
                                                        for its Phase 2b efficacy trial in South Africa, and is expected to begin its Phase 3 trials in the US and Mexico very soon. Novavax was also granted $1.6 billion in funding from 
                                                        the U.S. government to meet its Operation Warp Speed goals which are to expedite the delivery of millions of doses of the COVID-19 vaccines. This award is also funding the U.S. 
                                                        and Mexico Phase 3 trials, as well as the scaled-up manufacturing."), 
                           tags$a(href="https://www.novavax.com/our-pipeline#nvx-cov2373", "Click here to learn about Novavax's progress in their clinical trials"),
                           
                           h3("A New Way of Vaccination - FruitVaccine"),
                           h5("Have you ever imagined a new way of vaccination? Wouldn't it be cool if you could eat products made by whole fruits but get vaccinated 
                                  at the same time? EnterpriseWorks startup FruitVaccine is changing the world of vaccinations with an edible, orally-administered fruit-based vaccine for 
                                  human respiratory syncytial virus (RSV) and coronavirus (COVID). Founded by University of Illinois professors Dr. Indu Rupassara and Dr. Dennis Buetow in 2017, the two have worked to modify tomatoes to develop a sub-unit vaccine. The startup has 
                                  had a strong first year as a business with successful trials performed on mice, and the founders tell Smile Politely that their long term goal, 'is to not only create an edible RSV vaccine, 
                                  but to develop many different edible vaccines for different viruses.'"),
                           tags$a(href="http://www.fruitvaccine.org/", "Click here if you want to learn more about FruitVaccine and their mission"),
                           
                  ),

#Prevention
                  tabPanel("Prevention",
                           h4("The easiest and most effective way of preventing the contraction and spread of COVID-19 is wearing a mask!"),
                           tags$br(),
                           tags$a(href="https://www.youtube.com/watch?v=DNeYfUTA11s","Click here to learn how masks prevent the projection of particles"),
                           tags$br(),
                           tags$a(href="https://www.amazon.com/s?k=masks&ref=nb_sb_noss","Click here to purchase masks"),
                           tags$br(),
                           tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/prevention.html","Click here for information on how to protect yourself and others"),
                           tags$br(),
                           tags$a(href="https://www.cdc.gov/coronavirus/2019-ncov/prevent-getting-sick/social-distancing.html","Click here for why and how to practice social distancing"),
                           tags$br(),
                           tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/advice-for-public","Click here for WHO Advice for the public"),
                           tags$br(),
                           tags$img(src="https://www.nebraskamed.com/sites/default/files/images/covid19/Masks-Dos-Donts_Blog_v2.jpg", height = 400, width=500),
                           
                           tags$img(src="https://files.labcorp.com/labcorp-d8/2020-04/LabCorp_Coronavirus_Infographic_032720.jpg", height = 400, width=500),
                           tags$img(src="https://www.linnsheriff.org/wp-content/uploads/COVID-19-Graphic-web-banner.jpg", height = 400, width=800),
                           tags$img(src="https://www.stlukeshealth.org/sites/default/files/mask-effectiveness.jpg", height =400, width=800),
                           h4("Here is a graph displaying % effectiveness of the type of mask you use. The most effective being the standard surgical mask."),
                           tags$img(src="https://media.nature.com/lw800/magazine-assets/d41586-020-01248-1/d41586-020-01248-1_17940460.png", height =500, width=300), 
                           h4("Prevention matters! Nations such as China, Hong Kong, South Korea introduced heavy mask regulations/stay-at-home-orders and, thus, deaths plateaued."),
                           tags$br(),
                           h4("Staying at home is the best way to stop the spread. However, if you have to go out, make sure to wear a mask and practice social distancing"),
                           tags$img(src = "https://www.kellerisd.net/cms/lib/TX02215599/Centricity/Domain/117/COVIDProtocols/Mititgation_MaskExample.png", height = 400, width = 500),
                           h4("If you are going out..."), embed_youtube('quNi23GQ89c', width = NULL, height = 400, ratio = c("16by9","4by3"), frameborder = 0, allowfullscreen = TRUE),
                           h4("This risk factor chart shows you how risky certain activities could be during this time on a scale of 1 to 10, please refer to this chart when going out and deem if certain activities are a necesity."),
                           tags$img(src = "https://static.abcotvs.com/ktrk/images/cms/risk-chart.jpg", height = 800, width = 600),
                           h4("How to protect yourself against COVID-19"),
                           embed_youtube('1APwq1df6Mw',
                                         width = NULL,
                                         height = 300,
                                         ratio = c("16by9", "4by3"),
                                         frameborder = 0,
                                         allowfullscreen = TRUE),
                  ), 
                  
#Alert Information
                  tabPanel("Alert Information",
                           #Adding information about Alerts 
                           h4("Alert Information"),
                           tags$img(src="https://www.kxan.com/wp-content/uploads/sites/40/2020/05/Chart-1.jpg?resize=876,559", height = 500, width=800),
                           tags$br(),
                           h4("WHO Real Time Alert"),
                           tags$a(href = "https://www.who.int/csr/alertresponse/realtimealert/en/","Click here to get the WHO Real Time Alert information"),
                           tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/travelers/images/CBP_PROTECT_OTHERS_TravelAlert_1920x1080_stills-medium.jpg", height = 500, width=800),
                           tags$br(),
                           h4("COVID-19 Travel Recommendations"),
                           tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/travelers/map-and-travel-notices.html","COVID-19 Travel Recommendations by Destination"),
                           h4('For real-time covid19 updates visit'),
                           tags$a(href = "https://coronavirus.jhu.edu/", "Click here to get Real updates on all covid19 data"),
                           
                  ),

#Illinois travel guideline
                  tabPanel("Illinois travel guideline",
                           HTML('<div>
                                                   <h1>CDC Travel Guidance</h1>
                                                   <h2>If I travel, what steps should I take to help reduce my chances of getting sick?</h2>
                                                   <ul><li>Wash your hands often with soap and water for at least 20 seconds, especially after you have been in a public place, touching surfaces frequently touched by others, blowing your nose, coughing, and sneezing, and before touching your face or eating.</li>
                                                   <li>If soap and water are not available, use hand sanitizer that contains at least 60% alcohol. Cover all surfaces of your hands and rub your hands together until they feel dry.</li>
                                                   <li>Avoid touching your eyes, nose, or mouth with unwashed hands.</li>
                                                   <li>Avoid close contact with others, keeping 6 feet of distance.</li>
                                                   <li>Wear a cloth face covering in public.</li>
                                                   <li>Cover coughs and sneezes.</li>
                                                   <li>Pick up food at drive-throughs, curbside at restaurants, or stores.</li>
                                                   <li>Make sure you are up to date with your routine vaccinations, including the measles-mumps-rubella (MMR) vaccine and the seasonal flu vaccine.</li>
                                                   </ul><h1>Domestic and Interstate Travel</h1>
                                                   <h2>Can traveling to visit family or friends increase my chances of getting and spreading COVID-19?</h2>
                                                   <p>Yes. Travel increases your risk of getting and spreading COVID-19. Before you travel, learn if COVID-19 is spreading in your local area or in any of the places you are going. Traveling to visit family may be especially dangerous if you or your loved ones are more likely to suffer severe illness from COVID-19. People at higher risk for severe illness need to take extra precautions.</p>
                                                   <h1>International Travel</h1>
                                                   <h2>Should I avoid traveling internationally?</h2>
                                                   <p>Avoid all nonessential international travel because of the COVID-19 pandemic.&nbsp;Some health care systems are overwhelmed and there may be limited access to adequate medical care in affected areas. Many countries are implementing travel restrictions and mandatory quarantines, closing borders, and prohibiting non-citizens from entry with little advance notice. Airlines have cancelled many international flights and in-country travel may be unpredictable. If you choose to travel internationally, your travel plans may be disrupted, and you may have to remain outside the U.S. for an indefinite length of time.</p>
                                                   <p>CDC recommends travelers defer cruise ship travel worldwide at this time.</p>
                                                   <h2>What can I expect when departing other countries?</h2>
                                                   <p>Some countries are conducting exit screening for passengers leaving their country. Before being permitted to board a departing flight, you may have your temperature taken and be asked questions about your travel history and health.</p>
                                                   <h2>What can I expect when arriving to the U.S.?</h2>
                                                   <p>Travel restrictions and&nbsp;entry screening may apply to travelers arriving from some countries or regions with ongoing, widespread COVID-19.</p>
                                                   <p>You may be screened when you arrive in the U.S. If you have traveled to an area of higher risk, take the following steps upon your return to protect yourself and others:</p>
                                                   <ol><li>Stay home and avoid contact with others. Do not go to work or school for 14 days.</li>
                                                   <li>Monitor your health for 14 days. Take your temperature two times a day and monitor for fever. Also watch for cough or trouble breathing.</li>
                                                   <li>Keep at least 6 feet of distance from others.</li>
                                                   </ol><h2>When can I return to work after international travel?</h2>
                                                   <p>International travelers returning home from areas of higher risk should stay home for 14 days after their arrival into the U.S. &nbsp;Once home, monitor your health and practice social distancing.</p>
                                                   <h1>Air Travel</h1>
                                                   <h2>Can flying on an airplane increase my risk of getting COVID-19?</h2>
                                                   <p>Yes. Air travel requires spending time in security lines and airport terminals, which can bring you in close contact with other people and frequently touched surfaces. Most viruses and other germs do not spread easily on flights because of how air circulates and is filtered in airplanes. However, social distancing is difficult on crowded flights, and you may have to sit near others (within 6 feet), sometimes for hours. This may increase your risk for exposure to the virus that causes COVID-19.</p>
                                                   <h2>What happens if there is a sick passenger on an international or domestic flight?</h2>
                                                   <p>Under current federal regulations, pilots must report all illnesses and deaths to CDC before arriving to a U.S. destination. According to CDC disease protocols, if a sick traveler is considered a risk to the publics health, CDC works with local and state health departments and international public health agencies to&nbsp;contact exposed passengers and crew.</p>
                                                   <p>Be sure to give the airline your current contact information when booking your ticket so you can be notified if you are exposed to a sick traveler on a flight.</p>
                                                   </div>'),
                  )

          )

server <- function(input, output) {

#HOME  
  
#UIUC
output$lol <- renderPlot({
    
    date_range = input$date_range_covid
    data = Champaign_data %>% filter(date_range[1] < date & date < date_range[2])
    type = data$new_cases
    color = "new_cases"
    
    if(input$Graph_Type == "New Cases") {
      type = data$new_cases
      color = "new_cases"
    }
    else if (input$Graph_Type == "Total Cases"){
      type = data$cases
      color = "cases"
    }
    else if (input$Graph_Type == "New Deaths"){
      type = data$new_deaths
      color = "new_deaths"
    }
    else {
      type = data$deaths
      color = "deaths"
    }
    
    ggplot(data, aes(x = `date`)) +
      geom_line(aes(y = type, color = color), size = 1)+
      scale_color_manual("",values = "deepskyblue3")+
      labs(y= paste("Number of ",color,sep=""), x = "Date")+
      theme(legend.position = "none",
            panel.background = element_rect(fill="gray40",colour = "Black"),
            panel.grid.major = element_line(colour = "sienna3",size = .5),
            panel.grid.minor = element_blank(),
            plot.subtitle = element_text(size = 10))
  }  
  )
  
  output$testing_site = renderText({
    
    get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
      loadNamespace("purrr")
      loadNamespace("geosphere")
      longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
      longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
      distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
      distance_m = sapply(distance_list, function(col) { col[1] })
      
      if (units == "km") {
        distance = distance_m / 1000.0;
      }
      else if (units == "miles") {
        distance = distance_m / 1609.344
      }
      else {
        distance = distance_m
      }
      distance
    }
    # https://www.latlong.net/
    
    lat = input$lat
    long = 	input$long
    lat_long = c(lat,long)
    
    testing_sites = data.frame(site = c("CRCE", "Illini Union", "State Farm Center", "SDRP", "Veterinary Medicine"),
                               long = c(40.104141, 40.10939235	, 40.0962421, 40.10409, 40.101877),
                               lat = c(-88.221538, -88.2272187093397, -88.2359287628109, -88.2394624, -88.219142))
    
    
    
    distances = map2_dbl(testing_sites$long, testing_sites$lat, ~get_geo_distance(.x, .y, lat_long[1], lat_long[2]))
    
    paste(testing_sites[which.min(distances), 1], "is the closest testing site to your address.")
  })
  
  output$champaign_growth <- renderPlot({
    datas = NYTimes_US_Counties_Historical_Data %>%
      filter(
        county == "Champaign",
        state == "Illinois",
        as.Date(date) < input$champaign_growth_date
      ) %>%
      select(cases, date)
    datas$date = as.Date(datas$date)
    temp = c(0)
    
    datas$date = as.Date(datas$date)
    
    temp = c()
    
    for (i in 1:length(datas$cases)) {
      temp[i] = (datas$cases[i + 1] - datas$cases[i]) / datas$cases[i]
    }
    
    datas$rate = temp
    ggplot(data = datas) +
      geom_line(mapping = aes(x = date, y = rate)) +
      scale_x_continuous(breaks = seq(min(datas$date), max(datas$date), by = "1 weeks"), name = "Date") +
      scale_y_continuous(
        breaks = seq(0, 1, by = 0.25),
        labels = scales::percent_format(accuracy = 0.1),
        name = "Daily Growth Rate"
      ) + theme(axis.text.x=element_text(angle=70, hjust=1))
    
    
  })
  
  output$champaign_cases_search <- renderText({
    cas = NYTimes_US_Counties_Historical_Data %>% filter(county == "Champaign", state == "Illinois") %>% select(cases, date)
    out <-
      paste("At",
            input$champaignCasesSearch_Input,
            "there are",
            cas[which(cas$date == input$champaignCasesSearch_Input), ]$cases,
            "cases in Champaign area")
    
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)