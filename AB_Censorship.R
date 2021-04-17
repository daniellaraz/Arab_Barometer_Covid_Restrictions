#### Daniella Raz ####

# library imports
library(ggplot2)
library(reshape2)
library(dplyr)
library(haven)
library(scales)
library(ordinal)
library(tidyr)

ab6_data <- read_dta("Downloads/AB6_Part1.dta")
View(ab6_data)

# 1. CLEANING  -----------------------------------------------------------------

# country labels
# 1 = Algeria, 8 = Jordan, 10 = Lebanon, 13 = Morocco, 21 = Tunisia
country_labels = c('Algeria', 'Jordan', 'Lebanon', 'Morocco', 'Tunisia')

# factorizing country
ab6_data$country = as.factor(ab6_data$COUNTRY)

# estimations for country populations for weighting in regression
country_pops = c(43851044, 10203134, 6825445, 36910560, 11818619)
country_weights = data.frame(country=as.factor(c(1, 8, 10, 13, 21)),
                             norm_weight = country_pops/sum(country_pops))

# gender variable, 1 = male, 0 = female, no missing
table(ab6_data$Q1002)
ab6_data$gender = as.numeric(ab6_data$Q1002 == 1)
table(ab6_data$gender)

# age variable & age groups, 2 missing
# [18-29] = young adults, [30-59]= middle, 60+ = older
table(ab6_data$Q1001)
ab6_data$age = ab6_data$Q1001
#ab6_data = subset(ab6_data,age != 9999)
ab6_data %>% 
  mutate(age_group = 
           case_when(Q1001 < 30 ~ 'Young_Adult',
                     Q1001 >= 30 & Q1001 < 60 ~ 'Middle_Ages',
                     Q1001 >= 60 & Q1001 < 100 ~ 'Older', 
                     Q1001 == 9999 ~ 'Missing_Age')) -> ab6_data

# education variable, 8 missing
# 0 = basic education = no formal educ., elem. education, & prep./basic education
# 1 = secondary education = secondary
# 2 = higher education = mid-level diploma (professional or technical), BA, >MA
table(ab6_data$Q1003)
#ab6_data = subset(ab6_data,Q1003 != 98 & Q1003 != 99)
ab6_data %>% 
  mutate(education_level = 
           case_when(Q1003 <= 3 ~ 'Basic',
                     Q1003 == 4 ~ 'Secondary',
                     Q1003 >= 5 & Q1003 < 98 ~ 'Higher',
                     Q1003 == 98 | Q1003 == 99 ~ 'Missing_Education')) -> ab6_data

# religiosity variable
table(ab6_data$Q609)
#ab6_data = subset(ab6_data, Q609 != 98 & Q609 != 99)
ab6_data %>% 
  mutate(relig_level = 
           case_when(Q609 == 1 ~ 'Religious',
                     Q609 == 2 ~ 'Somewhat_Religious',
                     Q609 == 3 ~ 'Not_Religious', 
                     Q609 >= 98 ~ 'Missing_Religious')) -> ab6_data


# numbers for trust in government and views on free speech restrictions
ab6_data %>% filter(Q201A_1 <= 2) %>% 
  mutate(restrictions_ok = as.numeric(Q8COVID19_2 < 3)) -> df_trust
weighted.mean(df_trust$restrictions_ok, w = df_trust$WT)


ab6_data %>% filter(Q201A_1 > 2 & Q201A_1 < 90) %>% 
  mutate(restrictions_ok = as.numeric(Q8COVID19_2 < 3)) -> df_no_trust
weighted.mean(df_no_trust$restrictions_ok, w = df_no_trust$WT)



# 2. CHARTS --------------------------------------------------------------------

# chart colors
gender_colors = c("#003D4E", "#CCEAF2")
names(gender_colors) = c("Men", "Women")

age_colors = c("#B4ADDF", "#FF5E64")
names(age_colors) = c("18-29", "60+")

trust_colors = c("#EAA4A4", "#AC517F")
names(trust_colors) = c("Trust", "Little to No Trust")


# * Q8COVID19_2 ----------------------------------------------------------------
# * * Overall ------------------------------------------------------------------
Q8COVID19_2_frame_overall = ab6_data %>% 
  mutate(Q8COVID19_2 = ifelse(Q8COVID19_2%in%c(1,2),1, ifelse(Q8COVID19_2 %in% c(NA), NA, 0))) %>% 
  filter(!is.na(WT)) %>%
  group_by(country) %>%
  summarise(Q8COVID19_2= weighted.mean(Q8COVID19_2, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_2 = round(Q8COVID19_2*100)) %>%
  filter(!is.nan(Q8COVID19_2)) %>%
  arrange(desc(Q8COVID19_2))


Q8COVID19_2_frame_overall$country = factor(Q8COVID19_2_frame_overall$country, 
                                       levels = rev(as.character(Q8COVID19_2_frame_overall$country)))

country_lab_temp = c("Algeria", "Morocco", "Jordan", "Tunisia", "Lebanon")

overall_speech_plot = ggplot(Q8COVID19_2_frame_overall, aes(x = country,
                                                    y = Q8COVID19_2, 
                                                    fill = country)) +
  geom_col(position="dodge", na.rm = TRUE, show.legend = FALSE, fill = "#0098BE") + 
  coord_flip() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = '#545454'),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.x = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y = element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_2 , family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) + 
  ggtitle("Freedom of Speech during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to limit freedom of speech \n when there is a public health emergency"))


ggsave("Q8COVID19_2_Overall.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# * * By Age -------------------------------------------------------------------
Q8COVID19_2_frame_age = ab6_data %>% 
  mutate(Q8COVID19_2 = ifelse(Q8COVID19_2%in%c(1,2),1, ifelse(Q8COVID19_2 %in% c(NA), NA, 0))) %>% 
  mutate(Age = ifelse(Q1001 < 30, "18-29", ifelse(Q1001 >= 60, "60+", NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Age)) %>%
  group_by(country, Age) %>%
  summarise(Q8COVID19_2= weighted.mean(Q8COVID19_2, w=WT, na.rm=TRUE))%>%
  mutate(Q8COVID19_2 = round(Q8COVID19_2*100))%>%
  filter(!is.nan(Q8COVID19_2))%>%
  arrange(desc(Q8COVID19_2))

Q8COVID19_2_frame_age$country = factor(Q8COVID19_2_frame_age$country, 
                                       levels = rev(as.character(unique(Q8COVID19_2_frame_age$country))))

Q8COVID19_2_frame_age$Age = factor(Q8COVID19_2_frame_age$Age, 
                                   levels = rev(as.character(unique(Q8COVID19_2_frame_age$Age))))

country_lab_temp = c("Morocco", "Algeria", "Jordan", "Tunisia", "Lebanon")

speech_age_plot = ggplot(Q8COVID19_2_frame_age, aes(x = country,#x=reorder(country, -Q8COVID19_2),
                                                    y = Q8COVID19_2, fill = Age)) +
  geom_col(position="dodge", na.rm=TRUE, show.legend = TRUE) +
  scale_fill_manual(values = age_colors) + 
  coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold", size=16, color = '#545454'),
        plot.subtitle=element_text(hjust=0.5, face="italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_2 , family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) +
  ggtitle("Freedom of Speech during COVID-19")+
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to limit freedom of speech \n when there is a public health emergency"))+
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Q8COVID19_2_Age.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# * * By Gov't Trust -------------------------------------------------------------------
Q8COVID19_2_frame_trust = ab6_data %>% 
  mutate(Q8COVID19_2 = ifelse(Q8COVID19_2%in%c(1,2),1, ifelse(Q8COVID19_2 %in% c(NA), NA, 0))) %>% 
  mutate(Govt_Trust = ifelse(Q201A_1 <= 2,"Trust", ifelse(Q201A_1 >= 3, "Little to No Trust", NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Govt_Trust)) %>%
  group_by(country, Govt_Trust) %>%
  summarise(Q8COVID19_2= weighted.mean(Q8COVID19_2, w=WT, na.rm=TRUE))%>%
  mutate(Q8COVID19_2 = round(Q8COVID19_2*100))%>%
  filter(!is.nan(Q8COVID19_2)) %>%
  arrange(desc(Q8COVID19_2))

Q8COVID19_2_frame_trust$country = factor(Q8COVID19_2_frame_trust$country, 
                                       levels = rev(as.character(unique(Q8COVID19_2_frame_trust$country))))

Q8COVID19_2_frame_trust$Govt_Trust = factor(Q8COVID19_2_frame_trust$Govt_Trust, 
                                         levels = rev(as.character(unique(Q8COVID19_2_frame_trust$Govt_Trust))))

country_lab_temp = c("Tunisia", "Morocco", "Jordan", "Lebanon", "Algeria")

trust_speech_plot = ggplot(Q8COVID19_2_frame_trust, aes(x = country, #x=reorder(country, -Q8COVID19_2), 
                                                    y = Q8COVID19_2, fill = Govt_Trust)) +
  geom_col(position="dodge", na.rm=TRUE, show.legend = TRUE) +
  scale_fill_manual(values = trust_colors) + 
  coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold", size=16, color = '#545454'),
        plot.subtitle=element_text(hjust=0.5, face="italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_2 , family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = country_lab_temp) +
  ggtitle("Freedom of Speech during COVID-19")+
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to limit freedom of speech \n when there is a public health emergency"))+
  guides(fill = guide_legend(reverse = TRUE))

ggsave("Q8COVID19_2_Trust.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)



# * Q8COVID19_3 ----------------------------------------------------------------
# * * Overall ------------------------------------------------------------------
Q8COVID19_3_frame_overall = ab6_data %>% 
  mutate(Q8COVID19_3 = ifelse(Q8COVID19_3%in%c(1,2),1, ifelse(Q8COVID19_3 %in% c(NA), NA, 0))) %>% 
  filter(!is.na(WT)) %>%
  group_by(country) %>%
  summarise(Q8COVID19_3 = weighted.mean(Q8COVID19_3, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_3 = round(Q8COVID19_3*100)) %>%
  filter(!is.nan(Q8COVID19_3)) %>%
  arrange(desc(Q8COVID19_3))


Q8COVID19_3_frame_overall$country = factor(Q8COVID19_3_frame_overall$country, 
                                           levels = rev(as.character(Q8COVID19_3_frame_overall$country)))

country_lab_temp = c("Algeria", "Morocco", "Jordan", "Tunisia", "Lebanon")

overall_movement_plot = ggplot(Q8COVID19_3_frame_overall, aes(x = country,
                                                        y = Q8COVID19_3, 
                                                        fill = country)) +
  geom_col(position="dodge", na.rm = TRUE, show.legend = FALSE, fill = "#0098BE") + 
  coord_flip() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = '#545454'),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.x = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y = element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_3, family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) + 
  ggtitle("Freedom of Movement during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to limit freedom of movement \n when there is a public health emergency"))


ggsave("Q8COVID19_3_Overall.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)


# * * By Gender ----------------------------------------------------------------
Q8COVID19_3_gender_frame = ab6_data %>% 
  mutate(Q8COVID19_3 = ifelse(Q8COVID19_3 %in% c(1,2),1, ifelse(Q8COVID19_3 %in% c(NA), NA, 0)))%>% 
  mutate(Gender = ifelse(Q1002 == 2,"Women", ifelse(Q1002 == 1, "Men",NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Gender)) %>%
  group_by(country, Gender) %>%
  summarise(Q8COVID19_3= weighted.mean(Q8COVID19_3, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_3 = round(Q8COVID19_3*100)) %>%
  filter(!is.nan(Q8COVID19_3)) %>%
  arrange(desc(Q8COVID19_3))

Q8COVID19_3_gender_frame$country = factor(Q8COVID19_3_gender_frame$country, 
                                       levels = rev(as.character(unique(Q8COVID19_3_gender_frame$country))))

Q8COVID19_3_gender_frame$Gender = factor(Q8COVID19_3_gender_frame$Gender, 
                                          levels = rev(as.character(unique(Q8COVID19_3_gender_frame$Gender))))

country_lab_temp = c("Algeria", "Morocco", "Tunisia", "Jordan", "Lebanon")

movement_gender = ggplot(Q8COVID19_3_gender_frame, aes(x = country, #x = reorder(country, Q8COVID19_3), 
                                                       y = Q8COVID19_3, fill = Gender)) + 
  geom_col(position="dodge", na.rm=TRUE,show.legend = TRUE) +
  scale_fill_manual(values = gender_colors) + 
  coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold", size=16, color = '#545454'),
        plot.subtitle=element_text(hjust=0.5, face="italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_3, family = 'Montserrat'), size = 4, hjust = -.5, 
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) +
  ggtitle("Freedom of Movement during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to limit freedom of movement \n when there is a public health emergency")) +
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Q8COVID19_3_Gender.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# * * By Age -------------------------------------------------------------------
Q8COVID19_3_frame_age = ab6_data %>% 
  mutate(Q8COVID19_3 = ifelse(Q8COVID19_3 %in% c(1,2),1, ifelse(Q8COVID19_3 %in% c(NA), NA, 0))) %>% 
  mutate(Age = ifelse(Q1001 < 30,"18-29", ifelse(Q1001 >= 60, "60+", NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Age)) %>%
  group_by(country, Age) %>%
  summarise(Q8COVID19_3= weighted.mean(Q8COVID19_3, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_3 = round(Q8COVID19_3*100)) %>%
  filter(!is.nan(Q8COVID19_3)) %>%
  arrange(desc(Q8COVID19_3))

Q8COVID19_3_frame_age$country = factor(Q8COVID19_3_frame_age$country, 
                                       levels = rev(as.character(unique(Q8COVID19_3_frame_age$country))))

Q8COVID19_3_frame_age$Age = factor(Q8COVID19_3_frame_age$Age, 
                                       levels = rev(as.character(unique(Q8COVID19_3_frame_age$Age))))

levels(Q8COVID19_3_frame_age$country)
country_lab_temp = c("Morocco", "Algeria", "Tunisia", "Jordan", "Lebanon")


movement_age_plot = ggplot(Q8COVID19_3_frame_age, aes(x = country, #x=reorder(country, Q8COVID19_3), 
                                                    y = Q8COVID19_3, fill = Age)) +
  geom_col(position="dodge", na.rm=TRUE, show.legend = TRUE) +
  scale_fill_manual(values = age_colors) + 
  coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold", size=16, color = '#545454'),
        plot.subtitle=element_text(hjust=0.5, face="italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_3, family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) +
  ggtitle("Freedom of Movement during COVID-19")+
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to limit freedom of movement \n when there is a public health emergency"))+
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Q8COVID19_3_Age.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)


# * Q8COVID19_4 ----------------------------------------------------------------
# * * Overall ------------------------------------------------------------------

Q8COVID19_4_frame_overall = ab6_data %>% 
  mutate(Q8COVID19_4 = ifelse(Q8COVID19_4%in%c(1,2),1, ifelse(Q8COVID19_4 %in% c(NA), NA, 0))) %>% 
  filter(!is.na(WT)) %>%
  group_by(country) %>%
  summarise(Q8COVID19_4 = weighted.mean(Q8COVID19_4, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_4 = round(Q8COVID19_4*100)) %>%
  filter(!is.nan(Q8COVID19_4)) %>%
  arrange(desc(Q8COVID19_4))


Q8COVID19_4_frame_overall$country = factor(Q8COVID19_4_frame_overall$country, 
                                           levels = rev(as.character(Q8COVID19_4_frame_overall$country)))

country_lab_temp = c("Jordan", "Morocco", "Algeria", "Tunisia", "Lebanon")

overall_censorhship_plot = ggplot(Q8COVID19_4_frame_overall, aes(x = country,
                                                        y = Q8COVID19_4, 
                                                        fill = country)) +
  geom_col(position="dodge", na.rm = TRUE, show.legend = FALSE, fill = "#0098BE") +
  coord_flip() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = '#545454'),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.x = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y = element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_4, family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) + 
  ggtitle("Censorship of the Media during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is sometimes or always justifiable \n for the gov't to censor the media \n when there is a public health emergency"))


ggsave("Q8COVID19_4_Overall.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# * * By Age -------------------------------------------------------------------
Q8COVID19_4_frame_age = ab6_data %>% 
  mutate(Q8COVID19_4 = ifelse(Q8COVID19_4 %in% c(1,2),1, ifelse(Q8COVID19_4 %in% c(NA), NA, 0))) %>% 
  mutate(Age = ifelse(Q1001 < 30,"18-29", ifelse(Q1001 >= 60, "60+", NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Age))%>%
  group_by(country, Age)%>%
  summarise(Q8COVID19_4 = weighted.mean(Q8COVID19_4, w=WT, na.rm=TRUE))%>%
  mutate(Q8COVID19_4 = round(Q8COVID19_4*100))%>%
  filter(!is.nan(Q8COVID19_4)) %>%
  arrange(desc(Q8COVID19_4))

Q8COVID19_4_frame_age$country = factor(Q8COVID19_4_frame_age$country, 
                                       levels = rev(as.character(unique(Q8COVID19_4_frame_age$country))))

Q8COVID19_4_frame_age$Age = factor(Q8COVID19_4_frame_age$Age, 
                                       levels = rev(as.character(unique(Q8COVID19_4_frame_age$Age))))

country_lab_temp = c("Algeria", "Jordan", "Morocco", "Tunisia", "Lebanon")

media_censorship_age_plot = ggplot(Q8COVID19_4_frame_age, aes(x = country, #x=reorder(country, Q8COVID19_4), 
                                                              y = Q8COVID19_4, fill = Age)) +
  geom_col(position = "dodge", na.rm = TRUE, show.legend = TRUE) +
  scale_fill_manual(values = age_colors) + 
  coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust = 0.5, face = "bold", size = 16, color = '#545454'),
        plot.subtitle=element_text(hjust = 0.5, face = "italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_4, family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1))+
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) +
  ggtitle("Censorship of the Media during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is either sometimes or always justifiable \n for the gov't to censor the media \n when there is a public health emergency"))+
  guides(fill = guide_legend(reverse = TRUE))  

ggsave("Q8COVID19_4_Age.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)


# * Q8COVID19_5 ----------------------------------------------------------------
# * * Overall ------------------------------------------------------------------
Q8COVID19_5_frame_overall = ab6_data %>% 
  mutate(Q8COVID19_5 = ifelse(Q8COVID19_5%in%c(1,2),1, ifelse(Q8COVID19_5 %in% c(NA), NA, 0))) %>% 
  filter(!is.na(WT)) %>%
  group_by(country) %>%
  summarise(Q8COVID19_5= weighted.mean(Q8COVID19_5, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_5 = round(Q8COVID19_5*100)) %>%
  filter(!is.nan(Q8COVID19_5)) %>%
  arrange(desc(Q8COVID19_5))

Q8COVID19_5_frame_overall$country = factor(Q8COVID19_5_frame_overall$country, 
                                           levels = rev(as.character(Q8COVID19_5_frame_overall$country)))

country_lab_temp = c("Algeria", "Morocco", "Tunisia", "Jordan", "Lebanon")

overall_tracing_plot = ggplot(Q8COVID19_5_frame_overall, aes(x = country,
                                                        y = Q8COVID19_5, 
                                                        fill = country)) +
  geom_col(position="dodge", na.rm = TRUE, show.legend = FALSE, fill = "#0098BE") +
  coord_flip() + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = '#545454'),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.x = element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y = element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_5, family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) + 
  ggtitle("Gov't Tracing and Monitoring during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is either sometimes or always justifiable \n for the gov't to monitor or trace citizens' \n movements during a public health emergency"))

ggsave("Q8COVID19_5_Overall.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# * * By Gender ----------------------------------------------------------------
Q8COVID19_5_gender_frame = ab6_data %>% 
  mutate(Q8COVID19_5 = ifelse(Q8COVID19_5 %in% c(1,2),1, ifelse(Q8COVID19_5 %in% c(NA), NA, 0))) %>% 
  mutate(Gender = ifelse(Q1002 == 2,"Women", ifelse(Q1002 == 1, "Men",NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Gender)) %>%
  group_by(country,Gender) %>%
  summarise(Q8COVID19_5= weighted.mean(Q8COVID19_5, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_5 = round(Q8COVID19_5*100)) %>%
  filter(!is.nan(Q8COVID19_5)) %>%
  arrange(desc(Q8COVID19_5))

Q8COVID19_5_gender_frame$country = factor(Q8COVID19_5_gender_frame$country, 
                                       levels = rev(as.character(unique(Q8COVID19_5_gender_frame$country))))

Q8COVID19_5_gender_frame$Gender = factor(Q8COVID19_5_gender_frame$Gender, 
                                          levels = rev(as.character(unique(Q8COVID19_5_gender_frame$Gender))))

levels(Q8COVID19_5_gender_frame$country)
country_lab_temp = c("Algeria", "Morocco", "Tunisia", "Jordan", "Lebanon")

tracking_gender = ggplot(Q8COVID19_5_gender_frame, aes(x = country, #x=reorder(country, Q8COVID19_5), 
                                                       y=Q8COVID19_5, fill=Gender)) + 
  geom_col(position="dodge", na.rm=TRUE,show.legend = TRUE)+
  scale_fill_manual(values = gender_colors) + coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold", size=16, color = '#545454'),
        plot.subtitle=element_text(hjust=0.5, face="italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_5, family = 'Montserrat'), size = 4, hjust = -.5, 
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) +
  ggtitle("Gov't Tracing and Monitoring during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is either sometimes or always justifiable \n for the gov't to monitor or trace citizens' \n movements during a public health emergency")) +
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Q8COVID19_5_Gender.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)

# * * By Age -------------------------------------------------------------------
Q8COVID19_5_frame_age = ab6_data %>% 
  mutate(Q8COVID19_5 = ifelse(Q8COVID19_5 %in% c(1,2),1, ifelse(Q8COVID19_5 %in% c(NA), NA, 0))) %>% 
  mutate(Age = ifelse(Q1001 < 30,"18-29", ifelse(Q1001 >= 60, "60+", NA))) %>%
  filter(!is.na(WT)) %>%
  filter(!is.na(Age)) %>%
  group_by(country, Age) %>%
  summarise(Q8COVID19_5= weighted.mean(Q8COVID19_5, w=WT, na.rm=TRUE)) %>%
  mutate(Q8COVID19_5 = round(Q8COVID19_5*100)) %>%
  filter(!is.nan(Q8COVID19_5)) %>%
  arrange(Age,desc(Q8COVID19_5))

Q8COVID19_5_frame_age$country = factor(Q8COVID19_5_frame_age$country, 
                                       levels = rev(as.character(unique(Q8COVID19_5_frame_age$country))))

Q8COVID19_5_frame_age$Age = factor(Q8COVID19_5_frame_age$Age, 
                                       levels = rev(as.character(unique(Q8COVID19_5_frame_age$Age))))

country_lab_temp = c("Algeria", "Morocco", "Tunisia", "Jordan", "Lebanon")

monitoring_age_plot = ggplot(Q8COVID19_5_frame_age, aes(x = country, #x=reorder(country, Q8COVID19_5), 
                                                        y = Q8COVID19_5, fill = Age)) +
  geom_col(position="dodge", na.rm=TRUE, show.legend = TRUE) +
  scale_fill_manual(values = age_colors) + 
  coord_flip() +
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        legend.title = element_blank(),
        plot.title=element_text(hjust=0.5, face="bold", size=16, color = '#545454'),
        plot.subtitle=element_text(hjust=0.5, face="italic", color = '#545454'),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        text= element_text(family = 'Montserrat', size = 14, color = '#545454'),
        axis.text.y= element_text(family = 'Montserrat', size = 14, color = '#545454')) +
  geom_text(aes(label = Q8COVID19_5 , family = 'Montserrat'), size = 4, hjust = -.5,
            color = '#545454', position = position_dodge(width = 1)) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_discrete(labels = rev(country_lab_temp)) +
  ggtitle("Gov't Tracing and Monitoring during COVID-19") +
  labs(caption=c("Arab Barometer, Wave VI Part 1 (July-October 2020)"), 
       subtitle =c("% saying it is either sometimes or always justifiable \n for the gov't to monitor or trace citizens' \n movements during a public health emergency"))+
  guides(fill = guide_legend(reverse = TRUE)) 

ggsave("Q8COVID19_5_Age.png", plot = last_plot(),
       width = 7, height = 5, units = c("in"),
       dpi = 300, limitsize = TRUE)













# CLEANING PT. 2 ---------------------------------------------------------------
# Q1COVID19: concern about the spread of covid19 over the next 6 months
table(ab6_data$Q1COVID19)
ab6_data = subset(ab6_data, Q1COVID19 != 98 & Q1COVID19 != 99)

# flipping order from 1-4 = Very concerned to Not at all concerned, to 1-4 = Not at all concerned to Very concerned 
table(ab6_data$Q1COVID19)
ab6_data$Q1COVID19 = ab6_data$Q1COVID19 * -1 + 5
table(ab6_data$Q1COVID19)


# Q204_25: satisfaction w/ gov't performance on covid19
table(ab6_data$Q204_25)
ab6_data = subset(ab6_data, Q204_25 != 98 & Q204_25 != 99 & Q204_25 != 97)

# flipping order from 1-4 = Very good to very bad, to 1-4 = very bad to very good
table(ab6_data$Q204_25)
ab6_data$Q204_25 = ab6_data$Q204_25 * -1 + 5
table(ab6_data$Q204_25)


# Q6COVID19: trust in statistics on number of infections & deaths released by gov't
table(ab6_data$Q6COVID19)
ab6_data = subset(ab6_data, Q6COVID19 != 98 & Q6COVID19 != 99)

# flipping order from 1-4 = a great deal of trust to no trust at all, to 1-4 = no trust at all to a great deal of trust
table(ab6_data$Q6COVID19)
ab6_data$Q6COVID19 = ab6_data$Q6COVID19 * -1 + 5
table(ab6_data$Q6COVID19)


# Q201A_1: amount of trust in gov't
# table(ab6_data$Q201A_1)
# ab6_data = subset(ab6_data, Q201A_1 != 98 & Q201A_1 != 99)

# flipping order from 1-4 = a great deal of trust to no trust at all, to 1-4 = no trust at all to a great deal of trust
table(ab6_data$Q201A_1)
ab6_data$Q201A_1 = ab6_data$Q201A_1 * -1 + 5
table(ab6_data$Q201A_1)


# Q521_1: perceptions of freedom to express opinions
table(ab6_data$Q521_1)
ab6_data = subset(ab6_data, Q521_1 != 98 & Q521_1 != 99)

# flipping order from 1-4 = Guaranteed to a great extent to Not guaranteed at all, to 1-4 = not guaranteed at all to guaranteed to a great extent
table(ab6_data$Q521_1)
ab6_data$Q521_1 = ab6_data$Q521_1 * -1 + 5
table(ab6_data$Q521_1)


# Q521_2: perceptions of media freedom to criticize gov't
table(ab6_data$Q521_2)
ab6_data = subset(ab6_data, Q521_2 != 98 & Q521_2 != 99)

# flipping order from 1-4 = Guaranteed to a great extent to Not guaranteed at all, to 1-4 = not guaranteed at all to guaranteed to a great extent
table(ab6_data$Q521_2)
ab6_data$Q521_2 = ab6_data$Q521_2 * -1 + 5
table(ab6_data$Q521_2)


### response variables//variables of interest ###

# checking for & removing NAs across variables of interest
# table(ab6_data$Q8COVID19_2)/sum(table(ab6_data$Q8COVID19_2))
table(ab6_data$Q8COVID19_2)
ab6_data = subset(ab6_data, Q8COVID19_2 != 98 & Q8COVID19_2 != 99)

table(ab6_data$Q8COVID19_3)/sum(table(ab6_data$Q8COVID19_3))
ab6_data = subset(ab6_data, Q8COVID19_3 != 98 & Q8COVID19_3 != 99)

table(ab6_data$Q8COVID19_4)/sum(table(ab6_data$Q8COVID19_4))
ab6_data = subset(ab6_data, Q8COVID19_4 != 98 & Q8COVID19_4 != 99)

table(ab6_data$Q8COVID19_5)/sum(table(ab6_data$Q8COVID19_5))
ab6_data = subset(ab6_data, Q8COVID19_5 != 98 & Q8COVID19_5 != 99)


# ANALYSIS ---------------------------------------------------------------------

# 0 = never justifiable, 1 = always or sometimes justifiable
ab6_data %>% mutate(new_Q8COVID19_2 = 
                      case_when(Q8COVID19_2 <= 2 ~ 1,
                                Q8COVID19_2 == 3 ~ 0)) -> ab6_data

ab6_data %>% mutate(new_Q8COVID19_3 = 
                      case_when(Q8COVID19_3 <= 2 ~ 1,
                                Q8COVID19_3 == 3 ~ 0)) -> ab6_data

ab6_data %>% mutate(new_Q8COVID19_4 = 
                      case_when(Q8COVID19_4 <= 2 ~ 1,
                                Q8COVID19_4 == 3 ~ 0)) -> ab6_data

ab6_data %>% mutate(new_Q8COVID19_5 = 
                      case_when(Q8COVID19_5 <= 2 ~ 1,
                                Q8COVID19_5 == 3 ~ 0)) -> ab6_data

# Q1COVID19: concern about the spread of covid19 over the next 6 months
# Q204_25: satisfaction w/ gov't performance on covid19
# Q6COVID19: "How much trust do you have in the statistics on the number of infections and deaths released by the government?
# Q201A_1 amount of trust in gov't 
# Q521_1: perceptions of freedom of speech
# Q521_2: perceptions of media freedom to criticize gov't


# logistic regression
mod_Q8COVID19_2 = glm(new_Q8COVID19_2 
                    ~ country + gender + age_group + education_level + relig_level + 
                      Q1COVID19 + Q204_25 + Q6COVID19 + Q201A_1 + Q521_1 + Q521_2, 
                    data = ab6_data, weights = WT, family = binomial)
summary(mod_Q8COVID19_2)


mod_Q8COVID19_3 = glm(new_Q8COVID19_3 
                      ~ country + gender + age_group + education_level + relig_level + 
                        Q1COVID19 + Q204_25 + Q6COVID19 + Q201A_1 + Q521_1 + Q521_2, 
                      data = ab6_data, weights = WT, family = binomial)
summary(mod_Q8COVID19_3)


mod_Q8COVID19_4 = glm(new_Q8COVID19_4
                      ~ country + gender + age_group + education_level + relig_level + 
                        Q1COVID19 + Q204_25 + Q6COVID19 + Q201A_1 + Q521_1 + Q521_2, 
                      data = ab6_data, weights = WT, family = binomial)
summary(mod_Q8COVID19_4)


mod_Q8COVID19_5 = glm(new_Q8COVID19_5 
                      ~ country + gender + age_group + education_level + relig_level + 
                        Q1COVID19 + Q204_25 + Q6COVID19 + Q201A_1 + Q521_1 + Q521_2, 
                      data = ab6_data, weights = WT, family = binomial)
summary(mod_Q8COVID19_5)

