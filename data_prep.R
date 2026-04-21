# library
library(ggplot2)
library(dplyr)
# Data ----
# TODO: FUNCTION
df_strong = read.csv(file="CRONOS-3 Theme - Make it Strong/CRONOS-3 Theme - Make it Strong.csv")
df_strong$idunique = paste0(df_strong$idno, "_", df_strong$yrbrn, "_", df_strong$cntry)
df_strong = df_strong[, c("idunique", setdiff(names(df_strong), "idunique"))]
non_unique_ids = names(which(table(df_strong$idunique)>1))
nrow(df_strong[df_strong$idunique %in% non_unique_ids,]) # --> not the same people
View(df_strong)

df_equal = read.csv(file="CRONOS-3 Theme - Make it Equal/CRONOS-3 Theme - Make it Equal.csv")
df_equal$idunique = paste0(df_equal$idno, "_", df_equal$yrbrn, "_", df_equal$cntry)
df_equal = df_equal[, c("idunique", setdiff(names(df_equal), "idunique"))]
non_unique_ids = names(which(table(df_equal$idunique)>1))
nrow(df_equal[df_equal$idunique %in% non_unique_ids,]) # --> not the same people
View(df_equal)

df_healthy = read.csv(file="CRONOS-3 Theme - Make it Healthy/CRONOS-3 Theme - Make it Healthy.csv")
df_healthy$idunique = paste0(df_healthy$idno, "_", df_healthy$yrbrn, "_", df_healthy$cntry)
df_healthy = df_healthy[, c("idunique", setdiff(names(df_healthy), "idunique"))]
non_unique_ids = names(which(table(df_healthy$idunique)>1))
nrow(df_healthy[df_healthy$idunique %in% non_unique_ids,]) # --> not the same people
View(df_healthy)


length(intersect(df_strong$idunique, df_equal$idunique))
length(intersect(df_strong$idunique, df_healthy$idunique))
length(intersect(df_equal$idunique, df_healthy$idunique))

## all the same participants
common_cols = colnames(df_strong)[1:24]
df <- df_strong %>%
  left_join(df_equal[, c("idunique", "w4eq10")], by = "idunique") %>%
  left_join(df_healthy[, c("idunique", "w1hq8", "w3hq57")], by = "idunique")

# only using data where we have info on "How long to find first job after starting active search"
# --> variable w3sq69b must be category 1-4
df = df[df$w3sq69b %in% 1:4,]

df = df[df$age < 100, ]

# use birthyear instead of age --> set first year in dataset to 0
df$yrbrn0 = df$yrbrn - min(df$yrbrn)
df[df$agegroup35 == 9, "agegroup35"] = NA


# recode to factors
df$w4eq10[df$w4eq10 == 2] = 0
df$w4eq10[df$w4eq10 == 9] = NA
vars <- c("w1sq6_1", "w1sq6_2", "w1sq6_3", "w1sq6_4", "agegroup35", "w1sq11_1",
          "w1sq11_2", "w1sq11_3", "w1sq11_4", "w4eq10")

df <- df %>%
  mutate(across(all_of(vars), ~ factor(.)))

# Plots ---
## categories per country ---
round(prop.table(table(df$w3sq69b, df$cntry), margin = 2), 2)
tab <- prop.table(table(df$w3sq69b, df$cntry), margin = 2)

df_plot <- as.data.frame(tab)
names(df_plot) <- c("response", "country", "prop")

ggplot(df_plot, aes(x = country, y = prop, fill = factor(response))) +
  geom_bar(stat = "identity") +
  labs(x = "Country",
       y = "Proportion",
       fill = "Response category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


## categories per agegroups ---
# barplot agegroups
df$age_group <- cut(df$age,
                    breaks = c(15, 20, 25, 30, 35, 40),
                    labels = c("15–20", "21–25", "26–30", "31–35", "36–40"))

tab <- prop.table(table(df$w3sq69b, df$age_group), margin = 2)

df_plot <- as.data.frame(tab)
names(df_plot) <- c("response", "age_group", "prop")
ggplot(df_plot, aes(x = age_group, y = prop, fill = factor(response))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age group", y = "Proportion", fill = "Response") +
  theme_minimal()

# smooth
library(MASS)

model <- polr(as.factor(w3sq69b) ~ age, data = df, Hess = TRUE)
library(ggeffects)

plot(predict_response(model, "age"))

# line plot
df_plot <- df %>%
  group_by(age, w3sq69b) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot, aes(x = age, y = prop, color = factor(w3sq69b))) +
  labs(x = "Age", y = "Proportion", color = "Response") +
  theme_minimal() + geom_smooth(se = FALSE) # + geom_line()
# Model ---
#' EDUCATION
#' w1sq6_1 - Done during studies: Studied abroad
#' w1sq6_2 - Done during studies: Internship or work placement abroad
#' w1sq6_3 - Done during studies: Attended language course abroad
#' w1sq6_4 - Done during studies: Summer school, workshop or similar activity abroad
#'
#' w1sq7_1 - Stay abroad supported by mobility programme: Yes, by EU programme (e.g. Erasmus)
#' w1sq7_2 - Stay abroad supported by mobility programme: Yes, by national programme
#' w1sq7_3 - Stay abroad supported by mobility programme: Yes, by another programme
#'
#' w1sq11_1 - Ever participated in apprenticeship or internship: Yes, in unpaid apprenticeship
#' w1sq11_2 - Ever participated in apprenticeship or internship: Yes, in paid apprenticeship
#' w1sq11_3 - Ever participated in apprenticeship or internship: Yes, in unpaid internship
#' w1sq11_4 - Ever participated in apprenticeship or internship: Yes, in paid internship
#'
#'
#' w4sq1 - Importance of work in life

#' MENTAL HEALTH (healthy pillar)
#' w1hq8 - Needed to see psychologist, psychiatrist or other mental health services specialist last 12 months --> "healthy pillar"
#' w3hq57 - Subjective mental health in general
#'
#' FINANCIAL SITUATION (equal pillar)
#' w4eq10 - Expect to receive substantial inheritance in future --> indicator for financial situation in childhood


model1 = lm(w3sq69b ~ cntry + agegroup35 + #yrbrn0 +
     # Done during studies
     w1sq6_1 + w1sq6_2 + w1sq6_3 + w1sq6_4 +
     # Ever participated in apprenticeship or internship
     w1sq11_1 + w1sq11_2 + w1sq11_3 + w1sq11_4 +
    # Expect to receive substantial inheritance in future
     w4eq10, data = df
     ) #+ w1sq7_1 +w1sq7_2+w1sq7_3 +

summary(model1)
plot(model1)
# IDEEN:
#' in das modell:
#' w4sq1 - Importance of work in life --> einschätzung, ist es den leuten überhaupt wichtig?? --> das ist ein confounder (?)
#' Ist die Person Ausländer
#' Mental health --> allgemeine zufriedenheit im Leben
#' alter --> evtl nur für unter 35? oder alter nicht linear einbeziehen

# MAYBE ANALYSE MORE ON
#' w3sq71 - Main obstacle in finding first job

