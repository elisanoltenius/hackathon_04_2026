# library
library(ggplot2)
library(dplyr)
library(MASS) # polr function
# Data ----
## load pillars ----
df_strong = read.csv(file="CRONOS-3 Theme - Make it Strong/CRONOS-3 Theme - Make it Strong.csv")
df_strong$idunique = paste0(df_strong$idno, "_", df_strong$yrbrn, "_", df_strong$cntry)
df_strong = df_strong[, c("idunique", setdiff(names(df_strong), "idunique"))]
non_unique_ids = names(which(table(df_strong$idunique)>1))
nrow(df_strong[df_strong$idunique %in% non_unique_ids,]) # --> not the same people
# View(df_strong)

df_equal = read.csv(file="CRONOS-3 Theme - Make it Equal/CRONOS-3 Theme - Make it Equal.csv")
df_equal$idunique = paste0(df_equal$idno, "_", df_equal$yrbrn, "_", df_equal$cntry)
df_equal = df_equal[, c("idunique", setdiff(names(df_equal), "idunique"))]
non_unique_ids = names(which(table(df_equal$idunique)>1))
nrow(df_equal[df_equal$idunique %in% non_unique_ids,]) # --> not the same people
# View(df_equal)

df_healthy = read.csv(file="CRONOS-3 Theme - Make it Healthy/CRONOS-3 Theme - Make it Healthy.csv")
df_healthy$idunique = paste0(df_healthy$idno, "_", df_healthy$yrbrn, "_", df_healthy$cntry)
df_healthy = df_healthy[, c("idunique", setdiff(names(df_healthy), "idunique"))]
non_unique_ids = names(which(table(df_healthy$idunique)>1))
nrow(df_healthy[df_healthy$idunique %in% non_unique_ids,]) # --> not the same people
# View(df_healthy)

## merge data ----
length(intersect(df_strong$idunique, df_equal$idunique))
length(intersect(df_strong$idunique, df_healthy$idunique))
length(intersect(df_equal$idunique, df_healthy$idunique))

## all the same participants
common_cols = colnames(df_strong)[1:24]

# merge datasets
df = df_strong %>%
  left_join(df_equal[, c("idunique", "w4eq10")], by = "idunique") %>%
  left_join(df_healthy[, c("idunique", "w1hq8", "w3hq57")], by = "idunique")

# rename variables
df = df %>%
  rename(
    timetojob = w3sq69b,
    during_studies_abroad_studies = w1sq6_1,
    during_studies_abroad_work = w1sq6_2,
    during_studies_abroad_langcourse = w1sq6_3,
    during_studies_abroad_summerschool = w1sq6_4,
    workimportance_subj = w4sq1, # higher = not important
    apprent_unpaid = w1sq11_1,
    apprent_paid = w1sq11_2,
    intern_unpaid = w1sq11_3,
    intern_paid = w1sq11_4,
    psych_12month = w1hq8,
    mental_health_subj = w3hq57, # higher = bad
    inheritance = w4eq10,
    highested = eisced,
    bornincntry = w2sq18a,
    obstacle_firstjob = w3sq71
  )

# filter valid outcome
df = df[df$timetojob %in% 1:4, ]

# clean age
df = df[df$age < 100, ]

# birth year transformation
df$yrbrn0 = df$yrbrn - min(df$yrbrn, na.rm = TRUE)

# recode psych_12month
df$psych_12month[df$psych_12month %in% c(1,2)] = 1
df$psych_12month[df$psych_12month == 3] = 0
df$psych_12month[df$psych_12month == 9] = NA

# other missing recodes
df[df$agegroup35 == 9, "agegroup35"] = NA
df$mental_health_subj[df$mental_health_subj == 9] = NA
df$workimportance_subj[df$workimportance_subj == 9] = NA
df$eduyrs[df$eduyrs >= 77] = NA
df$highested[df$highested >= 55] = NA

# recode bornincntry
df$bornincntry[df$bornincntry == 2] = 0
df$bornincntry[df$bornincntry == 9] = NA

# recode inheritance
df$inheritance[df$inheritance == 2] = 0
df$inheritance[df$inheritance == 9] = NA

# variables to convert to factor
vars = c(
  "during_studies_abroad_studies",
  "during_studies_abroad_work",
  "during_studies_abroad_langcourse",
  "during_studies_abroad_summerschool",
  "agegroup35",
  "apprent_unpaid",
  "apprent_paid",
  "intern_unpaid",
  "intern_paid",
  "inheritance",
  "psych_12month",
  "mental_health_subj",
  "cntry",
  "bornincntry",
  "highested",
  "obstacle_firstjob"
)

df = df %>%
  mutate(across(all_of(vars), ~ factor(.)))

# overview
summary(df[, c("during_studies_abroad_studies", "during_studies_abroad_work",
               "during_studies_abroad_langcourse", "during_studies_abroad_summerschool",
               "agegroup35", "apprent_unpaid", "apprent_paid",
               "intern_unpaid", "intern_paid", "inheritance", "psych_12month",
               "mental_health_subj")])

# Model ----
#' EDUCATION
#' w1sq6_1 - Done during studies: Studied abroad -> during_studies_abroad_studies
#' w1sq6_2 - Done during studies: Internship or work placement abroad --> during_studies_abroad_work
#' w1sq6_3 - Done during studies: Attended language course abroad --> during_studies_abroad_langcourse
#' w1sq6_4 - Done during studies: Summer school, workshop or similar activity abroad --> during_studies_abroad_summerschool
#'
#' w1sq11_1 - Ever participated in apprenticeship or internship: Yes, in unpaid apprenticeship --> apprent_unpaid
#' w1sq11_2 - Ever participated in apprenticeship or internship: Yes, in paid apprenticeship
#' w1sq11_3 - Ever participated in apprenticeship or internship: Yes, in unpaid internship
#' w1sq11_4 - Ever participated in apprenticeship or internship: Yes, in paid internship
#'
#' eduyrs - Years of full-time education completed
#' w2sq18a - Respondent born in country --> indicator for migration backround
#' w4sq1 - Importance of work in life
#' w3sq71 - Main obstacle in finding first job
#'
#' MENTAL HEALTH (healthy pillar)
#' w1hq8 - Needed to see psychologist, psychiatrist or other mental health services specialist last 12 months --> "healthy pillar"
#' w3hq57 - Subjective mental health in general
#'
#' FINANCIAL SITUATION (equal pillar)
#' w4eq10 - Expect to receive substantial inheritance in future --> indicator for financial situation in childhood



## continuous outcome ----
# contrasts(df$cntry) = contr.treatment(nlevels(df$cntry)) # --> austria is reference category
# df$cntry = relevel(df$cntry, ref = "FR")  # France is reference category
contrasts(df$cntry) = contr.sum # --> deviation (sum-to-zero) coding (diff to overall mean)
model_cont = lm(timetojob ~ cntry +
                  during_studies_abroad_studies +
                  during_studies_abroad_work +
                  during_studies_abroad_langcourse +
                  during_studies_abroad_summerschool +
                  workimportance_subj +
                  apprent_unpaid +
                  apprent_paid +
                  intern_unpaid +
                  intern_paid +
                  psych_12month +
                  mental_health_subj +
                  inheritance, data = df
     ) #+ w1sq7_1 +w1sq7_2+w1sq7_3 +
summary(model_cont)

model_cont_35 = lm(timetojob ~ cntry +
                  during_studies_abroad_studies +
                  during_studies_abroad_work +
                  during_studies_abroad_langcourse +
                  during_studies_abroad_summerschool +
                  workimportance_subj +
                  apprent_unpaid +
                  apprent_paid +
                  intern_unpaid +
                  intern_paid +
                  psych_12month +
                  mental_health_subj +
                  inheritance, data = df[df$agegroup35 == 1,]
     ) #+ w1sq7_1 +w1sq7_2+w1sq7_3 +
summary(model_cont_35)

## categorical outcome ----
df$timetojob_fac = factor(df$timetojob,
                      levels = c(1, 2, 3, 4),
                      labels = c("<1 month", "1–6 months", "6–12 months", ">1 year"),
                      ordered = TRUE)
model_cat = polr(timetojob_fac ~ cntry + yrbrn0 +
                  during_studies_abroad_studies +
                  during_studies_abroad_work +
                  during_studies_abroad_langcourse +
                  during_studies_abroad_summerschool +
                  workimportance_subj +
                  apprent_unpaid * inheritance+
                  apprent_paid * inheritance+
                  intern_unpaid * inheritance+
                  intern_paid * inheritance +
                  eduyrs + bornincntry +
                  mental_health_subj, data = df, Hess = TRUE)

summary(model_cat)
coefs = summary(model_cat)$coefficients

p_values = 2 * pnorm(abs(coefs[, "t value"]), lower.tail = FALSE)

cbind(exp(coefs[,1]), p_value = p_values, "<0.01" = p_values < 0.01, "<0.05" = p_values < 0.05)

coef_values = coefficients(model_cat)
coef_values_all = c(coef_values[1:10], -1* sum(coef_values[1:10]), coef_values[11:length(coef_values)])
names(coef_values_all)[1:11]  = paste0("cntry", levels(df$cntry))
OR = exp(coef_values_all)
print(OR)

# Intepretation:
# internships and apprenticeships are more strongly associated with faster job entry among individuals without expected inheritance, while these effects are weaker among those with stronger financial security.”
# inheritance buffers the disadvantage of unpaid internships and apprenticeship in timing to employment
# 1.Inheritance expectation → slower labour market entry: people with financial safety are less pressured to transition quickly
# 2. Work experience matters less when financial safety is high: apprenticeship/internship effects are context-dependent
# 3. Strongest pattern:  Labour market integration mechanisms (internships/apprenticeships) are more important for those without financial safety nets.

# TODO:
#' during_studies_... variablen ersetzen/ergänzen mit ob any(w1sq7_1, w1sq7_2, w1sq7_3)
#' intern_paid1 und unpaid zu any zusammenführen
#' apprent_unpaid1 und unpaid zsmführen
#'
# IDEEN:
#' in das modell:
#' Ist die Person Ausländer
#' alter --> evtl nur für unter 35? oder alter nicht linear einbeziehen

# MAYBE ANALYSE MORE ON
#' w3sq71 - Main obstacle in finding first job


## categorical outcome simplified----
df = df %>%
  mutate(
    study_abroad = case_when(
      w1sq6_6 == 1 ~ NA_real_,
      during_studies_abroad_studies == 1 |
      during_studies_abroad_work == 1 |
      during_studies_abroad_langcourse == 1 |
      during_studies_abroad_summerschool == 1 ~ 1,
      w1sq6_5 == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    apprent = case_when(
      apprent_unpaid == 1 | apprent_paid == 1 ~ 1,
      apprent_unpaid == 0 & apprent_paid == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    intern = case_when(
      intern_unpaid == 1 | intern_paid == 1 ~ 1,
      intern_unpaid == 0 & intern_paid == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )
model_cat_2 = polr(timetojob_fac ~ cntry + yrbrn0 +
                  study_abroad +
                  workimportance_subj +
                  apprent* inheritance+
                  intern * inheritance +
                  psych_12month +
                  mental_health_subj, data = df, Hess = TRUE)
summary(model_cat_2)
coefs = summary(model_cat_2)$coefficients

p_values = 2 * pnorm(abs(coefs[, "t value"]), lower.tail = FALSE)

cbind(coefs, p_value = p_values)
p_values < 0.01

coef_values = coefficients(model_cat_2)
coef_values_all = c(coef_values[1:10], -1* sum(coef_values[1:10]), coef_values[11:length(coef_values)])
names(coef_values_all)[1:11]  = paste0("cntry", levels(df$cntry))
OR = exp(coef_values_all)
print(OR)
# Plots ----
## categories per country ----
round(prop.table(table(df$w3sq69b, df$cntry), margin = 2), 2)
tab = prop.table(table(df$w3sq69b, df$cntry), margin = 2)

df_plot = as.data.frame(tab)
names(df_plot) = c("response", "country", "prop")

ggplot(df_plot, aes(x = country, y = prop, fill = factor(response))) +
  geom_bar(stat = "identity") +
  labs(x = "Country",
       y = "Proportion",
       fill = "Response category") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


## categories per agegroups ----
# barplot agegroups
df$age_group = cut(df$age,
                    breaks = c(15, 20, 25, 30, 35, 40),
                    labels = c("15–20", "21–25", "26–30", "31–35", "36–40"))

tab = prop.table(table(df$w3sq69b, df$age_group), margin = 2)

df_plot = as.data.frame(tab)
names(df_plot) = c("response", "age_group", "prop")
ggplot(df_plot, aes(x = age_group, y = prop, fill = factor(response))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age group", y = "Proportion", fill = "Response") +
  theme_minimal()

# smooth
library(MASS)

model = polr(as.factor(w3sq69b) ~ age, data = df, Hess = TRUE)
library(ggeffects)

plot(predict_response(model, "age"))

# line plot
df_plot = df %>%
  group_by(age, w3sq69b) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot, aes(x = age, y = prop, color = factor(w3sq69b))) +
  labs(x = "Age", y = "Proportion", color = "Response") +
  theme_minimal() + geom_smooth(se = FALSE) # + geom_line()

## timetojob, country, mental health ----
library(tidyr)

df$timetojob = factor(
  df$timetojob,
  ordered = TRUE
)

# prediction grid varyiing country and mental health
newdata = expand.grid(
  cntry = levels(df$cntry),
  mental_health_subj = sort(unique(df$mental_health_subj)),

  during_studies_abroad_studies = levels(df$during_studies_abroad_studies)[1],
  during_studies_abroad_work = levels(df$during_studies_abroad_work)[1],
  during_studies_abroad_langcourse = levels(df$during_studies_abroad_langcourse)[1],
  during_studies_abroad_summerschool = levels(df$during_studies_abroad_summerschool)[1],

  apprent_unpaid = levels(df$apprent_unpaid)[1],
  apprent_paid = levels(df$apprent_paid)[1],
  intern_unpaid = levels(df$intern_unpaid)[1],
  intern_paid = levels(df$intern_paid)[1],

  psych_12month = levels(df$psych_12month)[1],
  inheritance = levels(df$inheritance)[1],

  workimportance_subj = mean(df$workimportance_subj, na.rm = TRUE)
)

# predicted probabilities
pred = predict(model_cat, newdata = newdata, type = "probs")
plot_data = cbind(newdata, pred) %>%
  mutate(prob_fast = `<1 month`) %>%   # category 1 = fastest job entry
  dplyr::select(cntry, mental_health_subj, prob_fast)

ggplot(plot_data, aes(
  x = mental_health_subj,
  y = prob_fast,
  group = cntry,
  color = cntry
)) +
  geom_line() +
  labs(
    x = "Mental health (higher = worse)",
    y = "Probability of job within 1 month",
    color = "Country"
  ) +
  theme_minimal()
