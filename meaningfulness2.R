# window
setwd('G:/내 드라이브/CNL/Meeting/meaningfulness/M2data')
# mac
# setwd('/Users/elisabeth/Library/CloudStorage/GoogleDrive-kmin0531@yonsei.ac.kr/내 드라이브/CNL/Meeting/Meaningfulness/M2data')

pacman::p_load(tidyverse, psych, knitr, rstatix, gghalves, ggpubr, BayesFactor)
options(dplyr.summarise.inform=FALSE) # suppress warning in regards to regrouping 

options(knitr.kable.NA = '')
klippy::klippy()

## Excluding Ss
rm_subject <- function(df, rx){
  for (i in rx){
    df <- df %>% filter(SN != i) %>% droplevels()
  }
  cat(sprintf('%d removed & %d left', 
              length(unique(rx)),
              length(unique(df$SN))))
  return(df)
}

## Plot
# stat summary plot to 25% quartile and 75% quartile
# https://bit.ly/3iFpV07
single_raincloud_plot <- function(df, Y, xMin, xMax, xBy, xLab){
  df %>% ggplot(aes(x = 1, y = Y)) +
    geom_half_violin(aes(y = Y), side = "r", 
                     color = "grey70", fill = "grey70") +
    geom_half_point(aes(y = Y), side = "l", size = 2,
                    color = "grey50", fill = "grey50", alpha = .5) +
    geom_pointrange(stat = "summary",
                    fun.min = function(z) {quantile(z,0.25)},
                    fun.max = function(z) {quantile(z,0.75)},
                    fun = median, color = "darkred", size = 1) +
    scale_y_continuous(breaks=seq(xMin,xMax,by=xBy)) +
    coord_flip(ylim = c(xMin, xMax), clip = "on") +
    labs(y = xLab) +
    theme_bw(base_size = 18) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          aspect.ratio = .3)
}

# data_exp_175175-v3_questionnaire-ndkl   동의서, 성별, 나이
# data_exp_175175-v3_task-4yt9  색시각 검사
# data_exp_175175-v3_task-r1gb  INT1SCR2_v01INT1
# data_exp_175175-v3_task-sg2x  INT1SCR2_v04SCR2
# data_exp_175175-v3_task-rxia  INT2SCR1_v02INT2
# data_exp_175175-v3_task-rjjm  INT2SCR1_v03SCR1
# data_exp_175175-v3_task-6b3e  SCR1INT2_v03SCR1
# data_exp_175175-v3_task-mn6l  SCR1INT2_v02INT2
# data_exp_175175-v3_task-7rew  SCR2INT1_v04SCR2
# data_exp_175175-v3_task-h2s7  SCR2INT1_v01INT1


# 1 Overflow Error -----------------------------------------------------------
demographic <- do.call(rbind, lapply(list.files(pattern = "*ndkl.csv"), read.csv))
dim(demographic)

# 필요한 자료만 남긴다.
d <- demographic %>% 
  filter(Key == "value") %>% 
  select(Experiment.Version,
         Participant.Private.ID,
         allocator.3ka9,
         Response,
         Object.ID) %>% 
  rename(Ver = Experiment.Version,
         Subject = Participant.Private.ID,
         Group = allocator.3ka9,
         Query = Object.ID) %>%
  mutate(Query = factor(Query, 
                        levels = c('object-7', 'object-8'), 
                        labels = c('gender', 'age'))) %>% 
  pivot_wider(id_cols = c(Ver, Subject, Group), 
              names_from = Query, 
              values_from = Response) %>%
  mutate(Subject = factor(Subject),
         Group = factor(Group),
         gender = factor(gender),
         age = as.numeric(age))
headTail(d)

str(d)

length(unique(d$Subject)) # 총 140명

table(d$gender)

summary(d$age)

table(d$Group, d$Ver) # ver.3의 Overflow는 모두 SCR2INT1

# Overflow 50명 중에서 "참가시간 순 15+12=27명"을 제외한 ID
# Accuracy outlier 1명이 Overflow이므로, Overflow에 한 명을 더 남겨야 한다.
# Participant.Private.ID= 10895494, global accuracy = 0.491
idOverflowExclusive = d %>% 
  filter(Group == "Overflow") %>% 
  slice_max(Subject, n = 51-27-1) %>% 
  select(Subject)

d2 <- d %>% 
  # Overflow 집단에서 순서가 느린 (50-27)명을 분석에서 잠정 제외한다.
  filter(!(Subject %in% idOverflowExclusive$Subject)) %>% 
  # Participant.Private.ID= 10895494, global accuracy = 0.491
  filter(!(Subject==10895494)) %>% 
  mutate(Group2 = Group,
         Group2 = replace(Group2, Group2=="Overflow", "SCR2INT1")) %>% 
  droplevels()

table(d2$Group, d2$Ver)
table(d2$Group2, d2$Ver) # 이제 모든 집단 29명씩.

## 1.1 N = 116 -------------------------------------------------------------
length(unique(d2$Subject)) # 참가자 수
table(d2$gender) # 성별 
summary(d2$age) # 나이 
d2 %>% single_raincloud_plot(.$age, 18, 36, 2, "Age") # 나이 분포

# 2 Main data -------------------------------------------------------------
g1 <- bind_rows(list(
  read.csv('data_exp_175175-v3_task-r1gb.csv', header = T), # v01INT1
  read.csv('data_exp_175175-v4_task-r1gb.csv', header = T),
  read.csv('data_exp_175175-v5_task-r1gb.csv', header = T),
  read.csv('data_exp_175175-v3_task-sg2x.csv', header = T), # v04SCR2
  read.csv('data_exp_175175-v4_task-sg2x.csv', header = T),
  read.csv('data_exp_175175-v5_task-sg2x.csv', header = T)))
g2 <- bind_rows(list(
  read.csv('data_exp_175175-v3_task-rxia.csv', header = T), # v02INT2
  read.csv('data_exp_175175-v4_task-rxia.csv', header = T),
  read.csv('data_exp_175175-v5_task-rxia.csv', header = T),
  read.csv('data_exp_175175-v3_task-rjjm.csv', header = T), # v03SCR1
  read.csv('data_exp_175175-v4_task-rjjm.csv', header = T),
  read.csv('data_exp_175175-v5_task-rjjm.csv', header = T)))
g3 <- bind_rows(list(
  read.csv('data_exp_175175-v3_task-6b3e.csv', header = T), # v03SCR1
  read.csv('data_exp_175175-v4_task-6b3e.csv', header = T),
  read.csv('data_exp_175175-v5_task-6b3e.csv', header = T),
  read.csv('data_exp_175175-v3_task-mn6l.csv', header = T), # v02INT2
  read.csv('data_exp_175175-v4_task-mn6l.csv', header = T),
  read.csv('data_exp_175175-v5_task-mn6l.csv', header = T)))
g4 <- bind_rows(list(
  read.csv('data_exp_175175-v3_task-7rew.csv', header = T), # v04SCR2
  read.csv('data_exp_175175-v4_task-7rew.csv', header = T),
  read.csv('data_exp_175175-v5_task-7rew.csv', header = T),
  read.csv('data_exp_175175-v3_task-h2s7.csv', header = T), # v01INT1
  read.csv('data_exp_175175-v4_task-h2s7.csv', header = T),
  read.csv('data_exp_175175-v5_task-h2s7.csv', header = T)))

dim(g1)
dim(g2)
dim(g3)
dim(g4)

str(g1)

mainTest <- bind_rows(list(g1, g2, g3, g4))
dim(mainTest)

str(mainTest)

# 필요한 자료만 남긴다. 
m <- mainTest %>% 
  filter(str_detect(Display, "up") | str_detect(Display, "down") ) %>% 
  filter(Screen == "probe") %>% 
  select(Experiment.Version,
         Participant.Private.ID,
         allocator.3ka9,
         Display,
         Spreadsheet..object1,
         Reaction.Time,
         Spreadsheet..key,
         Response,
         Correct) %>% 
  rename(Ver = Experiment.Version,
         Subject = Participant.Private.ID,
         Grp = allocator.3ka9,
         Object1 = Spreadsheet..object1,
         Cond = Display,
         RT = Reaction.Time, 
         TargLoc = Spreadsheet..key)

unique(m$Grp)

table(m$Grp, m$Ver)/192

headTail(m)

# Overflow 집단에서 순서가 느린 (51-15-12-1)명을 분석에서 잠정 제외한다.
m2 <- m %>% 
  filter(!(Subject %in% idOverflowExclusive$Subject)) %>% 
  mutate(Group = Grp,
         Group = replace(Group, Group=="Overflow", "SCR2INT1")) %>% 
  droplevels() %>% 
  # Intact 구획과 Scrambled 구획을 구분한다.
  mutate(Stimulus = str_detect(Object1, "_scram")) %>% 
  mutate(Stimulus = factor(Stimulus, 
                           levels = c(FALSE, TRUE), 
                           labels = c('Intact', 'Scrambled'))) %>%
  # Conjunction vs. Disjunction 조건을 구분한다.
  mutate(Condition = str_detect(Cond, "DIS")) %>% 
  mutate(Condition = factor(Condition, 
                            levels = c(FALSE, TRUE), 
                            labels = c('Conjunction', 'Disjunction'))) %>% 
  # 다루기 쉽도록 참가자에게 고유번호 부여
  group_by(Subject) %>% 
  mutate(SN = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(SN = factor(SN), 
         Subject = factor(Subject),
         Group = factor(Group)) %>% 
  select(SN, Ver, Subject, Group, Stimulus, Condition, RT, TargLoc, Response, Correct)

table(m2$Group, m2$Ver)/192 # 각 참가자 96시행 x 2구획; 네 집단에 29명씩 남았다.

headTail(m2)

# 이 참가자는 global accuracy가 우연수준보다 낮다. SN = 24
m2 %>% filter(Subject==10895494) %>% 
  summarise(Accuracy = mean(Correct))

m2 %>% filter(Subject==10895494) %>% 
  group_by(Stimulus, Condition) %>% 
  summarise(Accuracy = mean(Correct)) %>% 
  ungroup()

m2 <- m2 %>% filter(!(Subject==10895494))
table(m2$Group, m2$Ver)/192

table(m2$Stimulus)
table(m2$Condition)
length(unique(m2$Subject))
length(unique(m2$SN))
table(m2$Condition, m2$SN)

## 2.0.1 Anticipatory/Late RTs ---------------------------------------------
# 250msec보다 빠른 반응을 early로 표시
m2 %>%
  mutate(Response = replace(Response, RT<=250, "early")) %>% 
  filter(Response == "early") %>% 
  get_summary_stats(RT, type = "common")

# Gorilla가 late으로 표시한 반응
m2 %>% filter(Response == "late") %>% 
  get_summary_stats(RT, type = "common")

# early로 표시한 anticipatory error를 분석에서 제외.
# late 반응은 incorrect 반응으로 처리.
m2 <- m2 %>% filter(RT > 250)
table(m2$Response)

# late 응답을 제외한 전체 참가자 반응시간 분포 
m2 %>% 
  filter(!(Response=="late")) %>% 
  ggplot(aes(x = as.factor(SN), y = RT)) +
  geom_boxplot() +
  geom_hline(yintercept = 3000, color = 'red') +
  coord_flip()


## 2.0.2 Global Accuracy ---------------------------------------------------
# 조건 구분 없이 개별 참가자의 정확율 분포
m.gacc <- m2 %>%
  group_by(SN, Subject) %>% 
  summarise(M = mean(Correct)) %>% 
  ungroup() 

m.sum <- m.gacc %>% 
  summarise(MN = mean(M)
            , SD = sd(M)
            , MIN = min(M)
            , MAX = max(M)
            , Q1 = quantile(M, prob = .25)
            , MED = median(M)
            , Q3 = quantile(M, prob = .75)
            , IQR = IQR(M)
            , Outlier = Q1 - 1.5 * IQR
            , Extreme = Q1 - 3 * IQR) 
m.sum %>% 
  kable(digits = 2, caption = "Global accuracy: Distribution")

m.gacc %>% 
  single_raincloud_plot(.$M, 0.5, 1, 0.1, "Accuracy") + 
  geom_hline(yintercept=m.sum$Outlier, linetype="dotted") +
  geom_hline(yintercept=m.sum$Extreme, linetype='dashed', color='red', linewidth=0.5)

m.gacc %>% identify_outliers(M) # SN = 30,60

m2 %>% filter(SN==30) %>% slice(1L)
m2 %>% filter(SN==66) %>% slice(1L)

m2 %>% group_by(Group, Ver) %>% 
  summarise(Case = length(unique(Subject))) %>% 
  pivot_wider(names_from = "Ver", values_from = "Case")

# 3 Accuracy --------------------------------------------------------------
headTail(m2)

m.ind <- m2 %>% 
  group_by(SN, Stimulus, Condition) %>% 
  summarise(Accuracy = mean(Correct)) %>% 
  ungroup()

headTail(m.ind)

m.ind %>% 
  group_by(Stimulus, Condition) %>% 
  get_summary_stats(Accuracy, type = "common") %>%
  kable(digits = 3, format = "simple", caption = "Descriptive Stats")

df.w <- m.ind %>% pivot_wider(id_cols = c('SN', 'Condition'),
                              names_from = 'Stimulus', values_from = 'Accuracy')

ggplot(data=m.ind, aes(x = Condition, y = Accuracy, fill = Stimulus)) +
  geom_violin(width = 0.5, trim = TRUE) +
  geom_point(position=position_dodge(0.5), 
             color="black", alpha=0.2, size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(df.w, Condition=="Conjunction"), inherit.aes = FALSE,
               aes(x=1-.12, y=filter(df.w, Condition=="Conjunction")$Intact,
                   xend=1+.12, yend=filter(df.w, Condition=="Conjunction")$Scrambled),
               color="black", alpha=0.2) +
  geom_segment(data=filter(df.w, Condition=="Disjunction"), inherit.aes = FALSE,
               aes(x=2-.12, y=filter(df.w, Condition=="Disjunction")$Intact,
                   xend=2+.12, yend=filter(df.w, Condition=="Disjunction")$Scrambled),
               color="black", alpha=0.2) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(0.5), 
               colour = "darkred", linewidth = 1, size = 1, show.legend = FALSE) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"),
                    labels=c("Intact", "Scrambled")) +
  labs(x = "Condition",
       y = "Accuracy",
       fill='Stimulus Type') +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## 3.1 ANOVA ---------------------------------------------------------------
res.aov <- anova_test(
  data = m.ind, dv = Accuracy, wid = SN,
  within = c(Stimulus, Condition),
  effect.size = "pes"
)
get_anova_table(res.aov) %>% 
  kable(digits = 3, format = "simple", caption = "ANOVA")

# Disjunction만 t-test
DisInt <- m.ind[m.ind$Stimulus == 'Intact' & m.ind$Condition == 'Disjunction', 'Accuracy']
DisScr <- m.ind[m.ind$Stimulus == 'Scrambled' & m.ind$Condition == 'Disjunction', 'Accuracy']

t.test(DisInt, DisScr)

# 4 Dprime ----------------------------------------------------------------
headTail(m2)

m2 %>% group_by(SN, Stimulus, Condition, TargLoc) %>% 
  summarise(SUM = sum(Correct),
            N = n(),
            EDGE = SUM==N) %>% 
  ungroup() %>% 
  filter(EDGE==TRUE) %>% 
  print(n = Inf)

# Log-linear rule (Hautus, 1995, BRMIC) 
# 아래는 Uitvlugt, Pleskac, & Ravizza (CABN, 2016)
# Due to an overall ceiling effect in accuracy, an edge correction transformation 
# was applied to the dataset. We used the log-linear rule (Hautus, 1995) where 
# the frequency of hit, miss, false alarm, and correct rejection trials for 
# each condition of interest in each participant was calculated and 0.5 was added 
# to each of the four counts for each trial type. Then, hit rates and false alarm 
# rates were calculated in the typical manner.

m.ind2 <- m2 %>% group_by(SN, Stimulus, Condition, TargLoc) %>% 
  summarise(SUM = sum(Correct) + 0.5,
            N = n() + 1) %>% 
  ungroup() %>%
  mutate(ACC = SUM/N) %>%
  pivot_wider(id_cols = c(SN, Stimulus, Condition),
              names_from = TargLoc,
              values_from = ACC) %>% 
  mutate(DP = (qnorm(left) - qnorm(1-right))/sqrt(2))

headTail(m.ind2)
str(m.ind2)
dim(m.ind2)

m.ind2 %>% 
  group_by(SN, Stimulus, Condition) %>% 
  identify_outliers(DP) # 가외치 없음

m.ind2 %>% 
  group_by(Stimulus, Condition) %>% 
  get_summary_stats(DP, type = "common") %>%
  kable(digits = 3, format = "simple", caption = "Descriptive Stats")

df.w <- m.ind2 %>% pivot_wider(id_cols = c('SN', 'Condition'),
                               names_from = 'Stimulus', values_from = 'DP')

ggplot(data=m.ind2, aes(x = Condition, y = DP, fill = Stimulus)) +
  geom_violin(width = 0.5, trim = TRUE) +
  geom_point(position=position_dodge(0.5), 
             color="black", alpha=0.2, size=1.8, show.legend = FALSE) +
  geom_segment(data=filter(df.w, Condition=="Conjunction"), inherit.aes = FALSE,
               aes(x=1-.12, y=filter(df.w, Condition=="Conjunction")$Intact,
                   xend=1+.12, yend=filter(df.w, Condition=="Conjunction")$Scrambled),
               color="black", alpha=0.2) +
  geom_segment(data=filter(df.w, Condition=="Disjunction"), inherit.aes = FALSE,
               aes(x=2-.12, y=filter(df.w, Condition=="Disjunction")$Intact,
                   xend=2+.12, yend=filter(df.w, Condition=="Disjunction")$Scrambled),
               color="black", alpha=0.2) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(0.5), 
               colour = "darkred", linewidth = 1, size = 1, show.legend = FALSE) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9"),
                    labels=c("Intact", "Scrambled")) +
  labs(x = "Condition",
       y = "DP",
       fill='Stimulus Type') +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# 학회 포스터 그래프 --------------------------------------------------------------
# 예상 그래프
## meaningfulness effect X
eff.no <- data.frame(Condition = c('Conjunction', 'Conjunction', 'Disjunction', 'Disjunction'),
                     Stimulus = c('Intact','Scrambled','Intact','Scrambled'),
                     DP = c(1.26, 1.14, 1.13, 1.11)) 

eff.no %>% ggplot(aes(x = Condition, y = DP, group = Stimulus)) +
  geom_point(aes(shape=Stimulus, color=Stimulus), 
             size = 10) +
  geom_line(aes(color=Stimulus),size = 6) +
  theme_light(base_size = 18) +
  scale_color_manual(values=c("#F3B336", "#BBD135"),
                     labels=c("Intact", "Scrambled")) +
  scale_shape_manual(values=c(16, 15)) +
  labs(y = "d'",
       color = 'Stimulus Type',
       shape = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=40),
        axis.text.x  = element_text(size=40),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 16),
        legend.position = 'top') +
  coord_fixed(ratio = 12)

## meaningfulness effect O
eff.yes<- data.frame(Condition = c('Conjunction', 'Conjunction', 'Disjunction', 'Disjunction'),
                     Stimulus = c('Intact','Scrambled','Intact','Scrambled'),
                     DP = c(1.26, 1.13, 1.09, 0.96)) 

eff.yes %>% ggplot(aes(x = Condition, y = DP, group = Stimulus)) +
  geom_point(aes(shape=Stimulus, color=Stimulus), 
             size = 10) +
  geom_line(aes(color=Stimulus),size = 6) +
  theme_light(base_size = 18) +
  scale_color_manual(values=c("#F3B336", "#BBD135"),
                     labels=c("Intact", "Scrambled")) +
  scale_shape_manual(values=c(16, 15)) +
  labs(y = "d'",
       color = 'Stimulus Type',
       shape = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=40),
        axis.text.x  = element_text(size=40),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 16),
        legend.position = 'top') +
  coord_fixed(ratio = 6)

# point & line plot
ind2.graph <- m.ind2 %>% 
  group_by(Condition, Stimulus) %>% 
  summarise(dp.mean = mean(DP),
            dp.sd = sd(DP),
            n = length(DP)) %>% 
  mutate(dp.se = dp.sd/sqrt(n))

ind2.graph %>% ggplot(aes(x = Condition, y = dp.mean, group = Stimulus)) +
  stat_summary(fun = mean, 
               geom = "point", 
               aes(shape=Stimulus, color=Stimulus), 
               size = 6) +
  stat_summary(fun.y= mean,
               geom = "line",
               aes(color=Stimulus),
               size = 3) +
  theme_light(base_size = 18) +
  scale_color_manual(values=c("#F3B336", "#BBD135"),
                      labels=c("Intact", "Scrambled")) +
  scale_shape_manual(values=c(16, 15)) +
  labs(y = "d'",
       color = 'Stimulus Type',
       shape = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=32),
        axis.text.x  = element_text(size=32),
        axis.text.y = element_text(size=28),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        legend.position = 'top') +
  scale_y_continuous(limits = c(0.9, 1.3),breaks = seq(0.9, 1.3, by = 0.1)) +
  coord_fixed(ratio = 6)

# point & line plot + individual points
m.ind2 %>% ggplot(aes(x = Condition, y = DP, group = Stimulus)) +
  geom_point(position=position_dodge(0.5),
             aes(shape = Stimulus, color=Stimulus),
             alpha=0.2, size= 3, show.legend = FALSE) +
  stat_summary(fun = mean, 
               geom = "point", 
               position=position_dodge(0.5),
               aes(shape=Stimulus, color=Stimulus), 
               size = 6) +
  stat_summary(fun.y= mean,
               geom = "line",
               position=position_dodge(0.5),
               aes(color=Stimulus),
               size = 3) +
  theme_light(base_size = 18) +
  scale_color_manual(values=c("#F3B336", "#1E5D9B"),
                     labels=c("Intact", "Scrambled")) +
  scale_shape_manual(values=c(16, 15)) +
  labs(y = "d'",
       color = 'Stimulus Type',
       shape = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=32),
        axis.text.x  = element_text(size=32),
        axis.text.y = element_text(size=28),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        legend.position = 'top') +
  coord_fixed(ratio = 0.4)

# boxplot
m.ind2 %>% ggplot(aes(x = Condition, y = DP, color = Stimulus)) +
  geom_boxplot(aes(color = Stimulus)) +
  geom_point(position=position_dodge(0.6),
             aes(color = Stimulus),
             alpha=0.2, size= 3, show.legend = FALSE) +
  # stat_summary(fun.y = mean, 
  #              geom = "point", 
  #              position=position_dodge(0.5),
  #              aes(fill = Stimulus), 
  #              size = 6) +
  theme_light(base_size = 18) +
  scale_color_manual(values=c("#F3B336", "#1E5D9B"),
                    labels=c("Intact", "Scrambled")) +
  labs(y = "d'",
       fill = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=32),
        axis.text.x  = element_text(size=32),
        axis.text.y = element_text(size=28),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        legend.position = 'top') +
  coord_fixed(ratio = 0.8)

# bar plot
m.ind2 %>% ggplot(aes(x = Condition, y = DP, group = Stimulus)) +
  stat_summary(fun.y = mean, 
               geom = "bar",
               position=position_dodge(0.8),
               aes(fill = Stimulus),
               width = 0.8) +
  stat_summary(fun.data = "mean_cl_boot", 
               position = position_dodge(0.8), 
               color = "darkred",
               linewidth = 2, size = 1.5, show.legend = FALSE) +
  theme_light(base_size = 18) +
  scale_fill_manual(values=c("#F3B336", "#BBD135"),
                     labels=c("Intact", "Scrambled")) +
  labs(y = "d'",
       color = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=32),
        axis.text.x  = element_text(size=32),
        axis.text.y = element_text(size=28),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        legend.position = 'top') +
  scale_y_continuous(breaks = seq(0, 2.0, by = 0.4)) +
  coord_fixed(ratio = 1.8)


ind2.graph %>% ggplot(aes(x = Condition, y = dp.mean, group = Stimulus)) +
  geom_bar(stat = "identity",
           position=position_dodge(0.7),
           aes(fill = Stimulus),
           width = 0.7,
           alpha =0.6) +
  geom_pointrange(aes(ymin = dp.mean - dp.se, ymax = dp.mean + dp.se),
                  position = position_dodge(0.7), 
                  color = "#ED6363",
                  linewidth = 2, size = 1.5) +
  theme_light(base_size = 18) +
  scale_fill_manual(values=c("#F3B336", "#BBD135"),
                    labels=c("Intact", "Scrambled")) +
  labs(y = "d'",
       fill = 'Stimulus Type') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=32),
        axis.text.x  = element_text(size=32, color = "black"),
        axis.text.y = element_text(size=28),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        legend.position = 'top',
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0, 1.5),breaks = seq(0, 1.5, by = 0.5)) +
  coord_fixed(ratio = 1.7)

# 예상 그래프
## meaningfulness effect X
eff.no <- data.frame(Condition = c('Conjunction', 'Conjunction', 'Disjunction', 'Disjunction'),
                     Stimulus = c('Intact','Scrambled','Intact','Scrambled'),
                     DP = c(1.30, 1.13, 1.01, 1.0))

eff.no %>% ggplot(aes(x = Condition, y = DP, group = Stimulus)) +
  geom_bar(stat = "identity",
           position=position_dodge(0.8),
           aes(fill = Stimulus),
           width = 0.8,
           alpha =0.6) +
  theme_light(base_size = 18) +
  scale_fill_manual(values=c("#F3B336", "#BBD135"),
                    labels=c("Intact", "Scrambled")) +
  labs(y = "d'") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=44),
        axis.text.x  = element_text(size=44, color = "black"),
        axis.text.y = element_text(size=36),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0, 1.5),breaks = seq(0, 1.5, by = 0.5)) +
  coord_fixed(ratio = 1.2)


## meaningfulness effect O
eff.yes<- data.frame(Condition = c('Conjunction', 'Conjunction', 'Disjunction', 'Disjunction'),
                     Stimulus = c('Intact','Scrambled','Intact','Scrambled'),
                     DP = c(1.30, 1.13, 1.05, 0.88))

eff.yes %>% ggplot(aes(x = Condition, y = DP, group = Stimulus)) +
  geom_bar(stat = "identity",
           position=position_dodge(0.8),
           aes(fill = Stimulus),
           width = 0.8,
           alpha =0.6) +
  theme_light(base_size = 18) +
  scale_fill_manual(values=c("#F3B336", "#BBD135"),
                    labels=c("Intact", "Scrambled")) +
  labs(y = "d'") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=44),
        axis.text.x  = element_text(size=44, color = "black"),
        axis.text.y = element_text(size=36),
        legend.position = "none",
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(limits = c(0, 1.5),breaks = seq(0, 1.5, by = 0.5)) +
  coord_fixed(ratio = 1.2)

## 4.1 ANOVA ---------------------------------------------------------------
res.aov2 <- anova_test(
  data = m.ind2, dv = DP, wid = SN,
  within = c(Stimulus, Condition),
  effect.size = "pes"
)
get_anova_table(res.aov2) %>% 
  kable(digits = 3, format = "simple", caption = "ANOVA")

# Disjunction만 t-test
DisInt2 <- m.ind2[m.ind$Stimulus == 'Intact' & m.ind$Condition == 'Disjunction', 'DP']
DisScr2 <- m.ind2[m.ind$Stimulus == 'Scrambled' & m.ind$Condition == 'Disjunction', 'DP']

t.test(DisInt2, DisScr2)
# 5 BayesFactor -----------------------------------------------------------
bf <- anovaBF(DP ~ Stimulus*Condition + SN, data = as.data.frame(m.ind2), 
              whichRandom = "SN", progress = FALSE)
plot(bf)

bf[3]/bf[4]

bf
