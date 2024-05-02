# 운동처방데이터 하나의 데이터로 합침
if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
df1 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202008.csv")
df2 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202009.csv")
df3 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202010.csv")
df4 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202011.csv")
df5 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202012.csv")
df6 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202101.csv")
df7 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202102.csv")
df8 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202103.csv")
df9 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202104.csv")
df10 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202105.csv")
df11 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202106.csv")
df12 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202107.csv")
df13 <- read.csv("KS_DSPSN_FTNESS_MESURE_ACCTO_MVM_PRSCRPTN_LIST_202108.csv")

df <- rbind(rbind(df1, df2), rbind(df3, df4))
df_ <- rbind(rbind(df, df5), rbind(df6, df7))
df_2 <- rbind(rbind(df_, df8), rbind(df9, df10))
ACCTO <- rbind(rbind(df_2, df11), rbind(df12, df13))

# 나이를 나이대로 변경
ACCTO$MESURE_AGE_CO <- ifelse(ACCTO$MESURE_AGE_CO >= 89, "90대", ifelse(ACCTO$MESURE_AGE_CO >= 79, "80대", ifelse(ACCTO$MESURE_AGE_CO >= 69, "70대", ifelse(ACCTO$MESURE_AGE_CO >= 59, "60대", ifelse(ACCTO$MESURE_AGE_CO >= 49, "50대", ifelse(ACCTO$MESURE_AGE_CO >= 39, "40대", ifelse(ACCTO$MESURE_AGE_CO >= 29, "30대", ifelse(ACCTO$MESURE_AGE_CO >= 19, "20대", "10대"))))))))

ACCTO_s <- ACCTO %>% select(CNTER_NM, MESURE_PLACE_FLAG_NM, MESURE_AGE_CO, SEXDSTN_FLAG_CD, TROBL_TY_NM, TROBL_GRAD_NM, MVM_PRSCRPTN_CN) %>% mutate(ID = row_number())

# 운동별로 열을 나누어줌
if(!require(stringr)) {install.packages("stringr"); library(stringr)}
ACCTO_p <- ACCTO_s %>% mutate(사전운동 = ACCTO_s$MVM_PRSCRPTN_CN %>% str_split("/") %>% sapply('[',1), 
                                  본운동 = ACCTO_s$MVM_PRSCRPTN_CN %>% str_split("/") %>% sapply('[',2),
                                  마무리운동 = ACCTO_s$MVM_PRSCRPTN_CN %>% str_split("/") %>% sapply('[',3))

# 사전, 본, 마무리로 맞게 들어가게 해줌
ACCTO_p$마무리운동 <- ifelse(str_detect(ACCTO_p$사전운동, "^마무리운동"), gsub("마무리운동:", "", ACCTO_p$사전운동), ifelse(str_detect(ACCTO_p$본운동, "^마무리운동"), gsub("마무리운동:", "", ACCTO_p$본운동), gsub("마무리운동:","",ACCTO_p$마무리운동)))

ACCTO_p$본운동 <- ifelse(str_detect(ACCTO_p$사전운동, "^본운동"),gsub("본운동:", "", ACCTO_p$사전운동), ifelse(str_detect(ACCTO_p$본운동, "^본운동"),gsub("본운동:","",ACCTO_p$본운동) , NA))

ACCTO_p$사전운동 <- ifelse(str_detect(ACCTO_p$사전운동, "^사전운동"), gsub("사전운동:","",ACCTO_p$사전운동), NA)

# 롱데이터를 만들어줌
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
ACCTO_사전운동 <- ACCTO_p %>% select(사전운동, ID) %>% separate_rows(사전운동, sep= ",") %>% rename(운동이름 = 사전운동)%>% na.omit()
ACCTO_사전운동$운동구분 <- "사전운동"
ACCTO_본운동 <- ACCTO_p %>% select(본운동, ID) %>% separate_rows(본운동, sep= ",") %>% rename(운동이름 = 본운동) %>% na.omit()
ACCTO_본운동$운동구분 <- "본운동"
ACCTO_마무리운동 <- ACCTO_p %>% select(마무리운동, ID) %>% separate_rows(마무리운동, sep= ",") %>% rename(운동이름 = 마무리운동) %>% na.omit()
ACCTO_마무리운동$운동구분 <- "마무리운동"

# 롱데이터로 만들어준 것을 하나의 데이터로 합침
ACCTO_운동 <- rbind(ACCTO_사전운동,rbind(ACCTO_본운동,ACCTO_마무리운동))

# 사용할 열을 추출데이터에 롱데이터로 만든 데이터를 합침
ACCTO_합 <- merge(ACCTO_s, ACCTO_운동, by="ID")

# 연관성 분석
# 연관성 분석을 위해 전처리 진행
ACCTO_합$운동이름 <- gsub(" ", "", ACCTO_합$운동이름)
ACCTO_합$운동이름<-ifelse(ACCTO_합$운동구분 == "사전운동", paste0("사전_",ACCTO_합$운동이름),ifelse(ACCTO_합$운동구분 == "본운동", paste0("본_",ACCTO_합$운동이름),ifelse(ACCTO_합$운동구분 == "마무리운동",paste0("마무리_",ACCTO_합$운동이름),NA)))

# 위에 합친 데이터에서 필요한 열만 추출
m <- ACCTO_합 %>% select(MESURE_AGE_CO,SEXDSTN_FLAG_CD,TROBL_TY_NM,TROBL_GRAD_NM, 운동이름) %>% as.data.frame()
head(m)

# transactions 형태로 변환하기 위해 factor로 변경
m$MESURE_AGE_CO <- as.factor(m$MESURE_AGE_CO)
m$SEXDSTN_FLAG_CD <- as.factor(m$SEXDSTN_FLAG_CD)
m$TROBL_TY_NM <- as.factor(m$TROBL_TY_NM)
m$TROBL_GRAD_NM <- as.factor(m$TROBL_GRAD_NM)
m$운동이름 <- as.factor(m$운동이름) 

# 연과성분석을 위해 transactions 형태로 변경
m1 <- transactions(m)

# 하나의 아이템을 묶어주기 위해서 변수별로 묶어줌 
MESURE_AGE_CO <- grep("^MESURE_AGE_CO=", itemLabels(m1), value = T)
SEXDSTN_FLAG_CD <- grep("^SEXDSTN_FLAG_CD=", itemLabels(m1), value = T)
TROBL_TY_NM <- grep("^TROBL_TY_NM=", itemLabels(m1), value = T)
TROBL_GRAD_NM <- grep("^TROBL_GRAD_NM", itemLabels(m1), value = T)
사전운동 <- grep("^운동이름=사전_", itemLabels(m1), value = T)
본운동 <- grep("^운동이름=본_", itemLabels(m1), value = T)
마무리운동 <- grep("^운동이름=마무리_", itemLabels(m1), value = T)

# aprior 함수를 사용하여 연관분석을 시행함
if(!require(arules)) {install.packages("arules"); library(arules)}
rules <- apriori(m1, parameter = list(support = 0.001, confidence = 0.001), appearance = list(rhs= c(사전운동,본운동, 마무리운동)))

# 연관성 분석한 것을 변수에 넣어줌
out <- cbind(labels = labels(rules), quality(rules))

# 정규식을 사용해서 나이, 성별, 장애명, 장애등급이 포합되어 있는 것만 추출
g1 <- grep("\\{MESURE_AGE_CO=.+,SEXDSTN_FLAG_CD=.+,TROBL_TY_NM=.+,TROBL_GRAD_NM=.+\\}",out$labels, value = T)

# 추출한 변수를 원래 데이터와 합치기 위해서 rename 함수를 사용하여 
g1 <- rename(g1, labels = g1)

# 연관성 분석을 한 데이터와 정규식을 사용해서 추출한 데이터를 합쳐줌
rules_all <- merge(g1,out, by = "labels")

# 데이터 구분을 위해 strsplit 함수를 사용해서 변수를 나누어줌 
rules_all <- rules_all %>% mutate(변수 = rules_all$labels %>% strsplit("=>") %>% sapply('[',1),
                                    결과 =  rules_all$labels %>% strsplit("=>") %>% sapply('[',2)) %>% select(변수, 결과,support ,confidence,coverage, lift, count) %>% arrange(desc(lift))
head(rules_all)

# 불필요한 기호 삭제
rules_all$변수 <- gsub("\\{","",rules_all$변수)
rules_all$변수 <- gsub("\\}","",rules_all$변수)
rules_all$결과 <- gsub("\\{","",rules_all$결과)
rules_all$결과 <- gsub("\\}","",rules_all$결과)
rules_all$결과 <- gsub("운동이름=", "", rules_all$결과)
rules_all$결과 <- gsub(" ", "", rules_all$결과)

# 나이, 성별, 장애명, 장애등급으로 변수를 나누어줌
(age <- substr(rules_all$변수, 15, 17))
(gender <- substr(rules_all$변수, 35, 35))
(disable <- substr(rules_all$변수, 49, 52))
(grade <- substr(rules_all$변수, 68, nchar(rules_all$변수)-1))

# 변수를 나눈 후에 cibnd를 하여 하나로 합침
rules_final <- cbind(demographic, rules_all[, 2:ncol(rules_all)])
rules_final <- rules_final %>% filter(lift>1) %>% arrange(desc(lift))
demographic <- data.frame(age=age, gender=gender, disable=disable, grade=grade)

# lift값이 1 초과인 값만 선별 후 내림차순 정렬
rules_final <- cbind(demographic, rules_all[, 2:ncol(rules_all)])
rules_final <- rules_final %>%
  filter(lift>1) %>%
  arrange(desc(lift))

# 사전운동의 연관분석 결과
rules_final_사전 <- rules_final %>% filter(grepl("^사전_", 결과)) %>% rename(사전운동 = 결과)
rules_final_사전$사전운동 <- gsub("사전_", "", rules_final_사전$사전운동)
head(rules_final_사전)

# 본운동의 연관분석 결과
rules_final_본 <- rules_final %>% filter(grepl("^본_", 결과)) %>% rename(본운동 = 결과)
rules_final_본$본운동 <- gsub("본_", "", rules_final_본$본운동)
head(rules_final_본)

# 마무리운동의 연관분석 결과
rules_final_마무리 <- rules_final %>% filter(grepl("^마무리_", 결과)) %>% rename(마무리운동 = 결과)
rules_final_마무리$마무리운동 <- gsub("마무리_", "", rules_final_마무리$마무리운동)
head(rules_final_마무리)

