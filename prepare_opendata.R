## ============================================================
## prepare_opendata.R
## Reads the raw data (ema_summer.csv), removes PII, anonymises
## participant IDs, selects analysis-relevant columns, translates
## all Chinese values and column names to English, and writes the
## open-access dataset.
##
## Run once before uploading to OSF.
## ============================================================

library(dplyr)
library(readr)

## Set raw_path to the location of the non-public raw data file on your machine.
## The raw data are not distributed; this script is provided for transparency only.
raw_path <- "data/ema_summer.csv"   # adjust to your local path
out_path <- "ema_opendata.csv"

cat("Reading raw data...\n")
raw <- read_csv(raw_path, show_col_types = FALSE)
cat(sprintf("  %d rows × %d columns\n", nrow(raw), ncol(raw)))

## ── 1. Anonymise participant ID ───────────────────────────────
## uid.x=0 maps to two genuinely different individuals (different
## sex, age, and city). We break ties with gender + DOB + city so
## each true individual receives a unique anonymous ID.
## DOB is used only here and is excluded from the output.

set.seed(42)
id_map <- raw |>
  distinct(uid.x, gender, DOB, live_city_clean) |>
  slice_sample(prop = 1) |>
  mutate(participant_id = row_number())

dat <- raw |>
  left_join(id_map, by = c("uid.x", "gender", "DOB", "live_city_clean"))

cat(sprintf("  %d unique participants after anonymisation\n",
            n_distinct(dat$participant_id)))

## ── 2. Translate Chinese categorical values ───────────────────

## Nature exposures: tree density
dat <- dat |>
  mutate(tree = case_when(
    tree == "完全没有" ~ "None",
    tree == "很少"     ~ "Few",
    tree == "一般"     ~ "Moderate",
    tree == "较多"     ~ "Many",
    tree == "非常多"   ~ "Dense",
    .default = tree
  ))

## Nature exposures: wildlife and water presence
recode_yn <- function(x) case_when(
  x == "是"     ~ "Yes",
  x == "否"     ~ "No",
  x == "不确定" ~ "Not sure",
  .default = x
)
dat <- dat |>
  mutate(wildlife = recode_yn(wildlife),
         water    = recode_yn(water))

## Location
dat <- dat |>
  mutate(location = case_when(
    location == "室内" ~ "Indoor",
    location == "室外" ~ "Outdoor",
    .default = location
  ))

## Location type (detail)
dat <- dat |>
  mutate(location_detail_re = case_when(
    location_detail_re == "在我自己的家"               ~ "Own home",
    location_detail_re == "在我的工作地点或学校"       ~ "Work or school",
    location_detail_re == "在家人或朋友家"             ~ "Family or friend's home",
    location_detail_re == "在公共场所(如商店、餐厅或电影院)" ~ "Public indoor venue",
    location_detail_re == "在公共交通工具上"           ~ "Public transport",
    location_detail_re == "在花园或公园里"             ~ "Garden or park",
    location_detail_re == "在其他室外场所"             ~ "Other outdoor",
    location_detail_re == "在其他室内场所"             ~ "Other indoor",
    location_detail_re == "在我家的院子里/小区"        ~ "Own home courtyard or compound",
    location_detail_re == "在海边、湖边或河边"         ~ "Waterside (coast, lake, or river)",
    location_detail_re == "在街道或广场上"             ~ "Street or plaza",
    location_detail_re == "在健身房或娱乐中心"         ~ "Gym or recreation centre",
    location_detail_re == "在山上或山丘上"             ~ "Hillside or mountain",
    location_detail_re == "私家车"                    ~ "Private car",
    location_detail_re == "酒店"                      ~ "Hotel",
    location_detail_re == "其他"                      ~ "Other",
    .default = location_detail_re
  ))

## Park status
dat <- dat |>
  mutate(park_in = case_when(
    park_in == "刚入园"     ~ "Just entered",
    park_in == "继续在公园内" ~ "Staying in park",
    park_in == "准备离开"   ~ "About to leave",
    .default = park_in
  ))

## General mental and physical health ratings
recode_rating <- function(x) case_when(
  x == "非常好" ~ "Very good",
  x == "好"     ~ "Good",
  x == "一般"   ~ "Moderate",
  x == "差"     ~ "Poor",
  x == "非常差" ~ "Very poor",
  x == "不确定" ~ "Unsure",
  .default = x
)
dat <- dat |>
  mutate(mh_general = recode_rating(mh_general),
         ph_general = recode_rating(ph_general))

## Region
dat <- dat |>
  mutate(region_cn = case_when(
    region_cn == "华北"  ~ "North China",
    region_cn == "华东"  ~ "East China",
    region_cn == "华南"  ~ "South China",
    region_cn == "华中"  ~ "Central China",
    region_cn == "中部"  ~ "Central China",   # alternate spelling in raw data
    region_cn == "西北"  ~ "Northwest China",
    region_cn == "西南"  ~ "Southwest China",
    region_cn == "东北"  ~ "Northeast China",
    region_cn == "长三角" ~ "Yangtze River Delta",
    region_cn == "大湾区" ~ "Greater Bay Area",
    region_cn == "其他"  ~ "Other",
    .default = region_cn
  ))

## Chronic disease detail
dat <- dat |>
  mutate(chornic = case_when(
    chornic == "无上述慢性疾病" ~ "None",
    chornic %in% c("无", "没有") ~ "None",
    chornic == "高血压"        ~ "Hypertension",
    chornic == "高血脂/血脂异常" ~ "Hyperlipidaemia",
    chornic == "糖尿病"        ~ "Diabetes",
    chornic == "心脏病"        ~ "Heart disease",
    chornic == "冠心病"        ~ "Coronary heart disease",
    chornic == "心脏病（其他，如心律失常、瓣膜病、先天性心脏病）" ~ "Other heart disease",
    chornic == "哮喘"          ~ "Asthma",
    chornic == "胃病"          ~ "Gastrointestinal disease",
    chornic == "腰突"          ~ "Lumbar disc herniation",
    chornic == "关节炎"        ~ "Arthritis",
    chornic == "慢性肝病"      ~ "Chronic liver disease",
    chornic == "慢性肺病/呼吸道疾病" ~ "Chronic respiratory disease",
    chornic == "癌症/恶性肿瘤" ~ "Cancer",
    chornic == "精神或情绪障碍" ~ "Mental or mood disorder",
    chornic == "老年痴呆或记忆障碍" ~ "Dementia or memory disorder",
    chornic == "肾脏疾病"      ~ "Kidney disease",
    chornic == "脊柱侧弯"      ~ "Scoliosis",
    chornic %in% c("过敏", "寻麻疹") ~ "Allergy or urticaria",
    chornic %in% c("过敏性鼻炎", "鼻炎") ~ "Rhinitis",
    .default = chornic
  ))

## City names (Chinese → Pinyin / standard English)
dat <- dat |>
  mutate(live_city_clean = case_when(
    live_city_clean == "北京"   ~ "Beijing",
    live_city_clean == "天津"   ~ "Tianjin",
    live_city_clean == "上海"   ~ "Shanghai",
    live_city_clean == "重庆"   ~ "Chongqing",
    live_city_clean == "石家庄" ~ "Shijiazhuang",
    live_city_clean == "保定"   ~ "Baoding",
    live_city_clean == "邯郸"   ~ "Handan",
    live_city_clean == "邢台"   ~ "Xingtai",
    live_city_clean == "太原"   ~ "Taiyuan",
    live_city_clean == "忻州"   ~ "Xinzhou",
    live_city_clean == "呼和浩特" ~ "Hohhot",
    live_city_clean == "沈阳"   ~ "Shenyang",
    live_city_clean == "大连"   ~ "Dalian",
    live_city_clean == "长春"   ~ "Changchun",
    live_city_clean == "哈尔滨" ~ "Harbin",
    live_city_clean == "南京"   ~ "Nanjing",
    live_city_clean == "徐州"   ~ "Xuzhou",
    live_city_clean == "杭州"   ~ "Hangzhou",
    live_city_clean == "宁波"   ~ "Ningbo",
    live_city_clean == "温州"   ~ "Wenzhou",
    live_city_clean == "绍兴"   ~ "Shaoxing",
    live_city_clean == "金华"   ~ "Jinhua",
    live_city_clean == "台州"   ~ "Taizhou",
    live_city_clean == "丽水"   ~ "Lishui",
    live_city_clean == "衢州"   ~ "Quzhou",
    live_city_clean == "合肥"   ~ "Hefei",
    live_city_clean == "池州"   ~ "Chizhou",
    live_city_clean == "福州"   ~ "Fuzhou",
    live_city_clean == "厦门"   ~ "Xiamen",
    live_city_clean == "漳州"   ~ "Zhangzhou",
    live_city_clean == "南昌"   ~ "Nanchang",
    live_city_clean == "吉安"   ~ "Ji'an",
    live_city_clean == "济南"   ~ "Jinan",
    live_city_clean == "青岛"   ~ "Qingdao",
    live_city_clean == "郑州"   ~ "Zhengzhou",
    live_city_clean == "洛阳"   ~ "Luoyang",
    live_city_clean == "武汉"   ~ "Wuhan",
    live_city_clean == "宜昌"   ~ "Yichang",
    live_city_clean == "长沙"   ~ "Changsha",
    live_city_clean == "湘潭"   ~ "Xiangtan",
    live_city_clean == "广州"   ~ "Guangzhou",
    live_city_clean == "深圳"   ~ "Shenzhen",
    live_city_clean == "东莞"   ~ "Dongguan",
    live_city_clean == "佛山"   ~ "Foshan",
    live_city_clean == "中山"   ~ "Zhongshan",
    live_city_clean == "珠海"   ~ "Zhuhai",
    live_city_clean == "惠州"   ~ "Huizhou",
    live_city_clean == "成都"   ~ "Chengdu",
    live_city_clean == "贵阳"   ~ "Guiyang",
    live_city_clean == "遵义"   ~ "Zunyi",
    live_city_clean == "昆明"   ~ "Kunming",
    live_city_clean == "红河哈尼族彝族自治州" ~ "Honghe Hani and Yi Autonomous Prefecture",
    live_city_clean == "西安"   ~ "Xi'an",
    live_city_clean == "咸阳"   ~ "Xianyang",
    live_city_clean == "宝鸡"   ~ "Baoji",
    live_city_clean == "银川"   ~ "Yinchuan",
    live_city_clean == "兰州"   ~ "Lanzhou",
    .default = live_city_clean
  ))

## Province names (Chinese → English)
dat <- dat |>
  mutate(live_province_cn = case_when(
    live_province_cn == "北京"  ~ "Beijing",
    live_province_cn == "天津"  ~ "Tianjin",
    live_province_cn == "上海"  ~ "Shanghai",
    live_province_cn == "重庆"  ~ "Chongqing",
    live_province_cn == "河北"  ~ "Hebei",
    live_province_cn == "山西"  ~ "Shanxi",
    live_province_cn == "内蒙古" ~ "Inner Mongolia",
    live_province_cn == "辽宁"  ~ "Liaoning",
    live_province_cn == "吉林"  ~ "Jilin",
    live_province_cn == "黑龙江" ~ "Heilongjiang",
    live_province_cn == "江苏"  ~ "Jiangsu",
    live_province_cn == "浙江"  ~ "Zhejiang",
    live_province_cn == "安徽"  ~ "Anhui",
    live_province_cn == "福建"  ~ "Fujian",
    live_province_cn == "江西"  ~ "Jiangxi",
    live_province_cn == "山东"  ~ "Shandong",
    live_province_cn == "河南"  ~ "Henan",
    live_province_cn == "湖北"  ~ "Hubei",
    live_province_cn == "湖南"  ~ "Hunan",
    live_province_cn == "广东"  ~ "Guangdong",
    live_province_cn == "四川"  ~ "Sichuan",
    live_province_cn == "贵州"  ~ "Guizhou",
    live_province_cn == "云南"  ~ "Yunnan",
    live_province_cn == "陕西"  ~ "Shaanxi",
    live_province_cn == "甘肃"  ~ "Gansu",
    live_province_cn == "宁夏"  ~ "Ningxia",
    .default = live_province_cn
  ))

## Park visit frequency
dat <- dat |>
  mutate(park_freq = case_when(
    park_freq == "每天都来"  ~ "Daily",
    park_freq == "每周3-4次" ~ "3-4 times/week",
    park_freq == "每周1-2次" ~ "1-2 times/week",
    park_freq == "很少"      ~ "Rarely",
    .default = park_freq
  ))

## ── 2b. Privacy reduction ────────────────────────────────────
## Cities with ≤5 participants → "Other small city"
city_counts <- dat |>
  group_by(live_city_clean) |>
  summarise(n_pid = n_distinct(participant_id), .groups = "drop")
small_cities <- city_counts$live_city_clean[city_counts$n_pid <= 5]
dat <- dat |>
  mutate(live_city_clean = ifelse(live_city_clean %in% small_cities,
                                  "Other small city", live_city_clean))

## Relative study day index (1 = participant's first assessment day)
dat <- dat |>
  mutate(day_date = as.Date(substr(as.character(day), 1, 10))) |>
  group_by(participant_id) |>
  mutate(study_day_index = as.integer(day_date - min(day_date, na.rm = TRUE)) + 1L) |>
  ungroup()

## Time of day from local timestamp
dat <- dat |>
  mutate(
    local_hour  = as.integer(format(as.POSIXct(time_local, tz = "UTC"), "%H")),
    time_of_day = case_when(
      local_hour >= 6  & local_hour < 12 ~ "Morning",
      local_hour >= 12 & local_hour < 18 ~ "Afternoon",
      local_hour >= 18                   ~ "Evening",
      .default                            = "Night"
    )
  )

## Bin time since previous assessment (minutes → labelled categories)
dat <- dat |>
  mutate(time_since_prev_bin = case_when(
    is.na(time_gap) | time_gap > 480 ~ "First or new day",
    time_gap < 60                    ~ "<1 h",
    time_gap < 120                   ~ "1–2 h",
    time_gap < 240                   ~ "2–4 h",
    .default                          = ">4 h"
  ))

## Merge sensitive/rare chronic disease categories
dat <- dat |>
  mutate(chornic = case_when(
    chornic %in% c(
      "None", "Hypertension", "Hyperlipidaemia", "Diabetes",
      "Heart disease", "Asthma", "Gastrointestinal disease",
      "Lumbar disc herniation", "Arthritis"
    ) ~ chornic,
    is.na(chornic) ~ NA_character_,
    .default = "Other chronic disease"
  ))

## ── 3. Select and rename columns ─────────────────────────────
## Only columns used in or relevant to the reported analyses are
## retained. Internal processing artifacts, duplicate/raw versions
## of recoded variables, and exact geographic sub-identifiers are
## excluded.

dat_clean <- dat |>
  transmute(
    ## ── Identifiers & timing ────────────────────────────────
    participant_id,
    assessment_id          = aid,
    study_day_index,
    time_of_day,
    time_since_prev_bin,
    same_location_prev     = same_loc_mh,
    n_assessments          = n_obs.x,

    ## ── Location context ────────────────────────────────────
    location,
    location_type          = location_detail_re,
    park_status            = park_in,
    park_frequency         = park_freq,

    ## ── Nature exposures (main IVs) ─────────────────────────
    tree,
    wildlife,
    water,

    ## ── Mental well-being outcome ───────────────────────────
    ## Six momentary affect items (-10 to +10 each; _r = reversed)
    satisfaction_r         = `满足感越小越满足`,
    wellbeing              = `身心舒畅`,
    calmness               = `平静感`,
    relaxation_r           = `放松感越小越放松`,
    alertness              = `疲乏清醒`,
    energy_r               = `越小越精力充沛`,
    ## Composite well-being score (sum of 6 items; range -60 to 60)
    mh,
    ## Single-item general ratings
    mh_general_rating      = mh_general,
    ph_general_rating      = ph_general,
    mh_general_score       = mh_g,
    ph_general_score       = ph_g,
    ## Conflict behaviour items
    aggression_yell        = yell,
    aggression_hit         = hit,

    ## ── Weather ─────────────────────────────────────────────
    weather_temp           = temp_en,
    weather_humidity       = dry_en,
    weather_sky            = cloud_en,
    weather_temp_score     = temp_scale,
    weather_humidity_score = dry_scale,
    weather_sky_score      = cloud_scale,

    ## ── Individual-level covariates ──────────────────────────
    gender,
    age,
    education_3            = education_3.y,  # 3-category: Lower/Higher education
    work_status,
    work_hours_per_day     = work_time_per_day,
    income_individual      = indi_income_level,
    income_household       = house_income_level,
    chronic_disease        = chornic_bin,
    chronic_disease_detail = chornic,
    bmi,
    ever_rural,
    rural_years,
    lives_with_adults      = livewith_adult,
    lives_with_pet         = livewith_pet,

    ## ── Baseline mental health ───────────────────────────────
    phq_score,
    pss_score,
    who_score              = who,

    ## ── City-level variables ─────────────────────────────────
    city                   = live_city_clean,
    province               = live_province_cn,
    region                 = region_cn,
    ## Log-transformed city variables (used in models)
    city_gdp_log           = gdp_100m_log,
    city_pop_density_log   = prp_10k_log,
    ## Raw city variables (for reference)
    city_gdp_100m          = gdp_100m,
    city_pop_density_per10k = prp_10k,
    city_pm25              = pm25,
    city_air_quality_good  = air_good,
    city_built_up_sqkm     = built_up_area_2022_sqkm,
    city_pop_density_2022  = pop_density_2023_2022
  )

## ── 4. Quality checks ─────────────────────────────────────────
## Check every character column for residual Chinese characters
chr_cols <- names(dat_clean)[sapply(dat_clean, is.character)]
chinese_hits <- lapply(chr_cols, function(col) {
  vals <- na.omit(unique(dat_clean[[col]]))
  vals[grepl("[一-鿿]", vals)]
})
names(chinese_hits) <- chr_cols
has_chinese <- Filter(function(x) length(x) > 0, chinese_hits)

if (length(has_chinese) > 0) {
  for (col in names(has_chinese)) {
    cat(sprintf("  Chinese residue in '%s': %s\n", col,
                paste(has_chinese[[col]], collapse = ", ")))
  }
  stop("Untranslated Chinese values found — add recodes above and re-run.")
}

stopifnot(
  "participant_id present"  = "participant_id" %in% names(dat_clean),
  "mh present"              = "mh"             %in% names(dat_clean),
  "tree values are English" = all(
    na.omit(unique(dat_clean$tree)) %in%
      c("None", "Few", "Moderate", "Many", "Dense")
  ),
  "No raw uid column"       = !"uid.x" %in% names(dat_clean)
)

cat(sprintf(
  "Output: %d rows × %d columns  (%d unique participants)\n",
  nrow(dat_clean), ncol(dat_clean),
  n_distinct(dat_clean$participant_id)
))

## ── 5. Write output ───────────────────────────────────────────
write_csv(dat_clean, out_path)
cat("Saved:", out_path, "\n")
