## sample_map.R
## Produces a bubble map showing participant distribution across 55 Chinese cities.
## Requires: (1) the dataset file (available upon request) placed in the same
##           directory, and (2) China administrative boundary shapefiles
##           (prefecture-level and province-level) in GeoJSON or shapefile format.

# ===== Packages =====
pkgs <- c("readr", "dplyr", "stringr", "sf", "ggplot2")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

library(readr); library(dplyr); library(stringr)
library(sf); library(ggplot2)

# ===== 0) File paths — edit these before running =====
data_path       <- "data/ema_summer_2025.csv"     # dataset (full version with city names)
city_bound_path <- "data/2023年地级/地级.shp"     # prefecture-level boundary shapefile
prov_bound_path <- "data/2023年省级/省级.shp"     # province-level boundary shapefile

# ===== 1) Read data & count participants per city =====
raw <- read_csv(data_path, show_col_types = FALSE)

strip_suffix_prov <- function(x) {
  x |>
    str_squish() |>
    str_replace_all("新疆(维吾尔)?自治区", "新疆") |>
    str_replace_all("广西(壮族)?自治区",   "广西") |>
    str_replace_all("宁夏(回族)?自治区",   "宁夏") |>
    str_replace_all("内蒙古自治区",        "内蒙古") |>
    str_replace_all("西藏(自治区)?",        "西藏") |>
    str_replace_all("^香港(特别行政区)?$",  "香港") |>
    str_replace_all("^澳门(特别行政区)?$",  "澳门") |>
    str_replace_all("^台湾省?$",            "台湾") |>
    str_replace_all("(省|市|特别行政区)$",  "") |>
    str_replace_all("^贵$", "贵州")
}

strip_suffix_city <- function(x) {
  x |>
    str_squish() |>
    str_replace_all("自治州$", "") |>
    str_replace_all("(市辖区|地区|盟|市|区|县|特别行政区)$", "")
}

dat_loc <- raw |>
  distinct(uid.x, live_province_cn, live_city_cn, live_district_cn) |>
  filter(!is.na(live_province_cn)) |>
  mutate(
    prov_clean     = strip_suffix_prov(live_province_cn),
    city_clean_raw = ifelse(is.na(live_city_cn) | live_city_cn == "",
                            live_district_cn, live_city_cn),
    city_clean     = strip_suffix_city(city_clean_raw),
    city_clean     = ifelse(prov_clean %in% c("北京", "上海", "天津", "重庆") &
                              (is.na(city_clean) | city_clean == ""),
                            prov_clean, city_clean)
  ) |>
  filter(!is.na(prov_clean), prov_clean != "",
         !is.na(city_clean), city_clean != "")

city_count <- dat_loc |> count(prov_clean, city_clean, name = "user_n")

# ===== 2) Read boundary shapefiles =====
cn_crs <- 3857 # Web Mercator
cn_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

cn_city         <- st_read(city_bound_path, quiet = TRUE)
cn_prov_outline <- st_read(prov_bound_path, quiet = TRUE)

guess_col <- function(nms, candidates) {
  cand <- candidates[candidates %in% nms]
  if (length(cand) == 0) NA_character_ else cand[1]
}

nms_city      <- names(cn_city)
prov_col_city <- guess_col(nms_city, c("省级", "省", "省名", "NAME_1", "province"))
city_col_city <- guess_col(nms_city, c("地名", "市", "市名", "NAME_2", "NAME_CHN", "city", "名称"))
if (is.na(prov_col_city) | is.na(city_col_city))
  stop("Cannot find province/city columns. Available: ", paste(nms_city, collapse = ", "))

nms_prov      <- names(cn_prov_outline)
prov_col_prov <- guess_col(nms_prov, c("省级", "省", "省名", "NAME_1", "NAME_CHN", "province"))
if (is.na(prov_col_prov))
  stop("Cannot find province column. Available: ", paste(nms_prov, collapse = ", "))

# if (is.na(st_crs(cn_city))) 
#   st_crs(cn_city) <- cn_crs
# if (is.na(st_crs(cn_prov_outline))) 
#   st_crs(cn_prov_outline) <- cn_crs

cn_city         <- st_transform(cn_city,         cn_crs)
cn_prov_outline <- st_transform(cn_prov_outline, cn_crs)

# ===== 3) Join participant counts to city polygons =====
cn_city2 <- cn_city |>
  mutate(
    prov_clean_border = strip_suffix_prov(.data[[prov_col_city]]),
    city_clean_border = strip_suffix_city(.data[[city_col_city]]),
    city_clean_border = ifelse(
      prov_clean_border %in% c("北京", "上海", "天津", "重庆") &
        (is.na(city_clean_border) | city_clean_border == ""),
      prov_clean_border, city_clean_border)
  )

joined <- cn_city2 |>
  left_join(city_count,
            by = c("prov_clean_border" = "prov_clean",
                   "city_clean_border" = "city_clean")) |>
  mutate(user_n = dplyr::coalesce(user_n, 0L))

# ===== 4) Plot =====
city_pts <- st_point_on_surface(joined)

p <- ggplot() +
  geom_sf(data = cn_prov_outline, fill = "grey98", color = NA) +
  geom_sf(data = cn_prov_outline, fill = NA, color = "grey45", linewidth = 0.4) +
  geom_sf(data = filter(city_pts, user_n > 0),
          aes(size = user_n),
          shape = 21, stroke = 0.6,
          fill  = "#2AA198",
          color = "#0E7C86",
          alpha = 0.55) +
  scale_size_continuous(
    name   = "Participants",
    range  = c(3, 18),
    breaks = c(10, 30, 60, 90),
    labels = c("10", "30", "60", "90+")
  ) +
  coord_sf() +
  # labs(x = "Longitude", y = "Latitude") +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor  = element_blank(),
    legend.position   = c(0.15, 0.22),
    legend.background = element_rect(fill = scales::alpha("white", 0.7), color = NA)
  )

# print(p)
ggsave("./figures/sample_map_Albers.png", p, width = 7, height = 6, units = 'in', dpi = 300)
cat("Saved: ./figures/sample_map_Albers.png\n")

# ===== 5) Unmatched cities (diagnostic) =====
unmatched <- city_count |>
  anti_join(st_drop_geometry(cn_city2) |>
              transmute(prov_clean = prov_clean_border,
                        city_clean = city_clean_border),
            by = c("prov_clean", "city_clean"))
if (nrow(unmatched) > 0) { cat("Unmatched:\n"); print(unmatched) }
