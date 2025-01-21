
### KODE SOM MÅ VASKES #####

spc_data <- regdata %>%
  dplyr::filter(!is.na(PER_BLOOD_LOSS_VALUE))

spc_data <- spc_data %>%
  dplyr::filter(Sykehus == "Bergen")

spc_data <- spc_data %>%
  filter(SURGERY_DATE > as.Date("2024-01-01")) %>%
  select(SURGERY_DATE, PER_BLOOD_LOSS_VALUE)

spc_data <- spc_data %>%
  dplyr::group_by(week = cut(SURGERY_DATE, "month")) %>%
  dplyr::summarise(value = mean(PER_BLOOD_LOSS_VALUE))

spc_data$week <- as.Date(spc_data$week)


spc_data %>%
  dplyr::group_by(SURGERY_DATE) %>%
  dplyr::summarize(value = mean(PER_BLOOD_LOSS_VALUE))

spc_data |>
  ptd_spc(
    value_field = value,
    date_field = week,
    improvement_direction = "decrease"
