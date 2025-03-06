
#' Title District LQAS performance table for the last x month(x)
#'
#' @param CTRY → List of countries (e.g., c("CHAD", "NIGERIA"))
#' @param DS → List of districts to filter (e.g., c("CHADRA", "MICHEMIRE", "MOUSSORO"))
#' @param x → An integer specifying the number of months (last x month).
#' @returns final_table
#' @export # save final table into the output path (final_table)
#' @examples #
generate_lqas_table <- function(CTRY, DS, x) {
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(tidyverse, lubridate, readxl, flextable, scales)
  
  set_flextable_defaults(background.color = "white")
  
  # Define the data path
  lqas_data_path <- "C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/"
  
  # Read the data
  dat <- readr::read_csv(file.path(lqas_data_path, "AFRO_LQAS_data_C.csv")) |> 
    select(country, province, district, vaccine.type, round_start_date, total_missed) |> 
    filter(country %in% CTRY) |>  
    filter(district %in% DS)  
  
  ft <- dat |>
    mutate(
      date = as_date(round_start_date), 
      floor_date = floor_date(date, unit = "months")
    ) |>
    filter(
      date >= floor_date(Sys.Date(), unit = "months") - months(x)
    ) |>
    select(
      ctry = country, province, district, vaccine.type, total_missed, floor_date
    ) |>
    group_by(ctry, province, district, vaccine.type, floor_date) |>
    summarise(total_missed = sum(total_missed, na.rm = TRUE), .groups = "drop") |>
    mutate(
      Month_Yr = format_ISO8601(floor_date, precision = "ym")
    ) |>
    arrange(floor_date) |>
    select(ctry, province, district, vaccine.type, total_missed, Month_Yr) |>
    pivot_wider(
      names_from = Month_Yr, 
      values_from = total_missed, 
      id_cols = c(ctry, province, district, vaccine.type) 
    ) |>
    arrange(vaccine.type, ctry, province, district) |>
    rename(Country = ctry, Province = province, District = district, `Vaccine Type` = vaccine.type)
  
  
  month_labels <- colnames(ft)[5:ncol(ft)] #
  valid_month_labels <- month_labels[grepl("^\\d{4}-\\d{2}$", month_labels)]  
  
  
  year_labels <- ifelse(month_labels %in% valid_month_labels, 
                        format(as.Date(paste0(valid_month_labels, "-01")), "%Y"), "")
  
  month_abbr <- ifelse(month_labels %in% valid_month_labels, 
                       format(as.Date(paste0(valid_month_labels, "-01")), "%b"), "")
  
  
  header_mapping <- data.frame(
    keys = c("Country", "Province", "District", "Vaccine Type", month_labels),
    Year = c("", "", "", "", year_labels),
    Month = c("", "", "", "", month_abbr)
  )
  
  
  numeric_cols <- month_labels[month_labels %in% colnames(ft)]  
  ft[numeric_cols] <- lapply(ft[numeric_cols], as.numeric)  
  
  
  bg_picker <- col_bin(
    palette = c("green", "red"),
    domain = range(ft[numeric_cols], na.rm = TRUE),  
    bins = c(0, 3, max(ft[numeric_cols], na.rm = TRUE))  
  )
  
  
  final_table <- flextable(ft) |> 
    set_header_df(
      mapping = header_mapping,
      key = "keys"
    ) |>
    merge_h(part = "header") |>
    merge_v(part = "header") |>
    align(align = "center", part = "header") |> 
    align(align = "center", part = "body") |> 
    bg(j = month_labels, bg = bg_picker, part = "body") |> 
    hline(part = "all") |> 
    vline(part = "all") |> 
    width(j = "Country", width = 1.5) |>  
    width(j = "Province", width = 2) |>   
    width(j = "District", width = 2.5) |> 
    autofit() |>  
    set_table_properties(layout = "autofit") |>
    add_header_lines(paste("District LQAS performance Overview for the Last", x, "Months"))
  
  
  output_file <- "C:/Users/TOURE/Documents/LCB/outputs/LCB/lqas_table_.png"
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  save_as_image(final_table, output_file, background.color = "white")
  
  # Return the final table
  return(final_table)
}

# Example usage
CTRY <- c("CHAD")
DS <- c("CHADRA", "MICHEMIRE", "MOUSSORO", "SALAL", "ALIFA")
x <- 48

# Generate and display the table
generate_lqas_table(CTRY, DS, x)
