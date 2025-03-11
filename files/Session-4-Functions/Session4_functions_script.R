# Install and load the readxl package if you haven't already
if (!require(readxl)) install.packages("readxl")
library(readxl)



# Function to read an Excel sheet and consider only the first two rows for the header
read_excel_with_two_row_header_conditional <- function(file_path, sheet_name) {
  # Read the first two rows to create the header
  header_rows <- read_excel(file_path, sheet = sheet_name, n_max = 2)

  # Merge the two header rows, considering non-empty cells
  header <- apply(header_rows, 2, function(x) {
    # Combine the cell values, ignore if NA, and join non-empty values with an underscore
    cell_values <- na.omit(x)
    if (length(cell_values) == 0) {
      return("")  # Return an empty string if all values are NA
    } else {
      return(paste(cell_values, collapse = "_"))
    }
  })

  # Read the entire sheet, skipping the first two rows and using the merged header
  data <- read_excel(file_path, sheet = sheet_name, skip = 2, col_names = header)

  return(data)
}

# Usage example
# Replace "path/to/your/file.xlsx" with the path to your Excel file and "Sheet1" with the name of your sheet
data <- read_excel_with_two_row_header("Session4_Functions/data/Maloof_et_al_2010_SM.xls", "4. Morocco-Talat n' Yissi")
head(data)


file_path <- "Session4_Functions/data/Maloof_et_al_2010_SM.xls"
sheet_name <- "4. Morocco-Talat n' Yissi"
header_rows <- read_excel(file_path, sheet = sheet_name, n_max = 2,
                          trim_ws = FALSE, col_names = FALSE)


read_sheets_Maloof <- function(file_path, sheet_index) {
  # read the top two rows
  header_rows <- read_excel(file_path, sheet = sheet_index, n_max = 2,
                            trim_ws = FALSE, col_names = FALSE)
  column_names <- apply(header_rows, 2, function(x) paste(na.omit(x), collapse = "_"))
  output <- read_excel(file_path, sheet = sheet_index, skip = 2, col_names = column_names)
  output
}

multi_sheets_Maloof <- function(file_path, sheet_index, read_function) {
  lapply(sheet_index, function(s) {
    read_function(file_path = file_path, sheet_index = s)
  })
}

test <- multi_sheet(file_path = "Session4_functions/data/Maloof_et_al_2010_SM.xls",
            sheet = 2:3, read_function = read_sheets_Maloof)
merge()
