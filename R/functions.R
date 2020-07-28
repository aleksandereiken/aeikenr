#---------------------------------# NEW FUNCTION #---------------------------------#

#' Illustration of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#'
#' @param x A string of numeric factors, which need to be transformed to numeric
#'
#' @return Numeric values of the factor
#'
#' @examples
#' to_numeric(factor(c(1,2,3,43,7)))
#'
#' @export

ae_factors_to_numeric <- function(x) return(as.numeric(trimws(x)))

#---------------------------------# NEW FUNCTION #---------------------------------#

#' GoogleSheets to dataframe
#'
#' Retrieves data from a googlesheets online, if you have authorization
#'
#' @param worksheet The worksheet in the googlesheets, which you want to retrieve data from
#' @param sheet_key The sheet_key of the worksheet. Can be accessed through the URL
#' @param colnames Boolean on whether you wish to have the first row of the spreadsheet serve as column names
#' @param path_to_token Specify path to token, which allows access to googlesheets. If not specified, you
#' will be asked to grant access via a web browser
#'
#' @return Tibble of googlesheets data
#'
#' @examples
#' ae_get_gs_data("your_data","your_worksheet_name","your_sheet_key_id_like_1zmxDhJ...")
#'
#' @export

ae_get_gs_data <- function (worksheet,sheet_key,colnames = TRUE,path_to_token = NULL) {
  if(is.null(path_to_token)) {
    googlesheets::gs_auth()
  } else {
    googlesheets::gs_auth(token = path_to_token)
  }
  key <- googlesheets::gs_key(sheet_key)
  tryCatch( { data <- googlesheets::gs_read(key, ws = worksheet,col_names = colnames) }
            , error = function(e) {
              warning("Data not fount. Waiting 5 sec and retrying ...")
              Sys.sleep(5)
              data <- ae_get_gs_data(name,worksheet,sheet_key,colnames = TRUE)
            })
  return(data)
}

#---------------------------------# NEW FUNCTION #---------------------------------#

#' Latex Text Block
#'
#' Add textblock to latex document via R function
#'
#' @param text Specify the text you want to add to the Latex document
#' @param x_cor Specify the \code{x_cor} in cm for the top left corner of the textbox
#' @param y_cor Specify the \code{y_cor} in cm for the top left corner of the textbox
#' @param font_size Specify the \code{font_size} in points. I.e. "12".
#' @param font Specify the \code{font}. Accepted arguments include "light", "medium" and "bold.
#' @param colnames Boolean on whether you wish to have the first row of the spreadsheet serve as column names
#' @param path_to_token Specify path to token, which allows access to googlesheets. If not specified, you
#' will be asked to grant access via a web browser
#'
#' @return Tibble of googlesheets data
#'
#' @examples
#' ae_get_gs_data("your_data","your_worksheet_name","your_sheet_key_id_like_1zmxDhJ...")
#'
#' @export

ae_tex_add_block_text <- function(text,x_cor,y_cor,font_size,line_space, font = "medium",color = "2b2c2c",textbox_width = "13.25") {
  latex_text <- paste0("\\begin{textblock*}{",textbox_width,"cm}(",x_cor,"cm,",y_cor,"cm)",
                       "\\fontsize{",font_size,"}{",line_space,"}\\selectfont",
                       colorText(textFont(add_linebreak(suberscript(text)),font = font), color = color),
                       "\\end{textblock*}")
  return(latex_text)
}


#---------------------------------# NEW FUNCTION #---------------------------------#

#' Latex Text Block
#'
#' Add textblock to latex document via R function
#'
#' @param text Specify the text you want to add to the Latex document
#' @param x_cor Specify the \code{x_cor} in cm for the top left corner of the textbox
#' @param y_cor Specify the \code{y_cor} in cm for the top left corner of the textbox
#' @param font_size Specify the \code{font_size} in points. I.e. "12".
#' @param font Specify the \code{font}. Accepted arguments include "light", "medium" and "bold.
#' @param colnames Boolean on whether you wish to have the first row of the spreadsheet serve as column names
#' @param path_to_token Specify path to token, which allows access to googlesheets. If not specified, you
#' will be asked to grant access via a web browser
#'
#' @return Tibble of googlesheets data
#'
#' @examples
#' ae_get_gs_data("your_data","your_worksheet_name","your_sheet_key_id_like_1zmxDhJ...")
#'
#' @export

ae_tex_add_block_img <- function(path_to_image,x_cor,y_cor,textbox_width = "13.25",angle = 0) {
  latex_text <- paste0("\\begin{textblock*}{",textbox_width,"cm}(",x_cor,"cm,",y_cor,"cm)",
                       "\\includegraphics[angle =",angle,"]{",path_to_image,"}",
                       "\\end{textblock*}")
  return(latex_text)
}
