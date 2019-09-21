#' Generate a color palette from an image.
#'
#' @param file The path or url to an image file (urls should begin with http(s):// or ftp(s)://).
#' @param max The maximum number of colors to return.
#' @param exclude Colors to exclude from palette. Excludes white and black by default.
#' @param normalize Whether or not to apply \code{\link[magick]{image_normalize}}.
#'
#' @return A character vector containing a color palette.
#'
#' @examples
#' file <- system.file("man/sample-color-palette.png", package = "marketR") # Get example image
#' color_palette(file)
#'
#' \donttrun{
#' color_palette("https://cran.r-project.org/Rlogo.svg")
#' }
#'
#' @export color_palette
color_palette <- function(file,
                          max = 10,
                          exclude = c("#FFFFFF", "#000000"),
                          normalize = FALSE,
                          quantize = 20,
                          show_swatch = TRUE,
                          swatch_dir = "x",
                          export_csv = NULL,
                          export_img = NULL) {
  im <- magick::image_read(file, density = "72x72")

  if (length(im) > 1) {
    im <- im[1]
  }

  if (normalize) {
    im %<>% magick::image_normalize()
  }

  if(is.numeric(quantize)){
    im %<>% magick::image_quantize(max=quantize)
  }

  im %<>% imager::magick2cimg()

  df <- as.data.frame(im, wide = "c") %>%
    dplyr::mutate(rgb.val = rgb(c.1, c.2, c.3)) %>%
    dplyr::group_by(rgb.val) %>%
    dplyr::summarize(count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(count))

  if (is.vector(exclude)) {
    df %<>% dplyr::filter(!rgb.val %>% stringr::str_detect(paste0(exclude, collapse = "|")))
  }

  hex_codes <- df$rgb.val %>% head(n = max)


  if(show_swatch || !is.null(image_out)){
    swatch_dir %<>% stringr::str_to_lower()

    if(!stringr::str_detect(swatch_dir, "x|y")){
      swatch_dir = ifelse(length(hex_codes) <= 12, "y", "x")
      warning(paste("swatch_dir not recognized. Defaulting to",swatch_dir))
    }

    swatch_dim = c(200, 200)
    text.position = c(swatch_dim[[1]] / 20, swatch_dim[[2]] / 10)
    font = 80

    display <-
      imager::imfill(swatch_dim[[1]], swatch_dim[[2]], val = hex_codes[[1]]) %>%
      imager::draw_text(10,
                        text.position[[2]] ,
                        hex_codes[[1]],
                        color = "white",
                        fsize = 80)

    for (i in 2:length(hex_codes)) {
      to_append <- imager::imfill(swatch_dim[[1]],
                                  swatch_dim[[2]],
                                  val = hex_codes[[i]]) %>%
        imager::draw_text(10,
                          text.position[[2]],
                          hex_codes[[i]],
                          color = "white",
                          fsize = font)

      display <- list(display, to_append) %>%
        imager::imappend(swatch_dir)

    }

  }

  if(show_swatch){
    display %>% plot(axes=F)
  }

  if(!is.null(export_img)){

    img_format = export_img %>%
      stringr::str_extract("\\.[:alnum:]+") %>%
      stringr::str_remove("\\.")

    tryCatch({
      imager::save.image(display, file=export_img, quality = 1)
    },
    error= function(e){
      print(e)
      warning(paste("Unable to write image file to", export_img))
    })
  }

  if(!is.null(export_csv)){
    out_csv <- tibble::enframe(hex_codes, value = paste("Retrieved from", file), name=NULL)
    #names(outfile) <- paste("Retrieved from", csv)
    tryCatch({
      write.csv(out_csv, export_csv, row.names = F)
    },
    error = function(e){
      warning(paste0("Unable to save file to ", export_csv, ". Make sure the csv is not open in another program"))
    })
  }

  hex_codes
}

