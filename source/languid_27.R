seeds <- 3500:3599

make_art <- function(seed) {
    
  sys_id <- "27"
  sys_name <- "languid"
  cpp_path <- here::here("source", paste0(sys_name, "_", sys_id, ".cpp"))
  Rcpp::sourceCpp(cpp_path)
  
  # seed
  cat(seed, "\n")
  set.seed(seed)
  
  # fixed / default
  px <- 2000
  layers <- 30
  million <- 10^6
  iter <- 1000 * million
  zoom <- .25
  alpha <- .0001
  
  # palette specification ---------------------------------------------------
  
  palettes <- c(
    "palette_01.csv",
    "palette_02.csv",
    "palette_03.csv"
  ) |>
    purrr::map(
      \(x) here::here("source", "palettes", x) |>
        readr::read_csv(show_col_types = FALSE)
    ) |>
    dplyr::bind_rows()

  ncl <- 1024
  ind <- sample(nrow(palettes), 1)
  palette_base <- unlist(palettes[ind,])
  palette_base <- sample(palette_base, 1)
  palette_base <- sample(c("#ffffff", palette_base, "#222222"), 8, replace = TRUE)
  pal <- (colorRampPalette(palette_base))(ncl)
  bg <- pal[1]
  
  
  # helper functions --------------------------------------------------------
  
  generate_data <- function(seed, iter, layers, px, zoom, alpha) {
    set.seed(seed)
    df <- raster_data(iter, layers, px, zoom, alpha)
    return(df)
  }
  
  transform_data <- function(df) {
    df <- rank(abs(df))
    df <- df - min(df)
    df <- df / max(df)
    df <- as.integer(df * (ncl - 1)) + 1
    return(df)
  }
  
  colourise_data <- function(df) {
    df <- pal[df]
    df <- matrix(df, px, px, byrow = TRUE)
    return(df)
  }
  
  render_data <- function(df, fpath, px, bg) {
    rs <- as.raster(df)
    jpeg(
      filename = fpath,
      width = px,
      height = px,
      bg = bg 
    )
    op <- par(mar = c(0,0,0,0))
    plot(rs)
    dev.off()
    par(op)
  }
  
  fpath <- function(seed) {
    dir <- paste0("sys_", sys_id)
    dir <- here::here("output", dir)
    if(!dir.exists(dir)) dir.create(dir)
    prefix <- paste0(sys_name, "_", sys_id, "_")
    fname <- paste0(prefix, seed, ".jpg")
    fp <- file.path(dir, fname)
    return(fp)
  }
  
  # generate the data -------------------------------------------------------
  
  cat("generating...\n")
  
  
  df1 <- generate_data(seed, iter, layers, px, zoom, alpha)
  
  cat("transforming...\n")
  
  rank1 <- transform_data(df1)
  cols1 <- colourise_data(rank1)
  
  cat("rendering...\n")
  
  render_data(cols1, fpath(seed), px, bg)
  
}

for(s in seeds) make_art(s)