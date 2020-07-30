
init_progress <- function(len) {
  
  progress::progress_bar$new(
    total = len, 
    format = paste0(
      "[:spin] Completed: :current | :percent  ", 
      "Elapsed: :elapsed  Remaining: :eta"
    ),
    clear = FALSE
  )
}

with_progress <- function(.f, .n, ...) {
  
  pb <- init_progress(.n)
  
  function(...) {
    pb$tick()
    .f(...)
  }
}

make_tag <- function(site, year, date) {
  
  tag <- site
  
  if (!missing(year)) tag <- paste0(tag, stringr::str_sub(year, -2))
  
  if (!missing(date)) tag <- paste0(tag, "_", date)
  
  tag
}

tidy_rle <- function(x, explicit_na = FALSE, pull = NULL) {
  
  x <- as.vector(x)
  if (explicit_na) x <- tidyr::replace_na(x, -9999)
  
  rle <- rle(x)
  out <- tibble::tibble(
    id = rep(seq_along(rle$lengths), times = rle$lengths),
    lengths = rep(rle$lengths, times = rle$lengths),
    values = rep(rle$values, times = rle$lengths)
  )
  if (explicit_na) out$values <- dplyr::na_if(out$values, -9999)
  if (!is.null(pull)) out <- dplyr::pull(out, pull)
  
  out
}

between2 <- function(x, range) {
  # Like dplyr::between, but accepts a range vector instead of two values
  
  # Checks
  if (length(range) > 2) {
    stop("Range must be a vector of length 2.", call. = FALSE)
  } 
  if (range[2] < range[1]) range <- rev(range)
  
  dplyr::between(x, range[1], range[2])
}

