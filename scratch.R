bptosi <- function(bp) {
  if (bp>1e9) return(paste0(round(bp/1e9, 3), "GB"))
  else if (bp>1e6) return(paste0(round(bp/1e6, 3), "MB"))
  else if (bp>1e3) return(paste0(round(bp/1e3, 3), "KB"))
  else return(paste0(bp, "bp"))
}

stopifnot(bptosi(50) == "50bp")
stopifnot(bptosi(500) == "500bp")
stopifnot(bptosi(5000) == "5KB")
stopifnot(bptosi(7654321) == "7.654MB")
stopifnot(bptosi(1987654321) == "1.988GB")
