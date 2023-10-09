# from https://stackoverflow.com/a/20671197/11122513
seq_weekday <- function(selday, start, end) {
  fwd.7 <- start + 0:6
  first.day <- fwd.7[as.numeric(format(fwd.7, "%w")) == selday]
  seq.Date(first.day, end, by = "week")
}