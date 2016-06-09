library(parallel)

finalResult <- local({
  f <- fifo(tempfile(), open="w+b", blocking=T)
  if (inherits(parallel:::mcfork(), "masterProcess")) {
    # Child
    progress <- 0.0
    while (progress < 1 && !isIncomplete(f)) {
      msg <- readBin(f, "double")
      progress <- progress + as.numeric(msg)
      cat(sprintf("Progress: %.2f%%\n", progress * 100))
    } 
    parallel:::mcexit()
  }
  numJobs <- 100
  result <- mclapply(1:numJobs, function(...) {
    # Do something fancy here... For this example, just sleep
    Sys.sleep(0.05)
    # Send progress update
    writeBin(1/numJobs, f)
    # Some arbitrary result
    sample(1000, 1)
  })
  close(f)
  result
})

cat("Done\n")
