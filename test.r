#Analysis
ECm.analysis(db=Flow.db,FUN.month = mean,Min.days.annual = 360,Max.miss.days.month = 5,
             output = "Flow.syn",EC = F,matrix.start.year = 1960)


# slow
testit <- function(x = sort(runif(20)), ...)
{
        pb <- txtProgressBar(...)
        for(i in c(0, x, 1)) {Sys.sleep(0.5); setTxtProgressBar(pb, i)}
        Sys.sleep(1)
        close(pb)
}
testit()
testit(runif(10))
testit(style = 3)
