
I have a table that lookes like this:


suppressPackageStartupMessages(library(tidyverse))
tbl1 <- tibble::tribble(~datetime, ~systolic, ~diastolic,
lubridate::as_datetime("2017-03-25 17:00:00"), 104, 78,
lubridate::as_datetime("2017-03-26 04:00:00"), 107, 75,
lubridate::as_datetime("2017-03-26 17:15:00"), 107, 84
)
tbl1


To get the kind of plot I want I  realize that it would
be much easier to work with if it was a table like this:

tbl2 <- tibble::tribble(~datetime, ~kind, ~pressure,
lubridate::as_datetime("2017-03-25 17:00:00"), "systolic", 104,
lubridate::as_datetime("2017-03-25 17:00:00"), "diastolic", 78,
lubridate::as_datetime("2017-03-26 04:00:00"), "systolic", 107,
lubridate::as_datetime("2017-03-26 04:00:00"), "diastolic", 75,
lubridate::as_datetime("2017-03-26 17:15:00"), "systolic", 107,
lubridate::as_datetime("2017-03-26 17:15:00"), "diastolic", 84
)
tbl2

ggplot2::ggplot(tbl2, aes(datetime, pressure, color = factor(kind))) + geom_line() + geom_smooth()


