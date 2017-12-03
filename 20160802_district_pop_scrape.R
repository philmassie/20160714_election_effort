setwd("D:/My Folders/R/2016/blog/20160714_election_effort")

library("pdftools")
library("readr")
library("stringr")


txt <- pdf_text("data/Report-03-01-672011.pdf")


pop <- txt[66:124]

voters <- as.numeric()
district <- as.character()
check <- as.numeric()

i <- 1
for (i in 1:length(pop)) {
    print(i)

    stemp <- str_split(pop[i],"\r\n")

    data_a_row_start <- grep("Male", stemp[[1]])[1]
    data_b_row_start <- grep("Male", stemp[[1]])[2]

    data_a_row_end <- grep("Male", stemp[[1]])[2] - 1
    data_b_row_end <- grep("Census 2011: Population Dynamics", stemp[[1]]) - 1

    data_a <- stemp[[1]][data_a_row_start:data_a_row_end]
    data_b <- stemp[[1]][data_b_row_start:data_b_row_end]



    is.letter <- function(x) grepl("[[:alpha:]]", x)

    splitter <- function(df){
        #df <- data_a

        i <- 2
        for (i in 2:length(df)){
            # if you start with a space
            if (str_sub(df[i], 1, 1) == " ") {
                if () {}
                # if you contain Total, else
                } else if (str_sub(str_trim(df[i], side = "left"), 1, 5) == "Total" | nchar(df[i - 1]) > 60) {
                    df[i] <- paste0(NA, "    ", NA, "    ", NA, "    ", NA, "    ", str_trim(df[i], side = "left"))
                } else {
                    new_sub <- str_trim(df[i], side = "left")

                    # get last character of previous string
                    lastchar <- str_sub(df[i - 1], -1, -1)
                    if (is.letter(lastchar)) {
                        index <- regexpr("\\ [^\\ ]*$", df[i - 1])[1]

                    } else {
                        new_sub <- paste0("    ", new_sub)
                        index <- nchar(df[i - 1])
                    }

                    df[i - 1] <- paste0(str_sub(df[i - 1], 1, index), new_sub)
                    df[i] <- NA
                }
            } else if (is.letter(str_sub(df[i], 1, 1)) & nchar(df[i]) < 70) {
                df[i] <- NA

            }
        }


        df <- gsub("\\s{2,}", ",", str_trim(df))

        write(df, file = "tmp.csv")

        df <- read.csv("tmp.csv")

        df_a <- df[, c(1:4)]
        df_b <- df[, c(5:8)]

        end_a <- grep("Total", df_a[,1]) - 1
        end_b <- grep("Total", df_b[,1]) - 1

        df_a <- df_a[1:end_a, ]
        df_b <- df_b[1:end_b, ]

        df <- list(df_a, df_b)
        return(df)
    }

    stemp <- splitter(data_a)
    data_a_a <- stemp[[1]]
    data_a_b <- stemp[[2]]

    stemp <- splitter(data_b)
    data_b_a <- stemp[[1]]
    data_b_b <- stemp[[2]]

    tidy_up <- function(df){
        #df <- stemp_a
        # remove spaces and lines with NAs caused by jumping tables
        df <- as.data.frame(sapply(df,gsub,pattern=" ",replacement=""))
        df[df==""] <- NA
        df <- na.omit(df)
        # fix column names
        colnames(df)[c(2:4)] <- c("Male", "Female", "Total")
        # fix row numbers
        row.names(df) <- 1:nrow(df)
        # change columns to numerics
        df[,c(2:4)] <- lapply(df[,c(2:4)], function(x) as.numeric(as.character(x)))

        return(df)
    }

    get_voters <- function(df){
        if (nrow(df) != 18){
            break
        }

        # get district name
        d_name <- str_split(colnames(df)[1], "\\.\\.")[[1]][1]
        #calculate potential voters
        d_voters <- floor(sum(df[5:18, 4]) + ((2/5) * df[5, 4]))
        d_rows <- nrow(df)

        return(c(d_voters, d_name, d_rows))

    }

    data_a_a <- tidy_up(data_a_a)
    data_a_b <- tidy_up(data_a_b)
    data_b_a <- tidy_up(data_b_a)
    data_b_b <- tidy_up(data_b_b)

    voters_a_a <- get_voters(data_a_a)
    voters_a_b <- get_voters(data_a_b)
    voters_b_a <- get_voters(data_b_a)
    voters_b_b <- get_voters(data_b_b)

    voters <- c(voters, voters_a_a[1], voters_a_b[1], voters_b_a[1], voters_b_b[1])
    district <- c(district, voters_a_a[2], voters_a_b[2], voters_b_a[2], voters_b_b[2])
    check <- c(check, voters_a_a[3], voters_a_b[3], voters_b_a[3], voters_b_b[3])
}