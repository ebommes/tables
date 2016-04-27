# Install packages if not installed
libraries = c("xtable", "datasets")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Example summary function
summarize = function(df){
    df.mean  = round(sapply(df, mean), 2)
    df.sd    = round(sapply(df, sd), 2)
    df.quant = sapply(data.df, quantile)
    
    # Construct dataframe
    df.sum = t(rbind(df.mean, df.sd, df.quant))
    df.sum = data.frame(Variable = row.names(df.sum), df.sum)

    # Delete rownames
    row.names(df.sum) = NULL

    # Construct vector with TeX names
    cnames = c("Variable", "$\\mu$", "$\\sigma$", "$\\mbox{Min}$", 
               "$\\rho_{0.25}$", "$\\rho_{0.5}$", "$\\rho_{0.75}$", "$\\mbox{Max}$")

    # Small changes
    n           = length(cnames)
    cnames[1]   = paste("\\multicolumn{1}{c|}{", cnames[1], "}", sep = "")
    cnames[2:n] = paste("\\multicolumn{1}{c}{", cnames[2:n], "}", sep = "")

    # Change dataframe names
    names(df.sum) = cnames

    return(df.sum)
}

# Function: number of decimal places
dec = function(x){
    if(!is.numeric(x)) return(0);
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}

max.dec = function(x){
    return(max(sapply(x, dec)))
}

# Load example dataset
data(airquality)
data.df = airquality
data.df = na.omit(data.df)
data.df = data.df[!names(data.df) %in% c("Month", "Day")]

# Apply summary function
sum.df = summarize(data.df)

# Some xtable specifications
tab.cap              = "Summary statistics of airquality data set"
tab.dec              = c(0, sapply(sum.df, max.dec))
tab.dec[tab.dec > 2] = 2

tab.spec = xtable(sum.df, caption = tab.cap, digits = tab.dec)

# Alignment of columns - Use siunitx TeX package
al              = paste(rep("S", ncol(sum.df) - 1), collapse = "")
al              = paste("cl|", al, sep = "")
align(tab.spec) = al

# Print table
tab = print(tab.spec,
            include.rownames = FALSE,
            sanitize.colnames.function = function(x) x,
            hline.after = c(-1, -1, 0, nrow(tab.spec), nrow(tab.spec)))

cat("\n \n Copy the following table \n \n")
cat(tab)
