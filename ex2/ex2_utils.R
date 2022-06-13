# load\install required packages
for (pkg in c("dplyr", "afex", "Matrix", "lme4")){
    if (!require(pkg, character.only = TRUE)){
        install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
}
rm(list = c("pkg"))

generate_data = function(seed = "none"){
    # Generate Exercise Data
    # Description:  generate continuous observations using six categorical predictors.
    # Arguments:    seed                (optional) integer to use as random seed.
    # Output:       A data frame with the following variables:
    #               y                   a continuous dependent variable
    #               x1, ..., x6         categorical independent variables
    #               id                  subject identifier
    #
    # Example:      generate_data()
    
    # set seed (if specified)
    if (seed != "none") set.seed(seed)
    
    # simulate categorical predictors
    n = 3^8
    preds = data.frame("x1" = rep(c("A", "B", "C"), n/3)) %>%
        group_by(x1) %>%
        mutate(x2 = rep(c("D", "E", "F"), n/3^2)) %>%
        group_by(x1, x2) %>%
        mutate(x3 = rep(c("G", "H", "I"), n/3^3)) %>%
        group_by(x1, x2, x3) %>%
        mutate(x4 = rep(c("J", "K", "L"), n/3^4)) %>%
        group_by(x1, x2, x3, x4) %>%
        mutate(x5 = rep(c("M", "N", "O"), n/3^5)) %>%
        group_by(x1, x2, x3, x4, x5) %>%
        mutate(x6 = rep(c("P", "Q", "R"), n/3^6)) %>%
        ungroup
    
    # generate observations by predictor
    sigma_1 = 5
    sigma_2 = 5
    data = preds %>%
        mutate(y = case_when(x1 == "A" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x1 == "B" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x1 == "C" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2)) +
                   case_when(x2 == "D" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x2 == "E" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x2 == "F" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2)) +
                   case_when(x3 == "G" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x3 == "H" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x3 == "I" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2)) +
                   case_when(x4 == "J" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x4 == "K" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x4 == "L" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2)) +
                   case_when(x5 == "M" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x5 == "N" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x5 == "O" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2)) +
                   case_when(x6 == "P" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x6 == "Q" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2),
                             x6 == "R" ~ rnorm(1, rnorm(1, 0, sigma_1), sigma_2))) %>%
        rowwise %>%
        mutate(y = y + rnorm(1, 0, sigma_1 * 50)) %>% # add normally-distributed noise
        ungroup %>%
        mutate(id = row_number()) %>%
        mutate(across(starts_with("x"), as.factor)) %>%
        as.data.frame
    
    return(data)
}

get_ssw = function(m){
    # Get within-groups sum of squares
    # Description:  extract within-groups sum of squares (SSW) from an ANOVA model object.
    # Arguments:    m           object of class "afex_aov", "aov", or "aov_list"
    # Output:       SSW for the input ANOVA model 
    #
    # Examples:
    # ## using package afex:
    # m = aov_ez(data = data, id = "id", dv = "y", between = list("x1", "x2", "x3"))
    # get_ssw(m)
    # 
    # ## using base R:
    # m = aov(data = data, y ~ x1 * x2 * x3)
    # get_ssw(m)
    
    if (class(m)[1] == "afex_aov"){
        ssw = tail(m$Anova$`Sum Sq`, 1)
    }
    
    else if (class(m)[1] == "aov"){
        x = summary(m)
        ssw = tail(x[[1]]$`Sum Sq`, 1)
    }
    
    else if (class(m)[1] == "aovlist"){
        x = summary(m)
        ssw = tail(x[[2]][[1]]$`Sum Sq`, 1)
    }
    
    else {
        print("Error: class(m) is neither 'afex_aov', 'aov', nor 'aov_list'")
    }
    
    return(ssw)
}

get_mse = function(m){
    # Get mean squared error
    # Description:  extract mean squared error (MSE) from an ANOVA model object.
    # Arguments:    m           object of class "afex_aov", "aov", or "aov_list"
    # Output:       MSE for the input ANOVA model
    #
    # Examples:
    # ## using package afex:
    # m = aov_ez(data = data, id = "id", dv = "y", between = list("x1", "x2", "x3"))
    # get_mse(m)
    # 
    # ## using base R:
    # m = aov(data = data, y ~ x1 * x2 * x3)
    # get_mse(m)
    
    if (class(m)[1] == "afex_aov"){
        mse = tail(m$anova_table$MSE, 1)
        }
    
    else if (class(m)[1] == "aov"){
        x = summary(m)
        mse = tail(x[[1]]$`Mean Sq`, 1)
        }
    
    else if (class(m)[1] == "aovlist"){
        x = summary(m)
        mse = tail(x[[2]][[1]]$`Mean Sq`, 1)
        }
    
    else {
        print("Error: class(m) is neither 'afex_aov', 'aov', nor 'aov_list'")
    }

    return(mse)
}