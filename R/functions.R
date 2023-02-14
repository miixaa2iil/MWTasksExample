#' @import(dplyr)
#' @import(ggplot2)


#'
#' @docType data
#' @keywords datasets
#' @name df_list
#' @usage data(df_list)
#' @format A 6-element list

#' @title level2logical
#' @description Function adds one logical variable indicating if the given level is observed
#' @param df [[data.frame]] data
#' @param var [[character]] variable name to check the level presence
#' @param level [[character]]/[[numeric]] chosen level
#'
#' @return [[data.frame]] new data.frame with added variable
#' @export
level2logical <- function(df, var, level, negate = FALSE) {
  stopifnot(is.data.frame(df), # df must be a data.frame
            any(class(df[[var]]) == c("character",
                                      "factor",
                                      "numeric"
            ) # character and factor are natural, numeric is acceptable but maybe a bit strange
            ),
            is.logical(negate) # negate must be logical
  )
  res <- df %>% mutate(across(all_of(var),
                              "if"(negate,
                                   !(~.x %in% level),
                                   ~.x %in% level), # %in% instead ==, NA's returned as FALSE not NA
                              .names = paste("is",
                                             tolower(var), # only lowercase in output name
                                             tolower(gsub(" ", "_", level)), # the same and space replacement to _
                                             sep ="_"
                              ) # ouput name: is_<original name>_<given_level>
  )
  )
  return(res)
}

#' @title calculate_number_and_ratio
#' @description Function calculates number and ratio of given variable which can be grouped
#' @param df [[data.frame]] data
#' @param group [[character]] group variable name,
#' \itemize{
#' \item option1
#' \item  option2
#' \item option3},
#'  by default `NULL`
#' @param var [[character]] variable to summarise
#'
#' @return [[data.frame]]  summarised data.frame
#' @export
calculate_number_and_ratio <- function(df, group = NULL, var) {
  stopifnot(is.data.frame(df), # df must be a data.frame
            is.null(group) | is.character(group), # group is not a required parameter, if it occurs, must be a character
            is.character(var),#  var must be a character
            is.logical(df[[var]]) # and class of the column named var must be logical
  )
  if(!is.null(group)) df <- df %>% group_by_at(group) # group only if needed
  res <- df %>% summarise(across(all_of(var), # selected column
                                 .fns= list(number=sum,
                                            ratio=mean), # function used
                                 .names = "{.fn}" # names given by .fns list
  )
  )
  return(res)
}


#' @title calculate_rand_source
#' @description Function calculates rand source number and ratio in groups
#' @param df_list [[list]] list of data.frames
#' @param group [[character]] group variable name \itemize{
#' \item option1
#' \item option2
#' \item option3}
#'
#' @return [[data.frame]]  summarised data.frame
#' @export
calculate_randomized_subjects <- function(df_list,
                                          group) {
  stopifnot(is.list(df_list))
  task_var <- "is_my_date_NA"
  date_var <- "my_date"
  res <- switch(group,
                "option1" = {merge(df2,
                                   df_list$df1,
                                   by = "source", # merge data by subject
                                   all.x = TRUE) %>% level2logical(var=date_var, # only randomized subject
                                                                   level = NA)  %>%  calculate_number_and_ratio(group = "my_group",
                                                                                                                var = task_var
                                                                   ) # number and ratio by study
                },
                "option2" = {merge(df_list$df3,
                                   df_list$df4,
                                   by = "source",
                                   all.x = TRUE) %>% level2logical(var=date_var,
                                                                   level = NA) %>% calculate_number_and_ratio(group = group,
                                                                                                              var = task_var
                                                                   )
                },
                "option3" = {merge(df_list$df3,
                                   df_list$df1,
                                   by = "source",
                                   all.x = TRUE) %>% merge(df_list$df4,
                                                           by = "option2",
                                                           all.x = TRUE
                                   ) %>% level2logical(var=date_var,
                                                       level = NA) %>% calculate_number_and_ratio(group = group,
                                                                                                  var = task_var
                                                       )
                }
  )
  return(res)
}


#' @title calculate_missing_data
#' @description Function calculates missing data number and ratio in groups after subsetting
#' @param df_list [[list]] list of data.frames
#' @param group [[character]] group variable name \itemize{
#' \item study
#' \item site
#' \item country}
#' @param subset [[character]] subset name, currently only `rand source`
#' @return [[data.frame]]  calculated data.frame
#' @export
calculate_missing_data <- function(df_list,
                                   group,
                                   subset) {
  stopifnot(is.list(df_list),
            is.character(subset)
  )

  res <- switch(subset,
                "rand source" = { # currently only this case
                  task_var <- "is_task_col_type_missing_data" # task_var assign
                  missing_data_df <- merge(df_list$df1,
                                           df_list$df2,
                                           by = "source",
                                           all.x = TRUE # it means that only randomized subjects are being taken into consideration
                  ) %>% level2logical(
                    var = "task_col",
                    level = "Missing data"
                  )
                  switch(group,
                         "option1" = {merge(missing_data_df,
                                            df_list$df3,
                                            by = "source",
                                            all.x = TRUE) %>% calculate_number_and_ratio(group = "my_group",
                                                                                         var = task_var
                                            ) # number and ratio by study
                         },
                         "option2" = {merge(missing_data_df,
                                            df_list$df4,
                                            by = "source",
                                            all.x = TRUE) %>% calculate_number_and_ratio(group = group,
                                                                                         var = task_var
                                            ) # number and ratio by study
                         },
                         "option3" = {merge(missing_data_df,
                                            df_list$df4,
                                            by = "source",
                                            all.x = TRUE) %>% merge(df_list$country,
                                                                    by = "option2",
                                            ) %>% calculate_number_and_ratio(group = group,
                                                                             var = task_var
                                            )
                         })})

  return(res)

}


#' @title extract_biggest_value
#' @description Function extracts the row with the maximal value
#' @param df [[data.frame]] data
#' @param vars [[character]] variable names, ordered by the importance
#'
#' @return [[data.frame]]  1-row data.frame with the maximal value of the first chosen variable (and distant one in case of ties)
#' @export
extract_biggest_value <- function(df, vars) {
  stopifnot(is.data.frame(df),
            is.character(vars))
  res <- df %>% slice_max(across(all_of(vars[1]), ~.x))
  if (nrow(res) == 1) { # in this case only function ends
    return(res)
  } else {
    vars <- vars[-1] # exclude the first element
    extract_biggest_value(df=res, vars = vars)
  }
}


#' @title generate_date_plot
#' @description Function extracts the row with the maximal value
#' @param df [[data.frame]] data
#' @param vars [[character]] named vector with variable shown on x axis and y axis
#' @param colour [[character]] colour name, by default `"black"`
#' @param scale_date [[character]] named vector with tick settings, breaks denote the time period and labels denote the date format
#' @param titles [[character]] name vector with labels for the x axis, y axis and plot title as main
#'
#' @return [[gg]]/[[ggplot]] ggplot object  1-row data.frame with the maximal value of the first chosen variable(and distant one in case of ties)
#' @export
generate_date_plot <- function(df,
                               vars,
                               colour = "black",
                               scale_date,
                               titles = c(x="x", y = "y", main = "")
) {
  stopifnot(is.data.frame(df),
            is.character(vars),
            is.character(colour),
            is.character(scale_date),
            is.character(titles),
            length(vars) == 2,
            length(colour) == 1,
            length(scale_date) == 2,
            length(titles) == 3,
            all(names(vars) %in% c("x", "y")),
            all(names(scale_date) %in% c("breaks", "labels")),
            all(names(titles) %in% c("x", "y", "main"))) # parameters

  ggplot(df ,
         aes_string(x=vars["x"], # variable in x axis
                    y = vars["y"] # variable in y axos
         ) # increasing change check
  ) +
    geom_point(colour = colour) +  # points colour
    geom_smooth(method=lm,
                level = .95
    ) + # trend line plus confidence level
    scale_x_date(date_breaks = scale_date["breaks"], # period shown
                 date_labels = scale_date["labels"] # format shown
    ) + # date scale by month abbr
    xlab(titles["x"]) + # x axis label
    ylab(titles["y"]) + # y axos label
    ggtitle(titles["main"]) # plot title
}


#' @title fill_values_in_list
#' @description Function fills in or extend the given list
#' @param task_list [[list]] data
#' @param names_out [[character]] list element names for calculated values
#' @param names_in [[character]] list elements names used in calculations
#' @param fun [[character]] function name, currently only `"extract_biggest_value"`
#' @param groups [[character]] vector with group variable names
#' @param ... additional parameters
#'
#' @return [[list]] filled in/extended list
#' @export
fill_values_in_list <- function(task_list, names_in, names_out, fun, groups = NULL, ...) {
  task_list[names_out] <- switch(fun, # replace/append calculation into names_out
                                 "extract_biggest_value" = lapply(names_in, # list iteration (provided by name_in)
                                                                  function(el){
                                                                    sapply(groups, # groups iteration
                                                                           function(group) extract_biggest_value(task_list[[el]][[group]],
                                                                                                                 vars = ...)[[group]],
                                                                           simplify = TRUE, # vector if possible
                                                                           USE.NAMES = TRUE # name one
                                                                    )
                                                                  }
                                 )
  )
  return(task_list)
}
