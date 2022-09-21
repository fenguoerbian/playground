## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----cars---------------------------------------------------------------------
summary(cars)

## ----pressure, echo=FALSE-----------------------------------------------------
plot(pressure)

## ----all_lab1-----------------------------------------------------------------
knitr::all_labels()
ls()

## ---- fun_gen_str_output------------------------------------------------------
Gen_Str_Output_Atom <- function(in_num, digit = 1, pct = TRUE){
    # Generate string output from numeric input
    digit <- min(digit, 4)
    
    if(pct){ # rule for `percentage` output
        if((in_num == 0) | (in_num == 100)){
            res <- as.character(in_num)
        }else{
            res <- sprintf(paste0("%.", digit, "f"), in_num)
        }
    }else{ # rule for other output
        res <- sprintf(paste0("%.", digit, "f"), in_num)
    }
    return(res)
}

Gen_Str_Output <- function(in_num, digit = 1, pct = TRUE){
    res <- mapply(Gen_Str_Output_Atom, in_num = in_num, digit = digit, pct = pct)
    return(res)
}

## ---- fun_form_output---------------------------------------------------------
Form_Output <- function(df_long, 
                        by_var_name = "arm_fct", 
                        col_name = "trt_num", 
                        group_var_name = NULL, 
                        out_1st_name = NULL, 
                        out_1st_val = NULL){
    res <- df_long %>%
        mutate(out_str = str_c(.data[[col_name]], 
                               "(", 
                               .data[[glue::glue("{var_name}_pct_str", 
                                                 var_name = col_name)]], 
                               "%)"))
    if(is.null(group_var_name)){
        res <- res %>%
            select(all_of(by_var_name), out_str) %>%
            pivot_wider(names_from = all_of(by_var_name), 
                        values_from = out_str) 
        if(!is.null(out_1st_name)){
            res <- res %>%
                mutate("{out_1st_name}" := out_1st_val, .before = 1)
        }else{
            res <- res %>%
                mutate("{col_name}" := " ", .before = 1)
        }
    }else{
        grp_lvls <- levels(df_long %>% pull(all_of(group_var_name)))
        
        res <- res %>%
            pivot_wider(id_cols = .data[[group_var_name]], 
                        names_from = all_of(by_var_name), 
                        values_from = out_str) %>%
            arrange(factor(.data[[group_var_name]], levels = grp_lvls)) %>%    # 确保输出行的顺序与原始`group_var_name`的level一致
            mutate("{group_var_name}" := as.character(.data[[group_var_name]]))
        if(!is.null(out_1st_name)){
            res <- res %>%
                rename("{out_1st_name}" := all_of(group_var_name))
        }
    }
    
    return(res)
}

## ----load_child, echo = TRUE, results='hide'----------------------------------
child_res <- lapply(c("gen_str_output.Rmd", "form_output.Rmd"),
                    knitr::knit_child, 
                    quiet = TRUE, 
                    envir = environment())

## ----all_lab2-----------------------------------------------------------------
knitr::all_labels()
ls()

## ---- eval = TRUE-------------------------------------------------------------
Gen_Str_Output(1 : 10, digit = 2)

## ----show_child, echo = FALSE, results='asis'---------------------------------
cat(unlist(child_res), sep = "\n")

## ---- all_lab3----------------------------------------------------------------
knitr::all_labels()

## ---- test_ref2, ref.label="fun_gen_str_output"-------------------------------
Gen_Str_Output_Atom <- function(in_num, digit = 1, pct = TRUE){
    # Generate string output from numeric input
    digit <- min(digit, 4)
    
    if(pct){ # rule for `percentage` output
        if((in_num == 0) | (in_num == 100)){
            res <- as.character(in_num)
        }else{
            res <- sprintf(paste0("%.", digit, "f"), in_num)
        }
    }else{ # rule for other output
        res <- sprintf(paste0("%.", digit, "f"), in_num)
    }
    return(res)
}

Gen_Str_Output <- function(in_num, digit = 1, pct = TRUE){
    res <- mapply(Gen_Str_Output_Atom, in_num = in_num, digit = digit, pct = pct)
    return(res)
}

## ---- all_lab4----------------------------------------------------------------
knitr::all_labels()

