#' @include utils.R
NULL

#' Restrict function environment
#' 
#' Build a new function with an optimal scope. Normally the entire
#' environment tree (including the entire ancestry) is kept in memory,
#' but with `restrict_fn_env()` you get a function that has only
#' a copies of all needed variables and the scope ancestry holds only the
#' global environment.
#' @param fn A function whose parent scope should be restricted to the
#'   set of variables given in `vars` and the loaded packages.
#' @param vars An optional object, telling which variables should be
#'   available in `fn()`. It can either be
#'   - a `character` vector holding the names of the variables which should 
#'     looked up in the environment `lookup_env`.
#'   - a named `list`: In this case, the values are not looked up in
#'     `lookup_env`, but directly taken from the list item values and the
#'     list item names are used as variable names.
#' @param lookup_env The environment holding the variables for which the names
#'   are defined in the  character vector `vars`.
#'   If `vars` is a list or `NULL`, then `lookup_env` is not used.
#'   The default for `lookup_env` is the environment where the function `fn`
#'   was defined.
#' @return A new function with a small scope containing the variables given 
#'   in `vars`.
#' @export
restrict_fn_env <- function(fn, vars = NULL, lookup_env = environment(fn)) {
  err_h <- function(msg)
    stop(paste("Error while calling `restrict_fn_env()`:", msg), call. = FALSE)
  if (!is.function(fn))
    err_h("Argument `fn` must be a function.")
  if (!is.null(vars) && !is.list(vars) && !is.character(vars))
    err_h("Argument `vars` must either be a named list or a character vector or omitted.")
  if (is.list(vars)) {
    names_vars <- names(vars)
    if (is.null(names_vars) || any(is.na(names_vars)))
      err_h("Argument `vars` is a list, but not a named list.")
    id_names_invalid <- which(names_vars != make.names(names_vars))
    if (length(id_names_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a named list, but the names of the following",
        "entries are not valid variable names:\n  ",
        stringify(id_names_invalid)
      ))
    if (length(names_vars) != length(unique(names_vars)))
      err_h("Argument `vars` is a named list, but some names have duplicates.")
  } else if (is.character(vars)) {
    if (any(is.na(vars)))
      err_h("Argument `vars` is character vector, but contains `NA` values.")
    id_vars_invalid <- which(vars != make.names(vars))
    if (length(id_vars_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a character vector, but the following",
        "entries are not valid variable names:\n  ",
        stringify(id_vars_invalid)
      ))
    if (length(vars) != length(unique(vars)))
      err_h("Argument `vars` is a character vector, but has duplicate entries.")
    if (!is.environment(lookup_env))
      err_h(paste(
        "Since argument `vars` is a character vector, then `lookup_env`",
        "must be an environment, but it is not."
      ))
  }
  # restrict
  if (is.character(vars)) {
    names(vars) <- vars
    vars <- lapply(
      vars,
      function(v) get(v, envir = lookup_env)
    )
  }
  new_env <- new.env(parent = .GlobalEnv)
  for(var in names(vars)) {
    assign(
      var,
      vars[[var]],
      envir = new_env
    )
  }
  environment(fn) <- new_env
  fn
}

#' Eval code in closure without scoping problems (memory leaks)
#' 
#' @param vars An optional object, telling which variables should be
#'   available inside the closure. It can either be
#'   - a `character` vector holding the names of the variables which should 
#'     looked up in the environment `lookup_env`.
#'   - a named `list`: In this case, the values are not looked up in
#'     `lookup_env`, but directly taken from the list item values and the
#'     list item names are used as variable names.
#' @param expr The expression, which should be evaluated inside of the
#'   closure.
#' @inheritParams restrict_fn_env
#' @export
eval_closure <- function(
  expr,
  vars = NULL,
  lookup_env = parent.frame()
) {
  err_h <- function(msg)
    stop(paste("Error while calling `eval_closure()`:", msg), call. = FALSE)
  if (!is.null(vars) && !is.list(vars) && !is.character(vars))
    err_h("Argument `vars` must either be a named list or a character vector or omitted.")
  if (is.list(vars)) {
    names_vars <- names(vars)
    if (is.null(names_vars) || any(is.na(names_vars)))
      err_h("Argument `vars` is a list, but not a named list.")
    id_names_invalid <- which(names_vars != make.names(names_vars))
    if (length(id_names_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a named list, but the names of the following",
        "entries are not valid variable names:\n  ",
        stringify(id_names_invalid)
      ))
    if (length(names_vars) != length(unique(names_vars)))
      err_h("Argument `vars` is a named list, but some names have duplicates.")
  } else if (is.character(vars)) {
    if (any(is.na(vars)))
      err_h("Argument `vars` is character vector, but contains `NA` values.")
    id_vars_invalid <- which(vars != make.names(vars))
    if (length(id_vars_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a character vector, but the following",
        "entries are not valid variable names:\n  ",
        stringify(id_vars_invalid)
      ))
    if (length(vars) != length(unique(vars)))
      err_h("Argument `vars` is a character vector, but has duplicate entries.")
    if (!is.environment(lookup_env))
      err_h(paste(
        "Since argument `vars` is a character vector, then `lookup_env`",
        "must be an environment, but it is not."
      ))
  }
  # eval closure
  if (is.character(vars)) {
    names(vars) <- vars
    vars <- lapply(
      vars,
      function(v) get(v, envir = lookup_env)
    )
  }
  vars[["expr"]] <- substitute(expr)
  new_env <- new.env(parent = .GlobalEnv)
  for(var in names(vars)) {
    assign(
      var,
      vars[[var]],
      envir = new_env
    )
  }
  local(
    tryCatch(
      eval(expr),
      error = function(e) stop(
        "Error while calling `eval_closure()`: ",
        "The following expression could not be evaluated:\n  '",
        deparse(expr),
        "'\n",
        e,
        "Please pass the expression directly to `eval_closure()` and ensure ",
        "that all needed variables are specified in `vars`."
      ),
      finally = rm(expr)
    ),
    envir = new_env
  )
}

#' List all passed in function arguments
#' 
#' This functions returns a named list holding the argument values of the
#' current function call.
#' This includes:
#'   - arguments assigned by name (e.g. `foo(x = 1)`)
#'   - arguments assigned by position (e.g. `foo(1)`)
#'   - arguments for which the default value was overwritten (e.g. `foo(x = 1)` with `foo <- function(x = 99)`)
#'   - arguments for which the default value was used (e.g. `foo()` with `foo <- function(x = 99)`)
#'   - named and unnamed arguments inside of a three dots ellipsis
#' @param n The number ob frames to go back. The default value `n = 0L` means,
#'   that the given arguments of current function should be returned.
#' @return A named list holding the arguments of the function call (including
#'   non-changed default values)
#' @export
get_call_args <- function(n = 0L) {
  get_call_args_helper(
    n = n + 1L,
    err_h = composerr("Error while calling `get_call_args()`")
  )
}
  
#' Helper function for [get_call_args()]
#'
#' @inheritParams get_call_args
#' @param err_h An error handling function created with [composerr::composerr()].
get_call_args_helper <- function(n = 1L, err_h) {
  err_h <- function(msg) stop(paste("Error while calling `get_call_args()`:", msg), call. = FALSE)
  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 0 || as.integer(n) != n)
    err_h("Argument `n` must be a non-negative integer value.")
  invisible(tryCatch(
    if (is.null(sys.call(n + 2L)))
      stop(),
    error = function(e) err_h(paste0(
      "Argument `n` (", stringify(n), ") is greater than the number of ",
      "call stacks, please use a different value for `n`."
    ))
  ))
  # all passed in arguments from the call
  args_call <- as.list(match.call(
    definition = sys.function(sys.parent(n + 1L)),
    call = sys.call(sys.parent(n + 1L)),
    envir = parent.frame(n + 2L)
  ))
  args_call <- args_call[!names(args_call) %in% c("")]
  for(i in seq_along(args_call)) {
    args_call[[i]] <- eval(
      args_call[[i]],
      envir = parent.frame(n + 2L)
    )
  }
  # possible defaults values of the function
  args_default <- as.list(rlang::fn_fmls(fn = rlang::caller_fn(n = n + 1L)))
  args_default <- args_default[names(args_default) != "..."]
  args_default <- args_default[!unlist(lapply(
    args_default,
    rlang::is_missing
  ))]
  plyr::defaults(args_call, args_default)
}

#' Call a function with similar arguments as current function call
#' 
#' This function allows can be called from within a function call 
#' (e.g. `foo(x = 1, y = 2)`) in order to call another function
#' (e.g. `baz(x = 1, y = 2, z = 3`) with using the same (or
#' almost the same) arguments as in the current function call
#' (e.g. by calling `call_fn_with_similar_args(baz, z = 3)` inside of `foo()`).
#' @param fn The function that should be called.
#' @param ... Additional function parameters, which should be added to the 
#'   arguments of the current function call.
#' @param skip An optional character vector holding names of
#'   arguments of the current function call which should **not be used** for the
#'   call of `fn`.
#' @param n The number ob frames to go back. The default value `n = 0L` means,
#'   that the arguments of the current function call should be used.
#' @return The return value of the called function given in `fn`.
#' @export
call_with_similar_args <- function(fn, ..., skip = NULL, n = 0L) {
  err_h <- composerr("Error while calling `call_with_similar_args()`")
  if (!is.function(fn))
    err_h("Argument `fn` must be a function.")
  if ((!is.null(skip) && !is.character(skip)) || (is.character(skip) && any(is.na(skip)))) {
    err_h("Argument `skip` must either be `NULL` or a character vector without `NA` entries.")
  }
  args <- get_call_args_helper(n = n + 1L, err_h = err_h)
  wrong_names <- skip[!skip %in% names(args)]
  if (length(wrong_names) > 0L)
    err_h(paste0(
      "The following variables passed in argument `skip`",
      "are no arguments of the current function call:\n\t",
      stringify(wrong_names),
      "\nOnly the following are available:\n\t",
      stringify(names(args)),
      "\n"
    ))
  args <- plyr::defaults(
    list(...),
    args[!names(args) %in% skip]
  )
  do.call(
    fn,
    args = args
  )
}

