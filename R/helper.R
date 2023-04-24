eHandler = function(e, bin_root, bin_roots) {
  if (grepl("chol()", attr(e, "condition")) && (bin_root > 0)) {
    i = which(bin_root == bin_roots)
    msg = paste0("Trying to catch Cholesky decomposition error.",
      "This may appear due to too aggressive binning with a root of ",
      round(bin_root, 2))
    if (i == (length(bin_roots) - 1)) {
      msg = paste0(msg, ". Trying to fit model without binning.")
    } else {
      msg = paste0(msg, ". Now trying with a smaller root of ", round(bin_roots[i+1]), ".")
    }
    warning(msg)
  } else if (grepl("toms748", attr(e, "condition")) && (bin_root > 0)) {
    i = which(bin_root == bin_roots)
    msg = sprintf("Trying to catch optimization error with toms748.
      This may appear due to too aggressive binning with a root of %s",
      round(bin_root, 2))
    if (i == (length(bin_roots) - 1)) {
      msg = paste0(msg, ". Trying to fit model without binning.")
    } else {
      msg = paste0(msg, ". Now trying with a smaller root of ", round(bin_roots[i+1]), ".")
    }
    stop(sprintf("%s: This most likely occurred because of degrees of freedom bigger than the number of groups or unique values in a numerical features.", attr(e, "condition")$message))
    ecatch = TRUE
  } else {
    stop(e)
  }
}
