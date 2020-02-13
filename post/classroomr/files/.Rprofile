if (grepl("Windows", Sys.info()["sysname"])) {
    my.lib.path <- file.path("H:", "R", "win-library",
                             paste(R.version$major,
                                   sub("\\..*$", "", R.version$minor),
                                   sep="."))
    if (!dir.exists(my.lib.path)) dir.create(my.lib.path, recursive = TRUE)
    .libPaths(my.lib.path)
    rm(my.lib.path)
}
