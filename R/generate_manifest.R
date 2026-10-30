#!/usr/bin/env Rscript

# Writes data/manifest.json from the bundles listed in bundles/catalog.json.

library(jsonlite)

root <- normalizePath(".")

# Path relative to the image root, where the website serves it.
image_path <- function(path) {
    full <- normalizePath(path, mustWork = TRUE)
    if (!startsWith(full, root)) stop(path, " is outside the image")
    sub("^/", "", substring(full, nchar(root) + 1))
}

read_bundle <- function(id, kind) {
    path <- file.path("bundles", id, "bundle.json")
    bundle <- read_json(path)
    if (!identical(bundle$id, id) || !identical(bundle$kind, kind)) {
        stop(path, " is not the ", kind, " bundle ", id)
    }
    bundle$files <- lapply(bundle$files, function(file) {
        local <- file.path(dirname(path), file$path)
        if (!identical(unname(tools::sha256sum(local)), file$sha256) || file.size(local) != file$bytes) {
            stop(local, " does not match its sha256 or size in ", path)
        }
        names(file)[names(file) == "path"] <- "url"
        file$url <- paste0("../", image_path(local))
        file
    })
    bundle[c("$schema", "kind")] <- NULL
    bundle
}

catalog <- read_json("bundles/catalog.json")

releases <- lapply(catalog$releases, function(entry) {
    bundle <- read_bundle(entry$id, "release")
    c(bundle[c("id", "title")], entry["status"], bundle[setdiff(names(bundle), c("id", "title", "files"))],
      entry["views"], bundle["files"])
})
taxonomies <- lapply(catalog$taxonomies, function(entry) read_bundle(entry$id, "taxonomy"))

release_ids <- vapply(releases, `[[`, "", "id")
taxonomy_ids <- vapply(taxonomies, `[[`, "", "id")
if (!catalog$default_release %in% release_ids) stop("default_release ", catalog$default_release, " is not a release")
for (release in releases) {
    used <- c(release$native_taxonomy,
              vapply(release$views, `[[`, "", "taxonomy"),
              unlist(lapply(release$files, `[[`, "taxonomy")))
    missing <- setdiff(used, taxonomy_ids)
    if (length(missing)) stop(release$id, " uses unknown taxonomies: ", paste(missing, collapse = ", "))
}

manifest <- list(
    `$schema` = "https://fishtreeoflife.org/schema/manifest/v1.json",
    schema_version = 1L,
    default_release = catalog$default_release,
    releases = releases,
    taxonomies = taxonomies
)

dir.create("data", showWarnings = FALSE)
writeLines(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), "data/manifest.json")
