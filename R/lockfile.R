# Given a list of named lists, return a list of all the names used
collectFieldNames <- function(lists) {
  allFieldNames <- character(0)
  for (lst in lists)
    allFieldNames <- union(allFieldNames, unique(names(lst)))
  return(allFieldNames)
}

# Create a single-row data frame with the given column names and all NA values
naRow <- function(fieldNames) {
  structure(
    do.call(data.frame, as.list(rep.int(NA, length(fieldNames)))),
    names = fieldNames
  )
}

# Like rbind.data.frame but tolerates heterogeneous columns, filling in any
# missing values with NA
rbind2 <- function(df1, df2) {
  allNames <- union(names(df1), names(df2))
  
  missing1 <- setdiff(allNames, names(df1))
  missing2 <- setdiff(allNames, names(df2))
  
  return(rbind(
    cbind(df1, naRow(missing1)),
    cbind(naRow(missing2), df2)
  ))
}

writeLockFile <- function(file, lockinfo) {
  
  rver <- as.character(getRversion())
  
#   json <- toJSON(list(
#     packrat_format = '1.0',
#     packrat_version = as.character(packageVersion('packrat')), 
#     r_version = rver,
#     repos = activeRepos(),
#     packages = lockinfo
#   ), pretty=TRUE)
#   writeLines(json, con = file)
  
  # The first record contains metadata about the project and lockfile
  preamble <- data.frame(
    PackratFormat = "1.1",
    PackratVersion = as.character(packageVersion("packrat")),
    RVersion = rver,
    Repos = activeRepos()
  )
  
  # Remaining records are about the packages
  packages <- flattenPackageRecords(lockinfo, depInfo=TRUE, sourcePath=TRUE)
  fieldNames <- collectFieldNames(packages)
  packageInfo <- lapply(fieldNames, function(fieldName) {
    values <- data.frame(vapply(packages, function(pkg) {
      if (!is.null(pkg[[fieldName]]))
        pkg[[fieldName]]
      else
        NA_character_
    }, character(1), USE.NAMES = FALSE))
    names(values) <- fieldName
    return(values)
  })
  packageInfoDf <- do.call(data.frame, packageInfo)
  
  write.dcf(rbind2(preamble, packageInfoDf), file, indent = 2, width = 72)
  
  invisible()
}

readLockFile <- function(file) {
  df <- as.data.frame(read.dcf(file))
  list(
    packrat_format = df[1, 'PackratFormat'],
    packrat_version = df[1, 'PackratVersion'],
    r_version = df[1, 'RVersion'],
    repos = df[1, 'Repos'],
    packages = deserializePackages(tail(df, -1))
  )
}

# @param graph Named list where the names are the packages and the values
#   are the names of the packages that they depend on. Packages with no
#   dependencies should have character(0) or NULL.
# @return Sorted character vector of package names
topoSort <- function(graph) {
  packageNames <- names(graph)
  
  # Key: dependency, Value: dependent
  # Use this to answer: What things depend on this key?
  dependents <- new.env(parent = emptyenv(), size = as.integer(length(packageNames) * 1.3))
  # Key: dependent, Value: Number of dependencies
  # Use this to answer: How many things does this key depend on?
  dependencyCount <- new.env(parent = emptyenv(), size = as.integer(length(packageNames) * 1.3))
  for (packageName in packageNames)
    dependencyCount[[packageName]] <- 0
  
  # Initialize dependents and dependencyCount
  for (pkgName in packageNames) {
    for (r in graph[[pkgName]]) {
      dependents[[r]] <- c(dependents[[r]], pkgName)
      dependencyCount[[pkgName]] <- dependencyCount[[pkgName]] + 1
    }
  }
  
  if (length(setdiff(ls(dependents), packageNames)) > 0)
    stop("Corrupted lockfile: missing dependencies") # TODO: better message
  
  # Do topo sort
  sortedNames <- character(0)
  leaves <- packageNames[vapply(packageNames, function(pkgName) {
    identical(dependencyCount[[pkgName]], 0)
  }, logical(1))]
  
  while (length(roots) > 0) {
    leaf <- leaves[[1]]
    leaves <- tail(leaves, -1)
    
    sortedNames <- c(sortedNames, leaf)
    
    # See who depends on the leaf
    for (dependent in dependents[[leaf]]) {
      # Decrease the dependency count for this dependent
      dependencyCount[[dependent]] <- dependencyCount[[dependent]] - 1
      # Is this dependent now a leaf?
      if (identical(dependencyCount[[dependent]], 0)) {
        leaves <- c(leaves, dependent)
        do.call(rm, list(dependent, envir = dependencyCount))
      }
    }
    rm('leaf', dependents)
  }
  
  if (!setequal(sortedNames, packageNames))
    stop("Corrupt lockfile: circular package dependencies detected")
  
  sortedNames
}

deserializePackages <- function(df) {
  packageNames <- df[, 'name']
  if (any(is.na(packageNames)))
    stop("Invalid lockfile format: missing package name detected")
  
  graph <- lapply(seq.int(nrow(df)), function(i) {
    if (is.na(df[i, 'requires']))
      character(0)
    else
      unique(strsplit(df[i, 'requires'], '\\s*,\\s*'))
  })
  names(graph) <- packageNames
  topoSorted <- topoSort(graph)
  
  # LEFT OFF HERE
}

readLockFileLegacy <- function(file) {
  lines <- readLines(file, warn=FALSE, encoding='UTF-8')
  obj <- fromJSON(paste(lines, collapse='\n'))
  if (is.null(obj$packrat_format)) {
    # Earliest format. We can auto-upgrade.
    return(list(
      packrat_format = '1.0',
      packrat_version = '0.0.0',
      r_version = NULL,
      repos = activeRepos(),
      packages = obj
    ))
  } else if (compareVersion('1.0', obj$packrat_format) < 0) {
    # Future format. Abort.
    stop("The lockfile was written by an incompatible version of packrat (",
         obj$packrat_version,
         ").\nPlease upgrade packrat and try again.")
  } else {
    return(obj)
  }
}
