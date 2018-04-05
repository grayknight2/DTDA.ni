
# On load
#===================
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Non\u002Diterative estimator for the cumulative distribution of a doubly truncated variable\u002C see de U\u00F1a\u002D\u00C1lvarez \u00282018\u0029\u002E The package is restricted to interval sampling\u002E

de U\u00F1a\u002D\u00C1lvarez J\u002E \u00282018\u0029 A Non-iterative Estimator for Interval Sampling and Doubly Truncated Data\u002E In\u003A Gil E\u002E\u002C Gil E\u002E\u002C Gil J\u002E\u002C Gil M\u002E \u0028eds\u0029 The Mathematics of the Uncertain\u002E Studies in Systems\u002C Decision and Control\u002C vol 142\u002E Springer\u002C Cham\u002C pp\u002E 387\u002D400\u002E

SiDOR Group\u002E University of Vigo\u002E")
}

# Options
#===================
# .onLoad <- function(libname, pkgname) {
#   op <- options()
#   op.devtools <- list(
#     devtools.path = "~/R-dev",
#     devtools.install.args = "",
#     devtools.name = "José Carlos Soage",
#     devtools.desc.author = '"Jacobo de Uña Álvarez <jacobo@uvigo.es> [aut]"',
#     devtools.desc.license = "GL-2",
#     devtools.desc.suggests = NULL,
#     devtools.desc = list()
#   )
#   toset <- !(names(op.devtools) %in% names(op))
#   if (any(toset))
#     options(op.devtools[toset])
#   invisible()
# }
