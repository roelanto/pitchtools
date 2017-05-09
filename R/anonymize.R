#' Return an anonymized patient label.
#'
#' @param group The patient group of the patient.
#' @param name The name of the patient.
#' @param groups A vector with groups.
#' @param names A list with group names and patients, see example.
#' @export
#' @return A string such as "Subject #12", where the numbering is consistent with the names list.
#' @examples
#' library(pitchtools)
#'
#' names <- list(
#' alzheimer=c("A", "B", "C", "D I", "D II", "E"),
#' control=c("A", "B I", "B II")
#' )
#' anonymize(name="A", group="alzheimer", groups=c("alzheimer", "control"), names=names) # return 1
#' anonymize(name="A", group="control", groups=c("alzheimer", "control"), names=names) # return 7

anonymize <- function(group, name, groups, names, unique=TRUE) {
  if (length(group) != length(name) || length(group) < 1) {
    message(paste("Error, group and name vectors should be >0 and same size, length(group)=",length(group)," length(name)=",length(name)))
  } else {
    if (length(group) == 1) {
      return(anonymize_atomic(group, name, groups, names, unique))
    } else {
      retVal <- vector()
      for (i in c(1:length(group))) {
        theGroup <- group[i]
        theName <- name[i]
        retVal <- append(retVal, anonymize_atomic(theGroup, theName, groups, names, unique))
      }
      return(retVal)
    }
  }
}


anonymize_atomic <- function(group, name, groups, names, unique) {
  groupno <- which(groups == group)
  nameno <- which(names[[as.character(group)]] == name)
  namesCount <- 0
  translatedGroupName <- list(control="Control", alzheimer="AD", parkinson="PD", ppa="PPA-NF", ppasd="PPA-SD")
  if (unique == TRUE) {
    if (groupno > 1) {
      for (i in c(1:(groupno-1))) {
        namesCount <- namesCount + length(names[[groups[i]]])
      }
    }
  } else {
    namesCount <- 0
  }
  if (is.null(translatedGroupName[[as.character(group)]])) {
    groupname <- group
  } else {
    groupname <- translatedGroupName[[as.character(group)]]
  }
  return(paste0(groupname, " #", namesCount + nameno))
}

testAnonymize <- function() {
  names <- list(a=c("A", "B", "C"), b=c("X", "Y", "Z"), c=c("D", "E", "F"))
  groups <- c("a", "b", "c")
  retVal <- anonymize(group="b", name="Y", unique=FALSE, groups=groups, names=names)
  print(paste("#1 expected 'b #2', got: ", retVal))
  retVal <- anonymize(group="b", name="Y", unique=TRUE, groups=groups, names=names)
  print(paste("#1 expected 'b #5', got: ", retVal))
  retVal <- anonymize(group=c("a", "b"), name=c("A", "Y"), groups=groups, names=names)
  print(paste("#1 expected 'a #1' 'b #5', got: ", retVal))
}

testAnonymize()


