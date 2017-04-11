#' GetToIt
#'
#' A function about ordering boxplot
#' plot ggplot2 BoxPlot in the order you want.
#'
#'
#'
#'@param listOflist A list of lists, possibly named
#'@param newOrder An index of the lists in the order you want them to be plot
#'
#'
#'@examples
#'magic <- list(list(c(rnorm(20,2,1)), c(rnorm(8,8,1))), list(c(rnorm(20,3,1)), c(rnorm(8,7,1))), list(c(rnorm(20,4,1)), c(rnorm(8,6,1))))
#'names(magic) <- c(1:length(magic))
#'order= c("2","3", "1")
#'p <- ggBoxPlot(magic, order)
#'
#'@export
#'



ggBoxPlot <-function(listOflist, newOrder) {

  boxNames       <- names(listOflist)

  foo = lapply(seq_along(listOflist), function(i) {

    currentBox   <- listOflist[[i]]

    ##get p-value from wilcox.test
    if(all(is.na(unlist(currentBox)))){
      return(NA)
    }
    else{

      dfelectro  <- data.frame(value = unlist(currentBox[[1]]), type = "electro", group = boxNames[i], stringsAsFactors = FALSE)
      dfmetel    <- data.frame(value = unlist(currentBox[[2]]), type = "metal", group = boxNames[i], stringsAsFactors = FALSE)
      dfTot      <- rbind(dfelectro, dfmetel)

      return(dfTot)
    }
  })

  bigDF          <- do.call(rbind, foo)


  tmp            <- factor(bigDF$group, levels = newOrder, ordered = TRUE)
  bigDF          <- bigDF[order(tmp),]

  ind            <- apply(bigDF, 1, function(x) all(is.na(x)))
  bigDF          <- bigDF[!ind, ]

  vec            <- length(unique(bigDF$group))
  vecRep         <- rep(LETTERS[1:vec], each= length(bigDF$group) / vec)
  bigDF$newGroup <- paste(vecRep, bigDF$group, sep=".")

  ProbesBoxplot  <- ggplot2::ggplot(data= bigDF, ggplot2::aes(x = newGroup, y = value))  +
                    ggplot2::geom_boxplot((ggplot2::aes(fill=type))) +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

  return(list(ProbesBoxplot, bigDF))

}
