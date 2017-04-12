#' GetToIt
#'
#' A function about ordering boxplot
#' plot ggplot2 BoxPlot in the order you want.
#'
#'
#'
#'@param listOflist A list of lists, possibly named. The sub-lists must have 2 vectors of values to be boxplotted, each list must contain the same amount of values for now.
#'@param newOrder An index of the lists in the order you want them to be plot
#'@param noNames if no names given they are generated
#'@param df Yes, you can give a data-frame...
#'
#'@examples
#'magic <- list(list(c(rnorm(20,2,1)), c(rnorm(8,8,1))), list(c(rnorm(20,3,1)), c(rnorm(8,7,1))), list(c(rnorm(20,4,1)), c(rnorm(8,6,1))))
#'names(magic) <- c(1:length(magic))
#'order= c(1,3,2)
#'p <- ggBoxPlot(magic, order)
#'
#'@export
#'



ggBoxPlot <-function(listOflist, newOrder, noNames=FALSE, df = FALSE) {

  if(df){
    bigDF <- listOflist
  }
  else{
    if(!noNames){
      boxNames     <- names(listOflist)
    }
    else{
      names(listOflist) <- c(1:length(listOflist))
    }


    foo = lapply(seq_along(listOflist), function(i) {

      currentBox   <- listOflist[[i]]
      if(all(is.na(unlist(currentBox)))){
        return(NA)
      }
      else{

        dfelectro <- data.frame(value = unlist(currentBox[[1]]), type = "electro", group = boxNames[i], stringsAsFactors = FALSE)
        dfmetel   <- data.frame(value = unlist(currentBox[[2]]), type = "metal", group = boxNames[i], stringsAsFactors = FALSE)
        dfTot     <- rbind(dfelectro, dfmetel)

        return(dfTot)
      }
    })

    bigDF         <- do.call(rbind, foo)
  }

  ################################### Now we get the right order
  tmp             <- factor(bigDF$group, levels = newOrder, ordered = TRUE)
  bigDF           <- bigDF[order(tmp),]

  ind             <- apply(bigDF, 1, function(x) all(is.na(x)))
  bigDF           <- bigDF[!ind, ]

  ProbesBoxplot   <- ggplot2::ggplot(data= bigDF, ggplot2::aes(x = factor(group, levels = unique(group)), y = value))  +
    ggplot2::geom_boxplot((ggplot2::aes(fill=type))) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))

  return(list(ProbesBoxplot, bigDF))

}
