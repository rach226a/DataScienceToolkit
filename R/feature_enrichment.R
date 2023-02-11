#' Annotates list of features using g:OSt
#'
#' @description Enriches list of features/genes with most significant terms according
#' to g:OSt by using \link[gprofiler2]{gost}.
#' @importFrom gprofiler2 gost
#' @param list_features list of list containing groups of related features/genes
#' @param organism Organism name, constructed by concatenating the fist letter of the
#' name with the family name
#' @param n.terms Number of terms to save
#' @param user_threshold p-value threshold for significance
#' @param recall percent of intersect that overlap with reference terms
#' @param precision percent of intersect that overlap with query terms
#' @param ... Additional arguments passed to \link[gprofiler2]{gost}
#'
#' @return A list with the significant term names and associated p-values
#' respectively, ordered by significance.
#'
#' @examples
#' #Markers for proliferation and EMT transition
#' genes <- list(c("MKI67", "TOP2A", "MYBL2", "BUB1"), c("CA9", "VEGFA", "ZEB1", "VIM", "FN1"))
#' enrichment_gene_list(genes, organism = "hsapiens")
#'
#' @export
enrichment_gene_list <- function(list_features, organism, n.terms = 3, user_threshold = 0.01, recall = 0.2, precision = 0.2, ...){

  cluster_annotations <- vector(mode = "list", length = length(list_features))
  for(i in 1:length(list_features)){
    result <- suppressMessages(gprofiler2::gost(list_features[[i]],
                                                organism = organism,
                                                user_threshold = user_threshold, ...)$result)

    if(is.null(result)){
      message(paste0("No results pass user_threshold for list ", i, "\nPlease ensure that the organism is correct"))
      cluster_annotations[[i]] <- NA
    } else{
      ## Large number of intersecting terms with gene signature (recall/precision)
      g_results <- result[result$recall>recall | result$precision>precision,]
      if(nrow(g_results) == 0){
        message(paste0("No results passed recall/precision for list ", i))
        cluster_annotations[[i]] <- NA
      } else{
        cluster_annotations[[i]] <- g_results[order(g_results$p_value)[1:min(n.terms, nrow(g_results))], c("term_name", "term_id", "term_size", "query_size", "intersection_size", "precision", "recall", "p_value", "effective_domain_size", "source", "source_order", "parents")]
      }
    }

  }

  return(cluster_annotations)
}
