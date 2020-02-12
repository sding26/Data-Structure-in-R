#' Search_Store
#'
#' This function returns stores information for all the stores
#' within the radius of specified postal code.
#'
#' This function provides stores information for all Best Buy stores
#' in the United States and Puerto Rico.
#'
#' @param PostalCode Search for stores by postal code
#' @param Distance Radius of seaching area
#' @keywords  PostalCode Distance BestBuy
#' @importFrom magrittr "%>%"
#' @export
Search_Store <- function(PostalCode = 10025, Distance = 10) {
  if (Distance != 0){
    endpoint <- paste("https://api.bestbuy.com/v1/stores((area(",PostalCode,",",Distance,")))",sep="")
    query_params <- list( "apiKey"=Sys.getenv("apiKey"),
                          "format" = "json",
                          "pageSize"=100)
    get_result <- httr::GET(endpoint,query = query_params)
    #check response status
    if (get_result$status_code != 200) {
      warning(stringr::str_c("For PostalCode", PostalCode, "the status is", get_result$status_code, sep = " "))
    }
    try(httr::stop_for_status(get_result))

    stores_list = as.list(httr::content(get_result)$stores)
    store_info <- data.frame(StoreName=character(),
                             Address=character(),
                             City=character(),
                             State=character(),
                             FullPostalCode=character(),
                             Phone=character(),
                             Distance=double(),
                             stringsAsFactors=FALSE)
    for (i in 1:length(stores_list)){
      query_result <- stores_list[[i]]
      store_collect <- data.frame(
        StoreName=query_result$name,
        Address=query_result$address,
        City=query_result$city,
        State=query_result$region,
        FullPostalCode=query_result$fullPostalCode,
        Phone=query_result$phone,
        Distance=query_result$distance)
      store_info = rbind(store_info, store_collect)
    }
    store_info <- store_info %>%
      dplyr::arrange(Distance)
    return(store_info)
  }
  else {
    endpoint <- paste("https://api.bestbuy.com/v1/stores((postalCode=",PostalCode,"))",sep="")
    query_params <- list( "apiKey"=Sys.getenv("apiKey"),
                          "format" = "json",
                          "show"="all",
                          "pageSize"=100)
    get_result <- httr::GET(endpoint,query = query_params)
    #check response status
    try(httr::message_for_status(get_result))
    stores_list = as.list(httr::content(get_result)$stores)
    store_info <- data.frame(StoreName=character(),
                             Address=character(),
                             City=character(),
                             State=character(),
                             FullPostalCode=character(),
                             Phone=character(),
                             Distance=double(),
                             stringsAsFactors=FALSE)
    for (i in 1:length(stores_list)){
      try(query_result <- stores_list[[i]])
      store_collect <- data.frame(StoreName=query_result$name,
                                  Address=query_result$address,
                                  City=query_result$city,
                                  State=query_result$region,
                                  FullPostalCode=query_result$fullPostalCode,
                                  Phone=query_result$phone,
                                  Distance=query_result$distance)
      store_info = rbind(store_info, store_collect)
    }
    store_info <- store_info %>%
      dplyr::arrange(Distance)
    return(store_info)
  }
}






