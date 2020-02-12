#' Search_Trending_OpenBox
#'
#' This function provides infomation of trending products with Open Box service
#' of a wanted category
#'
#' This function allows the user to enter a specific category. It searches trending
#' products within the given category and returns specific number of trending products
#' with Open Box service.
#'
#' @param Category Enter category
#' @param Number_low Number of products to display
#' @keywords  Category OpenBox TrendingProducts BestBuy
#' @importFrom magrittr "%>%"
#' @export
Search_Trending_OpenBox <- function(Category = "camera", Number_low = 10) {
  # Enter category, seach for related category id
  # The api presents limited results per page.
  # It is necessary to renew cursorMark parameter to get full results.
  endpoint_category1 <- paste("https://api.bestbuy.com/v1/categories(name=",Category,"*)", sep = "")
  query_params1 <- list( "apiKey"=Sys.getenv("apiKey"),
                        "format" = "json",
                        "pageSize"=100,
                        "cursorMark"="*",
                        "show"="id")
  get_result1 <- httr::GET(endpoint_category1,query = query_params1)
  if (httr::content(get_result1)$total > 100) {
    endpoint_category2 <- paste("https://api.bestbuy.com/v1/categories(name=",Category,"*)", sep = "")
    query_params2 <- list( "apiKey"=Sys.getenv("apiKey"),
                           "format" = "json",
                           "pageSize"=100,
                           "cursorMark"=httr::content(get_result1)$nextCursorMark,
                           "show"="id")
    category_list1 = as.list(httr::content(get_result1))
    category1 = category_list1$categories
    get_result2 <- httr::GET(endpoint_category2,query = query_params2)
    category_list2 = as.list(httr::content(get_result2))
    category2 = category_list2$categories
    category_list = append(category1,category2)
  } else {
    category_list1 = as.list(httr::content(get_result1))
    category_list = category_list1$categories }

  # loop through category lists
  # find 10 trending sku numbers (id for product) of each category
  sku_list <- NULL
  for (category in category_list){
    endpoint_trending <- paste("https://api.bestbuy.com/beta/products/trendingViewed(categoryId=",category,")",sep="")
    query_params3 <- list( "apiKey"=Sys.getenv("apiKey"),
                           "format" = "json",
                           "pageSize"=100)
    get_result3 <- httr::GET(endpoint_trending,query = query_params3)
    temp <- as.list(httr::content(get_result3))
    if (length(temp$results) != 0){
      trending_list = as.list(httr::content(get_result3)$results)
      sku_list_un<- list()
      for (i in 1:length(trending_list)){
        if(!is.null(trending_list[[i]]$sku)){
        sku_collect <- trending_list[[i]]$sku
        sku_list_un <- c(sku_list_un, sku_collect)}
      sku_list <- append(sku_list_un,sku_list)}}
  }
  # Enter Number_low to limit the return number of products with lowest pricies.
  sku_list <- unique(sku_list)
  endpoint_openbox <- paste("https://api.bestbuy.com/beta/products/",sku_list,"/openBox",sep="")
  trending_product_info <- data.frame(SKU_num=character(),
                                      Name=character(),
                                      Price = double(),
                                      Description=character(),
                                      ReviewScore=double(),
                                      stringsAsFactors=FALSE)
  for (each in endpoint_openbox){
  query_params4 <- list( "apiKey"=Sys.getenv("apiKey"),
                         "format" = "json",
                         "pageSize"=100)
  get_result4 <- httr::GET(each,query = query_params4)
  temp2 <- as.list(httr::content(get_result4))
  if (length(temp2$results) != 0){
    product_list = as.list(httr::content(get_result4)$results)
    query_result <- product_list[[1]]
    trending_product_collect <- data.frame(SKU_num=query_result$sku,
                                 Name =query_result$names$title,
                                 Price=query_result$prices$current,
                                 Description=query_result$descriptions$short,
                                 ReviewScore=query_result$customerReviews$averageScore)
    trending_product_info = rbind(trending_product_collect,trending_product_info)
    }
  }
  trending_product_info <- trending_product_info%>%
      dplyr::arrange(trending_product_info$Price)
  trending_product_info <-trending_product_info[1:Number_low,]
  return(trending_product_info)
}



