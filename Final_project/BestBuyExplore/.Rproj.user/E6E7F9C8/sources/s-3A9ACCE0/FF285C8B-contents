#' Search_TV_Brand
#'
#' This function a dashboard of characteristics of one specific TV.
#'
#' This function allows the user to enter a brand name of TV
#' It will list all characteristics of that brand and
#' sort by the best-selling products over the last week.
#'
#' @param Brand Brand of the TV
#' @param Rank Best selling rank
#' @param Sort Sort by price
#' @keywords brand price BestBuy
#' @importFrom magrittr "%>%"
#' @export
Search_TV_Brand <- function(Brand = "Sony", Rank = TRUE , Sort = TRUE) {
  endpoint <- paste("https://api.bestbuy.com/v1/products((search=",Brand,")&(categoryPath.id=abcat0101000))",sep="")
  if (Rank == TRUE) {
    Rank <- "salesRankMediumTerm.asc"
    query_params <- list( "apiKey"=Sys.getenv("apiKey"),
                          "format" = "json",
                          "sort"=Rank,
                          "show"="all",
                          "pageSize"=100)
  }
  else {
    query_params <- list( "apiKey"=Sys.getenv("apiKey"),
                          "format" = "json",
                          "show"="all",
                          "pageSize"=100)
  }
  get_result <- httr::GET(endpoint,query = query_params)
  pro = as.list(httr::content(get_result)$product)
  product_info <- data.frame(Name=character(),
                             Color = character(),
                             Width=character(),
                             ScreenSize=double(),
                             DisplayType=character(),
                             Resolution=character(),
                             Description=character(),
                             Condition = character(),
                             lowPriceGuarantee=logical(),
                             SalePrice=double(),
                             stringsAsFactors=FALSE
  )
  for (i in 1:length(pro)){
  query_result <- pro[[i]]
  data_collect <- data.frame(Name =query_result$name,
                             Color=query_result$color,
                             Width=query_result$width,
                             ScreenSize=query_result$screenSizeIn,
                             DisplayType=query_result$displayType,
                             Resolution=query_result$verticalResolution,
                             Description=query_result$longDescription,
                             Condition = query_result$condition,
                             lowPriceGuarantee=query_result$lowPriceGuarantee,
                             SalePrice=query_result$salePrice
                             )
  product_info <- rbind(data_collect, product_info)
  }
  if (Sort == TRUE){
    product_info <- product_info %>%
      dplyr::arrange(product_info$SalePrice)
  product_info_final <- product_info
  }
  else {
    product_info_final <- product_info
  }

header <- shinydashboard::dashboardHeader(title = paste("Information of ",Brand," TV (Rank: ",Rank,")",sep=""))
sidebar <- shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(
  shinydashboard::menuItem('dashboard', tabName = 'dashboard')
  )
)
body <- shinydashboard::dashboardBody(
  shinydashboard::box(
    title = 'TV information', width = NULL, status = 'primary',
    DT::dataTableOutput('TV_table_final')
  )
)

ui<-shinydashboard::dashboardPage(header, sidebar, body)
server = function(input, output) {
  output$TV_table_final = DT::renderDataTable(
    product_info_final, options = list(lengthChange = FALSE)
  )
}
shiny::shinyApp(ui, server)

}
