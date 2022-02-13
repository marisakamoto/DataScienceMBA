distancia_social <- function(x, dmin, dmax, style){
  vessel.list <- base::list()
  x <- base::data.frame(x = x)
  for(i in 1:base::nrow(x)){
    y <- x$x[i]
    x %>% 
      dplyr::mutate(index = ifelse(x >= y & x <= y + dmax,
                                   yes = 1,
                                   no = 0)) %>% 
      dplyr::select(index) -> vector
    vector <- base::as.vector(base::unlist(vector))
    vector[[i]] <- 0
    vessel.list[[i]] <- vector
    }
  data <- base::matrix(base::unlist(vessel.list), ncol = base::nrow(x), byrow = T)
  if(style == "W"){
    wdata <- data * 1/base::rowSums(data)
    wdata[base::is.nan(wdata)] <- 0
    return(wdata)} 
  else if(style == "C"){
    sum.vector <- base::rowSums(data) == 0
    sum.verifier <- base::as.numeric(sum.vector)
    no.links <- base::sum(sum.verifier)
    links <- base::NROW(data) - no.links
    weightsC <- links / base::sum(data)
    vectorC <- base::gsub(1, weightsC, data)
    cdata <- base::matrix(vectorC, ncol = base::NCOL(data), nrow = base::NROW(data))
    return(cdata)} 
  else if(style == "U"){
    sum.vector <- base::rowSums(data) == 0
    sum.verifier <- base::as.numeric(sum.vector)
    no.links <- base::sum(sum.verifier)
    links <- base::NROW(data) - no.links
    weightsC <- links / base::sum(data)
    weightsU <- weightsC / links
    vectorU <- base::gsub(1, weightsU, data)
    udata <- base::matrix(vectorU, ncol = base::NCOL(data), nrow = base::NROW(data))
    return(udata)}
  else if(style == "S"){
    step1 <- base::sqrt(base::rowSums(data^2))
    res.step1 <- data / step1
    res.step1[base::is.nan(res.step1)] <- 0
    step2 <- base::sum(res.step1)
    sum.vector <- base::rowSums(data) == 0
    sum.verifier <- base::as.numeric(sum.vector)
    no.links <- base::sum(sum.verifier)
    links <- base::NROW(data) - no.links
    step3 <- links / step2
    sdata <- res.step1 * step3
    return(sdata)
  }
  else if(style == "B"){return(data)}
  else {break()}
}
