#' @title Turn an image into data
#'
#' @description Extract an image file (\code{"png", "tiff", "jpeg", "bmp"}) and turn it into an enjoyable data set, pixels being rows (subjects) and columns (variables) being their coordinate positions (x and y axis) and their respective color (in hex codes).
#'
#' @param path Path to image file.
#' @param type Type of extraction of data. \code{type = "fill"} (default) returns the complete image as data whereas \code{type = "line"}
#' returns a specific range of color (default is black).
#' @param scaling Tranform the data to a specified scale. Three options are available : \code{"standardized", "original", "normalized")}. \code{scaling = "standardized"} converts data in a standardized form, \eqn{\mu = 0, \sigma = 1} (default); \code{scaling = "normalized"} converts data in a normalized form (to unit vectors); and \code{scaling = "original"} keeps the data untransformed.   
#' @param showplot Show a preliminary plot of the data (default is \code{TRUE}).
#' @param reduce \code{reduce} can be a number \code{reduce > 0} or \code{reduce = "unique"}.  By default \code{reduce = 1}, so all pixels are returned. Specified values between \code{0} to \code{1} will return the corrresponding proportion of the pixels. Values over \code{1} will return the number of pixels (e.g., \code{reduce = 3} returns 3 data). If the chosen number is over the number of pixels, then random duplicates are added. If \code{reduce = "unique"} only unique elements (given a certain \code{precision}) are returned.
#' @param A Transparency, otherwise known as \eqn{\alpha}. By default, only non transparent (\code{A = 1}) values are returned. Semi-transparent colors (\code{0 < A < 1}) are supported. Values between the \code{A} to \code{1} range will be return. If \code{A = 0}, all pixels are returned regardless of transparency.
#' @param R,G,B Color to return with \code{type = "line"} (the default range is \code{c(0, .05)} for each, i.e., black). A single "range" of color can be used.
#' @param Grey Grey range to be returned with \code{type = "line"}. \code{Grey} overwrites \code{R, G, B} and behaves similarly. Default is \code{NULL}
#' @param precision Set precision of \code{reduce = "unique"}. Default is \code{1}. It can be any integer \code{>0}. Values closer to zero are less precised (less data), higher values are more precise (more data).
#' @param seed Set seed value for random pixel returned with \code{reduce}.
#'
#' @return A data frame with pixels as rows and columns are x and y coordinates and g is their color in hex (factors).
#' @importFrom readbitmap read.bitmap
#' @importFrom grDevices rgb
#' @importFrom stats sd
#' @export
#'
#' @examples
#' path <- system.file(file.path("extdata", "success.png"), package = "image2data")
#' image2data(path = path, type = "line")
#' image2data(path = path, type = "line", Grey = c(0,.50))
#'
#' \dontrun{
#'image2data(path = file.choose())
#'}

image2data <- function(path,
                       type = "fill",
                       scaling = "standardized",
                       showplot = TRUE,
                       reduce = 1,
                       A = 1,
                       R = c(0,.05),
                       G = c(0,.05),
                       B = c(0,.05),
                       Grey = NULL,
                       precision = 1,
                       seed = NULL) {
  
  # Check if the file exists ####
  stopifnot("Incorrect path. The file was not found." = (file.exists(path)))
  
  # Check extension ####
  extension <- (strsplit(basename(path), split="\\.")[[1]])
  extension <- extension[length(extension)]
  stopifnot("Incorrect file. Extension of the image is not compatible. \n
  #          Choose a .jpg, .png, .tiff or .bmp file" =
              (any(extension == c("jpg", "png", "tiff", "bmp"))))
  
  # Import ####
  image <- readbitmap::read.bitmap(path)
  Dim <- dim(image)
  
  # Extraction of colors in 2 dimensions ####
  if (length(Dim) < 3){
    RR <- GG <- BB <- image
  } else {
    RR <- image[,,1]; GG <- image[,,2]; BB <- image[,,3]
  }
  
  if(is.null(Grey)){
    Rmin <- min(R); Rmax <- max(R)
    Gmin <- min(G); Gmax <- max(G)
    Bmin <- min(B); Bmax <- max(B)
  }else{
    Gmin <- Rmin <- Bmin <- min(Grey)
    Gmax <- Rmax <- Bmax <- max(Grey)
  }
  
  D <- data.frame(x = rep(1:Dim[2], each = Dim[1]),
                  y = rep(Dim[1]:1, times = Dim[2]),
                  g = rgb(RR, GG, BB))
  
  # alpha ####
  if(Dim[length(Dim)] == 4){
    D <- D[(image[,,4]) >= A, ]
  }
  
  # Select a colour ####
  if(type == "line"){
    crit <- ((RR >= Rmin) & (RR <= Rmax)) *
      ((GG >= Gmin) & (GG <= Gmax)) *
      ((BB >= Bmin) & (BB <= Bmax))
    
    D <-  D[which(crit == 1), ]
  }
  
  M <- colMeans(D[,c("x", "y")])
  s <- apply(D[,c("x", "y")], MARGIN = 2, sd)
  D[,c("x", "y")] <- as.data.frame(apply(D[,c("x", "y")],
                                         MARGIN = 2,
                                         FUN = function(x){(x-mean(x))/sd(x)})
  )
  
  # reduce ####
  if(reduce != 1){
    
    if(reduce == "unique"){
      
      ifelse(precision >= 1 ,
             D[,c("x", "y")]  <- round(D[,c("x", "y")], precision),
             D[,c("x", "y")]  <- round(D[,c("x", "y")] * precision, 1)
      )
      
      Unique <- unique(D[,c("x","y")])
      D <- data.frame(x = Unique[, "x"],
                      y = Unique[, "y"],
                      g = D[row.names(Unique), "g"])
      
    } else {
      if(!is.null(seed)){set.seed(seed)}
      n <- dim(D)[1]
      idx <- sample(n,
                    size = ifelse(reduce >= 1, reduce, n * reduce),
                    replace = ifelse(reduce > n, TRUE, FALSE))
      D <- D[idx, ]
    }
  }
  
  # scaling ####
  scaling <- pmatch(scaling, 
                    c("original", "normalized", "standardized"))
  
  if(is.na(scaling)){
    warning("Partial matching of `scaling` failed. \n 
            `scaling = standardized` was used. \n 
            Please verifiy it was intended.")
    scaling <- "standardized"
  }
  
  if(scaling == "original"){
    D[,c("x", "y")] <- (D[,c("x", "y")] + M) * s
    
  } else if(scaling == "normalized"){
    D[,c("x", "y")] <- as.data.frame(apply(D[,c("x", "y")],
                                           MARGIN = 2,
                                           FUN = function(x){(x/sum(x^2))})
    )
    
  } else if(scaling == "standardized"){
    D[,c("x", "y")] <- as.data.frame(apply(D[,c("x", "y")],
                                           MARGIN = 2,
                                           FUN = function(x){(x-mean(x))/sd(x)})
    )
    
  }
  
  rownames(D) <- NULL

  # showplot ####
  if(showplot){
    plot(x = D$x, y = D$y,
         xlab = "x",
         ylab = "y",
         col = D$g,
         pch = 16,
         cex = .5)
  }
  
  return(DATA <- D)
  
}


