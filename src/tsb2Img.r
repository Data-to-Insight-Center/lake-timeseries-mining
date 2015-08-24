tsb2Img <- function(tsb_matrix, num_colors, out_filepath) {
  
  # save tsb_matrix as a color map image
  #   Input:
  #       tsb_matrix: TSB matrix 
  #
  #       num_colors: number of "contiguous" colors
  #
  #       out_filepath: the filepath of the output image (in ps format)
  #       
  #   Output:
  #       a side-effect image as specified by parameter 'out_filepath'
  
  postscript(out_filepath)
  
  require(matlab)
  
  # function 'jet.colors' is in 'matlab' package
  
  image(tsb_matrix, col = jet.colors(num_colors), axes = FALSE)
  
  dev.off()
  
}