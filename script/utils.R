# 画像のR,G,Bのそれぞれの平均を求める
#' @param img an object of class "cimg"
#' @return a numeric vector
calc_mean <- function(img) {
  r_mean <- mean(as.numeric(img[,,1]))
  g_mean <- mean(as.numeric(img[,,2]))
  b_mean <- mean(as.numeric(img[,,3]))
  c(r_mean,g_mean,b_mean)
}

# 画像を指定したサイズにリサイズする（imager版）
# （画像の縦長or横長が）指定したサイズの縦長or横長と異なる場合、右に90度回転してからリサイズする）
# 回転はimager::imrotate、リサイズはimager::resizeを用いる
#' @param x an object of class "cimg"
#' @param x_px numeric. px of col
#' @param y_px numeric. px of row
#' @return img
resize_img <- function(img,px_x,px_y) {
  px_x_before <- dim(img)[1]
  px_y_before <- dim(img)[2]
  # もとは縦長の画像を横長に変える場合
  if (px_x>px_y & px_x_before<px_y_before) {
    img <- imager::imrotate(img,angle=90)
  } else if (px_x<px_y & px_x_before>px_y_before) {
    # もとは横長の画像を縦長に変える場合
    img <- imager::imrotate(img,angle=90)
  }
  imager::resize(img,size_x=px_x,size_y=px_y,size_z=1,size_c=3)
}

# 画像を指定したサイズにリサイズする（magick版）
# （画像の縦長or横長が）指定したサイズの縦長or横長と異なる場合、右に90度回転してからリサイズする）
# 回転はmagick::image_rotate、リサイズはmagick::image_scaleを用いる
# imager版のresize_imgより高速
#' @param img a magick object
#' @param x_px a numeric. px of col
#' @param y_px a numeric. px of row
#' @return a magick object
resize_img2 <- function(img,px_x,px_y) {
  info_df <- img %>% 
    magick::image_info()
  px_x_before <- info_df$width[1]
  px_y_before <- info_df$height[1]
  # もとは縦長の画像を横長に変える場合
  if (px_x>px_y & px_x_before<px_y_before) {
    img <- magick::image_rotate(img,degrees=90)
  } else if (px_x<px_y & px_x_before>px_y_before) {
    # もとは横長の画像を縦長に変える場合
    img <- magick::image_rotate(img,degrees=90)
  }
  # imager::resize(img,size_x=px_x,size_y=px_y,size_z=1,size_c=3)
  magick::image_scale(img,str_glue("{px_x}x{px_y}!"))
}

# xとyの画像について、ピクセル単位でユークリッド距離の2乗を計算する
# xとyは同じサイズの画像でなければならない
#' @param x an object of class "cimg" (must be channel = 3)
#' @param y an object of class "cimg" (must be channel = 3)
#' @return a numeric
calc_similarity <- function(x,y) {
  sum((x[,,1,1]-y[,,1,1])^2+(x[,,1,2]-y[,,1,2])^2+(x[,,1,3]-y[,,1,3])^2)
}

# xの要素のうち、yにも含まれる要素の中で先頭に来るものを選ぶ
# yに含まれる要素が何もない時はNAを返す
#' @param x a vector
#' @param y a vector
#' @return a value (character, numeric, ...), or NA
find_first <- function(x,y) {
  x[x %in% y][1]
}

# counter（出現回数のベクトルであり、名前はその出現回数を有する要素名）について、
# 指定した要素名の出現回数に1を足す
#' @param counter a numeric or integer vector which has name
#' @param name a character
#' @return a numeric or integer vector which has name
add_counter <- function(counter,name) {
  counter[names(counter)==name] <- counter[names(counter)==name]+1
  return(counter)
}

# counterのうち、出現回数がvalue_threshold以下の要素のみ返す
#' @param counter a numeric or integer vector which has name
#' @param value_threshold a numeric or integer
#' @return a numeric or integer vector which has name
filter_counter <- function(counter,value_threshold) {
  counter[counter<=value_threshold]
}