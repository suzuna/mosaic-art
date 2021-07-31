library(tidyverse)
library(imager)
library(magick)
library(furrr)
library(magrittr)
library(here)
library(tictoc)


source(here("script/utils.R"),encoding="UTF-8")

# 定数 ----------------------------------------------------------------------
# ターゲット画像のパス
target_img_path <- here("target_img/littlecandy.png")
# 素材画像（リサイズ後）のパス。このフォルダ内にリサイズした素材画像が作られる
tile_resized_img_path <- here("material_img_theater_resized")

# モザイクアートの1タイルの縦と横のピクセル数
tile_rowpx <- 37
tile_colpx <- 50
# モザイクアートの縦のタイル数
tile_rownum <- 100

# 縮小した素材画像とターゲット画像の各タイルの類似度を求めるとき、両画像の縦と横をさらにこのサイズに縮小する
scaling_prop <- 1/4

# ターゲット画像の各タイルと素材画像の比較をRGB空間ではなくLab空間で行いたい場合はTRUEにする
is_to_Lab <- FALSE

# 同一の素材画像を使える回数の上限。1だと重複して使わない。重複に制限を設けない場合はNULLにする
max_used_count <- 1


# ターゲット画像をタイル数×タイルのpxまで引き伸ばす ----------------------------------------------
target_img <- imager::load.image(target_img_path)
target_img_colpx <- dim(target_img)[1]
target_img_rowpx <- dim(target_img)[2]
target_img_colrowprop <- target_img_colpx/target_img_rowpx

# tile_colpx <- round(tile_rowpx*target_img_colrowprop)
# モザイクアートの横のタイル数
tile_colnum <- floor(tile_rowpx*tile_rownum*target_img_colrowprop/tile_colpx)

# ターゲット画像のpx数がタイル画像のpx数の倍数になるように右端と下端を削る
target_img <- imager::resize(target_img,tile_colpx*tile_colnum,tile_rowpx*tile_rownum,1,3)

target_img_colpx_after <- dim(target_img)[1]
target_img_rowpx_after <- dim(target_img)[2]


# 縮小した素材画像を読み込み、matrixの形式で持ち、グレースケール画像はカラー画像にする ---------------------------------------------
tile_resized_path <- list.files(tile_resized_img_path,full.names=T)
df_tile_resized <- data.frame(id=1:length(tile_resized_path),path=tile_resized_path)

plan(multisession)
tictoc::tic()
mat <- tile_resized_path %>% 
  future_map(~{
    tryCatch({
      img <- imager::load.image(.x)
      if (dim(img)[4]==1L) {
        img <- imager::add.color(img,TRUE)
      }
      return(img)
    },error=function(e){
      print(.x)
      print(e)
      return(NULL)
    })
  },.progress=T) %>% 
  set_names(1:length(.)) %>% 
  discard(is.null) %>% 
  # チャンネル数は1か3か4であり、1の場合は上で3に直しているので4の場合が該当する
  # nullは上で除外しておかないと、dim(.x)[4]はNULLになりNULL!=3の比較でエラーが出る
  discard(~{
    dim(.x)[4]!=3
  })
tictoc::toc()
plan(sequential)


# ターゲット画像をタイルに分割し、ターゲット画像と素材画像をscaling_propへ縮小する -------------------------------------
# 各要素はx（＝横軸）方向の分割で、その各要素の中にy（＝縦軸）方向の分割が入っている
img_split <- imager::imsplit(target_img,axis="x",nb=tile_colnum) %>% 
  map(~{
    .x %>% 
      imager::imsplit(axis="y",nb=tile_rownum)
  }) %>% 
  map(function(x) {
    x %>% 
      map(function(y) {
        imager::resize(y,size_x=tile_colpx*scaling_prop,size_y=tile_rowpx*scaling_prop)
      })
  })

mat <- mat %>% 
  map(function(x) {
    imager::resize(x,size_x=tile_colpx*scaling_prop,size_y=tile_rowpx*scaling_prop)
  })


# RGB空間からLab空間へ変換する（Lab空間を使いたい場合のみ） ---------------------------------------
if (is_to_Lab) {
  img_split <- img_split %>% 
    map(function(x) {
      x %>% 
        map(function(y) {
          imager::RGBtoLab(y)
        })
    })
  mat <- mat %>% 
    map(function(x) {
      imager::RGBtoLab(x)
    })
}


# 類似度を計算する ----------------------------------------------------------------
options(future.globals.maxSize=1.8*1024^3)
plan(multisession)
tictoc::tic()
a <- img_split %>% 
  future_map(function(x) {
    x %>% 
      map(function(y) {
        mat %>% 
          map(function(z) {
            calc_similarity(y,z)
          }) %>% 
          unlist() %>% 
          sort(decreasing=F) %>% 
          names() %>% 
          head(tile_colnum*tile_rownum)
      })
  },.progress=T)
tictoc::toc()
plan(sequential)


# 最も類似度が高い画像を取り出す ------------------------------------------------------------------
# 同じ画像の使用回数に制限がある場合は、その回数を満たす下で最も類似度が高い画像を取り出す
if (is.null(max_used_count)) {
  # 理論上最大にする
  max_used_count <- tile_colnum*tile_rownum
}
if (max_used_count*length(mat)<tile_colnum*tile_rownum) {
  stop("素材画像の枚数×同一の素材画像を重複して使える回数＜タイル数です。
       max_used_countを大きい値にするか、NULLとしてください。")
}

df_grid <- expand.grid(x=1:tile_colnum,y=1:tile_rownum) %>% 
  arrange(x)
counter <- numeric(length(names(mat))) %>% 
  set_names(names(mat))

resa <- c()
for (x in 1:tile_colnum) {
  cat(str_glue("x = {x}"),"\n")
  for (y in 1:tile_rownum) {
    id <- find_first(a[[x]][[y]],names(counter))
    counter <- add_counter(counter,id) %>% 
      filter_counter(max_used_count)
    resa <- c(resa,id)
  }
}

tmp <- df_grid %>% 
  mutate(id=resa) %>% 
  mutate(id=as.integer(id)) %>% 
  left_join(df_tile_resized,by="id")


# 画像を生成する -----------------------------------------------------------------
tmp %>% 
  mutate(x=as.integer(x),y=as.integer(y)) %>% 
  split(.$x) %>% 
  map(~{
    .x %>% 
      pull(path) %>% 
      map(~magick::image_read(.)) %>% 
      reduce(c) %>% 
      magick::image_append(stack=T)
  }) -> p
p %>% 
  reduce(c) %>% 
  magick::image_append(stack=F) -> res
res %>% 
  magick::image_write(str_glue("res_{format(Sys.time(),'%Y%m%d%H%M%S')}.png"))
write_csv(tmp,str_glue("used_img_list_{format(Sys.time(),'%Y%m%d%H%M%S')}.csv"))

