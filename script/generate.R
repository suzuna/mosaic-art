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
path_target_img <- here("target_img/littlecandy.png")
# 素材画像（リサイズ後）のフォルダのパス。
dir_material_resized_img <- here("material_img_theater_resized")

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
max_count <- 1


# ターゲット画像をタイル数×タイルのpxまで引き伸ばす ----------------------------------------------
target_img <- imager::load.image(path_target_img)
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
path_material_resized_img <- list.files(dir_material_resized_img,pattern="(jpg|jpeg|png)$",full.names=T,recursive=T)
df_material_resized <- data.frame(id=1:length(path_material_resized_img),path=path_material_resized_img)

plan(multisession)
tictoc::tic()
mat_material <- path_material_resized_img %>% 
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
target_img_splited <- imager::imsplit(target_img,axis="x",nb=tile_colnum) %>% 
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

mat_material <- mat_material %>% 
  map(function(x) {
    imager::resize(x,size_x=tile_colpx*scaling_prop,size_y=tile_rowpx*scaling_prop)
  })


# RGB空間からLab空間へ変換する（Lab空間を使いたい場合のみ） ---------------------------------------
if (is_to_Lab) {
  target_img_splited <- target_img_splited %>% 
    map(function(x) {
      x %>% 
        map(function(y) {
          imager::RGBtoLab(y)
        })
    })
  mat_material <- mat_material %>% 
    map(function(x) {
      imager::RGBtoLab(x)
    })
}


# 類似度を計算する ----------------------------------------------------------------
options(future.globals.maxSize=1.8*1024^3)
plan(multisession)
tictoc::tic()
similar_order <- target_img_splited %>% 
  future_map(function(x) {
    x %>% 
      map(function(y) {
        mat_material %>% 
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
if (is.null(max_count)) {
  # 理論上最大にする
  max_count <- tile_colnum*tile_rownum+1
}
if (max_count*length(mat_material)<tile_colnum*tile_rownum) {
  stop("素材画像の枚数×同一の素材画像を重複して使える回数＜タイル数です。
       max_countを大きい値にするか、NULLとしてください。")
}

df_used_img <- expand.grid(x=1:tile_colnum,y=1:tile_rownum) %>% 
  arrange(x)
counter <- numeric(length(names(mat_material))) %>% 
  set_names(names(mat_material))

id_used_img <- c()
for (x in 1:tile_colnum) {
  cat(str_glue("x = {x}"),"\n")
  for (y in 1:tile_rownum) {
    id <- find_first(similar_order[[x]][[y]],names(counter))
    # counterをfilterするときは、まだmax_countに達していない画像から選ぶので、
    # max_countから1引いた数でfilterすることに注意
    counter <- add_counter(counter,id) %>% 
      filter_counter(max_count-1)
    id_used_img <- c(id_used_img,id)
  }
}

df_used_img <- df_used_img %>% 
  mutate(id=id_used_img) %>% 
  mutate(id=as.integer(id)) %>% 
  left_join(df_material_resized,by="id")


# 画像を生成する -----------------------------------------------------------------
result_img <- df_used_img %>% 
  mutate(x=as.integer(x),y=as.integer(y)) %>% 
  split(.$x) %>% 
  map(~{
    .x %>% 
      pull(path) %>% 
      map(~magick::image_read(.)) %>% 
      reduce(c) %>% 
      magick::image_append(stack=T)
  })
result_img <- result_img %>% 
  reduce(c) %>% 
  magick::image_append(stack=F)
result_img %>% 
  magick::image_write(str_glue("mosaic_art_{format(Sys.time(),'%Y%m%d%H%M%S')}.png"))
write_csv(df_used_img,str_glue("used_img_list_{format(Sys.time(),'%Y%m%d%H%M%S')}.csv"))
