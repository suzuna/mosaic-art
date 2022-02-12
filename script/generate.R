library(tidyverse)
library(imager)
library(magick)
library(furrr)
library(magrittr)
library(here)
library(tictoc)
library(fs)
library(jsonlite)


source(here("script/utils.R"),encoding="UTF-8")

# 定数 ----------------------------------------------------------------------
json_path <- here("settings_generate.json")
json_data <- jsonlite::fromJSON(json_path)

# ターゲット画像のパス
path_target_img <- here(json_data$path_target_img)
# 素材画像のフォルダのパス。読み込みを高速にするため、事前にtile_colpx x tile_rowpxのサイズにリサイズしておく方がよい
dir_material_img <- here(json_data$dir_material_img)
dir_output <- here(json_data$dir_output)

# モザイクアートの1タイルの縦と横のピクセル数
tile_colpx <- json_data$tile_colpx
tile_rowpx <- json_data$tile_rowpx
# モザイクアートの縦のタイル数
tile_rownum <- json_data$tile_rownum

# 縮小した素材画像とターゲット画像の各タイルの類似度を求めるとき、両画像の縦と横をさらにこのサイズに縮小する
scaling_prop <- json_data$scaling_prop

# ターゲット画像の各タイルと素材画像の比較をRGB空間ではなくLab空間で行いたい場合はTRUEにする
is_to_Lab <- json_data$is_to_Lab

# 同一の素材画像を使える回数の上限。1だと重複して使わない。重複に制限を設けない場合はNULLにする
max_count <- json_data$max_count

# 元の画像に合わせて色調変換する割合（0以上1以下）。色調変換しない場合は0にする
degree_of_colorchange <- json_data$degree_of_colorchange

# 類似度が最も高い画像を並べるタイルの順番を決めるseed。NULLの場合は左上から右下へ順に並べる
seed <- json_data$seed


# ターゲット画像をタイル数×タイルのpxまで引き伸ばす ----------------------------------------------
begin_time <- Sys.time()

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


# ターゲット画像をタイルに分割し、タイルのpx*scaling_propへ縮小する -------------------------------------
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


# 素材画像を読み込み、matrixの形式で持ち、グレースケール画像はカラー画像にする ---------------------------------------------
path_material_img <- list.files(dir_material_img,pattern="(jpg|jpeg|png)$",full.names=T,recursive=T)
df_material <- data.frame(id=1:length(path_material_img),path=path_material_img)

plan(multisession)
tictoc::tic()
mat_material <- path_material_img %>% 
  future_map(~{
    tryCatch({
      img <- imager::load.image(.x)
      # 素材画像をタイルのpx*scaling_propへ縮小した状態でデータを持つ
      img <- imager::resize(img,size_x=tile_colpx*scaling_prop,size_y=tile_rowpx*scaling_prop)
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
          head(tile_colnum*tile_rownum) %>% 
          names()
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
  arrange(x) %>% 
  mutate(idx=row_number(),id=NA_character_) %>% 
  relocate(idx,x,y,id)
counter <- numeric(length(names(mat_material))) %>% 
  set_names(names(mat_material))

if (is.null(seed)) {
  # seedがNULLの場合は、順番に並べていく
  idx <- df_used_img$idx
} else {
  set.seed(seed)
  idx <- sample(df_used_img$idx,size=length(df_used_img$idx),replace=FALSE) 
}

for (i in idx) {
  x <- df_used_img$x[i]
  y <- df_used_img$y[i]
  # cat(str_glue("idx: {i} x: {x} y: {y}"),"\n")
  
  selected_id <- find_first(similar_order[[x]][[y]],names(counter))
  # counterをfilterするときは、まだmax_countに達していない画像から選ぶので、
  # max_countから1引いた数でfilterすることに注意
  counter <- add_counter(counter,selected_id) %>% 
    filter_counter(max_count-1)
  df_used_img <- df_used_img %>% 
    mutate(id=if_else(idx==i,selected_id,id))
}

df_used_img <- df_used_img %>% 
  mutate(id=as.integer(id)) %>% 
  left_join(df_material,by="id")


# 画像を生成する -----------------------------------------------------------------
result_img <- df_used_img %>% 
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


# 書き出す --------------------------------------------------------------------
suffix_dttm <- format(Sys.time(),"%Y%m%d%H%M%S")
dir_output <- here(fs::path(dir_output,str_glue("created_at_{suffix_dttm}")))
if (!fs::dir_exists(dir_output)) {
  fs::dir_create(dir_output)
}

path_mosaic_art <- fs::path(dir_output,str_glue("mosaic_art_{suffix_dttm}.png"))
path_csv <- fs::path(dir_output,str_glue("used_img_list_{suffix_dttm}.csv"))

magick::image_write(result_img,path_mosaic_art)
write_csv(df_used_img,path_csv)

if (degree_of_colorchange>0) {
  img_before_colorchange <- imager::load.image(path_mosaic_art)
  img_after_colorchange <- img_before_colorchange*(1-degree_of_colorchange)+target_img*degree_of_colorchange
  
  path_mosaic_art_after_colorchange <- fs::path(dir_output,str_glue("mosaic_art_after_colorchange_{suffix_dttm}.png"))
  imager::save.image(img_after_colorchange,path_mosaic_art_after_colorchange)
}

finish_time <- Sys.time()

path_log <- fs::path(dir_output,str_glue("log_{suffix_dttm}.txt"))
log_params <- list(
  created_at=suffix_dttm,
  begin_time=as.character(begin_time),finish_time=as.character(finish_time),
  diff=format(finish_time-begin_time,units="auto"),
  path_mosaic_art=path_mosaic_art,
  path_mosaic_art_after_colorchange=ifelse(is.null(degree_of_colorchange),"not created",path_mosaic_art_after_colorchange),
  path_csv=path_csv,path_log=path_log,
  path_target_img=path_target_img,
  resolution_target_img=str_glue("{target_img_colpx} x {target_img_rowpx} -> {target_img_colpx_after} x {target_img_rowpx_after}"),
  dir_material_img=dir_material_img,
  num_material_img=str_glue("{length(path_material_img)} files"),
  tile_colpx=tile_colpx,tile_colnum=tile_colnum,
  tile_rowpx=tile_rowpx,tile_rownum=tile_rownum,
  output=str_glue("{tile_colpx*tile_colnum} x {tile_rowpx*tile_rownum} ({tile_colnum*tile_rownum} tiles)"),
  scaling_prop=scaling_prop,is_to_Lab=is_to_Lab,
  max_count=max_count,
  seed=ifelse(is.null(seed),"NULL",seed),
  degree_of_colorchange=ifelse(is.null(degree_of_colorchange),"NULL",degree_of_colorchange)
)

create_log(log_params,path_log)
