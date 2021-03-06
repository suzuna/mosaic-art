library(tidyverse)
library(imager)
library(magick)
library(furrr)
library(magrittr)
library(here)
library(fs)
library(tictoc)
library(jsonlite)


source(here("script/utils.R"),encoding="UTF-8")

# 定数 ----------------------------------------------------------------------
json_path <- here("settings_resize.json")
json_data <- jsonlite::fromJSON(json_path)

# 素材画像（リサイズ前）のパス
dir_material_img <- here(json_data$dir_material_img)
# 素材画像（リサイズ後）のパス。このフォルダ内にリサイズした素材画像が作られる
dir_material_img_resized <- here(json_data$dir_material_img_resized)

# TODO: tile_rowpx -> resized_rowpx, tile_colpx -> resized_colpxに変更して他に影響出ないか確認する
# モザイクアートの1タイルの縦と横のピクセル数
tile_colpx <- json_data$tile_colpx
tile_rowpx <- json_data$tile_rowpx


# 素材画像をタイルのpx数まで縮小する ------------------------------------------------------
path_material_img_rel <- list.files(dir_material_img,pattern="(jpg|jpeg|png)$",full.names=F,recursive=T)
path_material_img_abs <- list.files(dir_material_img,pattern="(jpg|jpeg|png)$",full.names=T,recursive=T)

# リサイズ前と後のパスの対照表
df_resize_path <- data.frame(old_path_rel=path_material_img_rel,old_path_abs=fs::path_abs(path_material_img_abs)) %>% 
  mutate(
    new_path_dir_root=dir_material_img_resized,
    new_path_abs=fs::path(new_path_dir_root,old_path_rel)
  ) %>% 
  # "_resized"を拡張子の前に挟む
  mutate(new_path_abs=str_c(fs::path_ext_remove(new_path_abs),"_resized.",fs::path_ext(new_path_abs)))

fs::path_dir(df_resize_path$new_path_abs) %>% 
  unique() %>% 
  walk(~{
    if (!fs::dir_exists(.x)) {
      # フォルダ"C:/Users/user"はあるが、フォルダ"C:/Users/user/a"もフォルダ"C:/Users/user/a/b"もない場合、
      # fs::dir_create("C:/Users/user/a/b")とすればフォルダaを作りその下にフォルダbを作れる
      # （base::dir.createは、2階層以上存在しないフォルダがある場合それはできない）
      fs::dir_create(.x)
    }
  })

plan(multisession)
tictoc::tic()
future_walk2(df_resize_path$old_path_abs,df_resize_path$new_path_abs,~{
  tryCatch({
    .x %>%
      magick::image_read() %>% 
      resize_img2(tile_colpx,tile_rowpx) %>% 
      magick::image_write(.y)
  },error=function(e){
    print(.x)
    print(e)
  })
},.progress=T)
tictoc::toc()
plan(sequential)
