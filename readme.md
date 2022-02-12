# mosaic_art

モザイクアートを作るコード。

以下に対応している。

- 並列計算
- モザイクアートで表現したい画像の各タイルと素材画像の色の類似度の計算を、RGB色空間とLab色空間のどちらかから選択可能
- 同一の素材画像を使用する回数の上限の設定
- 作成したモザイクアートの色調を元の画像の色調へ近付けるようにする色調変換

## 使い方

- step1: 以下を用意する
  - モザイクアートで表現したい画像1枚（以下、ターゲット画像と呼ぶ）
  - モザイクアートで使いたい画像たくさん（以下、素材画像と呼ぶ）
    - 作成したいモザイクアートのタイル数によるが数千枚以上あるとよい
- step2: settings_resize.jsonに設定を記載した上でresize.Rを実行する
  - 素材画像を、モザイクアートの1タイルのピクセル数まで縮小するコード。generate.Rを実行する前にこれを実行して素材画像を縮小しておく（そうしないとgenerate.Rで素材画像を読み込む所で時間がかかる）
  - settings_resize.jsonは、コード内で使用するパスやパラメータなどの設定値。内容は下記を参照
  - 素材画像を縮小すればよいので、このコードを実行しなくてもリサイズができるフリーソフトを使用すれば、このステップを飛ばせる
- step3: settings_generate.jsonに設定を記載した上でgenerate.Rを実行する
  - 縮小した素材画像をもとにモザイクアートを作成するコード
  - settings_generate.jsonは、コード内で使用するパスやパラメータなどの設定値。内容は下記を参照

## settings_resize.json

- dir_material_img: 素材画像が入ったフォルダのパス
- path_material_resized_img: 縮小した素材画像を格納したいフォルダのパス
- tile_colpx: 縮小後の素材画像の横方向のpx（モザイクアートのタイル1個の横方向のpxになる）
- tile_rowpx: 縮小後の素材画像の縦方向のpx（モザイクアートのタイル1個の縦方向のpxになる）

## settings_generate.json

- path_target_img: ターゲット画像のファイルパス
- dir_material_img: 素材画像のパス（通常縮小後の画像を指定するため、settings_resize.jsonのpath_material_resized_imgと同じになる）
- dir_output: 出力先のフォルダのパス
- tile_colpx: モザイクアートのタイル1個の横方向のpx（通常settings_resize.jsonのtile_colpxと同じになる）
- tile_rowpx: モザイクアートのタイル1個の縦方向のpx（通常settings_resize.jsonのtile_rowpxと同じになる）
- tile_rownum: モザイクアートのタイルの縦方向の枚数
- scaling_prop: 縮小した素材画像とターゲット画像の各タイルの類似度を求めるとき、素材画像とターゲット画像の縦と横をこのサイズを掛けた分まで縮小する。通常0.25。条件：0<scaling_prop<=1
- is_to_Lab: ターゲット画像の各タイルと素材画像の比較をRGB空間ではなくLab空間で行いたい場合はtrueにする。条件：bool
- max_count: 同一の素材画像を使える回数の上限。1にすると重複して使わない。重複に制限を設けない場合はNULLにする。条件：自然数 or null
- degree_of_colorchange: 完成したモザイクアートを、ターゲット画像の色調に合わせる度合いのパラメータ。条件：0<=degree_of_colorchange<=1 or null
- seed: モザイクアートを作る際、どのタイルから類似度を計算するかに使用する乱数のseed。nullの場合は左上から右下に縦方向を優先して順番に並べる。条件：整数 or null
