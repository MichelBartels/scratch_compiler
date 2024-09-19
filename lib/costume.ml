type t =
  { asset_id: string [@key "assetId"]
  ; name: string
  ; bitmap_resolution: int [@key "bitmapResolution"] [@default 1]
  ; rotation_center_x: int [@key "rotationCenterX"]
  ; rotation_center_y: int [@key "rotationCenterY"] }
[@@deriving show, yojson {strict= false}]
