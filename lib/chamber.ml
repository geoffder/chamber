open! Base
open! Scad_ml

module Slab = struct
  let l = 40.
  let w = 23.
  let h = 1.5
  let scad = Model.cube (l, w, h)
end

module Top = struct
  let w = 18.
  let h = 2.3
  let scad = Model.cube (Slab.l, w, h) |> Model.translate (0., (Slab.w -. w) /. 2., Slab.h)
end

module Well = struct
  let l = 22.
  let w = 14.
  let h = Slab.h +. Top.h +. 0.001
  let x_corner_radius = 2.
  let y_corner_radius = 4.
  let y_corner_x_scale = 1.5
  let x = 14.
  let y = Slab.w /. 2.

  let scad =
    let x_corner = Model.cylinder ~center:true ~fn:12 x_corner_radius h in
    let y_corner =
      Model.cylinder ~center:true ~fn:24 y_corner_radius h
      |> Model.scale (y_corner_x_scale, 1., 1.)
    in
    Model.hull
      [ Model.translate (x_corner_radius -. (l /. 2.), 0., 0.) x_corner
      ; Model.translate ((l /. 2.) -. x_corner_radius, 0., 0.) x_corner
      ; Model.translate (0., (w /. 2.) -. y_corner_radius, 0.) y_corner
      ; Model.translate (0., y_corner_radius -. (w /. 2.), 0.) y_corner
      ]
    |> Model.translate (x, y, h /. 2.)
end

module Inflow = struct
  let radius = 0.5
  let angle = Math.pi /. 12.
  let l = (Well.x -. (Well.l /. 2.)) *. 2.
  let z = (Slab.h +. Top.h) /. 2.

  let scad =
    Model.cylinder ~center:true ~fn:12 radius l
    |> Model.rotate (0., (Math.pi /. 2.) +. angle, 0.)
    |> Model.translate (0.5, Well.y, z)
end

let scad =
  Model.difference (Model.union [ Slab.scad; Top.scad ]) [ Well.scad; Inflow.scad ]
