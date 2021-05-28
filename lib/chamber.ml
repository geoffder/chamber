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
  let h = Slab.h +. Top.h
  let x_corner_radius = 2.
  let y_corner_radius = 4.
  let y_corner_x_scale = 1.5
  let divot_radius = 6.
  let divot_depth = 2.
  let x = 14.
  let y = Slab.w /. 2.

  let scad =
    let x_corner = Model.cylinder ~center:true ~fn:32 x_corner_radius (h +. 0.001)
    and y_corner =
      Model.cylinder ~center:true ~fn:32 y_corner_radius (h +. 0.001)
      |> Model.scale (y_corner_x_scale, 1., 1.)
    and divot = Model.sphere ~fn:32 divot_radius
    and divot_z = (h /. 2.) +. divot_radius -. divot_depth in
    Model.union
      [ Model.hull
          [ Model.translate (x_corner_radius -. (l /. 2.), 0., 0.) x_corner
          ; Model.translate ((l /. 2.) -. x_corner_radius, 0., 0.) x_corner
          ; Model.translate (0., (w /. 2.) -. y_corner_radius, 0.) y_corner
          ; Model.translate (0., y_corner_radius -. (w /. 2.), 0.) y_corner
          ]
      ; Model.translate (l /. -4., w /. 4., divot_z) divot
      ; Model.translate (l /. 4., w /. 4., divot_z) divot
      ; Model.translate (l /. 4., w /. -4., divot_z) divot
      ; Model.translate (l /. -4., w /. -4., divot_z) divot
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

module Outflow = struct
  let column_l = 10.
  let column_w = 5.
  let column_corner_radius = 2.
  let channel_radius = 1.
  let channel_l = 3.
  let channel_w = 2.
  let channel_x = Well.x +. (Well.l /. 2.)
  let column_x = channel_x +. channel_l -. (channel_radius *. 2.) +. (column_l /. 2.)
  let column_y = Well.y -. channel_w -. (column_w /. 2.)

  let channel =
    let slice = Model.sphere ~fn:16 channel_radius |> Model.rotate (0., Math.pi /. 2., 0.)
    and ps =
      let bez =
        Bezier.quad
          ~p1:(0., 0., 0.)
          ~p2:(channel_l, 0., 0.)
          ~p3:(channel_l, -.channel_w, 0.)
      in
      Bezier.curve bez 0.1
    in
    List.fold
      ~init:(slice, [])
      ~f:(fun (last, acc) p ->
        let next = Model.translate p slice in
        next, Model.hull [ last; next ] :: acc )
      ps
    |> fun (_, hs) -> Model.union hs |> Model.translate (channel_x, Well.y, 0.)

  let column =
    let corner = Model.cylinder ~center:true ~fn:32 column_corner_radius (Well.h +. 0.001)
    and x = column_corner_radius -. (column_l /. 2.)
    and y = column_corner_radius -. (column_w /. 2.) in
    Model.hull
      [ Model.translate (-.x, y, 0.) corner
      ; Model.translate (x, y, 0.) corner
      ; Model.translate (-.x, -.y, 0.) corner
      ; Model.translate (x, -.y, 0.) corner
      ]
    |> Model.translate (column_x, column_y, Well.h /. 2.)

  let scad = Model.union [ channel; column ]
end

let scad =
  Model.difference
    (Model.union [ Slab.scad; Top.scad ])
    [ Well.scad; Inflow.scad; Outflow.scad ]
