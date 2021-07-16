open! Base
open! Scad_ml

module Slab = struct
  let l = 40.
  let w = 23.
  let h = 1.5
  let scad = Model.cube (l, w, h)
end

module Seal = struct
  let w = 1.
  let h = 0.45
  let inset = 1.

  let scad =
    let rect =
      Model.square ~center:true (Slab.l -. (inset *. 2.), Slab.w -. (inset *. 2.))
    in
    Model.difference rect [ Model.offset (`Delta (-.w)) rect ]
    |> Model.linear_extrude ~height:h
    |> Model.translate (Slab.l /. 2., Slab.w /. 2., 0.)
end

module Top = struct
  let w = 18.
  let h = 2.3
  let scad = Model.cube (Slab.l, w, h) |> Model.translate (0., (Slab.w -. w) /. 2., Slab.h)
end

type electrode_clearance =
  | Divot of
      { major_radius : float
      ; major_depth : float
      ; minor_radius : float
      ; minor_depth : float
      }
  | Slope of
      { start : float
      ; height : float
      ; scale : float
      }

let divot_clearance =
  Divot { major_radius = 8.; major_depth = 2.; minor_radius = 2.5; minor_depth = 1.5 }

let slope_clearance = Slope { start = 2.15; height = 3.; scale = 1.6 }

module Well = struct
  let l = 22.
  let w = 14.
  let x_inset = 3.5
  let h = Slab.h +. Top.h
  let x_corner_radius = 2.
  let y_corner_radius = 4.
  let y_corner_x_scale = 1.5
  let x = x_inset +. (l /. 2.)
  let y = Slab.w /. 2.
  let clearance = slope_clearance

  let scad =
    let x_corner = Model.circle ~fn:32 x_corner_radius
    and y_corner =
      Model.circle ~fn:32 y_corner_radius |> Model.scale (y_corner_x_scale, 1., 1.)
    in
    let shape =
      Model.hull
        [ Model.translate (x_corner_radius -. (l /. 2.), 0., 0.) x_corner
        ; Model.translate ((l /. 2.) -. x_corner_radius, 0., 0.) x_corner
        ; Model.translate (0., (w /. 2.) -. y_corner_radius, 0.) y_corner
        ; Model.translate (0., y_corner_radius -. (w /. 2.), 0.) y_corner
        ]
    in
    match clearance with
    | Divot { major_radius; major_depth; minor_radius; minor_depth } ->
      let major_divot = Model.sphere ~fn:32 major_radius
      and major_divot_z = (h /. 2.) +. major_radius -. major_depth
      and minor_divot = Model.sphere ~fn:16 minor_radius
      and minor_divot_z = (h /. 2.) +. minor_radius -. minor_depth in
      Model.union
        [ Model.linear_extrude ~height:(h +. 0.0001) ~center:true shape
        ; Model.translate (l /. -4., w /. 4., major_divot_z) major_divot
        ; Model.translate (l /. 4., w /. 4., major_divot_z) major_divot
        ; Model.translate (l /. 4., w /. -4., major_divot_z) major_divot
        ; Model.translate (l /. -4., w /. -4., major_divot_z) major_divot
        ; Model.translate (l /. 2., 0., minor_divot_z) minor_divot
        ; Model.translate (l /. -2., 0., minor_divot_z) minor_divot
        ]
      |> Model.translate (x, y, h /. 2.)
    | Slope { start; height; scale } ->
      Model.union
        [ Model.linear_extrude ~height:start shape
        ; Model.linear_extrude ~height ~scale shape
          |> Model.translate (0., 0., start -. 0.0001)
        ]
      |> Model.translate (x, y, 0.)
end

module Inflow = struct
  (* TODO: might actually be counter productive to have the opening wider in
   * the case of using plastic pipette tip inflow. Try a wider all the way shaft,
   * (or long wide opening followed by narrower finish) adjusting the clearance
   * cutout accordingly. *)
  let opening_radius = 0.9
  let shaft_radius = 0.7
  let opening_l = 3.
  let shaft_l = Well.x_inset
  let angle = Float.pi /. 8.
  let z = Slab.h +. shaft_radius +. 0.2

  let scad =
    let shaft =
      Model.cylinder ~center:true ~fn:16 shaft_radius (shaft_l +. 2.)
      |> Model.rotate (0., Float.pi /. 2., 0.)
      |> Model.translate (shaft_l /. 2., 0., 0.)
    and opening =
      Model.cylinder ~center:true ~fn:16 opening_radius (opening_l +. 1.)
      |> Model.rotate (0., Float.pi /. 2., 0.)
      |> Model.translate ((opening_l /. 2.) -. 1., 0., 0.)
    in
    Model.union [ shaft; opening ]
    |> Model.rotate (0., angle, 0.)
    |> Model.translate (0., Well.y, z)
end

module Outflow = struct
  module Channel = struct
    let radius = 1.4
    let l = 3.
    let w = 2.
    let x = Well.x +. (Well.l /. 2.)

    let scad =
      let slice = Model.sphere ~fn:16 radius |> Model.rotate (0., Float.pi /. 2., 0.)
      and ps =
        let bez = Bezier.quad ~p1:(0., 0., 0.) ~p2:(l, 0., 0.) ~p3:(l, w, 0.) in
        Bezier.curve bez 0.1
      in
      List.fold
        ~init:(slice, [])
        ~f:(fun (last, acc) p ->
          let next = Model.translate p slice in
          next, Model.hull [ last; next ] :: acc )
        ps
      |> fun (_, hs) -> Model.union hs |> Model.translate (x, Well.y, 0.)
  end

  module Tank = struct
    let l = 9.
    let w = 5.
    let corner_radius = 2.
    let x = Channel.x +. Channel.l -. Channel.radius +. (l /. 2.)
    let y = Well.y +. Channel.w +. (w /. 2.)

    let scad =
      let corner = Model.cylinder ~center:true ~fn:32 corner_radius (Well.h +. 0.001)
      and cx = corner_radius -. (l /. 2.)
      and cy = corner_radius -. (w /. 2.) in
      Model.hull
        [ Model.translate (-.cx, cy, 0.) corner
        ; Model.translate (cx, cy, 0.) corner
        ; Model.translate (-.cx, -.cy, 0.) corner
        ; Model.translate (cx, -.cy, 0.) corner
        ]
      |> Model.translate (x, y, Well.h /. 2.)
  end

  module Column = struct
    let divider_w = 0.7
    let divider_h = 0.5
    let radius = 2.25
    let x = Tank.x +. (Tank.l /. 2.) -. (Channel.radius *. 2.)
    let w = (Tank.w /. 2.) +. divider_w +. radius

    let scad =
      let block =
        Model.cube ~center:true (radius *. 2., w, Well.h +. 0.001)
        |> Model.translate (x, Tank.y -. (w /. 2.), (Well.h /. 2.) +. divider_h)
      in
      Model.union
        [ Model.cylinder ~center:true ~fn:32 radius (Well.h *. 2.)
          |> Model.translate (x, Tank.y -. w, Well.h)
        ; block
        ]
  end

  module Slide = struct
    let l = 1.4 (* 1.3 too tight, 1.5 pretty good, maybe a bit loose. *)

    let angle = Float.pi /. 10.
    let z = 2.

    let scad =
      Model.cube (l, Slab.w /. 2., Well.h *. 2.)
      |> Model.translate (l /. -2., Slab.w /. -2., 0.)
      |> Model.rotate (-.angle, 0., 0.)
      |> Model.translate (Column.x, Tank.y -. Column.w -. Column.radius +. 0.001, z)
  end

  let scad = Model.union [ Channel.scad; Tank.scad; Column.scad; Slide.scad ]
end

module HolderBlock = struct
  let h = 3.5
  let outflow_encroach_w = 0.75
  let inner_wall_l = 2.
  let cut_angle = Float.pi /. 4.

  let w =
    Outflow.Tank.y -. Outflow.Column.w -. Outflow.Column.radius +. outflow_encroach_w

  let l = inner_wall_l +. (Outflow.Slide.l /. 2.) +. (Slab.l -. Outflow.Column.x)

  let cut =
    Model.cube (l, w, h) |> Model.rotate_about_pt (-.cut_angle, 0., 0.) (0., -.w, 0.)

  let scad =
    Model.difference (Model.cube (l, w, h)) [ cut ]
    |> Model.translate (Slab.l -. l, 0., Slab.h +. Top.h)
end

let scad =
  Model.difference
    (Model.union [ Slab.scad; Top.scad; HolderBlock.scad ])
    [ Well.scad; Inflow.scad; Outflow.scad; Seal.scad ]
