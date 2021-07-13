open! Base
open! Chamber

let write_thing ?(export = false) name scad =
  let filename ext = Printf.sprintf "things/%s.%s" name ext in
  let oc = Stdio.Out_channel.create (filename "scad") in
  Scad_ml.Core.write oc scad;
  if export
  then (
    try
      Printf.sprintf
        ( if Sys.unix
        then "openscad -q -o %s -D 'quality=\"production\"' %s"
        else "openscad.com -q -o %s -D 'quality=\"production\"' %s" )
        (filename "stl")
        (filename "scad")
      |> Caml.Sys.command
      |> function
      | 0 -> ()
      | _ -> failwith ""
    with
    | _ -> Stdio.print_endline "Openscad export shell command failed." )

let () =
  Stdio.print_endline "Building (and maybe exporting) scads...";
  write_thing ~export:true "chamber" Chamber.scad;
  Stdio.print_endline "Done!"
