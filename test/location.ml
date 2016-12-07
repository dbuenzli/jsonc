(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Modeling the location datatype here: http://ocsigen.org/tuto/4.0/manual/rest
   as an OCaml abstract data type. *)

module Location : sig
  type t
  val codec : t Jsont.codec
  val create : Jsont.nat_string -> (float * float) -> t
  val description : t -> Jsont.nat_string
  val coordinates : t -> float * float
  val set_coordinates : t -> float * float -> t
end = struct
  type t = Jsont.obj

  (* We could introduce another ADT like we do for location for coords
     but we choose here to model it as a pair of float. *)
  let coordinates =
    let objd = Jsont.objc ~kind:"coords" () in
    let mem_lat = Jsont.mem objd "latitude" Jsont.float in
    let mem_lon = Jsont.mem objd "longitude" Jsont.float in
    let codec = Jsont.obj objd in
    let view =
      let decode o = `Ok (Jsont.get mem_lat o, Jsont.get mem_lon o) in
      let encode (lat, lon) =
        Jsont.(new_obj codec [memv mem_lat lat; memv mem_lon lon])
      in
      decode, encode
    in
    Jsont.view view codec

  let objd = Jsont.objc ~kind:"loc" ()
  let mem_description = Jsont.mem objd "description" Jsont.nat_string
  let mem_coordinates = Jsont.mem objd "coordinates" coordinates
  let codec = Jsont.obj objd

  let create description coords =
    Jsont.(new_obj codec [ memv mem_description description;
                           memv mem_coordinates coords; ])

  let description = Jsont.get mem_description
  let coordinates = Jsont.get mem_coordinates
  let set_coordinates = Jsont.set mem_coordinates
end

open Testing;;

let data0 = " {\"description\": \"Somewhere\",\"coordinates\": \
              {\"latitude\": 42.0, \"longitude\": 6.0}}"

let data1 = " {\"description\": \"Somewhere\",\"coordinates\": \
              {\"latitude\": null, \"longitude\": 6.0}}"

let assert_location l d x y =
  is_equal pp_esc_str
    (Jsont_codec.nat_string_to_string (Location.description l)) d;
  is_equal pp_float (fst (Location.coordinates l)) x;
  is_equal pp_float (snd (Location.coordinates l)) y;
  ()

let test_trip () =
  log_test "Trip location";
  assert_location (ok_trip Location.codec data0) "Somewhere" 42. 6.;
  assert_location (err_decode Location.codec
                     (`Type ("null", "float")) data1) "Somewhere" 0. 6.;
  ()

let test_obj_fun () =
  log_test "Location creation and member setting";
  let l0 = Location.create (Jsont_codec.nat_string_of_string "hey") (1.,2.) in
  assert_location l0 "hey" 1. 2.;
  let l1 = Location.set_coordinates l0 (3., 4.) in
  assert_location l0 "hey" 1. 2.;
  assert_location l1 "hey" 3. 4.;
  ()

let test () =
  log_suite "Location test";
  test_obj_fun ();
  test_trip ();
  ()






(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
