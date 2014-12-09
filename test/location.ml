(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Modeling the location datatype here: http://ocsigen.org/tuto/4.0/manual/rest
   as an OCaml abstract data type. *)

module Location : sig
  type t
  val descr : t Jsont.descr
  val create : Jsont.nat_string -> (float * float) -> t
  val description : t -> Jsont.nat_string
  val coordinates : t -> float * float
  val set_coordinates : t -> float * float -> t
end = struct
  type t = Jsont.obj

  (* We could introduce another ADT like we do for location for coords
     but we choose here to model it as a pair of float. *)
  let coordinates =
    let objd = Jsont.objd ~kind:"coords" () in
    let mem_lat = Jsont.mem objd "latitude" Jsont.float in
    let mem_lon = Jsont.mem objd "longitude" Jsont.float in
    let descr = Jsont.obj objd in
    let codec =
      let decode o = `Ok (Jsont.get mem_lat o, Jsont.get mem_lon o) in
      let encode (lat, lon) =
        Jsont.(new_obj descr [memv mem_lat lat; memv mem_lon lon])
      in
      decode, encode
    in
    Jsont.codec codec descr

  let objd = Jsont.objd ~kind:"loc" ()
  let mem_description = Jsont.mem objd "description" Jsont.nat_string
  let mem_coordinates = Jsont.mem objd "coordinates" coordinates
  let descr = Jsont.obj objd

  let create description coords =
    Jsont.(new_obj descr [ memv mem_description description;
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
  assert_location (ok_trip Location.descr data0) "Somewhere" 42. 6.;
  assert_location (err_decode Location.descr
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
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
