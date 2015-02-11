(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Testing arrays. *)

open Testing;;

(* FIXME test error conditions. *)

let test_bool () =
  log_test "Jsont.bool";
  let trip = ok_trip Jsont.bool in
  let err = err_trip Jsont.bool in
  is_equal pp_bool (trip "true") true;
  is_equal pp_bool (trip "false") false;
  is_equal pp_bool (err (`Type ("float", "bool")) "1.0") false;
  ()

let test_float () =
  log_test "Jsont.float";
  let trip = ok_trip Jsont.float in
  let err = err_trip Jsont.float in
  is_equal pp_float (trip "0.0") 0.;
  is_equal pp_float (trip "1.0") 1.;
  is_equal pp_float (err (`Type ("bool", "float")) "true") 0.;
  ()

let test_int () =
  log_test "Jsont.int";
  let trip = ok_trip Jsont.int in
  let err = err_trip Jsont.int in
  is_equal pp_int (trip "0.9") 0;
  is_equal pp_int (trip "1.1") 1;
  is_equal pp_int (err (`Type ("bool", "int")) "true") 0;
  ()

let test_int_strict () =
  log_test "Jsont.int_strict";
  let trip = ok_trip Jsont.int_strict in
  let err = err_trip Jsont.int_strict in
  is_equal pp_int (trip "0") 0;
  is_equal pp_int (trip "1") 1;
  is_equal pp_int (err (`Type ("float", "int")) "0.9") 0;
  is_equal pp_int (err (`Type ("float", "int")) "1.9") 0;
  ()

let test_string () =
  log_test "Jsont.string";
  let trip = ok_trip Jsont.string in
  let err = err_trip Jsont.string in
  is_equal pp_esc_str (trip "\"hey\"") "hey";
  is_equal pp_esc_str (trip "\"\"") "";
  is_equal pp_esc_str (err (`Type ("float", "string")) "1.0") "";
  ()

let test_nat_string () =
  log_test "Jsont.nat_string";
  let trip = ok_trip Jsont.nat_string in
  let pp_nat_str ppf s = pp ppf "%S" (Jsont_codec.nat_string_to_string s) in
  is_equal pp_nat_str (trip "\"ho\"") (Jsont_codec.nat_string_of_string "ho");
  is_equal pp_nat_str (trip "\"\"") (Jsont_codec.nat_string_of_string "");
  ()

let test_nullable () =
  log_test "Jsont.nullable";
  let codec = Jsont.(nullable int) in
  let trip = ok_trip codec in
  let err = err_trip codec in
  let pp_oint = pp_ocaml_opt pp_int in
  is_equal pp_oint (trip "null") None;
  is_equal pp_oint (trip "1") (Some 1);
  is_equal pp_oint (err (`Type ("bool", "int")) "true") (Some 0);
  ()

let test_view () =
  log_test "Jsont.view";
  let pp_d ppf = function `Bla -> pp ppf "`Bla" | `Bli -> pp ppf "`Bli" in
  let err_unexp u = Printf.sprintf "unknown case %s" u in
  let decode = function
  | "bla" -> `Ok `Bla | "bli" -> `Ok `Bli
  | u -> `Error (err_unexp u)
  in
  let encode = function `Bla -> "bla" | `Bli -> "bli" in
  let view = decode, encode in
  let codec = Jsont.view ~default:`Bla view Jsont.string in
  let trip = ok_trip codec in
  let err = err_trip codec in
  is_equal pp_d (trip "\"bla\"") `Bla;
  is_equal pp_d (trip "\"bli\"") `Bli;
  is_equal pp_d (err (`Value_decoder (err_unexp "hu")) "\"hu\"") `Bla;
  ()

let assert_array pp_v l v = is_equal (pp_ocaml_list pp_v) l v

let test_type_match () =
  log_test "Jsont.type_match";
  let null_dec = function None -> `Ok `Null | _ -> assert false in
  let null_enc = function `Null -> None | _ -> assert false in
  let null_tag =
    Jsont.view (null_dec, null_enc) Jsont.(with_default None (nullable bool))
  in
  let bool_dec = function b -> `Ok (`Bool b) in
  let bool_enc = function `Bool b -> b | _ -> assert false in
  let bool_tag = Jsont.view (bool_dec, bool_enc) Jsont.bool in
  let float_dec = function f -> `Ok (`Float f) in
  let float_enc = function `Float f -> f | _ -> assert false in
  let float_tag = Jsont.view (float_dec, float_enc) Jsont.float in
  let str_dec = function s -> `Ok (`String s) in
  let str_enc = function `String s -> s | _ -> assert false in
  let str_tag = Jsont.view (str_dec, str_enc) Jsont.string in
  let arr_dec = function a -> `Ok (`Array a) in
  let arr_enc = function `Array a -> a | _ -> assert false in
  let arr_tag = Jsont.view (arr_dec, arr_enc) Jsont.(array soup) in
  let obj_dec = function o -> `Ok (`Object o) in
  let obj_enc = function `Object o -> o | _ -> assert false in
  let objc = Jsont.objc ~kind:"object" () in
  let _ = Jsont.anon objc Jsont.soup in
  let obj_tag = Jsont.view (obj_dec, obj_enc) (Jsont.obj objc) in
  let any_decode = function
  | `Null -> `Ok null_tag
  | `Bool -> `Ok bool_tag
  | `Float -> `Ok float_tag
  | `String -> `Ok str_tag
  | `Array -> `Ok arr_tag
  | `Object -> `Ok obj_tag
  in
  let any_encode = function
  | `Null -> null_tag
  | `Bool _ -> bool_tag
  | `Float _ -> float_tag
  | `String _ -> str_tag
  | `Array _ -> arr_tag
  | `Object _ -> obj_tag
  in
  let pp_tag ppf = function
  | `Null -> pp ppf "`Null"
  | `Bool b -> pp ppf "`Bool %b" b
  | `Float f -> pp ppf "`Float %f" f
  | `String s -> pp ppf "`String %S" s
  | `Array _ -> pp ppf "`Array ..."
  | `Object _ -> pp ppf "`Object ..."
  in
  let codec = Jsont.type_match ~default:`Null any_decode any_encode in
  let trip = ok_trip codec in
  is_equal pp_tag (trip "null") (`Null);
  is_equal pp_tag (trip "true") (`Bool true);
  is_equal pp_tag (trip "2.0") (`Float 2.0);
  is_equal pp_tag (trip "\"\"") (`String "");
  is_equal pp_tag (trip "\"aaa\"") (`String "aaa");
  is_equal pp_tag (trip "[]") (`Array []);
  ()

let test_array () =
  log_test "Jsont.array";
  let codec = Jsont.(array bool) in
  let trip = ok_trip codec in
  let err = err_trip codec in
  assert_array pp_bool (trip "[true,false,true]") [true; false; true];
  assert_array pp_bool (trip "[]") [];
  assert_array pp_bool (err (`Type ("bool", "array")) "true") [];
  assert_array pp_bool (err (`Type ("object", "array")) "{}") [];
  assert_array pp_bool (err (`Type ("float", "bool")) "[true,1.0]")
    [true; false];
  let codec = Jsont.(array (nullable bool)) in
  let trip = ok_trip codec in
  assert_array (pp_ocaml_opt pp_bool) (trip "[true,null]") [Some true; None];
  assert_array (pp_ocaml_opt pp_bool) (trip "[]") [];
  ()

let test_object_empty () =
  log_test "Object empty";
  let codec = Jsont.(obj (objc ~kind:"test" ())) in
  let trip = ok_trip codec in
  let err = err_trip codec in
  ignore (trip "{}");
  ignore (trip "{ \"a\": 1.0 }");
  ignore (err (`Type ("array", "object")) "[]");
  let trip = ok_trip ~unknown:`Error codec in
  let err = err_trip ~unknown:`Error codec in
  ignore (trip "{}");
  ignore (err (`Member ("test", "a", `Unknown)) "{ \"a\": 1.0 }");
  ()

let test_object_anons () =
  log_test "Object anonymous";
  let objc = Jsont.objc () in
  let a = Jsont.anon objc ~default:["a", true; "b", false] Jsont.bool in
  let codec = Jsont.obj objc in
  let trip = ok_trip codec in
  let err = err_trip codec in
  ignore (trip "{}");
  is_equal pp_bool (Jsont.get_anon a "a" (trip "{ \"a\" : true }")) true;
  is_equal pp_bool (Jsont.get_anon a "a" (err (`Type ("float", "bool"))
                                                 "{ \"a\" : 1.0 }")) false;
  is_equal pp_bool (Jsont.get_anon a "a" (err (`Type ("array", "object")) "[]"))
    true;
  is_equal pp_bool (Jsont.get_anon a "b" (err (`Type ("array", "object")) "[]"))
    false;
  ()

let test_object_mems () =
  log_test "Object members";
  let () =
    let objc = Jsont.objc ~kind:"test" () in
    let mem_a = Jsont.mem objc "a" Jsont.float in
    let codec = Jsont.obj objc in
    let trip, err = ok_trip codec, err_trip codec in
    is_equal pp_float (Jsont.get mem_a (trip "{ \"a\" : 2.0 }")) 2.0;
    is_equal pp_float (Jsont.get mem_a (err (`Type ("bool", "float"))
                                          "{ \"a\" : true }")) 0.0;
    is_equal pp_float (Jsont.get mem_a
                         (err (`Member ("test", "a", `Miss)) "{}")) 0.0;
    is_equal pp_float (Jsont.get mem_a
                         (trip "{ \"a\" : 2.0, \"a\" : 3.0 }")) 3.0;
    is_equal pp_float (Jsont.get mem_a
                         ((err_trip ~dups:`Error codec)
                            (`Member ("test", "a", `Dup))
                            "{ \"a\" : 2.0, \"a\" : 3.0 }")) 3.0;
  in
  let () =
    let objc = Jsont.objc ~kind:"test" () in
    let mem_a = Jsont.mem objc "a" ~opt:`Yes_rem Jsont.(some float) in
    let codec = Jsont.obj objc in
    let trip, err = ok_trip codec, err_trip codec in
    let pp_ofloat = pp_ocaml_opt pp_float in
    is_equal pp_ofloat (Jsont.get mem_a (trip "{ \"a\" : 2.0 }")) (Some 2.0);
    is_equal pp_ofloat (Jsont.get mem_a (err (`Type ("bool", "float"))
                                           "{ \"a\" : true }")) (Some 0.0);
    is_equal pp_ofloat (Jsont.get mem_a (trip "{}")) None
  in
  let () =
    let objc = Jsont.objc ~kind:"test" () in
    let mem_a = Jsont.mem objc "a" ~opt:`Yes Jsont.(with_default 3.0 float) in
    let codec = Jsont.obj objc in
    let trip, err = ok_trip codec, err_trip codec in
    is_equal pp_float (Jsont.get mem_a (trip "{ \"a\" : 2.0 }")) 2.0;
    is_equal pp_float (Jsont.get mem_a (err (`Type ("bool", "float"))
                                              "{ \"a\" : true }")) 3.0;
    is_equal pp_float (Jsont.get mem_a (trip "{}")) 3.0
  in
  let () =
    let objc = Jsont.objc ~kind:"test" () in
    let mem_a = Jsont.mem_opt objc "a" Jsont.float in
    let codec = Jsont.obj objc in
    let trip, err = ok_trip codec, err_trip codec in
    let pp_ofloat = pp_ocaml_opt pp_float in
    is_equal pp_ofloat (Jsont.get mem_a (trip "{ \"a\" : 2.0 }")) (Some 2.0);
    is_equal pp_ofloat (Jsont.get mem_a (err (`Type ("bool", "float"))
                                           "{ \"a\" : true }")) (Some 0.0);
    is_equal pp_ofloat (Jsont.get mem_a (trip "{}")) None
  in
  ()

type tag = [ `Bool of bool | `Int of int ]
let test_object_mem_match () =
  log_test "Object, matching members";
  let err_tag t = Printf.sprintf "unknown tag: %s" t in
  let tag =
    let dec = function
    | "bool" -> `Ok `Bool | "float" -> `Ok `Float
    | t -> `Error (err_tag t)
    in
    let enc = function `Bool -> "bool" | `Float -> "float" in
    Jsont.(view (dec, enc) (with_default "bool" string))
  in
  let bool_tag =
    let dec = function b -> `Ok (`Bool b) in
    let enc = function `Bool b -> b | _ -> assert false in
    Jsont.view (dec, enc) Jsont.bool
  in
  let float_tag =
    let dec = function f -> `Ok (`Float f) in
    let enc = function `Float f -> f | _ -> assert false in
    Jsont.view (dec, enc) Jsont.float
  in
  let select = function
  | `Bool -> bool_tag
  | `Float -> float_tag
  in
  let objc = Jsont.objc ~kind:"test" () in
  let mem_tag = Jsont.mem objc "tag" tag in
  let mem_data = Jsont.mem_match objc mem_tag "data" select in
  let codec = Jsont.obj objc in
  let trip, err = ok_trip codec, err_trip codec in
  let pp_data ppf = function
  | `Bool b -> pp_bool ppf b
  | `Float f -> pp_float ppf f
  in
  is_equal pp_data (Jsont.get mem_data
                     (trip "{ \"tag\" : \"bool\", \"data\" : true }"))
    (`Bool true);
  is_equal pp_data (Jsont.get mem_data
                     (trip "{ \"data\" : true, \"tag\" : \"bool\" }"))
    (`Bool true);
  is_equal pp_data (Jsont.get mem_data
                     (trip "{ \"tag\" : \"float\", \"data\" : 2.0 }"))
    (`Float 2.0);
  is_equal pp_data (Jsont.get mem_data
                      (trip "{ \"data\" : 2.0, \"tag\" : \"float\" }"))
    (`Float 2.0);
  is_equal pp_data (Jsont.get mem_data
                      (err (`Value_decoder (err_tag "bla"))
                         "{ \"data\" : true, \"tag\" : \"bla\" }"))
    (`Bool true);
  let objc = Jsont.objc ~kind:"test2" () in
  let tag =
    let dec = function
    | "bool" -> `Ok `Bool | "test" -> `Ok `Test
    | t -> `Error (err_tag t)
    in
    let enc = function `Bool -> "bool" | `Test -> "test" in
    Jsont.(view (dec, enc) (with_default "bool" string))
  in
  let bool_tag =
    let dec = function b -> `Ok (`Bool b) in
    let enc = function `Bool b -> b | _ -> assert false in
    Jsont.view (dec, enc) Jsont.bool
  in
  let test_tag =
    let dec = function t -> `Ok (`Test (Jsont.get mem_data t)) in
    let enc = function
    | `Test (`Bool b) ->
        Jsont.(new_obj codec [memv mem_tag `Bool; memv mem_data (`Bool b)])
    | `Test (`Float f) ->
        Jsont.(new_obj codec [memv mem_tag `Float; memv mem_data (`Float f)])
    | _ -> assert false
    in
    Jsont.view (dec, enc) codec
  in
  let select = function
  | `Bool -> bool_tag
  | `Test -> test_tag
  in
  let mem_tag = Jsont.mem objc "tag" tag in
  let mem_data = Jsont.mem_match objc mem_tag "data" select in
  let codec = Jsont.obj objc in
  let trip, err = ok_trip codec, err_trip codec in
  let pp_data ppf = function
  | `Bool b -> pp_bool ppf b
  | `Test t -> pp_data ppf t
  in
  is_equal pp_data (Jsont.get mem_data
                     (trip "{ \"tag\" : \"test\", \"data\" : \
                             { \"tag\" : \"float\", \"data\" : 2.0 } }"))
    (`Test (`Float 2.0));
  is_equal pp_data (Jsont.get mem_data
                     (trip "{ \"data\" :
                              { \"data\" : 2.0, \"tag\" : \"float\" }, \
                            \"tag\" : \"test\" }"))
    (`Test (`Float 2.0));
  ()

(* TODO
   get_def
   set
   anon_names
   find_anon
   find_anon_def
   get_anon
   get_anon_def
   set_anon
   rem_anon
*)

let test () =
  log_suite "Base tests";
  test_bool ();
  test_float ();
  test_int ();
  test_int_strict ();
  test_string ();
  test_nat_string ();
  test_nullable ();
  test_view ();
  test_type_match ();
  test_array ();
  test_object_empty ();
  test_object_anons ();
  test_object_mems ();
  test_object_mem_match ();
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
