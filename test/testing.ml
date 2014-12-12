(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Common test infrastructure *)

(* Formatters *)

type 'a formatter = Format.formatter -> 'a -> unit

let pp ppf fmt = Format.fprintf ppf fmt
let rpp fmt ppf = Format.fprintf ppf fmt
let nop fmt ppf = ()
let pp_cut = Format.pp_print_cut
let pp_sp = Format.pp_print_space
let pp_str = Format.pp_print_string
let pp_esc_str ppf = pp ppf "%S"
let pp_bool = Format.pp_print_bool
let pp_int = Format.pp_print_int
let pp_float = Format.pp_print_float
let pp_larrow ppf () = pp_str ppf "<=="
let pp_rarrow ppf () = pp_str ppf "==>"
let pp_opt ?(pp_none = fun ppf () -> ()) pp_v ppf = function
| None -> pp_none ppf ()
| Some v -> pp_v ppf v

let rec pp_list ?(pp_sep = pp_cut) pp_v ppf = function
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)
| [] -> ()

let pp_ocaml_list pp_v ppf l =
  if l = [] then pp ppf "[]" else
  let pp_sep ppf () = pp ppf ";@ " in
  pp ppf "[%a]" (pp_list ~pp_sep pp_v) l

let pp_ocaml_opt pp_v ppf v = match v with
| None -> pp ppf "None"
| Some v -> pp ppf "Some @[%a@]" pp_v v

let pp_white_str ~spaces ppf s =
  let left = ref 0 and right = ref 0 and len = String.length s in
  let flush () =
    Format.pp_print_string ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if spaces && s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ())
    else incr right;
  done;
  if !left <> len then flush ()

let pp_text = pp_white_str ~spaces:true
let pp_lines = pp_white_str ~spaces:false
let pp_range ppf ((l0, c0), (l1, c1)) = pp ppf "%d.%d-%d.%d" l0 c0 l1 c1

(* Logging *)

let assert_count = ref 0
let failure_count = ref 0

let exn_to_str = Printexc.to_string
let str = Format.sprintf
let pp = Format.fprintf
let log f = Format.printf (f ^^ "@?")
let log_test f = Format.printf ( "* " ^^ f ^^ "@.")
let log_suite f = Format.printf ( f ^^ "@.")
let log_results () =
  if !failure_count > 0 then begin
    if !failure_count = 1
    then log "There was 1 failure out of %d assertions" !assert_count
    else log "There were %d failures out of %d assertions"
        !failure_count !assert_count;
    false
  end else begin
    log "All tests suceeded (%d assertions).@." !assert_count;
    true
  end

let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let stack_to_loc stack =                                         (* Grrrrr. *)
  let stack = Printexc.raw_backtrace_to_string stack in
  try
    let start = String.index stack '\n' in
    let fstart = String.index_from stack start '\"' + 1 in
    let fend = String.rindex stack '\"' - 1 in
    let file = String.sub stack fstart (fend - fstart + 1) in
    let lstart = fend + 9 in
    let lend = String.rindex stack ',' - 1 in
    let line = String.sub stack lstart (lend - lstart + 1) in
    str "%s:%d: " file (int_of_string line)
  with
  | Not_found | Failure _ -> "??:"

let log_fail loc fmt =
  let loc = stack_to_loc loc in
  incr failure_count;
  Format.printf ("  %s" ^^ fmt ^^ "@.") loc

(* Codec *)

let unexpected_decode_error (file_loc, e) =
  incr assert_count;
  let loc = Printexc.get_callstack 3 in
  log_fail loc "unexpected decode error: %s" (Jsont.error_to_string e)

let trip ?loc ?dups ?unknown codec v =
  let nat_e, flush, get_data = Testing_backend.encoder () in
  let e = Jsont.encoder nat_e codec v in
  let rec encode () = match Jsont.encode e with
  | `Partial -> flush (); encode ()
  | `Ok -> ()
  in
  let data = (encode (); get_data nat_e) in
  let d, refill = Testing_backend.decoder data in
  let d = Jsont.decoder ?loc ?dups ?unknown d codec in
  let rec decode () = match Jsont.decode d with
  | `Await -> refill (); decode ()
  | `Error e -> decode ()
  | `Ok v -> (snd v)
  in
  decode ()

let ok_decode ?loc ?dups ?unknown codec data =
  let d, refill = Testing_backend.decoder data in
  let d = Jsont.decoder ?loc ?dups ?unknown d codec in
  let rec decode () = match Jsont.decode d with
  | `Await -> refill (); decode ()
  | `Error e -> unexpected_decode_error e; decode ()
  | `Ok v -> (snd v)
  in
  decode ()

let ok_trip ?loc ?dups ?unknown codec data =
  trip ?loc ?dups ?unknown codec (ok_decode ?loc ?dups ?unknown codec data)

let err_decode ?loc ?dups ?unknown codec err data =
  incr assert_count;
  let cs_loc = Printexc.get_callstack 3 in
  let d, refill = Testing_backend.decoder data in
  let d = Jsont.decoder ?loc ?dups ?unknown d codec in
  let err = ref (Some err) in
  let rec decode () = match Jsont.decode d with
  | `Await -> refill (); decode ()
  | `Ok v -> (snd v)
  | `Error (_, e as u) ->
      begin match !err with
      | None -> unexpected_decode_error u
      | Some e' ->
          err := None;
          if e <> e' then
            log_fail cs_loc "decode error mismatch: '%s' <> '%s'"
              (Jsont.error_to_string e) (Jsont.error_to_string e')
      end;
      decode ()
  in
  let v = decode () in
  match !err with
  | None -> v
  | Some e ->
      log_fail cs_loc "no decode error but expected: '%s'"
        (Jsont.error_to_string e);
      v

let err_trip ?loc ?dups ?unknown codec err data =
  trip ?loc ?dups ?unknown codec (err_decode ?loc ?dups ?unknown codec err data)

(* Asserting *)

let is_equal pp v v' =
  incr assert_count;
  let loc = Printexc.get_callstack 2 in
  if v <> v' then log_fail loc " %a differs from %a" pp v pp v'

(*---------------------------------------------------------------------------
   Copyright (c) 2012 Daniel C. Bünzli
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
