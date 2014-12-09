(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Testing for Jsont.

    A few tools to write tests for [Jsont].  *)

(** {1 Formatters} *)

type 'a formatter = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

val str : ('a, unit, string) format -> 'a
(** [str] is {!Format.sprintf} *)

val pp : Format.formatter -> ('a, Format.formatter, unit) Pervasives.format ->
  'a
(** [pp] is {!Format.fprintf} *)

val rpp : ('a, Format.formatter, unit) Pervasives.format ->
  Format.formatter -> 'a
(** [rpp] is [pp fmt ppf] *)

val nop : 'a formatter
(** [nop] does nothing. *)

val pp_cut : unit formatter
(** [pp_cut] is {!Format.pp_print_cut}. *)

val pp_sp : unit formatter
(** [pp_sp] is {!Format.pp_print_space}. *)

val pp_str : string formatter
(** [pp_str] is {!Format.pp_print_string}. *)

val pp_esc_str : string formatter
(** [pp_esc_str] is like {!pp_str} but the string is OCaml escaped. *)

val pp_int : int formatter
(** [pp_int] is {!Format.pp_print_int}. *)

val pp_bool : bool formatter
(** [pp_bool] is {!Format.pp_print_bool}. *)

val pp_float : float formatter
(** [pp_float] is {!Format.pp_print_float}. *)

val pp_opt : ?pp_none:unit formatter -> 'a formatter -> 'a option formatter
(** [pp_opt pp_none pp_v] formats value of type ['a option]. The default
    value of [pp_none] prints nothing. *)

val pp_list : ?pp_sep:unit formatter -> 'a formatter -> 'a list formatter
(** [pp_list pp_sep pp_v] formats lists of type ['a]. Each value
    is printed with [pp_v] followed by [pp_sep] (defaults to {!pp_cut}).
    Empty lists never print anything. *)

val pp_ocaml_list : 'a formatter -> 'a list formatter
(** [pp_ocaml_list pp_v] formats an OCaml lists of type ['a]. *)

val pp_ocaml_opt : 'a formatter -> 'a option formatter
(** [pp_ocaml_opt pp_v] formats an OCaml option of type ['a]. *)

val pp_text : string formatter
(** [pp_text] formats text by replacing spaces and newlines in the string
    with calls to {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

val pp_lines : string formatter
(** [pp_lines] formats lines by replacing newlines in the string
    with calls to {!Format.pp_force_newline}. *)

val pp_range : ((int * int) * (int * int)) formatter
(** [pp_range] formats a range. *)

(** {1 Logging} *)

val log : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
(** [log msg] logs [msg] on stdout. *)

val log_test : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
(** [log_test msg] logs [msg] on stdout. *)

val log_suite : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
(** [log_suite msg] logs [msg] on stdout. *)

val fail : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail msg] raises [Failure msg]. *)

val log_results : unit -> bool
(** [log_results ()] logs the result of the tests performed and returns
    [true] if there were no errors. *)

(** {1 Codec} *)

val ok_trip : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.descr -> string -> 'a

val ok_decode : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.descr -> string -> 'a

val err_decode : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.descr -> Jsont.error -> string -> 'a

val err_trip : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.descr -> Jsont.error -> string -> 'a

(** {1 Asserting} *)

val is_equal : (Format.formatter -> 'a -> unit) -> 'a -> 'a -> unit

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
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
