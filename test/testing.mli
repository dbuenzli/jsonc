(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.codec -> string -> 'a

val ok_decode : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.codec -> string -> 'a

val err_decode : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.codec -> Jsont.error -> string -> 'a

val err_trip : ?loc:bool -> ?dups:[ `Error | `Skip ] ->
  ?unknown:[ `Error | `Skip ] -> 'a Jsont.codec -> Jsont.error -> string -> 'a

(** {1 Asserting} *)

val is_equal : (Format.formatter -> 'a -> unit) -> 'a -> 'a -> unit

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
