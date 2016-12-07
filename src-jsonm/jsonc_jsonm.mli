(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Jsont}'s {!Jsonm} backend.

    This backend uses {!Jsonm} as a codec. *)

(** {1 JSON Values} *)

type loc = (int * int) * (int * int)
(** The type for value location ranges. See {!Jsont.loc}. *)

type 'a def = loc * 'a
(** The type for values tagged with a location. *)

type nat_string = string
(** The type for native strings. *)

type soup = Jsonm.lexeme def list
(** The type for arbitrary undescribed JSON values. *)

(** {1 JSON codec} *)

type error = Jsonm.error
(** The type for decode errors. *)

type decoder = Jsonm.decoder
(** The type for decoders. *)

type encoder = Jsonm.encoder
(** The type for encoders. *)

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
