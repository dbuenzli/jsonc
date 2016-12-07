(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Jsont}'s [js_of_ocaml] backend.

    This backend uses the browser's JavaScript
    [JSON.{parse,stringify}] functions as a codec.

    {b Limitations.} The [loc] and [mem_dups]
    argument of {!Jsont.decoder} are unsupported. *)

(** {1 JSON values} *)

type nat_string = Js.js_string Js.t
(** The type for native strings. *)

type soup
(** The type for arbitrary undescribed JSON values *)

(** {1 JSON decode} *)

type error = string
(** The type for decode errors. *)

type src = Js.js_string Js.t
(** The type for decoding sources. *)

type decoder
(** The type for decoder. *)

val decoder : src -> decoder
(** [decoder src] is a decoder that reads from [src]. *)

val decoder_src : decoder -> src
(** [decoder_src d] is [d]'s source. *)

(** {1 JSON encode} *)

type dst = Js.js_string Js.t
(** The type for encoding destinations. *)

type encoder
(** The type for encoders. *)

val encoder : unit -> encoder
(** [encoder ()] is a new encoder. *)

val encoder_dst : encoder -> dst
(** [encoder_dst e] is the result of the {!Jsont.encode}ing process. *)

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
