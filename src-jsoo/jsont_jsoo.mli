(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
