(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** JSON data structure codecs.

    [Jsont] describes JSON data structures with
    {{!base_codec}codec combinators}. The resulting typed descriptions
    enable JSON backend codecs to {{!enc}encode} and {{!dec}decode}
    these structures from/to OCaml values.

    Consult the {{!basics}basics} and {{!limits}limitations}.

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:value_codecs Value codecs}

    Value codec allow to refine JSON value codecs to more appopriate
    OCaml types and hook into [Jsont]'s error reporting mecanism. This
    allows, for example, to parse a JSON string value into an URI, a
    JSON object into a record, etc.  *)

type ('a, 'b) value_decoder = 'a -> [ `Ok of 'b | `Error of string ]
(** The type for value decoders.  *)

type ('b, 'a) value_encoder = 'b -> 'a
(** The type for value encoders. *)

type ('a, 'b) value_codec = ('a, 'b) value_decoder * ('b, 'a) value_encoder
(** The type for value codecs.  *)

(** {1:locations Value location} *)

type loc = (int * int) * (int * int)
(** The type for value location ranges. A pair of line and column numbers
    repsectively one and zero based. *)

type +'a def = loc * 'a
(** The type for values tagged with a location. *)

val invalid_loc : loc
(** [invalid_loc] is an invalid location. *)

val is_invalid_loc : loc -> bool
(** [is_invalid_loc l] is [true] iff [l] is {!invalid_loc}. *)

val invalid_def : 'a -> 'a def
(** [invalid_def v] is [(invalid_loc, v)] *)

val undef : 'a def -> 'a
(** [undef v] is [snd v]. *)

(** {1:json_values JSON values} *)

type nat_string = Jsont_codec.nat_string
(** The type for native strings. *)

val nat_string_of_string : string -> nat_string
(** [nat_string_of_string s] is a native string from the OCaml string [s]. *)

val nat_string_to_string : nat_string -> string
(** [nat_string_to_string s] is an OCaml string from the native string [s]. *)

type soup = Jsont_codec.soup
(** The type for arbitrary undescribed JSON values. *)

type obj
(** The type for JSON object values. Values of this type represent a
    concrete JSON object that conforms to a particular
    {{!type:objc}object codec}. See the functions in
    {!object_values} for using values of this type. *)

(** {1:base_codec JSON base value codecs} *)

type 'a codec
(** The type for JSON value codecs. A value of this type
    describes a JSON value structure and knows how to codec it to an
    OCaml value of type ['a].

    All value codecs have a {!default} value of type ['a]. This
    value is used either as a placeholder in case of error during
    robust parsing or as a default value during {{!new_obj}object creation}. *)

val default : 'a codec -> 'a
(** [default c] is [c]'s default value. *)

val with_default : 'a -> 'a codec -> 'a codec
(** [with_default v c] is [c] with default value [v]. *)

val bool : bool codec
(** [bool] is a JSON boolean. [true] is its default value. *)

val float : float codec
(** [float] is a JSON number. [0.] is its default value. *)

val int : int codec
(** [int] is a JSON number as an integer. [0] is its default value. Any
    existing fractional part is truncated, see also {!int_strict}. *)

val int_strict : int codec
(** [int_strict] is like {!int} but errors on a JSON number with a
    fractional part. *)

val string : string codec
(** [string] is a JSON string. [""] is its default value. *)

val nat_string : nat_string codec
(** [nat_string] is like {!string} but the OCaml representation is a
    backend native string value. The empty native string is its default
    value. *)

val nullable : 'a codec -> 'a option codec
(** [nullable c] is either the JSON value [c] or JSON null. [Some (default d)]
    is its default value. *)

val view : ?default:'b -> ('a, 'b) value_codec -> 'a codec -> 'b codec
(** [view view c] is the JSON value [c] whose OCaml representation is
    transformed by value codec [view].

    @raise Invalid_argument if [default] is absent and [c]'s default value
    cannot be parsed by [view]. *)

val type_match : default:'a ->
  ([ `Null | `Bool | `Float | `String | `Object | `Array ] ->
   [`Ok of 'a codec | `Error of string ]) ->
  ('a -> 'a codec) -> 'a codec
(** [type_match default dec enc] is a JSON value codec by:
    {ul
    {- On decoding: [dec typ] where [typ] is determined according
       to the type found in the data. If [`Error] is returned on a given
       data type the error will be returned by the decoder.
       If [`Ok d] is, [d] is used to decode the value. You
       must make sure that [d] does actually describe the given datatype
       (i.e. it would be wrong to return {!Jsont.int} on [`Bool]).}
    {- On encoding: [enc v] where [v] is the member value.}}
    [default] is its default value. *)

val soup : soup codec
(** [soup] is any JSON value. JSON's null is its default value. *)

val some : 'a codec -> 'a option codec
(** [some c] is the JSON value [c] but wrapped by [Some]. Its
    default value is [None].

    {b Warning.} [None] cannot be encoded with this combinator, it
    will raise [Invalid_argument], use {!nullable} for encoding an
    option. The result of [some c] is to be given to {!mem} with
    [~opt:`Yes_rem]. *)

(** {1:array_codec JSON array codecs} *)

val array : 'a codec -> 'a list codec
(** [array elt] is a JSON array whose elements are JSON values
    [elt]. [[]] is its default value. *)

val array_array : 'a codec -> 'a array codec
(** [array_array] is like {!array} but the OCaml representation is an
    array. [[||]] is its default value. *)

(** {1:obj_codec JSON object codec} *)

type objc
(** The type for JSON object codecs. *)

type 'a mem
(** The type for the JSON object member codecs. The type ['a] is
    the OCaml value used to represent the member JSON value. A value
    of this type always tied to a particular {{!objc}object codec}. *)

type 'a anon
(** The type for anonymous JSON object member codecs. The type
    ['a] is the OCaml value used to represent all the unknown
    member JSON values of an object. A value of this type always tied
    to a particular {{!objc}object codec}. *)

val objc : ?kind:string -> unit -> objc
(** [objc kind ()] is a new object codec. [kind] can be used to
    give a name the to kind of object described (for error
    messages). *)

val mem :
  ?eq:('a -> 'a -> bool) ->
  ?opt:[`Yes | `Yes_rem | `No] -> objc -> string -> 'a codec -> 'a mem
(** [mem opt o name c] declares in [o] an object member named [name]
    with JSON value [c]. If [opt] is
    {ul
    {- [`No] (default), it is a decoding error if the member is missing
       from the object.}
    {- [`Yes], the member can be absent on decoding in which case it will
       take [c]'s default value.}
    {- [`Yes_rem], the member can be absent on decoding in which case it will
       take [c]'s default value. If on encoding the member value is
       equal to [c]'s default value as determined by [eq]
       (defaults to [=]), the member will not be encoded in the output.}}

    @raise Invalid_argument if [name] is already described in [o]
    or if [o]'s codec was already {{!obj}used}. *)

val mem_opt : objc -> string -> 'a codec -> 'a option mem
(** [mem_opt o name c] is [mem objc name ~opt:`Yes_rem (some c)]. In
    other words: if the member is absent on decoding the member value
    with [None]; if the member value is [None] on encoding, the
    member is not encoded. *)

val mem_match :
  ?eq:('b -> 'b -> bool) ->
  ?opt:[ `No | `Yes | `Yes_rem] ->
  objc -> 'a mem -> string -> ('a -> 'b codec) -> 'b mem
(** [mem_match opt o m name select] is like {!mem}, except its value at encoding
    and decoding time is determined by [select v] where [v] is the value
    of the member [m] in the JSON object. Its default is the default of
    the codec resulting from applying [m]'s default to [select]. *)


val anon : ?default:(string * 'a) list -> objc -> 'a codec -> 'a anon
(** [anon o c] declares that all unknown object members of [o]
    have a JSON value [c].

    @raise Invalid_argument if [o] already has an anonymous member
    description or if [o]'s codec was already {{!obj}used}. *)

val obj : objc -> obj codec
(** [obj o] is a codec for a JSON object described by [o]. An
    object with the defaults of [o]'s members is the default value.

    {b Warning.} Once [o] has been used using [obj] it is no longer
    possible to modify the object codec [o] using {!anon} or {!mem}
    and its derivatives.  *)

(** {1:object_values JSON object value}

    {b Warning.} All the functions below when used on a JSON object
    value that is not derived from the object codec used to access
    them raise [Invalid_argument]. An object is derived from an
    object codec either if it was created with the
    {{!new_obj}codec} or if it was
    {{!dec}decoded} using that codec. *)

type memv
(** The type for a member and its value. *)

val memv : 'a mem -> 'a -> memv
(** [memv m v] is member [m] with value [v]. *)

val anonv : 'a anon -> string -> 'a -> memv
(** [anonv a name v] is an [a] anonymous member named [name] with value
    [v]. *)

val new_obj : obj codec -> memv list -> obj
(** [new_obj c memvs] is a new object described by [c] with
    members [memvs]. Unspecified members default to the defaults of [c]. *)

val get : 'a mem -> obj -> 'a
(** [get m o] is [o]'s [m] member value. *)

val set : 'a mem -> obj -> 'a -> obj
(** [set m o v] is [o] except [o]'s [m] member value is [v]. *)

val anon_names : 'a anon -> obj -> string list
(** [anon_names a o] is [o]'s list of anonymous member names. *)

val find_anon : 'a anon -> string -> obj -> 'a option
(** [find_anon a name o] is [o]'s [name] anonymous member
    value. [None] is returned if [name] is not an anonymous member of
    [o]. *)

val get_anon : 'a anon -> string -> obj -> 'a
(** [get_anon] is like {!find_anon} but @raise Invalid_argument if
    the name is absent. *)

val add_anon : 'a anon -> string -> obj -> 'a -> obj
(** [add_anon a name o v] is [o] except its [name] anonymous member is
    [v]. *)

val rem_anon : 'a anon -> string -> obj -> obj
(** [rem_anon a name o] is [o] except its [name] anonymous member is
    deleted. *)

(** {2 Accessing position information} *)

val get_def : 'a mem -> obj -> 'a def
(** [get_def] is like {!get} but with location information. *)

val find_anon_def : 'a anon -> string -> obj -> 'a def option
(** [find_anon_def] is like {!find_anon} but with location information. *)

val get_anon_def : 'a anon -> string -> obj -> 'a def
(** [get_anon_def] is like {!get_anon} but with location information. *)

(** {1:dec JSON decode} *)

type error =
  [ `Json_decoder of Jsont_codec.error
  | `Type of string * string
  | `Value_decoder of string
  | `Member of string * string * [ `Dup | `Miss | `Unknown ] ]
(** The type for decode errors. *)

val error_to_string : error -> string
(** [error_to_string e] is [e] as an unspecified string. *)

type 'a decoder
(** The type for decoders. The type ['a] is the resulting OCaml type decoded. *)

val decoder : ?loc:bool -> ?dups:[`Skip | `Error] ->
  ?unknown:[`Skip | `Error] -> Jsont_codec.decoder -> 'a codec -> 'a decoder
(** [decoder unsafe loc mem_dups d v] is a decoder for a value described
    [v] using the backend decoder [d]. The following optional may not
    be available in all backends:
    {ul
    {- [loc] if [true] will attach location information to the decoded
       values (default to [false]).}
    {- [dups] indicate the behaviour in case of duplicate object member
       in the data (defaults to [`Skip]).}
    {- [unknown] indicate the behaviour in case of unknown object member
       in the data (defaults to [`Skip]).}} *)

val decode : 'a decoder -> [ `Await | `Error of error def | `Ok of 'a def]
(** [decode d] is:
    {ul
    {- [`Await] if [d]'s backend decoder is non-blocking and needs a refill.}
    {- [`Error e] if an error occured. If the client is interested in
       a best-effort decoding it can still continue to decode.}
    {- [`Ok v] when the end of input was reached and the value is decoded.
       In case of error this value may be partially defined by codec
       default values.}}

    {b Note.} Repeated invocation always eventually return [`Ok] even
    in case of errors. In the worst case the returned value with be
    the decoder's default value. *)

val decoder_decoder : 'a decoder -> Jsont_codec.decoder
(** [decoder_decoder d] is [d]'s backend decoder. *)

(** {1:enc JSON encode} *)

type encoder
(** The type for encoders. *)

val encoder : Jsont_codec.encoder -> 'a codec -> 'a -> encoder
(** [encoder e c v] is an encoder that encodes [v] as described by
    [c] using the native encoder [e]. *)

val encode : encoder -> [ `Partial | `Ok ]
(** [encode e] encodes the value in [e] returns
    {ul
    {- [`Partial] if [e]'s native encoder is non-blocking an needs more
       output storage.}
    {- [`Ok] when the encoder finished to encode the value.}}

    {b Note.} You should always call the function until [`Ok] is returned. *)

val encoder_encoder : encoder -> Jsont_codec.encoder
(** [encoder_encoder e] is [e]'s backend encoder. *)

(** {1:basics Basics} *)

(** {1:tips Tips}

{ul
{- To keep the undescribed members of an object without bothering –
   e.g. if you are decoding an object to change a field and recode it
   back. Use {!anon} with {!soup}}
{- For handling unreasonable object schemes with conditional and
   optional members get all members as optional and sort out the logic
   through a {!Jsont.codec}.}} *)

(** {1:limits Limitations} *)

(** {1:examples Examples} *)

(** {1:model_data Modeling JSON data} *)


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
