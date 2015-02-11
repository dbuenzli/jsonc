(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* A few notes.

   * The use of JSON.{parse,stringify}'s reviver/replacer was
     considered. But unfortunately these callbacks are too generic and
     don't seem to give enough context information about the traversal
     to be used effectively in our case. Who the heck did such a poor
     design ?

   * Immutable objects. We first tried to copy using the prototyping
     trick: (Js.Unsafe.variable "Object") ## create (o). However it
     leads to all sorts of complexities. It seems better if Jsont
     managed JavaScript objects don't do prototyping tricks. *)

let str = Printf.sprintf

let err_conv_default msg = str "could not convert default value (%s)" msg

let err_objc_dup_mem k n =
  str "object codec %s: duplicate description for member %s" k n

let err_objc_dup_anon k =
  str "object codec %s: duplicate description for anonymous members" k

let err_objc_used k =
  str "object codec %s: description already in use" k

let err_some_combinator =
  str "Jsont.some misuse: cannot be used to encode None"

let err_oid = str "object not described by codec"
let err_mem_oid n = str "object not described by object codec of member %s" n
let err_anon_oid =
  str "object not described by object codec of anonymous member"

let err_anon_mem n = str "no anonymous member named %s" n

(* Value codecs *)

type ('a, 'b) value_decoder = 'a -> [ `Ok of 'b | `Error of string ]
type ('b, 'a) value_encoder = 'b -> 'a
type ('a, 'b) value_codec = ('a, 'b) value_decoder * ('b, 'a) value_encoder

(* Value location (unused in this backend) *)

type loc = (int * int) * (int * int)
type 'a def = loc * 'a

let invalid_loc = (-1, 0), (-1, 0)
let is_invalid_loc l = l = invalid_loc
let invalid_def v = invalid_loc, v
let undef (_, v) = v

(* JavaScript helpers *)

let (null : < > Js.t) = Obj.magic Js.null

let is_array (o : < > Js.t) =
  (* FIXME IE >= 9 *)
  Js.to_bool ((Js.Unsafe.variable "Array") ## isArray (o))

let jobj_keys (o : < > Js.t) : Js.js_string Js.t Js.js_array Js.t =
  (* FIXME IE >= 9 *)
  (Js.Unsafe.variable "Object") ## keys (o)

let jobj_raw_copy (o : < > Js.t) : < > Js.t = (* assumes no prototype tricks *)
  let keys = jobj_keys o in
  let newobj = Js.Unsafe.obj [||] in
  for i = 0 to (keys ## length) - 1 do
    let (key : Js.js_string Js.t) =  Obj.magic (Js.array_get keys i) in
    Js.Unsafe.set newobj key (Js.Unsafe.get o key);
  done;
  newobj

(* JSON values *)

type nat_string = Js.js_string Js.t

let nat_string_of_string = Jsont_codec.nat_string_of_string
let nat_string_to_string = Jsont_codec.nat_string_to_string

type soup = < > Js.t

(* JSON object values

   A Jsont decoded/created JSON object value is represented by an object with
   three fields

   - [oid] holds the object codec id.
   - [mems] holds an object that maps described member names to their value.
   - [anons] holds an object that maps unknown member names to their value. *)

module Id = struct                                (* uids for object codecs. *)
  type t = int
  let nil = -1
  let create = let count = ref nil in fun () -> incr count; !count
end

type obj = < > Js.t

let obj_empty oid : < > Js.t =
  Js.Unsafe.obj [| "oid", Js.Unsafe.inject oid;
                   "mems", Js.Unsafe.inject (Js.Unsafe.obj [||]);
                   "anons", Js.Unsafe.inject (Js.Unsafe.obj [||]); |]


let oid_key = Js.string "oid"
let mems_key = Js.string "mems"
let anons_key = Js.string "anons"

(* Note, the accessors assume the oid has been checked. *)

let obj_get_oid (o : obj) : int = Js.Unsafe.get o oid_key

let obj_mem_keys o = jobj_keys (Js.Unsafe.get o mems_key)
let obj_get_mem o n = Js.Unsafe.get (Js.Unsafe.get o mems_key) n
let obj_set_mem o n v = Js.Unsafe.set (Js.Unsafe.get o mems_key) n v
let obj_anon_keys o = jobj_keys (Js.Unsafe.get o anons_key)
let obj_get_anon o n = Js.Unsafe.get (Js.Unsafe.get o anons_key) n
let obj_set_anon o n v = Js.Unsafe.set (Js.Unsafe.get o anons_key) n v

let obj_set_mem_copy o n v =
  let copy = obj_empty (obj_get_oid o) in
  let mems = jobj_raw_copy (Js.Unsafe.get o mems_key) in
  Js.Unsafe.set mems n v;
  Js.Unsafe.set copy mems_key mems;
  Js.Unsafe.set copy anons_key (Js.Unsafe.get o anons_key);
  copy

let obj_add_anons_copy o kvs =
  let copy = obj_empty (obj_get_oid o) in
  let anons = jobj_raw_copy (Js.Unsafe.get o anons_key) in
  let set (k, v) = Js.Unsafe.set anons k v in
  List.iter set kvs;
  Js.Unsafe.set copy anons_key anons;
  Js.Unsafe.set copy mems_key (Js.Unsafe.get o mems_key);
  copy

let obj_rem_anon_copy o n =
  let copy = obj_empty (obj_get_oid o) in
  let anons = jobj_raw_copy (Js.Unsafe.get o anons_key) in
  Js.Unsafe.delete anons n;
  Js.Unsafe.set copy anons_key anons;
  Js.Unsafe.set copy mems_key (Js.Unsafe.get o mems_key);
  copy

let obj_fold_anons f acc o =
  let anons = Js.Unsafe.get o anons_key in
  let keys = jobj_keys anons in
  let rec loop acc max i =
    if i > max then acc else
    let (key : Js.js_string Js.t) = Obj.magic (Js.array_get keys i) in
    loop (f acc key (Js.Unsafe.get anons key)) max (i + 1)
  in
  loop acc (keys ## length - 1) 0

let obj_copy o =
  let copy = obj_empty (obj_get_oid o) in
  let mems = jobj_raw_copy (Js.Unsafe.get o mems_key) in
  let anons = jobj_raw_copy (Js.Unsafe.get o anons_key) in
  Js.Unsafe.set copy mems_key mems;
  Js.Unsafe.set copy anons_key anons;
  copy

(* JSON codecs and value codec types *)

type error =
  [ `Json_decoder of Jsont_codec.error
  | `Type of string * string
  | `Value_decoder of string
  | `Member of string * string * [ `Dup | `Miss | `Unknown ] ]

let error_to_string = function
| `Json_decoder e -> e
| `Value_decoder e -> e
| `Type (fnd, exp) -> str "value has type %s but expected type %s" fnd exp
| `Member (o, m, e)  ->
    str "member %s of object kind %s %s" m o begin match e with
    | `Dup -> "appears more than once"
    | `Miss -> "is missing"
    | `Unknown -> "is unknown"
    end

type 'a decode = [ `Await | `Ok of 'a def | `Error of error def ]
type 'a decoder =
  { dec_loc : bool;      (* [true] if location should be computed (unused). *)
    dec_dups : [ `Skip | `Error ];    (* behaviour on dup members (unused). *)
    dec_unknown : [ `Skip | `Error ];       (* behaviour on unknown member. *)
    dec : Jsont_codec.decoder;                          (* backend decoder. *)
    mutable dec_ctx : obj list;          (* currently decoded object stack. *)
    mutable dec_k : 'a decoder -> 'a decode }      (* decoder kontinuation. *)

type encode = [ `Partial | `Ok ]
and encoder =
  { enc : Jsont_codec.encoder;                          (* backend encoder. *)
    mutable enc_ctx : obj list;          (* currently encoded object stack. *)
    mutable enc_k : encoder -> encode }           (* encoding kontinuation. *)

and 'a codec =                                         (* JSON value codec. *)
  { default : 'a;                                         (* default value. *)
    decode :                                              (* value decoder. *)
      'b. 'a codec -> < > Js.t -> ('a -> 'b decoder -> 'b decode) ->
          'b decoder -> 'b decode;
    encode :                                              (* value encoder. *)
      'a codec -> 'a -> (< > Js.t -> encoder -> encode) -> encoder -> encode  }

type 'a mem =                                         (* JSON member codec. *)
  { mem_oid : int;                                      (* object codec id. *)
    mem_name : Js.js_string Js.t;                           (* member name. *)
    mem_opt :                                        (* optional behaviour. *)
      [ `Yes | `Yes_rem of 'a -> 'a -> bool | `No ];
    mem_codec : 'a codec }                           (* member value codec. *)

and mem_exists = Me : 'a mem -> mem_exists        (* hides mem's parameter. *)

type 'a anon =                                (* Unknown JSON member codec. *)
  { anon_oid : int;                                     (* object codec id. *)
    anon_default : (string * 'a) list;             (* default anon members. *)
    anon_codec : 'a codec; }                                (* value codec. *)

type anon_exists = Ae : 'a anon -> anon_exists   (* hides anon's parameter. *)

type objc =                                           (* JSON object codec. *)
  { objc_id : int;                                      (* object codec id. *)
    objc_kind : string;                     (* a name for the object codec. *)
    mutable objc_used : bool;                (* [true] when codec was used. *)
    mutable objc_mems :                            (* object member codecs. *)
      (string * mem_exists) list;
    mutable objc_anon : anon_exists option; }      (* unknown member codec. *)

(* Decode *)

let ret v k d = d.dec_k <- k; v

let err err k d = ret (`Error (invalid_def err)) k d
let err_json_decoder e k d = err (`Json_decoder (Js.string_of_error e)) k d
let err_type o fnd exp k d =
  let fnd = match Js.to_string fnd with
  | "boolean" -> "bool" | "number" -> "float"
  | "object" as s ->
      if o = null then "null" else
      if is_array o then "array" else
      s
  | s -> s
  in
  err (`Type (fnd, exp)) k d

let err_value_decoder msg k d = err (`Value_decoder msg) k d
let err_mem_miss okind n k d =
  err (`Member (okind, (Js.to_string n), `Miss)) k d

let err_mem_unknown okind n k d =
  err (`Member (okind, (Js.to_string n), `Unknown)) k d

let k_default codec k d = k codec.default d

let finish v d =
  let v = invalid_def v in
  d.dec_k <- (fun _ -> `Ok v); `Ok v

let decoder ?(loc = false) ?(dups = `Skip) ?(unknown = `Skip) dec codec =
  let dec_k d =
    try
      let obj = Js._JSON ## parse (Jsont_codec.decoder_src d.dec) in
      codec.decode codec obj finish d
    with Js.Error e -> err_json_decoder e (k_default codec finish) d
  in
  { dec_loc = loc; dec_dups = dups; dec_unknown = unknown; dec; dec_ctx = [];
    dec_k; }

let decode d = d.dec_k d
let decoder_decoder d = d.dec

(* Encode *)

let enc_partial k e = e.enc_k <- k; `Partial
let enc_next v k e = k v e

let finish o e =
  Jsont_codec.encoder_set_dst e.enc (Js._JSON ## stringify (o));
  e.enc_k <- (fun _ -> `Ok);
 `Ok

let encoder enc codec v =
  let enc_k e = codec.encode codec v finish e in
  { enc; enc_ctx = []; enc_k }

let encode e = e.enc_k e
let encoder_encoder e = e.enc

(* JSON base value codecs *)

let default d = d.default
let with_default v d = { d with default = v }

let typ_boolean = Js.string "boolean"
let bool : bool codec =
  let decode codec o k d =
    let typ = Js.typeof o in
    if typ <> typ_boolean then err_type o typ "bool" (k_default codec k) d else
    k (Js.to_bool (Obj.magic o : bool Js.t)) d
  in
  let encode codec v k e = k (Obj.magic (Js.bool v) : < > Js.t) e in
  { default = false; decode; encode }

let typ_number = Js.string "number"
let float : float codec =
  let decode codec o k d =
    let typ = Js.typeof o in
    if typ <> typ_number then err_type o typ "float" (k_default codec k) d else
    k (Obj.magic o : float) d
  in
  let encode codec v k e = k (Obj.magic v : < > Js.t) e in
  { default = 0.0; decode; encode }

let int : int codec =
  let decode codec o k d =
    let typ = Js.typeof o in
    if typ <> typ_number then err_type o typ "int" (k_default codec k) d else
    k (int_of_float (Obj.magic o : float)) d
  in
  let encode codec v k e = k (Obj.magic (float_of_int v) : < > Js.t) e in
  { default = 0; decode; encode }

let int_strict : int codec =
  let decode codec o k d =
    let typ = Js.typeof o in
    if typ <> typ_number then err_type o typ "int" (k_default codec k) d else
    let f : float = (Obj.magic o : float) in
    if f -. (floor f) <> 0.
    then err_type o typ "int" (k_default codec k) d else
    k (int_of_float f) d
  in
  let encode codec v k e = k (Obj.magic (float_of_int v) : < > Js.t) e in
  { default = 0; decode; encode }

let typ_string = Js.string "string"
let nat_string_empty = Js.string ""
let nat_string : Jsont_codec.nat_string codec =
  let decode codec o k d =
    let typ = Js.typeof o in
    if typ <> typ_string then err_type o typ "string" (k_default codec k) d else
    k (Js.Unsafe.coerce o : Js.js_string Js.t) d
  in
  let encode codec v k e = k (Js.Unsafe.coerce v : < > Js.t) e in
  { default = nat_string_empty; decode; encode }

let string : string codec =
  let decode codec o k d =
    let typ = Js.typeof o in
    if typ <> typ_string then err_type o typ "string" (k_default codec k) d else
    k (Js.to_string (Obj.magic o : Js.js_string Js.t)) d
  in
  let encode codec v k e = k (Js.Unsafe.coerce (Js.string v) : < > Js.t) e in
  { default = ""; decode; encode }

let nullable base =
  let decode codec o k d =
    if o = null then k None d else
    base.decode base o (fun v -> k (Some v)) d
  in
  let encode codec v k e = match v with
  | None -> k null e
  | Some v -> base.encode base v k e
  in
  { default = Some base.default; decode; encode }

let view ?default (vdec, venc) base =
  let default = match default with
  | Some v -> v
  | None ->
      match vdec base.default with
      | `Ok d -> d | `Error msg -> invalid_arg (err_conv_default msg)
  in
  let decode codec o k d =
    let vdec k v d = match vdec v with
    | `Error msg -> err_value_decoder msg (k_default codec k) d
    | `Ok v -> k v d
    in
    base.decode base o (vdec k) d
  in
  let encode codec v k e = base.encode base (venc v) k e in
  { default; decode; encode }

let type_match ~default decd encd =
  let decode codec o k d =
    let use typ = match decd typ with
    | `Ok vd -> vd.decode vd o k d
    | `Error e -> err_value_decoder e (k_default codec k) d
    in
    if o = null then use `Null else
    let typ = Js.typeof o in
    if typ = typ_boolean then use `Bool else
    if typ = typ_number then use `Float else
    if typ = typ_string then use `String else
    if is_array o then use `Array else
    use `Object
  in
  let encode codec v k e = let codec = encd v in codec.encode codec v k e in
  { default; decode; encode }

let soup =
  let decode codec o k d = k o d in
  let encode codec v k e  = k v e in
  { default = null; decode; encode }

let some base =
  let decode codec o k d = base.decode base o (fun v -> k (Some v)) d in
  let encode codec v k e = match v with
  | None -> invalid_arg err_some_combinator
  | Some v -> base.encode base v k e
  in
  { default = None; decode; encode }

(* JSON array codecs *)

let decode_array elt codec o k d =
  let rec loop acc a max i k d =
    if i > max then k (List.rev acc) d else
    let (o : < > Js.t) = Obj.magic (Js.array_get a i) in
    elt.decode elt o (fun v -> loop (v :: acc) a max (i + 1) k) d
  in
  if not (is_array o)
  then err_type o (Js.typeof o) "array" (k_default codec k) d
  else
  let (a : < > Js.t Js.js_array Js.t) = Js.Unsafe.coerce o in
  loop [] a (a ## length - 1) 0 k d

let encode_array elt codec vs k e  =
  let rec loop elt vs result i k e = match vs with
  | [] -> k (Js.Unsafe.coerce result : < > Js.t) e
  | v :: vs ->
      let set v d = Js.Unsafe.set result i v; loop elt vs result (i + 1) k d in
      elt.encode elt v set e
  in
  loop elt vs (Js.array [||]) 0 k e

let array elt =
  let decode codec o k d = decode_array elt codec o k d in
  let encode codec v k e = encode_array elt codec v k e in
  { default = []; decode; encode }

let array_array elt =
  (* FIXME this could avoid lists. *)
  let c = (fun v -> `Ok (Array.of_list v)), (fun v -> Array.to_list v) in
  view c (array elt)

(* JSON object codecs *)

let objc ?kind () =
  let objc_id = Id.create () in
  let objc_kind = match kind with None -> str "o%d" objc_id | Some k -> k in
  let objc_used = false in
  let objc_mems = [] in
  let objc_anon = None in
  { objc_id; objc_kind; objc_used; objc_mems; objc_anon; }

let check_add objc name =
  if objc.objc_used then invalid_arg (err_objc_used objc.objc_kind) else
  if List.mem_assoc name objc.objc_mems
  then invalid_arg (err_objc_dup_mem objc.objc_kind name) else
  ()

let mem ?(eq = ( = )) ?(opt = `No) objc name mem_codec =
  check_add objc name;
  let mem_oid = objc.objc_id in
  let mem_name = Js.string name in
  let mem_opt = match opt with `No | `Yes as v -> v | `Yes_rem -> `Yes_rem eq in
  let mem = { mem_oid; mem_name; mem_codec; mem_opt } in
  objc.objc_mems <- (name, (Me mem)) :: objc.objc_mems;
  mem

let mem_opt objc mem_name codec = mem objc mem_name ~opt:`Yes_rem (some codec)

let mem_match ?eq ?opt objc mmatch name select =
  if objc.objc_id <>  mmatch.mem_oid
  then invalid_arg (err_mem_oid (Js.to_string mmatch.mem_name)) else
  let codec =
    let default = (select mmatch.mem_codec.default).default in
    let decode codec o k d =
      let ctx = match d.dec_ctx with [] -> assert false | ctx :: _ -> ctx in
      let v = obj_get_mem ctx mmatch.mem_name in
      let codec = select v in
      codec.decode codec o k d
    in
    let encode codec o k e =
      let ctx = match e.enc_ctx with [] -> assert false | ctx :: _ -> ctx in
      let v = obj_get_mem ctx mmatch.mem_name in
      let codec = select v in
      codec.encode codec o k e
    in
    { default; decode; encode }
  in
  mem ?eq ?opt objc name codec

let anon ?default objc anon_codec =
  if objc.objc_used then invalid_arg (err_objc_used objc.objc_kind) else
  if objc.objc_anon <> None then invalid_arg (err_objc_dup_anon objc.objc_kind)
  else
  let anon_oid = objc.objc_id in
  let anon_default = match default with None -> [] | Some v -> v in
  let anon = { anon_oid; anon_default; anon_codec } in
  objc.objc_anon <- Some (Ae anon);
  anon

let objc_default objc =
  let o = obj_empty objc.objc_id in
  let set_mem (k, (Me m)) = obj_set_mem o m.mem_name m.mem_codec.default in
  List.iter set_mem objc.objc_mems;
  begin match objc.objc_anon with
  | None -> ()
  | Some (Ae a) ->
      let add_anon (k, v) = obj_set_anon o (Js.string k) v in
      List.iter add_anon a.anon_default
  end;
  o

let rec decode_anon_mems objc anons o result k d = match anons with
| [] -> k result d
| (_, js_name) :: anons ->
    match objc.objc_anon with
    | None ->
        begin match d.dec_unknown with
        | `Skip -> decode_anon_mems objc anons o result k d
        | `Error ->
            err_mem_unknown objc.objc_kind js_name
              (decode_anon_mems objc anons o result k) d
        end
    | Some (Ae a) ->
        let set v d =
          obj_set_anon result js_name v;
          decode_anon_mems objc anons o result k d
        in
        a.anon_codec.decode a.anon_codec (Js.Unsafe.get o js_name) set d

let rec decode_mems objc names o mems result k d = match mems with
| [] -> decode_anon_mems objc names o result k d
| (n, Me mem) :: mems ->
    match try Some (List.assoc n names) with Not_found -> None with
    | None ->
        obj_set_mem result mem.mem_name mem.mem_codec.default;
        begin match mem.mem_opt with
        | `Yes | `Yes_rem _  -> decode_mems objc names o mems result k d
        | `No ->
            err_mem_miss objc.objc_kind mem.mem_name
              (decode_mems objc names o mems result k) d
        end
    | Some js_name ->
        let names = List.remove_assoc n names in
        let set v d =
          obj_set_mem result js_name v;
          decode_mems objc names o mems result k d
        in
        mem.mem_codec.decode mem.mem_codec (Js.Unsafe.get o n) set d

let typ_object = Js.string "object"
let decode_obj objc codec o k d =
  let typ = Js.typeof o in
  if not (typ = typ_object) || (is_array o)
  then err_type o typ "object" (k_default codec k) d else
  let names =
    let keys = jobj_keys o in
    let rec loop acc max i =
      if i > max then acc else
      let (n : Js.js_string Js.t) = Obj.magic (Js.array_get keys i) in
      loop ((Js.to_string n, n) :: acc) max (i + 1)
    in
    loop [] (keys ## length - 1) 0
  in
  let result = obj_empty objc.objc_id in
  let pop k result d = d.dec_ctx <- List.tl d.dec_ctx; k result d in
  d.dec_ctx <- result :: d.dec_ctx;
  decode_mems objc names o objc.objc_mems result (pop k) d

let encode_anons objc o k result e =
  let rec loop names max i result k e =
    if i > max then k result e else
    let (name : Js.js_string Js.t) = Obj.magic (Js.array_get names i) in
    match objc.objc_anon with
    | None -> assert false
    | Some (Ae a) ->
        let set v e =
          Js.Unsafe.set result name v; loop names max (i + 1) result k e
        in
        a.anon_codec.encode a.anon_codec (obj_get_anon o name) set e
  in
  let names = obj_anon_keys o in
  loop names (names ## length - 1) 0 result k e

let encode_mems objc o result k e =
  let rec loop names result k e = match names with
  | [] -> k result e
  | (n, Me mem) :: names ->
      let v = obj_get_mem o mem.mem_name in
      match mem.mem_opt with
      | `Yes_rem eq when eq v mem.mem_codec.default ->
          loop names result k e
      | _ ->
          let set v e =
            Js.Unsafe.set result mem.mem_name v; loop names result k e
          in
          mem.mem_codec.encode mem.mem_codec v set e
  in
  loop objc.objc_mems result k e

let encode_obj objc codec o k e =
  if obj_get_oid o <> objc.objc_id then invalid_arg err_oid else
  let result = Js.Unsafe.obj [||] in
  let pop k result d = e.enc_ctx <- List.tl e.enc_ctx; k result d in
  e.enc_ctx <- o :: e.enc_ctx;
  encode_mems objc o result (encode_anons objc o (pop k)) e

let obj objc =
  objc.objc_used <- true;
  objc.objc_mems <- List.rev objc.objc_mems; (* order for dec. match mems *)
  let decode codec o k d = decode_obj objc codec o k d in
  let encode codec o k e = encode_obj objc codec o k e in
  { default = objc_default objc; decode; encode }

(* JSON object values *)

let check_mem_oid o m =
  if (obj_get_oid o) <>  m.mem_oid
  then invalid_arg (err_mem_oid (Js.to_string m.mem_name)) else ()

let get m o = check_mem_oid o m; obj_get_mem o m.mem_name
let set m o v = check_mem_oid o m; obj_set_mem_copy o m.mem_name v

let check_anon_oid o a =
  if (obj_get_oid o) <> a.anon_oid then invalid_arg err_anon_oid else ()

let anon_names a o =
  check_anon_oid o a;
  List.rev (obj_fold_anons (fun acc key _ -> Js.to_string key :: acc) [] o)

let find_anon a name o =
  check_anon_oid o a; Js.Optdef.to_option (obj_get_anon o name)

let get_anon a name o = match find_anon a name o with
| None -> invalid_arg (err_anon_mem name) | Some v -> v

let add_anon a name o v = check_anon_oid o a; obj_add_anons_copy o [name, v]
let rem_anon a name o = check_anon_oid o a; obj_rem_anon_copy o name

let get_def m o = invalid_def (get m o)

let find_anon_def a name o = match find_anon a name o with
| None -> None | Some v -> Some (invalid_def v)

let get_anon_def a name o = match find_anon a name o with
| None -> invalid_arg (err_anon_mem name) | Some v -> invalid_def v

(* JSON object value creation. *)

type memv =
  | M : 'a mem * 'a -> memv
  | A : 'a anon * string * 'a -> memv

let memv m v = M (m, v)
let anonv a n v = A (a, n, v)

let new_obj d mems =
  let o = obj_copy d.default in
  let set = function
  | M (m, v) -> check_mem_oid o m; obj_set_mem o m.mem_name v
  | A (a, n, v) -> check_anon_oid o a; obj_set_anon o n v
  in
  List.iter set mems;
  o

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
