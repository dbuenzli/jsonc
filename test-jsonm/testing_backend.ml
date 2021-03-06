(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* We test directly with the non-blocking codec 1 byte per byte to make
   sure we have that right. *)

let decoder data =
  let d = Jsonm.decoder `Manual in
  let max = String.length data - 1 in
  let pos = ref (-1) in
  let refill () =
    incr pos;
    if !pos > max then Jsonm.Manual.src d data 0 0 else
    Jsonm.Manual.src d data !pos 1;
  in
  d, refill

let encoder () =
  let e = Jsonm.encoder `Manual in
  let res = Buffer.create 255 in
  let buf = " " in
  let flush () =
    Buffer.add_substring res buf 0 (1 - (Jsonm.Manual.dst_rem e));
    Jsonm.Manual.dst e buf 0 1
  in
  let get_data _ = Buffer.contents res in
  Jsonm.Manual.dst e buf 0 1;
  e, flush, get_data

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
