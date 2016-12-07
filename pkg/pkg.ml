#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let jsonm = Conf.with_pkg "jsonm"
let jsoo = Conf.with_pkg "js_of_ocaml"

let build =
  let cmd c os files =
    let jsoo_args = Cmd.(v "-plugin-tag" % "package(js_of_ocaml.ocamlbuild)") in
    let jsoo = Cmd.(on (Conf.value c jsoo) jsoo_args) in
    OS.Cmd.run @@ Cmd.(Pkg.build_cmd c os %% jsoo %% of_list files)
  in
  Pkg.build ~cmd ()

let jsoo_test ~cond test =
  Pkg.flatten
    [ Pkg.test ~run:false ~cond ~auto:false (test ^ ".js");
      Pkg.test ~run:false ~cond ~auto:false (test ^ ".html"); ]

let () =
  Pkg.describe "jsonc" ~build @@ fun c ->
  let jsonm = Conf.value c jsonm in
  let jsoo = Conf.value c jsoo in
  Ok [
    Pkg.mllib ~cond:jsonm ~dst_dir:"jsonm" "src-jsonm/jsonc_jsonm.mllib";
    Pkg.mllib ~cond:jsoo ~dst_dir:"jsoo" "src-jsoo/jsonc_jsoo.mllib";
(*
    Pqqkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsonm/jsonc" "src-jsonm/jsonc";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsonm/jsonc_codec" "src-jsonm/jsonc_codec";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library
      ~dst:"jsoo/jsonc_jsoo" "src-jsoo/jsonc_jsoo";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsoo/jsonc" "src-jsoo/jsonc";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsoo/jsonc_codec" "src-jsoo/jsonc_codec";*) ]
