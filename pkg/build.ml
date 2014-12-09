#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let jsonm = Env.bool "jsonm"
let jsoo = Env.bool "jsoo"

let builder =
  if not jsoo then `OCamlbuild else
  `Other
    ("ocamlbuild -use-ocamlfind -classic-display \
                 -plugin-tag \"package(js_of_ocaml.ocamlbuild)\" \
                 -I src-jsonm -I src-jsoo",
     "_build")

let () =
  Pkg.describe "jsont" ~builder [
    Pkg.lib "pkg/META";
    Pkg.lib ~cond:jsonm ~exts:Exts.module_library
      ~dst:"jsonm/jsont_jsonm" "src-jsonm/jsont_jsonm";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsonm/jsont" "src-jsonm/jsont";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsonm/jsont_codec" "src-jsonm/jsont_codec";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library
      ~dst:"jsoo/jsont_jsoo" "src-jsoo/jsont_jsoo";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsoo/jsont" "src-jsoo/jsont";
    Pkg.lib ~cond:jsonm ~exts:Exts.interface_opt
      ~dst:"jsoo/jsont_codec" "src-jsoo/jsont_codec";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
