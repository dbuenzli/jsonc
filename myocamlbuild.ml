open Ocamlbuild_plugin

let () =
  dispatch begin fun d ->
    Ocamlbuild_js_of_ocaml.dispatcher d;
    match d with
    | After_rules ->
        ocaml_lib ~tag_name:"jsonc_jsonm" "src-jsonm/jsonc_jsonm";
        ocaml_lib ~tag_name:"jsonc_jsoo" "src-jsoo/jsonc_jsoo";
    | _ -> ()
  end
