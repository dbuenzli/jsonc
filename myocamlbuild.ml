open Ocamlbuild_plugin

let () =
  dispatch begin fun d ->
    Ocamlbuild_js_of_ocaml.dispatcher d;
    match d with
    | After_rules ->
        ocaml_lib ~tag_name:"jsont_jsonm" "src-jsonm/jsont_jsonm";
        ocaml_lib ~tag_name:"jsont_jsoo" "src-jsoo/jsont_jsoo";
    | _ -> ()
  end
