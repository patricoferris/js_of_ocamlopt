let run () =
  let open Js_of_ocaml in
  List.iter
    (fun (v : Protocol.static_cmi) ->
      Sys_js.create_file ~name:("ocamlopt/" ^ v.sc_name) ~content:v.sc_content)
    Static_files.stdlib_cmis
