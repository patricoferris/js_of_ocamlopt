open Clflags

let log s = Brr.Console.log [ Jstr.v s ]

module Backend = struct
  (* See backend_intf.mli. *)

  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol
  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol
  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    (* The "-1" is to allow for a potential closure environment parameter. *)
    Proc.max_arguments_for_tailcalls - 1
end

let backend = (module Backend : Backend_intf.S)

module Options = Main_args.Make_optcomp_options (Main_args.Default.Optmain)

let main argv ppf =
  native_code := true;
  let program = "ocamlopt" in
  match
    Compenv.readenv ppf Before_args;
    Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
    Clflags.add_arguments __LOC__
      [
        ( "-depend",
          Arg.Unit Makedepend.main_from_option,
          "<options> Compute dependencies (use 'ocamlopt -depend -help' for \
           details)" );
      ];
    Compenv.parse_arguments (ref argv) Compenv.anonymous program;
    Compmisc.read_clflags_from_env ();
    dump_into_file := true;
    dump_dir := None;
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    let impl ~start_from ~source_file ~output_prefix =
      log "impl";
      Optcompile.implementation ~backend ~start_from ~source_file ~output_prefix
    in
    (try
       log "deferred";
       Compenv.process_deferred_actions
         (ppf, impl, Optcompile.interface, ".cmx", ".cmxa")
     with Arg.Bad msg ->
       prerr_endline msg;
       Clflags.print_arguments program;
       failwith "Exiting with something");
    Compenv.readenv ppf Before_link;
    (if
       List.length
         (List.filter
            (fun x -> !x)
            [
              make_package;
              make_archive;
              shared;
              Compenv.stop_early;
              output_c_object;
            ])
       > 1
     then
       let module P = Clflags.Compiler_pass in
       match !stop_after with
       | None ->
           Compenv.fatal
             "Please specify at most one of -pack, -a, -shared, -c, -output-obj"
       | Some ((P.Parsing | P.Typing | P.Scheduling | P.Emit | P.Lambda) as p)
         ->
           assert (P.is_compilation_pass p);
           Printf.ksprintf Compenv.fatal
             "Options -i and -stop-after (%s) are  incompatible with -pack, \
              -a, -shared, -output-obj"
             (String.concat "|"
                (P.available_pass_names ~filter:(fun _ -> true) ~native:true)));
    if !make_archive then (
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Asmlibrarian.create_archive
        (Compenv.get_objfiles ~with_ocamlparam:false)
        target;
      Warnings.check_fatal ())
    else if !make_package then (
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Asmpackager.package_files ~ppf_dump (Compmisc.initial_env ())
            (Compenv.get_objfiles ~with_ocamlparam:false)
            target ~backend);
      Warnings.check_fatal ())
    else if !shared then (
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          Asmlink.link_shared ~ppf_dump
            (Compenv.get_objfiles ~with_ocamlparam:false)
            target);
      Warnings.check_fatal ())
    else if
      (not !Compenv.stop_early)
      && (!objfiles <> [] || !Compenv.has_linker_inputs)
    then (
      let target =
        if !output_c_object then
          let s = Compenv.extract_output !output_name in
          if
            Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
          then s
          else
            Compenv.fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll)
        else Compenv.default_output !output_name
      in
      Compmisc.init_path ();
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          let objs = Compenv.get_objfiles ~with_ocamlparam:true in
          Asmlink.link ~ppf_dump objs target);
      Warnings.check_fatal ())
  with
  | exception Compenv.Exit_with_status n -> n
  | exception Env.Error e ->
      Env.report_error Format.str_formatter e;
      Brr.Console.warn [ Jstr.v (Format.flush_str_formatter ()) ];
      2
  | exception Cmi_format.Error e ->
      Cmi_format.report_error Format.str_formatter e;
      Brr.Console.warn [ Jstr.v (Format.flush_str_formatter ()) ];
      2
  | () ->
      Compmisc.with_ppf_dump ~file_prefix:"profile" (fun ppf ->
          Profile.print ppf !Clflags.profile_columns);
      0

open Js_of_ocaml

type dump =
  | Source
  | Parsetree
  | Typedtree
  | Shape
  | Rawlambda
  | Lambda
  | Rawclambda
  | Clambda
  | Sel
  | Cmm
  | Linear

let all_dump =
  [
    Lambda;
    Source;
    Parsetree;
    Typedtree;
    Shape;
    Rawlambda;
    Rawclambda;
    Clambda;
    Sel;
    Cmm;
    Linear;
  ]

let dump_to_string (d : dump) : string =
  match d with
  | Source -> "Source"
  | Parsetree -> "Parsetree"
  | Typedtree -> "Typedtree"
  | Shape -> "Shape"
  | Rawlambda -> "Rawlambda"
  | Lambda -> "Lambda"
  | Rawclambda -> "Rawclambda"
  | Clambda -> "Clambda"
  | Sel -> "Sel"
  | Cmm -> "Cmm"
  | Linear -> "Linear"

let string_to_dump (s : string) : dump option =
  match String.lowercase_ascii s with
  | "source" -> Some Source
  | "parsetree" -> Some Parsetree
  | "typedtree" -> Some Typedtree
  | "shape" -> Some Shape
  | "rawlambda" -> Some Rawlambda
  | "lambda" -> Some Lambda
  | "rawclambda" -> Some Rawclambda
  | "clambda" -> Some Clambda
  | "sel" -> Some Sel
  | "cmm" -> Some Cmm
  | "linear" -> Some Linear
  | _ -> None

let reset_flags () =
  dump_source := false;
  dump_parsetree := false;
  dump_typedtree := false;
  dump_shape := false;
  dump_rawlambda := false;
  dump_lambda := false;
  dump_rawclambda := false;
  dump_clambda := false;
  dump_flambda := false;
  dump_selection := false;
  dump_instr := false;
  dump_cmm := false;
  dump_linear := false

let set_clfags v =
  reset_flags ();
  match v with
  | Source -> dump_source := true
  | Parsetree -> dump_parsetree := true
  | Typedtree -> dump_typedtree := true
  | Shape -> dump_shape := true
  | Rawlambda -> dump_rawlambda := true
  | Lambda -> dump_lambda := true
  | Rawclambda -> dump_rawclambda := true
  | Clambda -> dump_clambda := true
  | Sel -> dump_selection := true
  | Cmm -> dump_cmm := true
  | Linear -> dump_linear := true

let dump_program prog flags =
  List.iter set_clfags flags;
  (try Sys.remove "main.ml" with Sys_error _ -> ());
  Sys_js.create_file ~name:"main.ml" ~content:prog;
  let buff = Buffer.create 128 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string buff);
  Sys_js.set_channel_flusher stderr (Buffer.add_string buff);
  let args =
    [| "-impl"; "main.ml"; "-stop-after"; "emit"; "-I"; "./ocamlopt" |]
  in
  let _code : int = try main args Format.str_formatter with _exn -> 0 in
  Brr.Console.log [ _code ];
  let asm_file = Sys.readdir "/tmp" |> fun v -> Array.get v 0 in
  log asm_file;
  let dump = Sys_js.read_file ~name:"main.cmx.dump" in
  let asm = Sys_js.read_file ~name:("/tmp/" ^ asm_file) in
  (dump, asm)
