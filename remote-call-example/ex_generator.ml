let prefix = "ex"

let remote_definitions fmt =
  let time = Unix.gettimeofday () in  
  let pr x = Format.fprintf fmt x in
  begin
    pr "#define EX_SEMAPHORE_ARG_NAME \"EX_SEMAPHORE_ARG_%f\"@\n" time;
    pr "#define EX_SEMAPHORE_RET_NAME \"EX_SEMAPHORE_RET_%f\"@\n" time;
    pr "#define EX_SHMEM_NAME \"EX_SHMEM_%f\"@\n" time;
    pr "#include \"ex_remote_helpers.h\"@\n";
    pr "#include \"ctypes/cstubs_remote_internals.h\"@\n";
    pr "#include \"ex.h\"@\n";
    pr "@\n"
  end

let generate_header fmt bindings =
  begin
    Format.fprintf fmt "#ifndef EX_SHARED_DEFINITIONS_H@\n";
    Format.fprintf fmt "#define EX_SHARED_DEFINITIONS_H@\n@\n";
    remote_definitions fmt;
    Cstubs.write_enum fmt ~prefix bindings;
    Cstubs.write_frame_structs fmt ~prefix bindings;
    Format.fprintf fmt "@\n#endif /* EX_SHARED_DEFINITIONS_H */@\n"
  end

let generate_remote_stubs fmt bindings =
  begin
    Format.fprintf fmt "#include \"ex_shared_definitions.h\"@\n";
    Cstubs.write_remote_dispatcher fmt ~prefix bindings;
  end

let generate_local_stubs fmt bindings =
  begin
    Format.fprintf fmt "#include \"ex_shared_definitions.h\"@\n";
    Cstubs.write_c fmt ~prefix bindings;
    Cstubs.write_remote_initializer_c fmt ~prefix bindings;
  end

let generate_ml fmt bindings =
  begin
    Cstubs.write_ml fmt ~prefix bindings;
    Cstubs.write_remote_initializer_ml fmt ~prefix bindings;
  end

let with_open_formatter filename f =
  let out = open_out filename in
  let fmt = Format.formatter_of_out_channel out in
  let close_channel () = close_out out in
  try
    let rv = f fmt in
    close_channel ();
    rv
  with e ->
    close_channel ();
    raise e

let filenames argv =
  let usage = "ex_generator.native [arguments]" in
  let ml_filename = ref ""
  and c_filename = ref ""
  and h_filename = ref ""
  and c_remote_filename = ref "" in
  let spec = Arg.([("--ml-file",
                    Set_string ml_filename, "ML filename");
                   ("--c-file",
                    Set_string c_filename, "C filename");
                   ("--h-file",
                    Set_string h_filename, "C header filename");
                   ("--c-remote-file",
                    Set_string c_remote_filename, "C remote filename");]) in
  let no_positional_args _ =
    prerr_endline "No positional arguments" in
  begin
    Arg.parse spec no_positional_args usage;
    (!ml_filename, !c_filename, !c_remote_filename, !h_filename)
  end

let main bindings argv =
  let ml_filename, c_filename, c_remote_filename, h_filename = filenames argv in
  begin
    if ml_filename <> "" then
      with_open_formatter ml_filename (fun fmt -> generate_ml fmt bindings);
    if c_filename <> "" then
      with_open_formatter c_filename (fun fmt -> generate_local_stubs fmt bindings);
    if h_filename <> "" then
      with_open_formatter h_filename (fun fmt -> generate_header fmt bindings);
    if c_remote_filename <> "" then
      with_open_formatter c_remote_filename (fun fmt -> generate_remote_stubs fmt bindings);
  end

let () = main (module Ex_bindings.Bindings) Sys.argv
