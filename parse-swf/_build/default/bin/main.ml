open Base;;


[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-69"]
[@@@ocaml.warning "-34"]
let hex_repr x = Printf.sprintf "0x%x" x


let read_all_bytes filename = 
  try
    In_channel.with_open_bin filename (fun input ->
      let content_string = In_channel.input_all input in Some (Bytes.of_string content_string)
    )
  with Sys_error msg -> 
    Stdlib.prerr_endline ("[Error] " ^ msg); 
    None
;;


module Parse_swf = struct
  type le_uint32              = Stdint.uint32
  type swf_compression_method = Simple | Deflate | Lzma | Unknown 

  type swf_sig = {
    compression_method: swf_compression_method;
    swf_version       : Stdint.uint8;
    uncompressed_size : le_uint32
  }

  let string_of_compression_method met =
    match met with
    | Simple    -> "Simple"
    | Deflate   -> "Deflate"
    | Lzma      -> "Lzma"
    | Unknown   -> "Unknown compression method, something is wrong"
  ;;

  let parse_swf_sig bytes =
      if Bytes.length bytes < 8 then
        Error (Printf.sprintf "parse_swf_sig: Input bytes too short (length %d, expected at least 8)" (Bytes.length bytes))
      else
        let compression_method =
          if (Bytes.get bytes 0) |> Char.equal 'F' &&
             (Bytes.get bytes 1) |> Char.equal 'W' &&
             (Bytes.get bytes 2) |> Char.equal 'S' then Simple
          else if Char.equal (Bytes.get bytes 0) 'C' &&
             (Bytes.get bytes 1) |> Char.equal 'W' &&
             (Bytes.get bytes 2) |> Char.equal 'S' then Deflate
          else if (Bytes.get bytes 0) |> Char.equal 'Z' &&
             (Bytes.get bytes 1) |> Char.equal 'W' &&
             (Bytes.get bytes 2) |> Char.equal 'S' then Lzma
          else Unknown
        in
          let swf_version_char  = (Bytes.get bytes 3) in
          let swf_version       = Stdint.Uint8.of_int (Char.to_int swf_version_char) in
          let uncompressed_size = Stdint.Uint32.of_bytes_little_endian bytes 4 in
      Ok { compression_method; swf_version; uncompressed_size }
  ;;

  let string_of_swf_sig swf_sig =
    let compression_method_str = string_of_compression_method swf_sig.compression_method in
    let swf_version_str        = Int.to_string (Stdint.Uint8.to_int swf_sig.swf_version) in
    let uncompressed_size_str  = Stdint.Uint32.to_string swf_sig.uncompressed_size in
    Printf.sprintf "SwfSignature: (CompressionMethod: %s; SwfVersion: %s; UncompressedSize: %s;)" compression_method_str swf_version_str uncompressed_size_str
  ;;

  let parse_le_ufixed8_p8 bytes pos =
    let n_uint16 = Stdint.Uint16.of_bytes_little_endian bytes pos in
    let n_float  = Stdint.Uint16.to_float n_uint16 in
    n_float /. 256.0
  ;;

  
  type rect = float (*CHANGE THIS WHEN WE FIND ACTUAL SPEC FOR Rect*)
  type le_uint8 = Stdint.uint8
  type le_ufixed8_p8 = float
  type movie_header = {
    frame_size : rect;
    frame_rate : le_ufixed8_p8;
    frame_count: le_uint8
  }

end




let () =
  let open Parse_swf in
    let bytes_opt = read_all_bytes "test.swf" in
      let swf_sig = match bytes_opt with
      | Some bytes -> (parse_swf_sig bytes)
      | None       ->  Error "Bytes are None!!"
      in 
      match swf_sig with
      | Ok sign   -> Stdlib.print_endline (string_of_swf_sig sign)
      | Error msg -> Stdlib.prerr_endline msg
