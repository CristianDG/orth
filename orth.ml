open Core

type token =
  | Add
  | Sub
  | Mul
  | IntDiv
  | PrintNumber
  | PrintChar
  | Swap
  | Copy
  | Number of int
  | Id of string

type program = token list

let rec lex (source : string) : token list =
  let chars : char list = String.to_list source in
  let rec loop ?(tokens : token list = []) = function
    | [] -> tokens
    | c :: rest ->
        let res =
          match c with
          | c when Char.is_whitespace c -> (None, rest)
          | '+' -> (Some Add, rest)
          | '/' -> (Some IntDiv, rest)
          | '*' -> (Some Mul, rest)
          | '-'
            when match rest with
                 | char :: _ -> Char.is_whitespace char
                 | _ -> false ->
              (Some Sub, rest)
          | n
            when equal_char '-' n
                 || Option.is_some (Int.of_string_opt (String.of_char n)) ->
              parse_int n rest
          | c -> parse_word (c :: rest)
        in

        loop
          ~tokens:
            (List.append tokens
               (match fst res with Some token -> [ token ] | None -> []))
          (snd res)
  in
  loop chars

and parse_int n rest =
  let s = String.of_char n in
  let is_number n = Option.is_some (Int.of_string_opt (String.of_char n)) in
  let number_str =
    s ^ String.of_char_list (List.take_while ~f:is_number rest)
  in
  (Some (Number (Int.of_string number_str)), List.drop_while ~f:is_number rest)

and parse_word source =
  let str = List.take_while ~f:Char.is_alpha source |> String.of_char_list in
  let consumed = List.drop_while ~f:Char.is_alpha source in
  match str with
  | "putn" -> (Some PrintNumber, consumed)
  | "putc" -> (Some PrintChar, consumed)
  | _ -> assert false

let tokens_of_file (input_file_pahth : string) : token list =
  lex (Core.In_channel.read_all input_file_pahth)

let asm_of_token (token : token) : string =
  let instructions =
    match token with
    | Add ->
        [ "    pop rax"; "    pop rbx"; "    add rax, rbx"; "    push rax" ]
    | Sub ->
        [ "    pop rax"; "    pop rbx"; "    sub rax, rbx"; "    push rax" ]
    | IntDiv -> [ "    pop rax"; "    pop rbx"; "    idiv rbx"; "    push rax" ]
    | Number n -> [ Printf.sprintf "    mov rax, %d" n; "    push rax" ]
    | PrintNumber ->
        [ "    pop rax"; "    lea rbx, .putn_format[rip]"; "    call put" ]
    | PrintChar ->
        [ "    pop rax"; "    lea rbx, .putc_format[rip]"; "    call put" ]
    | _ -> assert false
  in
  List.map ~f:(fun s -> s ^ "\n") instructions |> String.concat

let to_bytes_as_is x = List.map ~f:Char.of_int_exn x |> Bytes.of_char_list
let to_bytes_as_le x = List.rev x |> to_bytes_as_is

let output_elf_to_channel _tokens ch =
  let open Stdio.Out_channel in
  (* NOTE: o elf será em little endian *)
  let e_ident =
    to_bytes_as_is
      [ 0x7f; 0x45; 0x4c; 0x46; 0x02; 0x01; 0x01; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
  in
  let e_type = to_bytes_as_le [ 0; 2 ] (* uint16_t ET_DYN *) in
  let e_machine = to_bytes_as_le [ 0; 0x3e ] (* EM_X86_64 *) in
  let e_version = to_bytes_as_le [ 0; 0; 0; 1 ] in
  let e_entry = to_bytes_as_le [ 0; 0; 0; 0; 0x08; 0x04; 0x80; 0x78 ] in
  let e_phoff = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0; 0x40 ] in
  let e_shoff = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0; 0 ] in
  let e_flags = to_bytes_as_le [ 0; 0; 0; 0 ] in
  let e_ehsize = to_bytes_as_le [ 0; 0x40 ] in
  let e_phentsize = to_bytes_as_le [ 0; 0x38 ] in
  let e_phnum = to_bytes_as_le [ 0; 1 ] in
  let e_shentsize = to_bytes_as_le [ 0; 0 ] in
  let e_shnum = to_bytes_as_le [ 0; 0 ] in
  let e_shstrndx = to_bytes_as_le [ 0; 0 ] in
  let elf64_ehdr =
    [
      e_ident;
      e_type;
      e_machine;
      e_version;
      e_entry;
      e_phoff;
      e_shoff;
      e_flags;
      e_ehsize;
      e_phentsize;
      e_phnum;
      e_shentsize;
      e_shnum;
      e_shstrndx;
    ]
  in

  let program =
    []
    @ [ 0x48; 0xb8 + 0; 34; 0; 0; 0; 0; 0; 0; 0 ] (* mov rax, 35 *)
    @ [ 0x48; 0x50 + 0 ] (* push rax *)
    (* -- *)
    @ [ 0x48; 0xb8 + 0; 35; 0; 0; 0; 0; 0; 0; 0 ] (* mov rax, 34 *)
    @ [ 0x48; 0x50 + 0 ] (* push rax *)
    (* -- *)
    @ [ 0x48; 0x58 + 0 ] (* pop rax *)
    @ [ 0x48; 0x58 + 3 ] (* pop rbx *)
    @ [ 0x48; 0x03; List.fold ~f:Int.bit_or ~init:0 [0b11000000; 0b011000; 0b000;] ] (* add rbx, rax *)
    (* saindo *)
    @ [ 0x48; 0x89; List.fold ~f:Int.bit_or ~init:0 [0b11000000; 0b011000; 0b000;] ] (* mov rbx, rax *)
    @ [ 0xb8; 0x01; 0; 0; 0 ] (* mov eax, 1 *)
    @ [ 0xcd; 0x80 ]
    (* syscall *)
  in

  let p_type = to_bytes_as_le [ 0; 0; 0; 1 ] in
  let p_flags = to_bytes_as_le [ 0; 0; 0; 5 ] in
  let p_offset = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0; 0x78 ] in
  (* sla *)
  let p_vaddr = to_bytes_as_le [ 0; 0; 0; 0; 0x08; 0x04; 0x80; 0x78 ] in
  let p_paddr = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0; 0 ] in
  let p_filesz =
    let buf = Stdlib.Buffer.create 0 in
    Stdlib.Buffer.add_int64_le buf (Int64.of_int (List.length program));
    Stdlib.Buffer.to_bytes buf
  in
  (* encontrar o tamanho do código *)
  let p_memsz =
    let buf = Stdlib.Buffer.create 0 in
    Stdlib.Buffer.add_int64_le buf (Int64.of_int (List.length program));
    Stdlib.Buffer.to_bytes buf
  in
  let p_align = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0x10; 0x00 ] in
  let elf64_phdr =
    [ p_type; p_flags; p_offset; p_vaddr; p_paddr; p_filesz; p_memsz; p_align ]
  in

  (* Printando o header *)
  List.iter ~f:(output_bytes ch) elf64_ehdr;
  List.iter ~f:(output_bytes ch) elf64_phdr;

  output_bytes ch (to_bytes_as_is program)

let bytes_of_token (token : token) : bytes =
  let open Stdlib in
  let buf = Buffer.create 0 in
  let () =
    match token with
    | Number n ->
        (* mov eax, n *)
        Buffer.add_uint8 buf 0xb8;
        Buffer.add_int64_le buf (Int64.of_int n);
        (* push eax *)
        Buffer.add_uint8 buf (0x50 + 0)
    | _ -> assert false
  in
  Buffer.to_bytes buf

let output_asm_to_channel tokens ch =
  let open Stdio.Out_channel in
  (* inicialização *)
  List.iter ~f:(output_string ch)
    [
      ".intel_syntax noprefix\n";
      ".section .rodata\n";
      "    .putn_format: .asciz \"%d\\n\"\n";
      "    .putc_format: .asciz \"%c\\n\"\n";
      ".section .text\n";
      "put:\n";
      "    push rbp\n";
      "    mov rbp, rsp\n";
      "    sub rsp, 16\n";
      "    mov rsi, rax\n";
      "    mov rax, rbx\n";
      "    mov rdi, rax\n";
      "    call printf@PLT\n";
      "    xor rax, rax\n";
      "    nop\n";
      "    leave\n";
      "    ret\n";
      ".global main\n";
      "main:\n";
      "    push rbp\n";
      "    mov rbp, rsp\n";
    ];

  List.iter ~f:(fun (t : token) -> asm_of_token t |> output_string ch) tokens;

  (* finalização *)
  output_string ch "    pop rbp\n";
  output_string ch "    ret\n";

  close ch

let () =
  let usage = "orth -o <output> -i <input> [-asm]" in
  let input_file = ref "" in
  let output_file = ref "" in
  let asm = ref false in
  let speclist =
    [
      ("-o", Arg.Set_string output_file, "Destinação do executável");
      ("-i", Arg.Set_string input_file, "Destinação do executável");
      ("-asm", Arg.Set asm, "Destinação do executável");
    ]
  in
  let anon_fun _ = () in
  let () = Arg.parse speclist anon_fun usage in
  if equal_string !input_file "" || equal_string !output_file "" then
    print_endline "Erro :+1:"
  else
    let tokens = tokens_of_file !input_file in
    let ch = Out_channel.create !output_file in
    let output_fn =
      if !asm then
        let () = print_endline "asm" in
        output_asm_to_channel
      else
        let () = print_endline "elf" in
        output_elf_to_channel
    in
    output_fn tokens ch
(*
   let nome, ch = Stdlib.Filename.open_temp_file "sla.s" "sla.s" in
   let () = output_asm_to_channel tokens ch in
   let ich, och =
     Core_unix.open_process (sprintf "gcc -o %s %s" !output_file nome)
   in
   In_channel.close ich;
   Out_channel.close och
*)

(* *)
