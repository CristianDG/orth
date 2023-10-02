open Core

type token =
  | Add
  | Sub
  | Mul
  | Eq
  | IntDiv
  | PrintNumber
  | PrintChar
  | Swap
  | Dup
  | Dup2
  | If
  | Else
  | Fi
  | While
  | Done
  | Number of int

type program = token list

let rec lex (source : string) : token list =
  let chars : char list = String.to_list source in
  let rec loop ?(tokens : token list = []) = function
    | [] -> tokens
    | c :: rest ->
        let res =
          match c with
          | '#' ->
              (None, List.drop_while ~f:(fun c -> not (equal_char '\n' c)) rest)
          | '+' -> (Some Add, rest)
          | '/' -> (Some IntDiv, rest)
          | '*' -> (Some Mul, rest)
          | '-'
            when match rest with
                 | char :: _ -> Char.is_whitespace char
                 | _ -> false ->
              (Some Sub, rest)
          | n when equal_char '-' n || Char.is_digit n -> parse_int n rest
          | c when Char.is_whitespace c -> (None, rest)
          | c when equal_char '\'' c -> parse_char rest
          | c -> parse_word (c :: rest)
        in

        loop
          ~tokens:
            (List.append tokens
               (match fst res with Some token -> [ token ] | None -> []))
          (snd res)
  in
  loop chars

and parse_char rest =
  let consumed, rest = List.split_while ~f:(fun c -> not (equal_char c '\'')) rest in
  let chr = if List.length consumed > 1 then 
    match consumed with
    | [ '\\'; 'n' ] -> '\n'
    | [ '\\'; '\'' ] -> '\''
    | [ '\\'; '\\' ] -> '\\'
    | _ -> '?'
  else List.hd_exn consumed in
  (Some (Number (Char.to_int chr)), List.tl_exn rest)

and parse_int n rest =
  let s = String.of_char n in
  let is_number n = Option.is_some (Int.of_string_opt (String.of_char n)) in
  let number_str =
    s ^ String.of_char_list (List.take_while ~f:is_number rest)
  in
  (Some (Number (Int.of_string number_str)), List.drop_while ~f:is_number rest)

and parse_word source =
  let str = List.take_while ~f:Char.is_alphanum source |> String.of_char_list in
  let consumed = List.drop_while ~f:Char.is_alphanum source in
  match str with
  | "swap" -> (Some Swap, consumed)
  | "dup2" -> (Some Dup2, consumed)
  | "dup" -> (Some Dup, consumed)
  | "putn" -> (Some PrintNumber, consumed)
  | "putc" -> (Some PrintChar, consumed)
  | "while" -> (Some While, consumed)
  | "done" -> (Some Done, consumed)
  | "if" -> (Some If, consumed)
  | "fi" -> (Some Fi, consumed)
  | "else" -> (Some Else, consumed)
  | _ ->
      print_endline str;
      assert false

let tokens_of_file (input_file_pahth : string) : token list =
  lex (Core.In_channel.read_all input_file_pahth)

let ( |. ) : int -> int -> int = Int.bit_or
let ( & ) : int -> int -> int = Int.bit_and
let ( << ) : int -> int -> int = Int.shift_left
let ( >> ) : int -> int -> int = Int.shift_right
let to_bytes_as_is x = List.map ~f:Char.of_int_exn x |> Bytes.of_char_list
let to_bytes_as_le x = List.rev x |> to_bytes_as_is

let bytes_of_token (token : token) : bytes =
  let open Stdlib in
  let buf = Buffer.create 0 in
  let () =
    match token with
    | Number n ->
        (* mov eax, n *)
        Buffer.add_uint8 buf 0x48;
        Buffer.add_uint8 buf 0xb8;
        Buffer.add_int64_le buf (Int64.of_int n);
        (* push eax *)
        Buffer.add_uint8 buf (0x50 + 0)
    | Swap ->
        (* pop rax *)
        Buffer.add_uint8 buf 0x58;
        (* pop rbx *)
        Buffer.add_uint8 buf 0x5b;
        (* push rax *)
        Buffer.add_uint8 buf 0x50;
        (* push rbx *)
        Buffer.add_uint8 buf 0x53
    | Dup ->
        (* pop rax *)
        Buffer.add_uint8 buf 0x58;
        (* push rax *)
        Buffer.add_uint8 buf 0x50;
        (* push rax *)
        Buffer.add_uint8 buf 0x50
    | Dup2 ->
        (* pop rax *)
        Buffer.add_uint8 buf 0x58;
        (* pop rbx *)
        Buffer.add_uint8 buf 0x5b;
        (* push rbx *)
        Buffer.add_uint8 buf 0x53;
        (* push rax *)
        Buffer.add_uint8 buf 0x50;
        (* push rbx *)
        Buffer.add_uint8 buf 0x53;
        (* push rax *)
        Buffer.add_uint8 buf 0x50
    | Sub -> assert false
    | Mul -> assert false
    | IntDiv -> assert false
    | Eq -> assert false
    | Add ->
        (* pop eax *)
        Buffer.add_uint8 buf 0x58;
        (* pop ebx *)
        Buffer.add_uint8 buf (0x58 + 3);

        (* add eax, ebx *)
        Buffer.add_uint8 buf 0x48;
        Buffer.add_uint8 buf 0x03;
        Buffer.add_uint8 buf (0b11 << 6 |. (0b000 << 3) |. 0b011);
        (* push eax *)
        Buffer.add_uint8 buf (0x50 + 0)
    | If ->
        (* pop eax *)
        Buffer.add_uint8 buf 0x58;

        (* cmp eax, 0 *)
        Buffer.add_uint8 buf 0x48;
        Buffer.add_uint8 buf 0x83;
        Buffer.add_uint8 buf 0b11_111_000;
        Buffer.add_uint8 buf 0x00;

        (* jge ... *)
        Buffer.add_uint8 buf 0x7d;
        Buffer.add_uint8 buf 0x04;

        (* jump ... *)
        Buffer.add_uint8 buf 0xeb;
        Buffer.add_uint8 buf 0x00;
        Buffer.add_uint8 buf 0x00;
        Buffer.add_uint8 buf 0x00;
        Buffer.add_uint8 buf 0x00
    | Else ->
        Buffer.add_uint8 buf 0xeb;
        Buffer.add_uint8 buf 0x00;
        Buffer.add_uint8 buf 0x00;
        Buffer.add_uint8 buf 0x00;
        Buffer.add_uint8 buf 0x00
    | Fi -> ()
    | While ->
        (* pop eax *)
        Buffer.add_uint8 buf 0x58;

        (* cmp eax, 0 *)
        Buffer.add_uint8 buf 0x48;
        Buffer.add_uint8 buf 0x83;
        Buffer.add_uint8 buf 0b11_111_000;
        Buffer.add_uint8 buf 0x00;

        (* push eax *)
        Buffer.add_uint8 buf (0x50 + 0);

        (* jle ... *)
        Buffer.add_uint8 buf 0x7e;
        Buffer.add_uint8 buf 0x00
    | Done ->
        Buffer.add_uint8 buf 0xeb;
        Buffer.add_uint8 buf 0x00
    | PrintChar ->
        (* mov    rax, 1 *)
        Buffer.add_string buf "\x48\xc7\xc0\x01\x00\x00\x00";
        (* mov    edi, 1 *)
        Buffer.add_string buf "\x48\xc7\xc7\x01\x00\x00\x00";
        (* mov    rsi,rsp *)
        Buffer.add_string buf "\x48\x89\xe6";
        (* mov    rdx, 1 *)
        Buffer.add_string buf "\x48\xc7\xc2\x01\x00\x00\x00";
        (* syscall *)
        Buffer.add_string buf "\x0f\x05";
        Buffer.add_uint8 buf 0x58
    | PrintNumber ->
        (* mov    r8, 0 *)
        Buffer.add_string buf "\x49\xc7\xc0\x00\x00\x00\x00";
        (* pop    rcx *)
        Buffer.add_string buf "\x59";
        (* loop: *)
        Buffer.add_string buf "\x49\xff\xc0";
        Buffer.add_string buf "\x48\xc7\xc2\x00\x00\x00\x00";
        Buffer.add_string buf "\x48\x89\xc8";
        Buffer.add_string buf "\x48\xc7\xc3\x0a\x00\x00\x00";
        Buffer.add_string buf "\x48\xf7\xf3";
        Buffer.add_string buf "\x48\x89\xc1";
        Buffer.add_string buf "\x48\x83\xc2\x30";
        Buffer.add_string buf "\x52";
        Buffer.add_string buf "\x48\x83\xf9\x00";
        Buffer.add_string buf "\x75\xdb";

        (* jne loop *)
        Buffer.add_string buf "\x48\xc7\xc0\x01\x00\x00\x00";
        Buffer.add_string buf "\x48\xc7\xc7\x01\x00\x00\x00";
        Buffer.add_string buf "\x48\x89\xe6";
        Buffer.add_string buf "\x48\xc7\xc2\x01\x00\x00\x00";
        Buffer.add_string buf "\x0f\x05";
        Buffer.add_string buf "\x58";
        Buffer.add_string buf "\x49\xff\xc8";
        Buffer.add_string buf "\x75\xe0"
  in
  Buffer.to_bytes buf

let output_elf_to_channel tokens ch =
  let open Stdio.Out_channel in
  let module Buffer = Stdlib.Buffer in
  let program_buf =
    let if_stack : int Base.Stack.t = Stack.create () in
    let while_stack : int Base.Stack.t = Stack.create () in
    let buf = ref (Buffer.create 0) in
    let add_bytes_of_token buf token =
      Buffer.add_bytes !buf (bytes_of_token token);
      let length = Buffer.length !buf in
      let set buf idx byte =
        let new_buf = Buffer.create 0 in
        let new_buf_bytes = Buffer.to_bytes !buf in
        Bytes.set new_buf_bytes idx (Char.of_int_exn byte);
        Buffer.add_bytes new_buf new_buf_bytes;
        new_buf
      in
      match token with
      | While -> Stack.push while_stack (length - 1)
      | Done ->
          let prev_length = Stack.pop_exn while_stack in
          let delta = length - prev_length in
          buf := set buf prev_length delta;
          buf := set buf (length - 1) (0xff - delta)
      | If -> Stack.push if_stack (length - 1)
      | Else ->
          (* TODO: pegar o último item do if_stack, subtrair com Buffer.length buf e adicionar o valor para conseguir fzr o jnz, além disso, deixar espaço de um jump antes de tudo *)
          let prev_length = Stack.pop_exn if_stack in
          Stack.push if_stack (length - 1);
          let delta = length - prev_length in
          buf := set buf (prev_length - 0) (delta >> 24);
          buf := set buf (prev_length - 1) ((delta & 0x00ff0000) >> 16);
          buf := set buf (prev_length - 2) ((delta & 0x0000ff00) >> 8);
          buf := set buf (prev_length - 3) (delta & 0x000000ff)
      | Fi ->
          (* TODO: pegar o último item do if_stack, subtrair com Buffer.length buf e adicionar o valor para conseguir fzr o jump *)
          let prev_length = Stack.pop_exn if_stack in
          let delta = length - prev_length in
          buf := set buf (prev_length - 0) (delta >> 24);
          buf := set buf (prev_length - 1) ((delta & 0x00ff0000) >> 16);
          buf := set buf (prev_length - 2) ((delta & 0x0000ff00) >> 8);
          buf := set buf (prev_length - 3) (delta & 0x000000ff)
      | _ -> ()
    in

    List.iter ~f:(add_bytes_of_token buf) tokens;
    (*
    movq $0x3C, %rax
    movq $0x00, %rbx
    syscall
 *)
    (* saindo *)
    Buffer.add_bytes !buf
      (to_bytes_as_is
         ([ 0x58 + 7 ] (* pop rbx *)
         @ [ 0xb8; 0x3c; 0; 0; 0 ] (* mov eax, 1 (exit) *)
         @ [ 0x0f; 0x05 ]));
    buf.contents
  in
  let entry_offset = 0x78 in
  let entry_addr = 0x08048000 + entry_offset in
  let program = Buffer.to_bytes program_buf in
  let program_size = Buffer.length program_buf in
  (* elf64_ehdr *)
  let e_ident =
    to_bytes_as_is
      [ 0x7f; 0x45; 0x4c; 0x46; 0x02; 0x01; 0x01; 0; 0; 0; 0; 0; 0; 0; 0; 0 ]
  in
  let e_type = to_bytes_as_le [ 0; 2 ] in
  let e_machine = to_bytes_as_le [ 0; 0x3e ] in
  let e_version = to_bytes_as_le [ 0; 0; 0; 1 ] in
  let e_entry =
    to_bytes_as_le
      [
        0;
        0;
        0;
        0;
        entry_addr >> 24 & 0xFF;
        entry_addr >> 16 & 0xFF;
        entry_addr >> 8 & 0xFF;
        entry_offset & 0xFF;
      ]
  in
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

  (* elf64_phdr *)
  let p_type = to_bytes_as_le [ 0; 0; 0; 1 ] in
  let p_flags = to_bytes_as_le [ 0; 0; 0; 5 ] in
  let p_offset = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0; entry_offset ] in
  let p_vaddr =
    to_bytes_as_le
      [
        0;
        0;
        0;
        0;
        entry_addr >> 24 & 0xFF;
        entry_addr >> 16 & 0xFF;
        entry_addr >> 8 & 0xFF;
        entry_offset & 0xFF;
      ]
  in
  let p_paddr = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0; 0 ] in
  let p_filesz =
    let buf = Buffer.create 0 in
    Buffer.add_int64_le buf (Int64.of_int program_size);
    Buffer.to_bytes buf
  in
  (* encontrar o tamanho do código *)
  let p_memsz =
    let buf = Buffer.create 0 in
    Buffer.add_int64_le buf (Int64.of_int program_size);
    Buffer.to_bytes buf
  in
  let p_align = to_bytes_as_le [ 0; 0; 0; 0; 0; 0; 0x10; 0x00 ] in
  let elf64_phdr =
    [ p_type; p_flags; p_offset; p_vaddr; p_paddr; p_filesz; p_memsz; p_align ]
  in

  (* Printando o header *)
  List.iter ~f:(output_bytes ch) elf64_ehdr;
  List.iter ~f:(output_bytes ch) elf64_phdr;
  output_bytes ch program

let () =
  let usage = "orth -o <output> -i <input>" in
  let input_file = ref "" in
  let output_file = ref "" in
  let speclist =
    [
      ("-o", Arg.Set_string output_file, "Destinação do executável");
      ("-i", Arg.Set_string input_file, "Destinação do executável");
    ]
  in
  let anon_fun _ = () in
  let () = Arg.parse speclist anon_fun usage in
  if equal_string !input_file "" || equal_string !output_file "" then
    print_endline "Erro :+1:"
  else
    let tokens = tokens_of_file !input_file in
    let ch = Out_channel.create !output_file in
    let _ = Core_unix.open_process (sprintf "chmod +x %s" !output_file) in
    output_elf_to_channel tokens ch

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
