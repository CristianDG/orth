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
  | If
  | Else
  | Fi
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

        (* jle ... *)
        Buffer.add_uint8 buf 0x7e;
        Buffer.add_uint8 buf 0x00
    | Else -> 
        Buffer.add_uint8 buf 0xeb;
        Buffer.add_uint8 buf 0x00
    | Fi -> ()
    | _ -> assert false
  in
  Buffer.to_bytes buf

let output_elf_to_channel _tokens ch =
  let open Stdio.Out_channel in
  let module Buffer = Stdlib.Buffer in
  (* TODO: função que printa inteiros *)
  let print_int_proc =
    let buf = Buffer.create 20 in
    Buffer.to_bytes buf
  in

  (*

  10 0 = if
    69 putn
  else
    42 putn
  fi

  [
    if 0x69
    else 0x74
    fi 0x77
  ]
   *)
  (* TODO: criar o map de nome_fn -> addr *)
  let program_buf =
    let if_stack : int Base.Stack.t = Stack.create () in
    let buf = ref (Buffer.create 0) in
    let add_bytes_of_token buf token =
      Buffer.add_bytes !buf (bytes_of_token token);
      let length = Buffer.length !buf in
      let set buf idx byte =
        let new_buf = Buffer.create 0 in
        let new_buf_bytes = Buffer.to_bytes !buf in
        let () = Bytes.set new_buf_bytes idx (Char.of_int_exn (byte - 1)) in
        let () = Buffer.add_bytes new_buf new_buf_bytes in
        new_buf
      in
      match token with
      | If -> Stack.push if_stack (length - 1)
      | Else ->
          (* TODO: pegar o último item do if_stack, subtrair com Buffer.length buf e adicionar o valor para conseguir fzr o jnz,
              além disso, deixar espaço de um jump antes de tudo *)
          let prev_length = Stack.pop_exn if_stack in
          let delta = length - prev_length in
          buf := set buf prev_length delta;
          printf "ELSE %d\n" delta;
          Stack.push if_stack (length - 1)
      | Fi ->
          (* TODO: pegar o último item do if_stack, subtrair com Buffer.length buf e adicionar o valor para conseguir fzr o jump *)
          let prev_length = Stack.pop_exn if_stack in
          let delta = length - prev_length in
          printf "FI %d\n" delta;
          buf := set buf prev_length delta;
      | _ -> ()
    in

    Buffer.add_bytes !buf print_int_proc;
    add_bytes_of_token buf (Number 33);
    add_bytes_of_token buf (Number 0);
    add_bytes_of_token buf If;
    add_bytes_of_token buf (Number 34);
    add_bytes_of_token buf Else;
    add_bytes_of_token buf (Number 35);
    add_bytes_of_token buf Fi;
    add_bytes_of_token buf Add;


    (* saindo *)
    Buffer.add_bytes !buf
      (to_bytes_as_is
         ([ 0x58 + 3 ] (* pop rbx *)
         @ [ 0xb8; 0x01; 0; 0; 0 ] (* mov eax, 1 (exit) *)
         @ [ 0xcd; 0x80 ]));
    buf.contents
  in
  let program = Buffer.to_bytes program_buf (* syscall *) in
  let program_size = Buffer.length program_buf in
  let entry_offset = 0x78 + Bytes.length print_int_proc in
  let entry_addr = 0x08048000 |. entry_offset in
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
    let buf = Stdlib.Buffer.create 0 in
    Stdlib.Buffer.add_int64_le buf (Int64.of_int program_size);
    Stdlib.Buffer.to_bytes buf
  in
  (* encontrar o tamanho do código *)
  let p_memsz =
    let buf = Stdlib.Buffer.create 0 in
    Stdlib.Buffer.add_int64_le buf (Int64.of_int program_size);
    Stdlib.Buffer.to_bytes buf
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
