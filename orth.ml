open Base

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
    | Number n -> [ Printf.sprintf "    mov rax, %d" n; "    push rax" ]
    | PrintNumber ->
        [ "    pop rax"; "    lea rbx, .putn_format[rip]"; "    call put" ]
    | PrintChar ->
        [ "    pop rax"; "    lea rbx, .putc_format[rip]"; "    call put" ]
    | _ -> assert false
  in
  List.map ~f:(fun s -> s ^ "\n") instructions |> String.concat

let output_asm_to_file tokens output_file_path =
  let open Stdio.Out_channel in
  let ch = create output_file_path in
  (* inicialização *)
  Core.printf "Inicializando\n";
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

  printf "asm\n";
  List.iter ~f:(fun (t : token) -> asm_of_token t |> output_string ch) tokens;

  (* finalização *)
  printf "finalizando\n";
  output_string ch "    pop rbp\n";
  output_string ch "    ret\n";

  close ch

(*
let tokens = tokens_of_file "./examples/add.orth"
let () = output_asm_to_file tokens "./tmp/teste.s"
 *)
