open Core

type token =
  | Add
  | Sub
  | Mul
  | Div
  | PrintNumber
  | PrintChar
  | Swap
  | Copy
  | Number of int
  | Id of string

type program = token list

let lex (source : string) : token list =
  let chars : char list = String.to_list source in
  let rec loop ?(tokens : token list = []) = function
    | [] -> tokens
    | c :: rest ->
        let token =
          match c with
          | '+' -> Add
          | v when Option.is_some (Int.of_string_opt (String.of_char v)) ->
              let _s = String.of_char v in
              Number 0
          | _ -> assert false
        in
        loop ~tokens:(List.append tokens [ token ]) rest

  in
  loop chars

and take_while _f _chars = ""

let tokens_of_file (input_file_pahth : string) : token list =
  lex (In_channel.read_all input_file_pahth)

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
  let open Out_channel in
  let ch = create output_file_path in
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
  output_string ch "    ret\n"

let tokens = tokens_of_file "./examples/add.orth"
let () = output_asm_to_file tokens "./tmp/teste.s"
