open Core

type token 
  = Add
  | Sub
  | Mul
  | Div
  | Print
  | Swap
  | Number of int
  | Id of string


type program = token list


let asm_of_token (token: token): string =
  let instructions = match token with
  | Add -> ["pop rax"; "pop rbx"; "add rax, rbx"; "push rax" ]
  | Number n -> [ Printf.sprintf "mov rax, %d" n; "push rax" ]
  | _ -> assert false in
  List.map ~f:(fun s -> "    "^s^"\n") instructions |> String.concat

let output_asm_to_file output_file_path =
  let open Out_channel in
  let ch = create output_file_path  in 
  output_string ch ".intel_syntax noprefix\n";
  output_string ch ".section .text\n";
  output_string ch ".global main\n";
  output_string ch "main:\n";
  output_string ch "    push rbp\n";
  output_string ch "    mov rbp, rsp\n";

  output_string ch (asm_of_token (Number 34));
  output_string ch (asm_of_token (Number 35));
  output_string ch (asm_of_token Add);

  output_string ch "    mov rsp, rbp\n";
  output_string ch "    pop rbp\n";
  output_string ch "    ret\n"

let () = output_asm_to_file "./tmp/teste.s"
