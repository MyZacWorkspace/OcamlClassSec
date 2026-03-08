(* ================================================================
   Exercise 5: Calculator Parser -- Main Driver
   ================================================================

   This file reads expressions from built-in examples, parses them
   with the Menhir-generated parser, and prints + evaluates the
   resulting AST.

   TODO: Implement [string_of_op], [string_of_expr], and [eval].
   The parse + main driver code is provided.

   Run with:  dune exec modules/module0-warmup/exercises/calculator-parser/starter/main.exe
   ================================================================ *)

open Ast

(* ----------------------------------------------------------------
   Part 1: Pretty-Printing
   ---------------------------------------------------------------- *)

(** [string_of_op op] returns "+", "-", "*", or "/". *)
let string_of_op (_o : op) : string =
  (* EXERCISE: pattern match on Add, Sub, Mul, Div *)
  match _o with 
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"

[@@warning "-32"]

(** [string_of_expr e] returns a fully parenthesized string.
    Examples:
      Num 3            --> "3"
      Var "x"          --> "x"
      Neg (Num 5)      --> "(- 5)"
      BinOp(Add, Num 1, Num 2)  --> "(1 + 2)" *)
let rec string_of_expr (_e : expr) : string =
  (* EXERCISE: pattern match on Num, Var, Neg, BinOp
     Hint: add [rec] when ready *)
  match _e with
  | Ast.Num n -> string_of_int n
  | Ast.Var v -> v
  | Ast.Neg g -> "(- " ^ string_of_expr g ^ ")"
  | Ast.BinOp (o , e1, e2) -> 
    let es1 = string_of_expr e1 in 
    let es2 = string_of_expr e2 in 
      "(" ^ es1 ^ string_of_op o ^ es2 ^ ")"

(* ----------------------------------------------------------------
   Part 2: Evaluation
   ---------------------------------------------------------------- *)

(** [eval e] evaluates [e] if it contains no variables.
    Returns [Some n] on success, [None] if a Var is encountered.
    Division by zero returns [None]. *)

let rec eval (_e : expr) : int option =
  (* EXERCISE: handle Num, Var, Neg, and BinOp.
     For BinOp: evaluate both sides; if both are Some, compute.
     For Div: check for zero denominator.
     Hint: add [rec] when ready *)
    match _e with
    | Ast.Num n -> Some n
    | Ast.Var _ -> None
    | Ast.Neg neg -> 
      begin
        match eval neg with
        | Some n -> Some (-1 *n)
        | None -> None
      end
    | Ast.BinOp (o , e1 , e2) -> 
      match eval e1, eval e2 with
      | None, None -> None
      | Some _ , None -> None
      | None , Some _ -> None
      | Some a, Some b -> 
        match o with
        | Ast.Add -> Some (a + b)
        | Ast.Sub -> Some (a - b)
        | Ast.Mul -> Some (a * b)
        | Ast.Div -> if b > 0 || b < 0 then Some (a / b) else None

(* ----------------------------------------------------------------
   Provided: Parse helper and main driver
   ---------------------------------------------------------------- *)

(** [parse_string s] parses the string [s] into an expr. *)
let parse_string (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.token lexbuf

(** [test_expr s] parses, prints, and evaluates the expression. *)
let test_expr (s : string) : unit =
  Printf.printf "Input:  %s\n" s;
  let e = parse_string s in
  Printf.printf "AST:    %s\n" (string_of_expr e);
  (match eval e with
   | Some n -> Printf.printf "Result: %d\n" n
   | None   -> Printf.printf "Result: <cannot evaluate>\n");
  Printf.printf "\n"

let () =
  Printf.printf "=== Exercise 5: Calculator Parser ===\n\n";

  test_expr "42";
  test_expr "1 + 2";
  test_expr "3 * 4 + 5";
  test_expr "(3 + 4) * 5";
  test_expr "10 - 3 - 2";
  test_expr "-7";
  test_expr "x + 1";

  Printf.printf "Done!\n"
