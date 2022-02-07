(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml and [refmt] for reason *)

open Import

type error =
  | Unsupported_syntax of Document.Syntax.t option
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

val message : error -> string

val run : Document.t -> (TextEdit.t list, error) result Fiber.t
