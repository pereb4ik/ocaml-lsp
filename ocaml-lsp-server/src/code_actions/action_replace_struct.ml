open Import
open Fiber.O

(* Actually this file is edited copypaste of add_rec *)
let action_title = "Replace reason-style defenition with 'struct end'"

let code_action_replace_struct uri diagnostics doc (loc : Range.t) =
  let edit =
    let loc1 = { loc with end_ = {loc.start with character = loc.start.character + 1}} in
    (* Maybe here shoud place space before and after newText?*)
    let textedit1 : TextEdit.t =
      { range = loc1; newText = "struct " }
    in
    let loc2 = { loc with start = {loc.end_ with character = loc.end_.character - 1}} in
    let textedit2 : TextEdit.t =
      { range = loc2; newText = " end" }
    in
    let version = Document.version doc in
    let textDocument =
      OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
    in
    let edit =
      TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit1; `TextEdit textedit2 ]
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  CodeAction.create ~diagnostics ~title:action_title
    ~kind:CodeActionKind.QuickFix ~edit ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let m_diagnostic =
    List.find params.context.diagnostics ~f:(fun d ->
        let is_unbound () =
          String.is_prefix d.Diagnostic.message ~prefix:"Warning 71"
        and in_range () =
          match Position.compare_inclusion params.range.start d.range with
          | `Outside _ -> false
          | `Inside -> true
        in
        in_range () && is_unbound ())
  in
  match m_diagnostic with
  | None -> Fiber.return None
  | Some d ->
    (*We already know location of structure defenition from parser warning throwing*)
    let+ loc = Fiber.return (Some d.range) in
    Option.map loc ~f:(code_action_replace_struct params.textDocument.uri [ d ] doc)

let t = { Code_action.kind = QuickFix; run = code_action }
