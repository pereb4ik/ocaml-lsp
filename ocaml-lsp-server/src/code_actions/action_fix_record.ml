open Import
open Fiber.O

(* Actually this file is edited copypaste of add_rec *)
let action_title = "Change all ',' to ';'"

let code_action_replace_switch uri diagnostics doc (locs : DiagnosticRelatedInformation.t list) =
  let edit =
    let lcs = List.map ~f:(fun (dri : DiagnosticRelatedInformation.t) -> dri.location.range ) locs in
    (* ',' -> ';' *)
    let textedits : TextEdit.t list = List.map ~f:(fun r -> TextEdit.{ range = r; newText = ";" }) lcs in
    let edits = List.map ~f:(fun (te : TextEdit.t) -> `TextEdit te) textedits in
    let version = Document.version doc in
    let textDocument =
      OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
    in
    let edit =
      TextDocumentEdit.create ~textDocument ~edits
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  CodeAction.create ~diagnostics ~title:action_title
    ~kind:CodeActionKind.QuickFix ~edit ~isPreferred:false ()

let code_action doc (params : CodeActionParams.t) =
  let m_diagnostic =
    List.find params.context.diagnostics ~f:(fun d ->
        let is_unbound () =
          String.is_prefix d.Diagnostic.message ~prefix:"Warning 73"
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
    let+ locs = Fiber.return d.relatedInformation in
    Option.map locs ~f:(code_action_replace_switch params.textDocument.uri [ d ] doc)

let t = { Code_action.kind = QuickFix; run = code_action }