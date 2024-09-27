open Import

(* Actually this file is edited copypaste of add_rec *)
let action_title = "Replace 'struct end' with '{ }'"

let code_action_replace_struct uri diagnostics doc =
  let edit =
    let d = List.hd diagnostics in
    let loc = d.Diagnostic.range in
    let loc1 = { loc with end_ = {loc.start with character = loc.start.character + 6}} in
    let textedit1 : TextEdit.t =
      { range = loc1; newText = "{" }
    in
    let loc2 = { loc with start = {loc.end_ with character = loc.end_.character - 3}} in
    let textedit2 : TextEdit.t =
      { range = loc2; newText = "}" }
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
          String.is_suffix d.Diagnostic.message ~suffix:"use '{ }'."
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
    (* We already know location of structure from parser warning throwing *)
    Fiber.return (Some (code_action_replace_struct params.textDocument.uri [ d ] doc))

let t = { Code_action.kind = QuickFix; run = code_action }
