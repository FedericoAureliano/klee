open Cil
open Feature
open Printf

module E = Errormsg
module H = Hashtbl
module L = List
module S = String

(* default entry function name *)
let entryName = ref "entry"

(* function name constants *)
let kleeMakeSymbolicName = "klee_make_symbolic"
let kleeAssumeName = "klee_assume"

(* find the function with given name if exists *)
let findFunc (f:file) (name:string) : fundec option =
  let rec search glist =
    match glist with
    | GFun (func,_) :: rest when func.svar.vname = name
                              && isFunctionType func.svar.vtype ->
      Some func
    | _ :: rest -> search rest
    | [] -> None
  in search f.globals

(* klee_make_symbolic prototype *)
let kleeMakeSymbolic: varinfo option ref = ref None
let makeKleeMakeSymbolicFunction () : varinfo =
  match !kleeMakeSymbolic with
  | Some v -> v
  | None -> begin
      let v = makeGlobalVar kleeMakeSymbolicName
          (TFun(voidType,
                Some [("addr", voidPtrType, []);
                      ("nbytes", uintType, []);
                      ("name", charPtrType, [])],
                false, [])) in
      kleeMakeSymbolic := Some v;
      v.vstorage <- Extern;
      v
    end

(* call klee_make_symbolic *)
let mkMakeSymbolic (x : varinfo) : instr =
  let p: varinfo = makeKleeMakeSymbolicFunction () in
  Call(None, Lval(var p), [(mkAddrOf (var x)); (SizeOf x.vtype);
                           (mkString x.vname)], !currentLoc)

(* klee_assume prototype *)
let kleeAssume: varinfo option ref = ref None
let makeKleeAssumeFunction () : varinfo =
  match !kleeAssume with
  | Some v -> v
  | None -> begin
      let v = makeGlobalVar kleeAssumeName
          (TFun(voidType,
                Some [("cond", intType, [])],
                false, [])) in
      kleeAssume := Some v;
      v.vstorage <- Extern;
      v
    end

(* call klee_assume *)
let mkAssume (x : varinfo) : instr =
  let p: varinfo = makeKleeAssumeFunction () in
  Call(None, Lval(var p), [Lval(var x)], !currentLoc)


let instKlee (f : file) : unit =
  match findFunc f !entryName with
  | Some entry ->
    (*Iterate over foramls of types we can handle*)
    L.map (fun formal ->
      (* make each argument a local variable *)
      let lv = makeLocalVar entry formal.vname formal.vtype in
      (* make each argument symbolic *)
      entry.sbody.bstmts <- compactStmts (
        [mkStmt
            (Instr
              [(mkMakeSymbolic(lv))])
        ] @ entry.sbody.bstmts
        );
      formal
      ) entry.sformals;
    
    setFormals entry [];

    let a = makeKleeAssumeFunction () in
    let q = makeKleeMakeSymbolicFunction () in
    E.log "Adding prototype for klee assume function %s\n" a.vname;
    f.globals <- GVarDecl (a, locUnknown) :: f.globals;
    E.log "Adding prototype for klee make symbolic function %s\n" q.vname;
    f.globals <- GVarDecl (q, locUnknown) :: f.globals;
    
  | None -> begin
      E.log "entry function %s not found\n" !entryName
    end

let feature : Feature.t = {
  fd_name = "instKlee";
  fd_enabled = false;
  fd_description = "instrument program with KLEE intrinsics";
  fd_extraopt = [("--entry",
                  Arg.String (fun s -> entryName := s),
                  " the name of the entry")];
  fd_doit = instKlee;
  fd_post_check = true;
}

(* register feature *)
let () = Feature.register feature
