def getFileStream (filePath : System.FilePath) : IO (Option IO.FS.Stream) := do
  if not (← filePath.pathExists) then
    (← IO.getStderr).putStrLn s!"File not Found"
    pure none
  else 
    let handle ← IO.FS.Handle.mk filePath IO.FS.Mode.read
    pure $ some $ IO.FS.Stream.ofHandle handle 

partial def getLines (s : IO.FS.Stream): IO (List String) := do
  let l ← s.getLine
  if (← s.isEof) then
    pure []
  else 
    pure (l::(← getLines s))

inductive FSTree where
| Dir (name : String) (content: List FSTree) : FSTree
| File (name : String) (size : Int) : FSTree
deriving Repr

abbrev FilePath := List String

def intToString (n : Int) : String :=
  toString n.toNat

partial def FSTree.toString : FSTree -> String
  | .File n s => "(" ++ n ++ " " ++ (intToString s) ++ "),"
  | .Dir n content => 
    let r := " ".intercalate (content.map toString)
    "\ndir " ++ n ++ " [ " ++ r ++ " ],"

instance : ToString FSTree := ⟨ FSTree.toString ⟩

def FSTree.add : FSTree → FilePath → FSTree → FSTree
  | .File a b, _, _ => .File a b
  | .Dir n content, [], what => .Dir n (what::content)
  | .Dir n content, targetDir::tail, what =>
    let foo := fun (t : FSTree) => 
      match t with
      | .Dir n _ => n == targetDir
      | _ => false
    let matched := content.filter foo
    let notMatched := content.filter fun t => !(foo t)
    match matched with
    | [] =>
      let newDir : FSTree := .Dir targetDir []
      let res : FSTree := newDir.add tail what
      .Dir n (res::notMatched)
    | (h::_) => 
      let res : FSTree := h.add tail what
      .Dir n (res::notMatched)

partial def FSTree.sum : FSTree -> Int × Int
  | .File _ s => ⟨s, 0⟩
  | .Dir _ c =>
    match c with 
    | [] => ⟨0, 0⟩
    | l => 
      let lRes := l.map (FSTree.sum)
      let sumFst : Int := (lRes.map (Prod.fst)).foldl (. + .) 0
      let sumSnd : Int := (lRes.map (Prod.snd)).foldl (. + .) 0
      if sumFst > 100000 then
        ⟨sumFst, sumSnd⟩
      else 
        ⟨sumFst, sumSnd + sumFst⟩

def infinity := 10000000000000000

partial def FSTree.getDims : FSTree → List Int
  | .File _ _ => []
  | .Dir n c => 
    match c with
    | [] => []
    | l =>
      let res := (l.map FSTree.getDims).foldr (List.append) []
      let s := (FSTree.Dir n c).sum.fst
      s::res

def min' (l : List Int) : Int :=
  match l with
  | [] => infinity
  | h::t => 
    let r := min' t
    if r <= h then r else h

def FSTree.solve2 (t : FSTree) (tar : Int) : Int := 
  let l := t.getDims.filter (. >= tar) 
  min' l

def List.takeMost {α : Type} (l : List α) := l.take (l.length - 1)

def parseString (s : String) (tp : FSTree × FilePath) : (FSTree × FilePath) := 
  let spl : List String := s.splitOn " " 
  match spl with
  | "$"::tail =>
    match tail with 
    | "ls"::_ => ⟨tp.1, tp.2⟩
    | "cd"::tail' => 
      match tail' with
      | "/"::_ => ⟨tp.1, []⟩
      | ".."::_ => 
        match tp.2 with 
        | [] => ⟨tp.1, []⟩ 
        | p => ⟨ tp.1, p.takeMost ⟩
      | d::_ => ⟨ tp.1, tp.2 ++ [d] ⟩
      | _ => ⟨ tp.1, tp.2 ⟩ 
    | _ => ⟨ tp.1, tp.2 ⟩ 
  | f::tail => 
    match tail with
    | [] => ⟨ tp.1, tp.2 ⟩ 
    | name::_ =>  
      match f.toInt? with
      | some size => 
        ⟨ tp.1.add tp.2 (.File name size), tp.2 ⟩ 
      | none => ⟨ tp.1, tp.2 ⟩
  | _ => ⟨ tp.1, tp.2 ⟩ 


def parseString' (tp : FSTree × FilePath) (s : String): (FSTree × FilePath) := parseString s tp

def FSTree.solve1 (t : FSTree) := t.sum.snd

def main : IO Unit := do
  let fs ← getFileStream "./input"
  match fs with
  | none => pure ()
  | some stream => 
    let lines : List String := (← getLines stream).map (String.trim)
    let t := (lines.foldl parseString' (.Dir "/" [], [])).1
    let s1 := t.solve1
    IO.println s1
    let target := t.sum.fst - 40000000 
    let s2 := t.solve2 target
    IO.println s2
