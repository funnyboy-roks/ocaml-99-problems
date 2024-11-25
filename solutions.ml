(* Problem 1: Tail of a List *)
let rec last a = match a with
    | [] -> None
    | [x] -> Some x
    | _ :: rest -> last rest
;;

(* Problem 2: Last Two Elements of a List *)
let rec last_two a = match a with
    | [] | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: rest -> last_two rest
;;

(* Problem 3: N'th Element of a List *)
let rec nth l n = match (l, n) with
    | ([], n) -> raise (Failure "nth")
    | (x :: _, 0) -> x
    | (x :: rest, n) -> nth rest (n - 1)
;;

(* Problem 4: Length of a list *)
let rec length l = match l with
    | [] -> 0
    | _ :: rest -> 1 + length rest
;;

(* Problem 5: Reverse a List *)
let rec reverse l = match l with
    | [] -> []
    | x :: rest -> reverse rest @ [x]
;;

(* Problem 6: Palendrome *)
let rec palendrome l = l = reverse l;;

(* Problem 7: Flatten a List *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec flatten node = match node with
    | [] -> []
    | x :: rest -> match x with
        | One y -> [y] @ flatten rest
        | Many l -> flatten l @ flatten rest
;;

(* Problem 8: Eliminate Duplicates *)
let rec compress list = match list with
    | [] -> []
    | [x] -> [x]
    | x :: y :: rest -> match x = y with
        | true -> compress ([x] @ rest)
        | false -> [x] @ compress ([y] @ rest)
;;

(* Problem 9: Pack Consecutive Duplicates *)
let rec pack list =
    let rec split_cons (build, list) = match (build, list) with
        | (build, []) -> (build, [])
        | ([], x :: rest) -> split_cons ([x], rest)
        | (x :: _, y :: rest) -> match x = y with
            | true -> split_cons (build @ [y], rest)
            | false -> (build, list)
    in match list with 
        | [] -> []
        | list -> match split_cons ([], list) with
            | (build, []) -> [build]
            | (build, rest) -> [build] @ pack rest
;;

(* Problem 10: Run-Length Encoding *)
let rec encode list =
    let rec stuff packed = match (packed) with
        | [] -> []
        | x :: rest -> [(length x, nth x 0)] @ stuff rest
    in stuff (pack list)
;;

(* Problem 11: Modified Run-Length Encoding *)
type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec encode list =
    let rec stuff packed = match packed with
        | [] -> []
        | x :: rest -> [match length x with
            | 1 -> One (nth x 0)
            | n -> Many (n, nth x 0)
        ] @ stuff rest
    in stuff (pack list)
;;

(* Problem 12: Decode a Run-Length Encoded List *)
let rec decode (list: 'a rle list): 'a list =
    let rec expand (rle: 'a rle): 'a list = match rle with
        | One x -> [x]
        | Many (0, _) -> []
        | Many (n, x) -> [x] @ expand (Many (n - 1, x))
    in
    match list with
    | [] -> []
    | x :: rest -> expand x @ decode rest
;;

(* Problem 13: Run-Length Encoding of a List (Direct Solution) *)
let rec encode (list: 'a list): 'a rle list =
    let rec chop ((prev, rest): 'a rle option * 'a list): 'a rle option * 'a list = match (prev, rest) with
        | (_, []) -> (prev, [])
        | (None, x :: rest) -> chop (Some (One x), rest)
        | (Some (One x), y :: rest) -> if x = y
            then chop (Some (Many (2, x)), rest)
            else (prev, [y] @ rest)
        | (Some (Many (n, x)), y :: rest) -> if x = y
            then chop (Some (Many (n + 1, x)), rest)
            else (prev, [y] @ rest)
    in match list with 
        | [] -> []
        | list -> match chop (None, list) with
            | (None, _) -> raise (Failure "nth")
            | (Some x, []) -> [x]
            | (Some x, rest) -> [x] @ encode rest
;;

(* Problem 14: Duplicate the Elements of a List *)
let rec duplicate list = match list with
    | [] -> []
    | x :: rest -> [x; x] @ duplicate rest
;;

(* Problem 14: Replicate the Elements of a List a Given Number of Times  *)
let rec replicate list n =
    let rec clone elt n = match n with | 0 -> [] | n -> [elt] @ clone elt (n - 1)
    in match list with
        | [] -> []
        | x :: rest -> clone x n @ replicate rest n
;;

(* Problem 15: Drop Every N'th Element From a List *)
let rec drop list freq =
    let rec inner list n = match (list, n) with
        | ([], _) -> []
        | (_ :: rest, 1) -> inner rest freq
        | (x :: rest, n) -> [x] @ inner rest (n - 1)
    in inner list freq
;;

(* Problem 16: Split a List Into Two Parts; The Length of the First Part Is Given  *)
let rec split_at list n: 'a list * 'a list = match (list, n) with
    | (list, 0) -> ([], list)
    | ([], n) -> ([], [])
    | (x :: list, n) -> ([x] @ fst (split_at list (n - 1)), snd (split_at list (n - 1)))
;;

(* Problem 16:  Extract a Slice From a List *)
let rec slice list i k =
    let rec split_at list n: 'a list * 'a list = match (list, n) with
        | (list, 0) -> ([], list)
        | ([], n) -> ([], [])
        | (x :: list, n) -> ([x] @ fst (split_at list (n - 1)), snd (split_at list (n - 1)))
    in match (list, i, k) with
        | ([], n, k) -> []
        | (list, 0, k) -> fst (split_at list (k + 1))
        | (x :: rest, n, k) -> slice rest (n - 1) (k - 1)
;;

(* Problem 16:  Rotate a List N Places to the Left  *)
let rec rol list k =
    let rec split_at list n: 'a list * 'a list = match (list, n) with
        | (list, 0) -> ([], list)
        | ([], n) -> ([], [])
        | (x :: list, n) -> ([x] @ fst (split_at list (n - 1)), snd (split_at list (n - 1)))
    in snd (split_at list k) @ fst (split_at list k)
;;

(* Problem 17: Remove the K'th Element From a List *)
let rec del k list = match (list, k) with
    | ([], _) -> []
    | (x :: rest, 0) -> rest
    | (x :: rest, n) -> [x] @ del (n - 1) rest
;;

(* Problem 18: Insert an Element at a Given Position Into a List *)
let rec insert list elt n = match (list, n) with
    | ([], _) -> [elt]
    | (x :: rest, 1) -> [x; elt] @ rest
    | (x :: rest, n) -> [x] @ insert rest elt (n - 1)
;;
insert ["a"; "b"; "c"; "d"] "alfa" 1;;

(* Problem 19: Create a List Containing All Integers Within a Given Range *)
let rec range min max = if min = max then [min]
    else if min < max then [min] @ range (min + 1) max
    else range min (max + 1) @ [max]
;;
range 9 4;;

(* Problem 20: Extract a Given Number of Randomly Selected Elements From a List *)
let rand_select list n =
    let rec pick n length = if n = 0 then []
    else [nth list (Random.int length)] @ pick (n - 1) length
    in pick n (length list)
;;

(* Problem 21: Lotto: Draw N Different Random Numbers From the Set 1..=M *)
let rec lotto_select n max = if n = 0 then []
    else [Random.int max] @ lotto_select (n - 1) max
;;
