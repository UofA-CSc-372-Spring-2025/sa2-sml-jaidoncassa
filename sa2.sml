(* Solutions to SA2 assignment, Intro to ML *)

(* Name:  Jaidon McIntosh-cassa *)
(* Time spent on HW6: 3 hours *)
(* Collaborators and references: 
* ChatGBT -- Helped me a lot syntacilly. Explained a lot of things not explained in the readings. 
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";


(**** Problem A ****)
fun mynull [] = true
  | mynull _ = false;

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [1, 2] should be false"
    (fn () => mynull [1, 2])
    false


(**** Problem B ****)
fun firstVowel [] = false
  | firstVowel (x::xs) = 
      case x of 
          #"a" => true
        | #"e" => true
        | #"i" => true
        | #"o" => true
        | #"u" => true
        | _ => false;

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'but' should be false"
    (fn () => firstVowel [#"b",#"u",#"t"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel '' should be false"
    (fn () => firstVowel [])
    false


(**** Problem C ****)
fun reverse (l : 'a list) = foldl (fn (x, acc) => x :: acc) [] l;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1, 2, 3, 4, 5] should be [5, 4, 3, 2, 1]"
  (fn () => reverse [1, 2, 3, 4, 5])
  [5, 4, 3, 2, 1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [] should be []"
  (fn () => reverse [])
  []


(**** Problem D ****)
fun minlist [] = raise Match 
  | minlist (a::xs) = foldl (fn (x, acc) => Int.min (x, acc)) a xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minList [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minList [1,2,3,4,0])
  0


(**** Problem E ****)
exception Mismatch;

fun zip ( [], [] ) = []
  | zip ( (a::al), [] ) = raise Mismatch
  | zip ( [], (b::bl) ) = raise Mismatch
  | zip ( (a::al), (b::bl) ) = (a, b) :: zip (al, bl);

val () = Unit.checkExpectWith 
  (Unit.listString (Unit.pairString Int.toString Char.toString))
  "zip ([1, 2, 3], [#'a', #'b', #'c']) should be [(1, #'a'), (2, #'b'), (3, #'c')]"
  (fn () => zip ([1, 2, 3], [#"a", #"b", #"c"]))
  [(1, #"a"), (2, #"b"), (3, #"c")];

val () = Unit.checkExpectWith 
  (Unit.listString (Unit.pairString Char.toString Unit.stringString))
  "zip ([#'a', #'b'], ['Jaidon', 'cassa'])"
  (fn () => zip ([#"a", #"b"], ["Jaidon", "cassa"]))
  [(#"a", "Jaidon"), (#"b", "cassa")];

val () = Unit.checkExnWith 
  (Unit.listString (Unit.pairString Int.toString Char.toString))
  "zip ([1, 2], []) should raise exception"
  (fn () => zip ([1, 2], [#"a"]))

(**** Problem F ****)
fun concat [] = []
  | concat (x::xs) = x @ concat xs;

val () = Unit.checkExpectWith 
  (Unit.listString Int.toString)
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6];

(**** Problem G ****)

fun isDigit (ch:char) = ch >= #"0" andalso ch <= #"9";

val () = Unit.checkExpectWith Bool.toString
  "isDigit #'5' should be true"
  (fn () => isDigit #"5")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isDigit #'-' should be false"
  (fn () => isDigit #"-")
  false;

val () = Unit.checkExpectWith Bool.toString
  "isDigit #'0' should be true"
  (fn () => isDigit #"0")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isDigit #'9' should be true"
  (fn () => isDigit #"9")
  true;

                                                                            
(**** Problem H ****)
fun isAlpha (c:char) = (c >= #"a" andalso c <= #"z") orelse (c >= #"A" andalso c <= #"Z");

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'g' should be true"
  (fn () => isAlpha #"g")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'a' should be true"
  (fn () => isAlpha #"a")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'z' should be true"
  (fn () => isAlpha #"z")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'A' should be true"
  (fn () => isAlpha #"A")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'H' should be true"
  (fn () => isAlpha #"H")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'Z' should be true"
  (fn () => isAlpha #"Z")
  true;

val () = Unit.checkExpectWith Bool.toString
  "isAlpha #'g' should be fals"
  (fn () => isAlpha #"0")
  false;


(**** Problem I ****)

fun svgCircle (cx:int, cy:int, r:int, fill:string) = 
  "<circle cx=\"" ^ Int.toString cx ^ "\" cy=\"" ^ Int.toString cy ^ 
  "\" r=\"" ^ Int.toString r ^ "\" fill=\"" ^ fill ^ "\" />"; 

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";


(**** Problem J ****)
fun partition (p: 'a->bool) [] = ([], [])
  | partition (p: 'a->bool) (x :: xs): 'a list * 'a list = 
      let
        val (trueList, falseList) = partition p xs
      in 
        case p(x) of
          true => (x::trueList, falseList)
        | false => (trueList, x::falseList)
      end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Char.toString l1 ^ ", " ^ Unit.listString Char.toString l2 ^ ")")
  "partition Char.isAlpha [#'a', #'1', #'b', #'2', #'c'] should return ([#'a', #'b', #'c'], [#'1', #'2'])"
  (fn () => partition Char.isAlpha [#"a", #"1", #"b", #"2", #"c"])
  ([#"a", #"b", #"c"], [#"1", #"2"]);

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x > 0) [~3, ~2, 0, 1, 2] should return ([1, 2], [~3, ~2, 0])"
  (fn () => partition (fn x => x > 0) [~3, ~2, 0, 1, 2])
  ([1, 2], [~3, ~2, 0]);

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x > 0) [] should return ([], [])"
  (fn () => partition (fn x => x > 0) [])
  ([], []);

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
