type crazy2 =
    | NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2 ;;

let rec crazy2val crazy =
    match crazy with 
    NIL -> 0 
    | ZERO(e) -> 0 + 2*(crazy2val e)
    | ONE(e) -> 1 + 2*(crazy2val e)
    | MONE(e) -> -1 + 2*(crazy2val e) ;;

let rec crazy2add (c1, c2) =
    match c1 with
    NIL -> c2
    | ZERO(e1) -> (
        match c2 with
            NIL -> c1
            | ZERO(e2) -> ZERO(crazy2add (e1, e2))
            | ONE(e2) -> ONE(crazy2add (e1, e2))
            | MONE(e2) -> MONE(crazy2add (e1, e2))
        )
    | ONE(e1) -> (
        match c2 with
            NIL -> c1
            | ZERO(e2) -> ONE(crazy2add (e1, e2))
            | ONE(e2) -> ZERO(crazy2add ((crazy2add (ONE(NIL), e1)), e2))
            | MONE(e2) -> ZERO(crazy2add (e1, e2))
        )
    | MONE(e1) -> (
        match c2 with
            NIL -> c1
            | ZERO(e2) -> MONE(crazy2add (e1, e2))
            | ONE(e2) -> ZERO(crazy2add (e1, e2))
            | MONE(e2) -> ZERO(crazy2add ((crazy2add (MONE(NIL), e1)), e2))
        );;

let a = ONE(ONE(ONE(NIL))) in
let b = MONE(ONE(NIL)) in
print_int(crazy2val (crazy2add (a, b)))
