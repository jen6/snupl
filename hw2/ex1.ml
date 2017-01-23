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

print_int(crazy2val(ONE(MONE(ZERO(MONE NIL)))));; 
