type nat = ZERO | SUCC of nat;;

let rec natadd (n, m) = 
        match (n, m) with
        (ZERO, ZERO) -> ZERO
        | (ZERO, SUCC(e)) ->  m
        | (SUCC(e), ZERO) -> n
        | (SUCC(e1), SUCC(e2)) -> natadd(SUCC(n), e2);;

let rec natmul (n, m) =
        match m with
        ZERO -> ZERO
        | SUCC(ZERO) -> n
        | SUCC(e) -> natadd(n, natmul(n, e));;

let rec natToInt n =
        match n with
        ZERO -> 0
        | SUCC(e) -> 1 + (natToInt(e)) ;;

let n = SUCC(SUCC(SUCC(ZERO))) in 
let m = SUCC(SUCC(ZERO)) in
print_int(natToInt(natmul(n, m)));;
