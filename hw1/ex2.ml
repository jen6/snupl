let rec sigma(a, b, f) = 
        if ( a >= b ) then f(b) + sigma(a, b+1, f)
        else 0;;
let num(n) = n;;

let test = sigma(10, 1, num);;
print_int test
