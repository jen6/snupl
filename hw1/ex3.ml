let rec iter(n, f) = 
        match n with
         0 -> 0 
        | _ ->  f(iter(n-1, f));;

let test = iter(10, function x-> 2+x);;
print_int test;;
