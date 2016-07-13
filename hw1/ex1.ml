let rec merge ((ll : int list), (rl : int list)) : int list =
        match (ll, rl) with
        (m :: lt, n :: rt) -> 
                if m > n then
                        m :: merge(lt, n::rt)
                else
                        n :: merge(m::lt, rt)
        | ([], n :: rt) ->
                        rl @ merge([], [])
        | (m :: lt, []) -> 
                        ll @ merge([], [])
        | ([], []) -> [];;

let test = merge ([8;5;1], [7;3;2]) in
print_string (String.concat " " (List.map string_of_int test));;
