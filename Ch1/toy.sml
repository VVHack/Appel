use "../utility.sml";
fun f x = (x, x + 1);

fun f2 (x::xs) =
    let val (y,z) = f x in
    y::z::f2 xs
    end
  | f2 [] = [];  

print_list (f2 [1,2,3,4]);
