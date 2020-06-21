use "../utility.sml";

fun process_string_exploded (#"\\"::c::suff) =
    if c =  #"\\" then #"\\"::(process_string_exploded suff)
    else if c =  #"n" then #"\n"::(process_string_exploded suff)
    else if c =  #"\"" then #"\""::(process_string_exploded suff)
    else process_string_exploded (c::suff)
  | process_string_exploded (c::suff) = c::(process_string_exploded suff)
  | process_string_exploded [] = [];

fun process_string s = String.implode (process_string_exploded (String.explode s));

println(process_string "aodno\\ndv\"oish");

