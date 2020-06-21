use "../utility.sml";

fun f (#"c" :: #"a" :: #"r" :: suff) = suff
  | f suff = suff;

println(String.implode (f (String.explode "carno\\ndv\"oish")));

