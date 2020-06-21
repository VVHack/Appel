CM.make("sources.cm");
fun parse (fname::[]) = Parse.parse fname
  | parse fnames = raise Domain;
parse (CommandLine.arguments());
val _ = OS.Process.exit(OS.Process.success);
