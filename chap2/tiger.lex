type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun remove_whitespace_exploded (#" "::suff) = remove_whitespace_exploded suff
  | remove_whitespace_exploded (#"\t"::suff) = remove_whitespace_exploded suff
  | remove_whitespace_exploded (#"\n"::suff) = remove_whitespace_exploded suff
  | remove_whitespace_exploded str_exploded = str_exploded;

fun remove_whitespace str = String.implode (remove_whitespace_exploded (String.explode str));

fun process_string_exploded (#"\\" :: #"s" :: #"." :: #"." :: #"." :: #"\\" :: #"s" ::suff) =
    remove_whitespace_exploded suff
  | process_string_exploded (#"\\" :: #"\\" :: suff) = #"\\"::(process_string_exploded suff)
  | process_string_exploded (#"\\" :: #"n" :: suff) = #"\n"::(process_string_exploded suff)
  | process_string_exploded (#"\\" :: #"\"" :: suff) = #"\""::(process_string_exploded suff)
  | process_string_exploded (#"\\" :: c :: suff) =
    raise Fail ("Unknown escape sequence: " ^ "\\" ^ (String.implode [c]))
  | process_string_exploded (#"\""::suff) = process_string_exploded suff          
  | process_string_exploded (#"\n"::suff) = process_string_exploded suff          
  | process_string_exploded (c::suff) = c::(process_string_exploded suff)          
  | process_string_exploded [] = [];                                               
                                                                                   
fun process_string s = String.implode (process_string_exploded (String.explode s));

%%
chars="\""([^"\""]|"\\\"")*"\"";

%s INITIAL;
%s STRING;
%%
<INITIAL> {chars} => (Tokens.STRING(process_string yytext, yypos, yypos + size yytext));
<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> " "	=> (continue());
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> "123"	=> (Tokens.INT(123,yypos,yypos+3));
<INITIAL> .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

