type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun process_string_exploded (#"\\"::c::suff) =                                     
    if c =  #"\\" then #"\\"::(process_string_exploded suff)                       
    else if c =  #"n" then #"\n"::(process_string_exploded suff)                   
    else if c =  #"\"" then #"\""::(process_string_exploded suff)                  
    else process_string_exploded (c::suff)                                         
  | process_string_exploded (#"\""::suff) = process_string_exploded suff          
  | process_string_exploded (#"\n"::suff) = process_string_exploded suff          
  | process_string_exploded (c::suff) = c::(process_string_exploded suff)          
  | process_string_exploded [] = [];                                               
                                                                                   
fun process_string s = String.implode (process_string_exploded (String.explode s));

%%
chars="\""([a-zA-Z0-9]|"\\"|"\\\""|"\n")*"\"";

%s INITIAL;
%s STRING;
%%
<INITIAL> {chars} => (Tokens.STRING(process_string yytext, yypos, yypos + size yytext));
<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> "123"	=> (Tokens.INT(123,yypos,yypos+3));
<INITIAL> .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

