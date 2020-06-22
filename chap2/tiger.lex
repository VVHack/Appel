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

val comment_level = ref 0;

%%
alphanumeric=[a-zA-Z0-9_];
letter=[a-zA-Z];
strchars="\""([^"\""]|"\\\"")*"\"";
digits=[0-9];
commentstart = "/*"[^"/*""*/"]*;
commentend = [^"/*""*/"]*"*/";

%s INITIAL;
%s COMMENT;;
%%
<INITIAL> {strchars} => (Tokens.STRING(process_string yytext, yypos, yypos + size yytext));
<INITIAL> {digits}+	=> (let val SOME num = Int.fromString(yytext) in
                        Tokens.INT(num, yypos, yypos + size yytext)
                        end);
<INITIAL> var  	=> (Tokens.VAR(yypos,yypos+3));
<INITIAL> function  => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL> do  => (Tokens.DO(yypos,yypos+2));
<INITIAL> to  => (Tokens.TO(yypos,yypos+2));
<INITIAL> while  => (Tokens.WHILE(yypos,yypos+5));
<INITIAL> break  => (Tokens.BREAK(yypos,yypos+5));
<INITIAL> type  => (Tokens.TYPE(yypos,yypos+4));
<INITIAL> of  => (Tokens.OF(yypos,yypos+2));
<INITIAL> if  => (Tokens.IF(yypos,yypos+2));
<INITIAL> in  => (Tokens.IN(yypos,yypos+2));
<INITIAL> end  => (Tokens.END(yypos,yypos+3));
<INITIAL> nil  => (Tokens.NIL(yypos,yypos+3));
<INITIAL> let  => (Tokens.LET(yypos,yypos+3));
<INITIAL> for  => (Tokens.FOR(yypos,yypos+3));
<INITIAL> else  => (Tokens.FOR(yypos,yypos+4));
<INITIAL> then  => (Tokens.THEN(yypos,yypos+4));
<INITIAL> array  => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL> "|"  => (Tokens.OR(yypos,yypos+1));
<INITIAL> "&"  => (Tokens.AND(yypos,yypos+1));
<INITIAL> ">="  => (Tokens.GE(yypos,yypos+2));
<INITIAL> ">"  => (Tokens.GT(yypos,yypos+1));
<INITIAL> "<="  => (Tokens.LE(yypos,yypos+2));
<INITIAL> "<"  => (Tokens.LT(yypos,yypos+1));
<INITIAL> ":="  => (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL> "<>"  => (Tokens.NEQ(yypos,yypos+2));
<INITIAL> "="  => (Tokens.EQ(yypos,yypos+1));
<INITIAL> "/"  => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL> "*"  => (Tokens.TIMES(yypos,yypos+1));
<INITIAL> "-"  => (Tokens.MINUS(yypos,yypos+1));
<INITIAL> "+"  => (Tokens.PLUS(yypos,yypos+1));
<INITIAL> "."  => (Tokens.DOT(yypos,yypos+1));
<INITIAL> "}"  => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL> "{"  => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL> "]"  => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL> "["  => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL> ")"  => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL> "("  => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL> ";"  => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL> ":"  => (Tokens.COLON(yypos,yypos+1));
<INITIAL> ","	=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL> {letter}{alphanumeric}* => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL> "\n"	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> " "	=> (continue());
<INITIAL> "\t"	=> (continue());
<INITIAL> "\r"	=> (continue());
<INITIAL> {commentstart}  => (YYBEGIN COMMENT; continue());
<COMMENT> {commentstart}  => (comment_level := !comment_level+1; continue());
<COMMENT> {commentend}  => ((if !comment_level = 0 then 
                             YYBEGIN INITIAL
                             else comment_level := !comment_level-1); continue());
<COMMENT> .     => (continue());
<COMMENT> "\n"     => (continue());
<INITIAL> .     => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

