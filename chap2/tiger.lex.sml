structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | INITIAL
    structure UserDeclarations = 
      struct

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
  | process_string_exploded (c::suff) = c::(process_string_exploded suff)          
  | process_string_exploded [] = [];                                               
                                                                                   
fun process_string s = String.implode (process_string_exploded (String.explode s));



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (Tokens.STRING(process_string yytext, yypos, yypos + size yytext))
      end
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(yypos,yypos+1)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(yypos,yypos+3)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INT(123,yypos,yypos+3)))
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"r"
              then yyQ9(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ8(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"3"
              then yyQ11(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"2"
              then yyQ10(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ13(strm', lastMatch)
            else if inp < #"A"
              then if inp = #"#"
                  then yystuck(lastMatch)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ12(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"0"
                  then yyQ13(strm', lastMatch)
                else if inp < #"0"
                  then yystuck(lastMatch)
                else if inp <= #"9"
                  then yyQ13(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"]"
              then yystuck(lastMatch)
            else if inp < #"]"
              then if inp = #"["
                  then yystuck(lastMatch)
                  else yyQ13(strm', lastMatch)
            else if inp = #"a"
              then yyQ13(strm', lastMatch)
            else if inp < #"a"
              then yystuck(lastMatch)
            else if inp <= #"z"
              then yyQ13(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"#"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ12(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                      else yyAction0(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction0(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyAction0(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #"["
                  then yyAction0(strm, yyNO_MATCH)
                  else yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp = #"a"
              then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if inp < #"a"
              then yyAction0(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ13(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ13(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"#"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"#"
                  then if inp = #"\""
                      then yyQ12(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ13(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ13(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"]"
              then if inp = #"["
                  then yyAction5(strm, yyNO_MATCH)
                  else yyQ13(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp = #"a"
              then yyQ13(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"a"
              then yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ13(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #","
              then yyQ5(strm', lastMatch)
            else if inp < #","
              then if inp = #"\v"
                  then yyQ2(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ3(strm', lastMatch)
                      else yyQ2(strm', lastMatch)
                else if inp = #"\""
                  then yyQ4(strm', lastMatch)
                  else yyQ2(strm', lastMatch)
            else if inp = #"2"
              then yyQ2(strm', lastMatch)
            else if inp < #"2"
              then if inp = #"1"
                  then yyQ6(strm', lastMatch)
                  else yyQ2(strm', lastMatch)
            else if inp = #"v"
              then yyQ7(strm', lastMatch)
              else yyQ2(strm', lastMatch)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ1(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
