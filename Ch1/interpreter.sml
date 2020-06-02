use "../utility.sml";

type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

and exp = IdExp of id
        | NumExp of int
        | OpExp of exp * binop * exp
        | EseqExp of stm * exp

fun maxargsexp (exp:exp) = fn maxargs =>
    case exp of
        IdExp var => 0
    |   NumExp num => 0
    |   OpExp (lhs,opr,rhs) => max (maxargsexp lhs maxargs) (maxargsexp rhs maxargs)
    |   EseqExp (st,expr) => max (maxargs st) (maxargsexp expr maxargs);

fun maxargs (stm:stm) =
    case stm of
        CompoundStm(stm1,stm2) => max (maxargs stm1) (maxargs stm2)
    |   AssignStm(id,exp) => maxargsexp exp maxargs
    |   PrintStm(explist) =>
        max (List.length explist) 
            (maxlist (callall maxargs (List.map maxargsexp explist)))

type node = id * int
type table = node list

fun update (t: table, var: id, new_val: int) = (var, new_val)::t

fun lookup ((first_id:id, first_val:int)::tail, var:id) =
    if first_id = var
    then first_val
    else lookup(tail,var) 
  | lookup ([], var) = raise Empty;

fun interpExp (exp: exp, table) =
    fn interpStm => 
        case exp of
            IdExp(id) => (lookup(table, id), table)
        |   NumExp(num) => (num, table)
        |   OpExp(exp1, binop, exp2) =>
                let val (eval1, table) = interpExp(exp1, table) interpStm in
                let val (eval2, table) = interpExp(exp2, table) interpStm in
                case binop of
                    Plus => (eval1 + eval2, table)
                |   Minus => (eval1 - eval2, table)
                |   Times => (eval1 * eval2, table)
                |   Div => (eval1 div eval2, table)
                end
                end
        |   EseqExp(stm, exp) => (interpExp (exp, interpStm(stm, table))) interpStm;

fun interpStm (stm: stm, table: table) =
    case stm of
        CompoundStm(stm1,stm2) => interpStm(stm2, interpStm(stm1, table))
    |   AssignStm(id,exp) => let val (eval, table_exp) = (interpExp(exp, table) interpStm)
                             in
                             update(table_exp, id, eval)
                             end
    |   PrintStm(explist) => 
        let fun evalexp (exp::exps, t) =
                let val (eval,table) = (interpExp(exp, t) interpStm) in
                    (print(Int.toString(eval) ^ " "); evalexp(exps, table))
                end
              | evalexp ([], t) = t
        in
        let val table = evalexp(explist, table) in
            (print("\n"); table)
        end
        end;

fun interp(stm) = interpStm(stm, []);
