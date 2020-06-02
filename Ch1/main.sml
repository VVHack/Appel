use "interpreter.sml";

val prog = CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
            CompoundStm(AssignStm("b",
                EseqExp(PrintStm[IdExp "a", OpExp(IdExp "a", Minus, NumExp 1)],
                    OpExp(NumExp 10, Times, IdExp "a"))),
            PrintStm [IdExp "b",IdExp "a",EseqExp(PrintStm[IdExp "a",IdExp "a",IdExp "a",IdExp "a",IdExp "a",IdExp "a"], NumExp 10)]));

print(Int.toString(maxargs prog) ^ "\n");
interp(prog);
