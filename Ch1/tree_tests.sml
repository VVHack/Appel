use "tree.sml";

val tree = insert("a", 1, empty);
printIntLn(depth(tree));
printtree tree;

val tree = insert("a", 1, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("b", 2, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("c", 2, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("d", 2, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("e", 2, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("e", 2, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("c", 3, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("i", 3, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("f", 4, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("x", 90, tree);
printIntLn(depth(tree));
printtree tree;

val tree = insert("h", 90, tree);
printIntLn(depth(tree));
printtree tree;
