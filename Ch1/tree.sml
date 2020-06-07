use "../utility.sml";
type key = string;
type 'a node = key * 'a
datatype 'a tree = LEAF | TREE of 'a tree * 'a node * 'a tree;
val empty = LEAF;

fun lookup (key, LEAF) = nil
  | lookup (key, TREE(l,(k,v),r)) =
    if key < k then lookup(key, l)
    else if key > k then lookup(key, r)
    else v;

fun depth LEAF = 0
  | depth (TREE(l,(k,v),r)) = depth(l) + depth(r) + 1;

fun left_rotate LEAF = LEAF
  | left_rotate (TREE(l, (k, v), r)) =
    case r of LEAF => raise Fail "Invalid tree for left rotation"
            | TREE(rl, (kr, vr), rr) => TREE(TREE(l, (k,v), rl), (kr, vr), rr);

fun right_rotate LEAF = LEAF
  | right_rotate (TREE(l, (k, v), r)) =
    case l of LEAF => raise Fail "Invalid tree for right rotation"
            | TREE(ll, (kl, vl), lr) => TREE(ll, (kl, vl), TREE(lr, (k, v), r));

fun balance LEAF = LEAF
  | balance (TREE(l,(k,v),r)) =
    let val depthl = depth(l)
        val depthr = depth(r) in
    if depthl - depthr > 2 then
        case l of LEAF => raise Fail "This is not possible"
                | TREE(ll, (kl, vl), lr) =>
                  let val depthll = depth(ll)
                      val depthlr = depth(lr) in
                  if depthll > depthlr then
                      right_rotate (TREE(l, (k, v), r))
                  else
                      right_rotate (TREE(left_rotate(TREE(ll, (kl, vl), lr)), (k, v), r))
                  end
    else if depthr - depthl > 2 then
        case r of LEAF => raise Fail "This is not possible"
                | TREE(rl, (kr, vr), rr) =>
                  let val depthrl = depth(rl)
                      val depthrr = depth(rr) in
                  if depthrl > depthrr then
                      left_rotate (TREE(l, (k, v), r))
                  else
                      left_rotate (TREE(l, (k, v), right_rotate(TREE(rl, (kr, vr), rr))))
                  end
    else TREE(l,(k,v),r)
    end;

fun insert (key,value,LEAF) = TREE(LEAF,(key,value),LEAF)
  | insert (key,value,TREE(l,(k,v),r)) =
        if key < k then balance(TREE(balance(insert(key,value,l)),(k,v),r))
        else if key > k then balance(TREE(l,(k,v),balance(insert(key,value,r))))
        else TREE(l,(k,value),r);

val tree = insert("a", 1, empty);
printIntLn(depth(tree));

val tree = insert("a", 1, tree);
printIntLn(depth(tree));

val tree = insert("b", 2, tree);
printIntLn(depth(tree));

val tree = insert("c", 2, tree);
printIntLn(depth(tree));

val tree = insert("d", 1, tree);
printIntLn(depth(tree));

