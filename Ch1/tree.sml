use "../utility.sml";
type key = string;
type 'a node = key * 'a
datatype 'a tree = LEAF | TREE of 'a tree * 'a node * 'a tree;
val empty = LEAF;

fun insert (key,value,LEAF) = TREE(LEAF,(key,value),LEAF)
  | insert (key,value,TREE(l,(k,v),r)) =
        if key < k then TREE(insert(key,value,l),(k,v),r)
        else if key > k then TREE(l,(k,v),insert(key,value,r))
        else TREE(l,(k,value),r);

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
  | right_rotate (TREE(LEAF, (k, v), r)) = raise Domain
  | right_rotate (TREE(l, (k, v), r)) =
    case l of LEAF => raise Fail "Invalid tree for right rotation"
            | TREE(ll, (kl, vl), lr) => TREE(ll, (kl, vl), TREE(lr, (k, v), r));

(*fun balance LEAF = LEAF
  | balance (TREE(LEAF,(k,v),LEAF)) = TREE(LEAF,(k,v),LEAF)
  | balance (TREE(LEAF,(k,v),TREE(LEAF,(kr,vr),LEAF))) =
            TREE(LEAF,(k,v),TREE(LEAF,(kr,vr),LEAF))
  | balance (TREE(LEAF,(k,v),TREE(rl,(kr,vr),LEAF))) =
            let val TREE(rll,(krl,vrl),rlr) = rl in
            if not(rll=LEAF) orelse not(rlr=LEAF) then
                raise Fail "Expected a balanced sub-tree"
            else
            TREE(TREE(LEAF,(k,v),LEAF),(krl,vrl),TREE(LEAF,(kr,vr),LEAF))
            end;*)
