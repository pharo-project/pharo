rewriting
rewriteStatements: statements in: seq
"You have to remove the first statement from the collection before removing all nodes in the sequence.
RemoveNode first uses == to detect the node , then =
So if you don't remove the first statement of the collection (which has been replaced by the cascade and no longer exists), then this will remove all nodes = to first node (and not ==).
So if you have the same node twice in the sequence, and this node is the first message of a cascade, then all occurences of this node would be removed"
	seq replaceNode: statements first withNode: (RBCascadeNode messages: statements copy).
	(statements
		removeFirst;
		yourself) do: [ :each | seq removeNode: each ]