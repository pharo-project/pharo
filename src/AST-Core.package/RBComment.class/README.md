A RBComment represents a text comment associated to an AST node.

A RBComment is not an AST-Node (not subclass of program node). But its instance are just wrapping the comment text and (start-) position.

Due to the way the parser handles comments, the RBComment is assigned to its preceding (real) AST node, although we often write the comment prior to a statement.

For example:

foo
"method comment"

self firstStatement.

"comment about the return"
^ self

The "method comment" is assigned to the method node, the "comment about the return" is assigned
to the "self firstStatement" node!

instance variables
	contents 	<String> the comment text
	start	<Number> (start-) position within the method source
