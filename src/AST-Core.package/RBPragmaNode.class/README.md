RBPragmaNode is an AST node that represents a method pragma.

Instance Variables:
	arguments <SequenceableCollection of: RBLiteralNode> our argument nodes
	left <Integer | nil> position of <
	right <Integer | nil> position of >
	selector	<Symbol>	the selector we're sending
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords