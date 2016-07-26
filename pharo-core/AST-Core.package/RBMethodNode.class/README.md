RBMethodNode is the AST that represents a Smalltalk method.

Instance Variables:
	arguments	<SequenceableCollection of: RBVariableNode>	the arguments to the method
	body	<BRSequenceNode>	the body/statements of the method
	nodeReplacements	<Dictionary>	a dictionary of oldNode -> newNode replacements
	replacements	<Collection of: RBStringReplacement>	the collection of string replacements for each node replacement in the parse tree
	selector	<Symbol>	the method name
	keywordsPositions	<IntegerArray | nil>	the positions of the selector keywords
	source	<String>	the source we compiled
	tags	<Collection of: Interval>	the source location of any resource/primitive tags

