RBBlockNode is an AST node that represents a block "[...]".

Instance Variables:
	arguments	<SequenceableCollection of: RBVariableNode>	the arguments for the block
	bar	<Integer | nil>	position of the | after the arguments
	body	<RBSequenceNode>	the code inside the block
	colons	<SequenceableCollection of: Integer>	positions of each : before each argument
	left	<Integer>	position of [
	right	<Integer>	position of ]

