RBSequenceNode is an AST node that represents a sequence of statements. Both RBBlockNodes and RBMethodNodes contain these.

Instance Variables:
	leftBar	<Integer | nil>	the position of the left | in the temporaries definition
	rightBar	<Integer | nil>	the position of the right | in the temporaries definition
	statements	<SequenceableCollection of: RBStatementNode>	the statement nodes
	periods	<SequenceableCollection of: Integer>	the positions of all the periods that separate the statements
	temporaries	<SequenceableCollection of: RBVariableNode>	the temporaries defined

