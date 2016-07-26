RBPatternBlockNode is the node in matching parse trees (it never occurs in normal Smalltalk code) that executes a block to determine if a match occurs. valueBlock takes two arguments, the first is the actual node that we are trying to match against, and second node is the dictionary that contains all the metavariable bindings that the matcher has made thus far.

Instance Variables:
	valueBlock	<BlockClosure>	The block to execute when attempting to match this to a node.

