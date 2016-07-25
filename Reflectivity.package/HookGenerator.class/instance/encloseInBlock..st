ast manipulation
encloseInBlock: nodeOrCollection
	"Creates a new block node from a node or a list of nodes"
	^ RBBlockNode body: nodeOrCollection asSequenceNode