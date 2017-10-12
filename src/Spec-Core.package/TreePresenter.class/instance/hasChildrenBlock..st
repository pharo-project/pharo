api-compatibility
hasChildrenBlock: aBlock
	"< api: #block getter: #hasChildrenBlock registration: #whenHasChildrenBlockChanged:>"
	"Set the block used to  check if a node has children.
	The optional block arguments are:
		- the node
		- the tree"

	hasChildrenBlockHolder value: aBlock