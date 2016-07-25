accessing
change
	"creates new AST by replacing the node. Then the 'add method' change is generated from new AST"

	| newTree newTreeNode |
	newTree := sourceAnchor entity parseTree.
	newTreeNode := newTree nodeAtTraversalIndex: (
		oldNode methodNode traversalIndexOf: oldNode).
	self assert: newTreeNode = oldNode.

	newTreeNode replaceMethodSource: newNode.
	newTreeNode becomeForward: newNode.
	^ RBAddMethodChange
		compile: newTree methodNode newSource
		in: sourceAnchor entity methodClass