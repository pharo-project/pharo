visitor-double dispatching
visitNode: aNode
	methodBuilder mapToNode: aNode.
	super visitNode: aNode.
	methodBuilder popMap.
