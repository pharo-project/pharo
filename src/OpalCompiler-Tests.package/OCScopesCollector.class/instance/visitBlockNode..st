as yet unclassified
visitBlockNode: aBlockNode
	scopes add: aBlockNode owningScope.
	super visitBlockNode: aBlockNode