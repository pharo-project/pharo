as yet unclassified
visitMethodNode: aMethodNode
	scopes add: aMethodNode owningScope.
	super visitMethodNode: aMethodNode