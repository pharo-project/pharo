running
retrieveCircuit: aNode

	|c n i|
	c := OrderedCollection new.
	i := aNode.
	[ i notNil ] whileTrue: [
		c addFirst: i model.
		i := i bfsAncestor.
	].
	^ c