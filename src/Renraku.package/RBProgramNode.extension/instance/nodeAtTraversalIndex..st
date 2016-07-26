*renraku
nodeAtTraversalIndex: anIndex
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		index = anIndex ifTrue: [ ^ n ] ].
	
	NotFoundError signal.