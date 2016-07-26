*renraku
traversalIndexOf: aNode
	| index |
	index := 0.

	self nodesDo: [ :n |
		index := index + 1.
		n == aNode ifTrue: [ ^ index ] ].
	
	^ -1.