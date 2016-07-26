examples - cascades
exampleCascadeIfTrueIfFalse2
	^ {true} asOrderedCollection
		size;
		add:
				(1=1
						ifTrue: [ 'A' ]
						ifFalse: [ 'B' ])