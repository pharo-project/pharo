layouting
layoutPolicy: aBrick on: aTraverser

	^ (aBrick owner
		ifNil: [ aBrick ]
		ifNotNil: [ :owner |
			owner isBrick
				ifTrue: [ owner ]
				ifFalse: [ aBrick ] ]) layoutPolicy