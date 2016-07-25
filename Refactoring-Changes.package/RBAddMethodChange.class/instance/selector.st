accessing
selector
	selector isNil ifTrue: [ 
		selector := RBParser parseMethodPattern: source.
		selector isNil
			ifTrue: [ selector := #unknown ] ].
	^ selector