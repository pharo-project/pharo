querying
requires

	requires == nil ifTrue: [ requires := #() ].
	^requires