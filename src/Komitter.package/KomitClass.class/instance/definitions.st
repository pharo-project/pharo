protocol
definitions

	^ definitions ifNil: [ definitions := (self operations
		select: [ :each | each isClassPatch ]
		thenCollect: [ :each | each koDefinition ]) sorted ]