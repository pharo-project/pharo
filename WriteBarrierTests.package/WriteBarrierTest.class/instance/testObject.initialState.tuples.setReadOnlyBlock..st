tests - helper
testObject: object initialState: initialState tuples: tuples setReadOnlyBlock: setImmutabilityBlock
	self assert: object isReadOnlyObject equals: initialState.
	tuples do: [ :tuple |
		| stateToSet expectedResult expectedNewState |
		stateToSet := tuple first.
		expectedResult := tuple second.
		expectedNewState := tuple last.
		[self assert: (setImmutabilityBlock value: stateToSet) equals: expectedResult ]
				on: PrimitiveFailed 
				do: [ self assert: (self alwaysReadOnlyObjects , self alwaysWritableObjects includes: object) ].
		self assert: object isReadOnlyObject equals: expectedNewState ]