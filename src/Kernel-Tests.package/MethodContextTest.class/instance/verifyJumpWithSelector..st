tests
verifyJumpWithSelector: selector
	| guineaPig normalStackp readOnlyStackp |
	guineaPig := SimulationMock new.
	normalStackp := (guineaPig perform: selector) stackPtr.
	guineaPig beReadOnlyObject.
	[ readOnlyStackp := (guineaPig perform: selector) stackPtr ]
		on: NoModificationError 
		do: [ :ex | ex resume ].
	self assert: normalStackp = readOnlyStackp.
	