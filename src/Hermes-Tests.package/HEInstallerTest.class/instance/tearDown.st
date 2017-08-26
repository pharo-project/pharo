initialization
tearDown
	aClass ifNotNil: #removeFromSystem.
	aTrait ifNotNil: #removeFromSystem.

	super tearDown.
