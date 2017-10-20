initialization
tearDown
	aClass ifNotNil: #removeFromSystem.
	aTrait ifNotNil: #removeFromSystem.

	UndefinedObject removeSelector: #xxx.

	super tearDown.
