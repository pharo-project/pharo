running
tearDown
	newClass ifNotNil: #removeFromSystem.
	superClass ifNotNil: #removeFromSystem.
	subClass ifNotNil: #removeFromSystem.
	
	super tearDown.