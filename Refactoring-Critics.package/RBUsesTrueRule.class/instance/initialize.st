initialization
initialize
	super initialize.
	trueBinding := Smalltalk globals associationAt: #True.
	falseBinding := Smalltalk globals associationAt: #False