running
tearDown

	Smalltalk globals removeKey: self globalName ifAbsent: [ ].
	globalName := nil.
	
	testClass := nil.