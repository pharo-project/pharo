accessing-testing
errorSelected: anInteger
	errorSelected := errorList at: anInteger ifAbsent: nil.
	self changed: #errorSelected.
	errorSelected ifNotNil: [ self debug: errorSelected ].