accessing-testing
failedSelected: anInteger
	failedSelected := failedList at: anInteger ifAbsent: nil.
	self changed: #failedSelected.
	failedSelected ifNotNil: [ self debug: failedSelected ].