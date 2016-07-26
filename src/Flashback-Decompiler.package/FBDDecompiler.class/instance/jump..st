control flow instructions
jump: distance
	distance < 0 ifTrue: [ ^ self error: 'should have been detected by the scanner and skipped' ].
	jumpSize := distance.