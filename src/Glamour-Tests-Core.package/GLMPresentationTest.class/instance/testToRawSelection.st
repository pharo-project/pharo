tests
testToRawSelection
	| presentation |
	presentation := GLMPresentation new.

	presentation selection: 1.
	self assert: presentation selection equals: 1.
	self assert: presentation rawSelection equals: 1.
	
	presentation selectionTransformation: [ :num  | num + 1].
	presentation selection: 1.
	self assert: presentation selection equals: 2.
	self assert: presentation rawSelection equals: 1.