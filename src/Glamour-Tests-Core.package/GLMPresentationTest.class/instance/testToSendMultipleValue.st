tests
testToSendMultipleValue
	| presentation |
	presentation := GLMPresentation new.
	presentation selection: (GLMMultiValue withAll: #(#foo #bar )).
	self assert: presentation selection equals: #foo.
	presentation selectionTransformation: [ :first :second | (first , second) asSymbol ].