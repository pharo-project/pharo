tests
testToSend
	| presentation |
	presentation := GLMPresentation new.
	presentation selection: #foo.
	self assert: presentation selection equals: #foo.
	presentation selection: #(#foo #bar ).
	self assert: presentation selection equals: #(#foo #bar ).
	presentation selectionTransformation: [ :first :second | (first , second) asSymbol ].
	presentation selection: (GLMMultiValue withAll: #(#foo #bar )).
	self assert: presentation selection equals: #foobar