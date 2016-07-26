tests
testToSendAs
	| presentation |
	presentation := GLMPresentation new.
	presentation selection: #foo.
	self assert: presentation selection equals: #foo.
	self assert: (presentation pane port: #newport) value equals: nil.
	presentation send: [ :num | num + 1 ] as: #newport.
	presentation selection: 1.
	self assert: (presentation pane port: #newport) value equals: 2.
	self assert: presentation selection equals: 1