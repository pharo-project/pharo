test readonly vars
testAssignmentOfClassNameBinding
	interactive := true.
	self initializeErrorMessage.
	text := 'temp | | MockForCompilation := nil'.

	self
		compileWithFailBlock: [ self assert: errorMessage = 'Cannot store into ->'.
			self assert: errorLocation = 10.
			self assert: errorSource contents = 'temp | | MockForCompilation := nil'.
			^ nil ].
	self fail