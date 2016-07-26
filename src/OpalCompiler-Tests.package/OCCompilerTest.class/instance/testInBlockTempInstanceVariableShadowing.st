test shadowing
testInBlockTempInstanceVariableShadowing
	interactive := true.
	self initializeErrorMessage.
	text := 'temp [:temp | |var1|]'.

	self compileWithFailBlock: [
				self assert: (errorMessage = 'Name already defined ->').
				self assert: (errorLocation = 16).
				self assert: (errorSource contents = 'temp [:temp | |var1|]').
				^nil].
	self fail.