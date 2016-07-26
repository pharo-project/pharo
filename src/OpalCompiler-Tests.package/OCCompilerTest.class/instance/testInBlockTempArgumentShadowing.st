test shadowing
testInBlockTempArgumentShadowing
	interactive := true.
	self initializeErrorMessage.
	text := 'temp [:temp | |temp|]'.

	self compileWithFailBlock: [
				self assert: (errorMessage = 'Name already defined ->').
				self assert: (errorLocation = 16).
				self assert: (errorSource contents = 'temp [:temp | |temp|]').
						^nil].
	self fail.
