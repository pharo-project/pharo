test shadowing
testInBlockTempShadowing

	interactive := true.
	self initializeErrorMessage.
	text := 'temp |var2| [:temp| |var2|]'.

	self compileWithFailBlock: [
				self assert: (errorMessage = 'Name already defined ->').
				self assert: (errorLocation = 22).
				self assert: (errorSource contents = 'temp |var2| [:temp| |var2|]').
				^nil].
	self fail.


