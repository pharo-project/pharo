test shadowing
testInBlockArgumentInstanceVariableShadowing
	interactive := true.
	self initializeErrorMessage.
	text := 'temp [:var1 | ]'.
	
	self compile.
	
	self flag: 'display menu instead of going into failblock'.
	
	self assert: (errorMessage = 'Name already defined ->').
				self assert: (errorLocation = 8).
				self assert: (errorSource contents = 'temp [:var1 | ]').
				
