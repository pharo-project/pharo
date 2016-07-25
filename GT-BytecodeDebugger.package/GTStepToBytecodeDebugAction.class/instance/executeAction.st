actions
executeAction

	self session
		runToBytecode: self debugger bytecodePresentation selection 
		inContext: self currentContext