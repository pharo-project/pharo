actions
executeAction

	self session 
		runToSelection: self debugger code getSelection 
		inContext: self currentContext