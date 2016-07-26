actions
executeAction

	self session 
		runToSelection: self debugger selectionInterval
		inContext: self currentContext