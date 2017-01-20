actions
executeAction

	self debugger codePresentation hasUnacceptedEdits ifTrue: [  
		self debugger codePresentation clearUserEdits.
		self debugger updateCodeEditor ]