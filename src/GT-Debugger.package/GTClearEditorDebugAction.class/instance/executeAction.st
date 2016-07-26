actions
executeAction

	self debugger codePresentation hasUnacceptedEdits ifTrue: [  
		(self confirm:
'This action will cancel your changes.
Is it OK to cancel changes?' translated) ifTrue: [
		self debugger codePresentation clearUserEdits.
		self debugger updateCodeEditor ] ]