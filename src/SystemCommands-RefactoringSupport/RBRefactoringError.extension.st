Extension { #name : 'RBRefactoringError' }

{ #category : '*SystemCommands-RefactoringSupport' }
RBRefactoringError >> notifyUserOfCommand: aCommand [

	self uiManager alert: self messageText.
]
