Extension { #name : 'RBRefactoringError' }

{ #category : '*SystemCommands-RefactoringSupport' }
RBRefactoringError >> notifyUserOfCommand: aCommand [

	aCommand application alert: self messageText
]
