Extension { #name : #RBRefactoringFailure }

{ #category : #'*SystemCommands-RefactoringSupport' }
RBRefactoringFailure >> notifyUserOfCommand: aCommand [

	UIManager default abort: self messageText
]
