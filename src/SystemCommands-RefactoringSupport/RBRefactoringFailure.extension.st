Extension { #name : #RBRefactoringFailure }

{ #category : #'*SystemCommands-RefactoringSupport' }
RBRefactoringFailure >> notifyUserOfCommand: aCommand [

	self uiManager abort: self messageText
]
