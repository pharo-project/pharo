Extension { #name : #RBRefactoringWarning }

{ #category : #'*SystemCommands-RefactoringSupport' }
RBRefactoringWarning >> notifyUserOfCommand: aCommand [

	| answer title |
	title := 'Warning. Want to proceed?'.
	answer := self uiManager confirm: self messageText label: title.
	answer ifTrue: [ self resume: true ]
]
