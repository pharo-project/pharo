Extension { #name : #RBRefactoringError }

{ #category : #'*SystemCommands-RefactoringSupport' }
RBRefactoringError >> notifyUserOfCommand: aCommand [

	| answer title |
	title := self actionBlock ifNotNil: [ 'Warning' ] ifNil: [ 'Warning. Want to proceed?' ].

	answer := UIManager default confirm: self messageText label: title. 

	answer ifTrue: [ 
		"Existing actionBlock means that there is already defined handled of refactoring error.
		We should not resume in that case. Otherwise user can resume by his own risk"
		self actionBlock 
			ifNil: [ self resume: true ] 
			ifNotNil: #value]
]
