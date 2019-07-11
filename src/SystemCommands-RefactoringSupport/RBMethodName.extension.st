Extension { #name : #RBMethodName }

{ #category : #'*SystemCommands-RefactoringSupport' }
RBMethodName >> methodName [
	^ String streamContents: [ :s | self printOn: s ]
]
