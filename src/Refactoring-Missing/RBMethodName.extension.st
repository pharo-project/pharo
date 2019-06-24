Extension { #name : #RBMethodName }

{ #category : #'*Refactoring-Missing' }
RBMethodName >> methodName [
	^ String streamContents: [ :s | self printOn: s ]
]
