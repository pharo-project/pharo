Trait {
	#name : #RBTExceptionBuilder,
	#category : #'Refactoring2-Transformations-Model-Migration'
}

{ #category : #private }
RBTExceptionBuilder >> refactoringConfirmWarning: aString [
	| ret |
	ret := self uiManager 
		confirm: aString 
		orCancel: [ ^ RBRefactoringFailure signal: 'aborted' ].
	^ ret
]

{ #category : #private }
RBTExceptionBuilder >> refactoringError: aString [ 
	^ RBRefactoringError signal: aString
]

{ #category : #private }
RBTExceptionBuilder >> refactoringError: aString with: aBlock [ 
	^ RBRefactoringError signal: aString with: aBlock
]

{ #category : #private }
RBTExceptionBuilder >> refactoringFailure: aString [ 
	^ RBRefactoringFailure signal: aString
]

{ #category : #private }
RBTExceptionBuilder >> refactoringFailure: aString with: aBlock [
	^ RBRefactoringFailure signal: aString with: aBlock
]

{ #category : #private }
RBTExceptionBuilder >> refactoringWarning: aString [ 
	^ RBRefactoringWarning signal: aString
]
