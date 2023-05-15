Trait {
	#name : #RBTRefactoringBuilder,
	#category : #'Refactoring-Core-Support'
}

{ #category : #'defining conditions' }
RBTRefactoringBuilder >> checkInstanceVariableName: aName in: aClass [
	^RBCondition checkInstanceVariableName: aName in: aClass
]

{ #category : #'defining conditions' }
RBTRefactoringBuilder >> checkMethodName: aName in: aClass [
	^RBCondition checkMethodName: aName in: aClass
]

{ #category : #'defining conditions' }
RBTRefactoringBuilder >> emptyCondition [
	^ RBCondition empty
]

{ #category : #parsing }
RBTRefactoringBuilder >> parseTreeRewriter [
	^ self parseTreeRewriterClass new
]

{ #category : #parsing }
RBTRefactoringBuilder >> parseTreeRewriterClass [
	^ RBParseTreeRewriter
]

{ #category : #parsing }
RBTRefactoringBuilder >> parseTreeSearcher [
	^ self parseTreeSearcherClass new
]

{ #category : #parsing }
RBTRefactoringBuilder >> parseTreeSearcherClass [
	^ RBParseTreeSearcher
]

{ #category : #parsing }
RBTRefactoringBuilder >> parserClass [
	^ RBParser
]

{ #category : #exceptions }
RBTRefactoringBuilder >> refactoringConfirmWarning: aString [
	| ret |
	ret := self uiManager
		confirm: aString
		orCancel: [ ^ RBRefactoringFailure signal: 'aborted' ].
	^ ret
]

{ #category : #exceptions }
RBTRefactoringBuilder >> refactoringError: aString [
	^ RBRefactoringError signal: aString
]

{ #category : #exceptions }
RBTRefactoringBuilder >> refactoringError: aString with: aBlock [
	^ RBRefactoringError signal: aString with: aBlock
]

{ #category : #exceptions }
RBTRefactoringBuilder >> refactoringFailure: aString [
	^ RBRefactoringFailure signal: aString
]

{ #category : #exceptions }
RBTRefactoringBuilder >> refactoringFailure: aString with: aBlock [
	^ RBRefactoringFailure signal: aString with: aBlock
]

{ #category : #exceptions }
RBTRefactoringBuilder >> refactoringWarning: aString [
	^ RBRefactoringWarning signal: aString
]
