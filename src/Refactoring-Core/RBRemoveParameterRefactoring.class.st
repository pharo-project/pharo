"
I am a refactoring for removing (unused) arguments.

My preconditions verify that the argument to be removed is not referenced by the methods and that the new method name isn't alread used.
Any sender of the prior selector will be changed to the new.

If the method contains more than one argument, I request the user to choose one of the arguments.
"
Class {
	#name : #RBRemoveParameterRefactoring,
	#superclass : #RBChangeMethodNameRefactoring,
	#instVars : [
		'parameterIndex',
		'argument'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBRemoveParameterRefactoring class >> model: aRBSmalltalk removeParameter: aString in: aClass selector: aSelector [ 
	^(self new)
		model: aRBSmalltalk;
		removeParameter: aString
			in: aClass
			selector: aSelector;
		yourself
]

{ #category : #'instance creation' }
RBRemoveParameterRefactoring class >> removeParameter: aString in: aClass selector: aSelector [
	^self new removeParameter: aString in: aClass selector: aSelector
]

{ #category : #private }
RBRemoveParameterRefactoring >> computeNewSelector [
	| keywords |
	oldSelector numArgs == 0
		ifTrue: [ self refactoringFailure: 'This method contains no arguments' ].
	oldSelector isInfix
		ifTrue: [ self refactoringFailure: 'Cannot remove parameters of infix selectors' ].
	keywords := oldSelector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: parameterIndex.
	^ (String streamContents: [ :str | keywords do: [ :each | str nextPutAll: each ] ]) asSymbol
]

{ #category : #transforming }
RBRemoveParameterRefactoring >> getNewSelector [
	| tree |
	(class directlyDefinesMethod: oldSelector)
		ifFalse: [ self refactoringFailure: 'Method doesn''t exist' ].
	tree := class parseTreeFor: oldSelector.
	tree ifNil: [ self refactoringFailure: 'Cannot parse sources' ].
	argument ifNil:[ self refactoringFailure: 'This method does not have an argument' ].
	parameterIndex := tree argumentNames indexOf: argument ifAbsent: [ self refactoringFailure: 'Select a parameter!!' ].
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	newSelector := self computeNewSelector
]

{ #category : #transforming }
RBRemoveParameterRefactoring >> hasReferencesToTemporaryIn: each [

	| tree |

	tree := each parseTreeFor: oldSelector.
	tree ifNil: [ self refactoringFailure: 'Cannot parse sources.' ].
	^ tree references: ( tree argumentNames at: parameterIndex )
]

{ #category : #preconditions }
RBRemoveParameterRefactoring >> myConditions [
	| imps |
	imps := self model allImplementorsOf: oldSelector.
	self getNewSelector.
	^imps inject: (RBCondition definesSelector: oldSelector in: class)
		into: 
			[:cond :each | 
			cond 
				& (RBCondition withBlock: [(self hasReferencesToTemporaryIn: each) not]
						errorString: 'This argument is still referenced in at least one implementor!!')]
]

{ #category : #initialization }
RBRemoveParameterRefactoring >> removeParameter: aString in: aClass selector: aSelector [ 
	oldSelector := aSelector.
	class := self classObjectFor: aClass.
	argument := aString
]

{ #category : #printing }
RBRemoveParameterRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' removeParameter: ''';
		nextPutAll: argument;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: oldSelector.
	aStream nextPut: $)
]
