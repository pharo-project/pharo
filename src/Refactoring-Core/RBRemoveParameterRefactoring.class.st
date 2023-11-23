"
I am a refactoring for removing (unused) arguments.

My preconditions verify that the argument to be removed is not referenced by the methods and that the new method name isn't alread used.
Any sender of the prior selector will be changed to the new.

If the method contains more than one argument, I request the user to choose one of the arguments.
"
Class {
	#name : 'RBRemoveParameterRefactoring',
	#superclass : 'RBChangeMethodNameRefactoring',
	#instVars : [
		'parameterIndex',
		'argument'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRemoveParameterRefactoring class >> model: aRBSmalltalk removeParameter: aString in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		removeParameter: aString
			in: aClass
			selector: aSelector;
		yourself
]

{ #category : 'instance creation' }
RBRemoveParameterRefactoring class >> removeParameter: aString in: aClass selector: aSelector [
	^ self new
		removeParameter: aString
		in: aClass
		selector: aSelector
]

{ #category : 'validating' }
RBRemoveParameterRefactoring >> applyValidations [

	| tree |
	(class directlyDefinesMethod: oldSelector) ifFalse: [
		self refactoringError: 'Method doesn''t exist' ].
	tree := class parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources' ].
	argument ifNil: [
		self refactoringError: 'This method does not have an argument' ].
	parameterIndex := tree argumentNames
		                  indexOf: argument
		                  ifAbsent: [
		                  self refactoringError: 'Select a parameter!!' ].
	oldSelector numArgs == 0 ifTrue: [
		self refactoringError: 'This method contains no arguments' ].
	oldSelector isInfix ifTrue: [
		self refactoringError:
			'Cannot remove parameters of infix selectors' ]
]

{ #category : 'private' }
RBRemoveParameterRefactoring >> computeNewSelector [
	| keywords |

	keywords := oldSelector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: parameterIndex.
	^ (String streamContents: [ :str | keywords do: [ :each | str nextPutAll: each ] ]) asSymbol
]

{ #category : 'transforming' }
RBRemoveParameterRefactoring >> getNewSelector [
	self applyValidations.
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	^ newSelector ifNil: [ newSelector := self computeNewSelector ]
]

{ #category : 'transforming' }
RBRemoveParameterRefactoring >> hasReferencesToTemporaryIn: each [

	| tree |

	tree := each parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources.' ].
	^ tree references: ( tree argumentNames at: parameterIndex )
]

{ #category : 'preconditions' }
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

{ #category : 'accessing' }
RBRemoveParameterRefactoring >> newSelector: aSelector [
	newSelector := aSelector
]

{ #category : 'initialization' }
RBRemoveParameterRefactoring >> removeParameter: aString in: aClass selector: aSelector [
	oldSelector := aSelector.
	class := self classObjectFor: aClass.
	argument := aString
]

{ #category : 'printing' }
RBRemoveParameterRefactoring >> storeOn: aStream [

	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream
		nextPutAll: ' removeParameter: ''';
		nextPutAll: argument;
		nextPutAll: ''' in: '.
	aStream nextPutAll: class name.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: oldSelector.
	aStream nextPut: $)
]
