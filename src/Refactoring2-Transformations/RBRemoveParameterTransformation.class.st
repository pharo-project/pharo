Class {
	#name : #RBRemoveParameterTransformation,
	#superclass : #RBChangeMethodNameTransformation,
	#instVars : [
		#parameterIndex,
		#argument
	],
	#category : #'Refactoring2-Transformations-Model-Unused'
}

{ #category : #'instance creation' }
RBRemoveParameterTransformation class >> model: aRBSmalltalk removeParameter: aString in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		removeParameter: aString
			in: aClass
			selector: aSelector;
		yourself
]

{ #category : #'instance creation' }
RBRemoveParameterTransformation class >> removeParameter: aString in: aClass selector: aSelector [
	^ self new
		removeParameter: aString
		in: aClass
		selector: aSelector
]

{ #category : #private }
RBRemoveParameterTransformation >> computeNewSelector [
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

{ #category : #private }
RBRemoveParameterTransformation >> getNewSelector [
	| tree |
	(class directlyDefinesMethod: oldSelector)
		ifFalse: [ self refactoringFailure: 'Method doesn''t exist' ].
	tree := class parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringFailure: 'Cannot parse sources' ].
	argument ifNil:[ self refactoringFailure: 'This method does not have an argument' ].
	parameterIndex := tree argumentNames indexOf: argument ifAbsent: [ self refactoringFailure: 'Select a parameter!!' ].
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	newSelector := self computeNewSelector
]

{ #category : #private }
RBRemoveParameterTransformation >> hasReferencesToTemporaryIn: each [

	| tree |

	tree := each parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringFailure: 'Cannot parse sources.' ].
	^ tree references: ( tree argumentNames at: parameterIndex )
]

{ #category : #private }
RBRemoveParameterTransformation >> myConditions [
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

{ #category : #private }
RBRemoveParameterTransformation >> removeParameter: aString in: aClass selector: aSelector [
	oldSelector := aSelector.
	class := self model classObjectFor: aClass.
	argument := aString asSymbol
]

{ #category : #private }
RBRemoveParameterTransformation >> storeOn: aStream [
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
