Class {
	#name : 'RBRemoveParameterTransformation',
	#superclass : 'RBChangeMethodNameTransformation',
	#instVars : [
		'parameterIndex',
		'argument'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'instance creation' }
RBRemoveParameterTransformation class >> model: aRBSmalltalk removeParameter: aString in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		removeParameter: aString
			in: aClass
			selector: aSelector;
		yourself
]

{ #category : 'instance creation' }
RBRemoveParameterTransformation class >> removeParameter: aString in: aClass selector: aSelector [
	^ self new
		removeParameter: aString
		in: aClass
		selector: aSelector
]

{ #category : 'preconditions' }
RBRemoveParameterTransformation >> applicabilityPreconditions [ 

	| imps |
	self getNewSelector.
	imps := self model allImplementorsOf: oldSelector.
	^  super applicabilityPreconditions ,
			 (imps collect: [ :each |
				(RBCondition 
					withBlock: [ (self hasReferencesToTemporaryIn: each) not ]
					errorString: 'This argument is still referenced in at least one implementor!!')]) asArray
]

{ #category : 'private' }
RBRemoveParameterTransformation >> computeNewSelector [

	| keywords |
	oldSelector numArgs == 0
		ifTrue: [ self refactoringError: 'This method contains no arguments' ].
	oldSelector isInfix
		ifTrue: [ self refactoringError: 'Cannot remove parameters of infix selectors' ].
	keywords := oldSelector keywords asOrderedCollection.
	keywords size = 1
		ifTrue: [ ^ (keywords first copyWithout: $:) asSymbol ].
	keywords removeAt: parameterIndex.
	^ (String streamContents: [ :str | keywords do: [ :each | str nextPutAll: each ] ]) asSymbol
]

{ #category : 'private' }
RBRemoveParameterTransformation >> getNewSelector [
	| tree |
	(class directlyDefinesMethod: oldSelector)
		ifFalse: [ self refactoringError: 'Method doesn''t exist' ].
	tree := class parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources' ].
	argument ifNil:[ self refactoringError: 'This method does not have an argument' ].
	parameterIndex := tree argumentNames indexOf: argument ifAbsent: [ self refactoringError: 'Select a parameter!!' ].
	permutation := (1 to: oldSelector numArgs) copyWithout: parameterIndex.
	newSelector := self computeNewSelector
]

{ #category : 'private' }
RBRemoveParameterTransformation >> hasReferencesToTemporaryIn: each [

	| tree |
	tree := each parseTreeForSelector: oldSelector.
	tree ifNil: [ self refactoringError: 'Cannot parse sources.' ].
	^ tree references: ( tree argumentNames at: parameterIndex )
]

{ #category : 'private' }
RBRemoveParameterTransformation >> removeParameter: aString in: aClass selector: aSelector [
	oldSelector := aSelector.
	class := self model classObjectFor: aClass.
	argument := aString
]

{ #category : 'storing' }
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
