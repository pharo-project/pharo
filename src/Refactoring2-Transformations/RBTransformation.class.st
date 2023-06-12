"
I represent an elementary code transformation.
My subinstances can be composed into composite transformations. 

I'm not a refactoring since a transformation I do not pay attention to behavior preservation. 
However as a transformation, I will be used by refactorings. 

My main API is 
	- transform which is actually executing the transformation
	- asRefactoring to optionally (false by default) check preconditions, if some apply

"
Class {
	#name : #RBTransformation,
	#superclass : #RBAbstractTransformation,
	#instVars : [
		'preconditionChecking'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #'private - initialization' }
RBTransformation class >> initializeRefactoringOptions [

	RefactoringOptions := IdentityDictionary new.
	RefactoringOptions
		at: #implementorToInline
		put: [ :ref :imps | self error: #implementorToInline ];

		at: #methodName
		put: [ :methodName | self error: #methodName ];

		at: #selfArgumentName
		put: [ :ref | self error: #selfArgumentName ];

		at: #selectVariableToMoveTo
		put: [ :ref :class :selector | self error: #selectVariableToMoveTo ];

		at: #variableTypes
		put: [ :ref :types :selected | self error: #variableTypes ];

		at: #extractAssignment
		put: [ :ref :varName | self error: #extractAssignment ];

		at: #inlineExpression
		put: [ :ref :string | self error: #inlineExpression ];

		at: #alreadyDefined
		put: [ :ref :cls :selector |  self error: #alreadyDefined ];

		at: #useExistingMethod
		put: [ :ref :selector |  self error: #useExistingMethod ];

		at: #openBrowser
		put: [ :ref :env |  self error: #openBrowser ]
]

{ #category : #converting }
RBTransformation >> asRefactoring [

	self preconditionChecking: true.
	^ self yourself
]

{ #category : #private }
RBTransformation >> buildSelectorString: aSelector [
	aSelector numArgs = 0 ifTrue: [^aSelector].
	^self buildSelectorString: aSelector
		withPermuteMap: (1 to: aSelector numArgs)
]

{ #category : #private }
RBTransformation >> buildSelectorString: aSelector withPermuteMap: anIntegerCollection [
	| stream keywords |
	aSelector numArgs == 0 ifTrue: [^aSelector asString].
	stream := WriteStream on: String new.
	keywords := aSelector keywords.
	keywords with: anIntegerCollection
		do:
			[:each :i |
			stream
				nextPutAll: each;
				nextPutAll: ' ``@arg';
				nextPutAll: i printString;
				nextPut: $ ].
	^stream contents
]

{ #category : #preconditions }
RBTransformation >> checkPreconditions [

	| conditions result |
	conditions := self preconditions.

	result := conditions check.
	result ifFalse: [ self refactoringError: conditions errorString ].
	^ result
]

{ #category : #accessing }
RBTransformation >> copyOptionsFrom: aDictionary [
	| dict |
	dict := self options.
	dict == self class refactoringOptions
		ifTrue: [^self options: aDictionary copy].
	dict keysAndValuesDo:
			[:key :value |
			value == (self class refactoringOptions at: key)
				ifTrue: [dict at: key put: (aDictionary at: key)]].
	self options: dict
]

{ #category : #execution }
RBTransformation >> execute [
	self primitiveExecute.
	RBRefactoryChangeManager instance
		performChange: self changes;
		addUndoPointer: (RBRefactoryChangeManager nextCounter)
]

{ #category : #initialization }
RBTransformation >> initialize [
	super initialize.
	preconditionChecking := false
]

{ #category : #accessing }
RBTransformation >> model [

	^ model ifNil: [ model := self newModel ]
]

{ #category : #preconditions }
RBTransformation >> preconditionChecking [

	^ preconditionChecking
]

{ #category : #preconditions }
RBTransformation >> preconditionChecking: aBoolean [

	preconditionChecking := aBoolean
]

{ #category : #executing }
RBTransformation >> primitiveExecute [
	"compatibility method RBRefactoring"
	self transform
]

{ #category : #executing }
RBTransformation >> privateTransform [

	self subclassResponsibility
]

{ #category : #accessing }
RBTransformation >> setOption: aSymbol toUse: aBlock [

	self options at: aSymbol put: aBlock
]

{ #category : #executing }
RBTransformation >> transform [
	"Performs this transformation in a model,
	 then it adds the refactoring to the manager"

	self preconditionChecking ifTrue: [ self checkPreconditions ].
	self privateTransform

	"below executes the refactoring without prompt"
	"RBRefactoringManager instance addRefactoring: self"
]

{ #category : #displaying }
RBTransformation >> whatToDisplayIn: aBrowser [

	^ self changes changes
		flatCollect: [ :e | e whatToDisplayIn: aBrowser ]
]
