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
	#superclass : #Object,
	#instVars : [
		'model',
		'options',
		'preconditionChecking'
	],
	#classVars : [
		'RefactoringOptions'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #cleanup }
RBTransformation class >> cleanUp [
	"RefactoringOptions holds on to blocks, we should make sure to recreate them
	 so the block references the current method"
	self initializeRefactoringOptions.
]

{ #category : #initialization }
RBTransformation class >> initialize [

	self initializeRefactoringOptions
]

{ #category : #initialization }
RBTransformation class >> initializeRefactoringOptions [

	RefactoringOptions := IdentityDictionary new.
	RefactoringOptions
		at: #implementorToInline
		put: [ :ref :imps | self error: #implementorToInline ];
		
		at: #methodName
		put: [ :ref :methodName | self error: #methodName ];
		
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

{ #category : #accessing }
RBTransformation class >> refactoringOptions [

	^ RefactoringOptions
]

{ #category : #converting }
RBTransformation >> asRefactoring [

	^ self 
		checksPreconditions;
		yourself
]

{ #category : #accessing }
RBTransformation >> changes [

	^ self model changes
]

{ #category : #preconditions }
RBTransformation >> checkPreconditions [

	| conditions result |
	conditions := self preconditions.
	
	result := conditions check.
	result ifFalse: [ self refactoringError: conditions errorString ].
	^ result
]

{ #category : #preconditions }
RBTransformation >> checksPreconditions [

	self preconditionChecking: true
]

{ #category : #private }
RBTransformation >> convertMethod: selector for: aClass using: searchReplacer [
	"Convert the parse tree for selector using the searchReplacer. If a
	change is made then compile it into the changeBuilder."

	| parseTree |
	parseTree := aClass parseTreeFor: selector.
	parseTree ifNil: [ ^ self ].
	( searchReplacer executeTree: parseTree )
		ifTrue: [ aClass compileTree: searchReplacer tree ]
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

{ #category : #initialize }
RBTransformation >> defaultEnvironment [

	^ RBBrowserEnvironment new
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

{ #category : #accessing }
RBTransformation >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : #initialize }
RBTransformation >> newModel [

	^ (RBNamespace2
		onEnvironment: self defaultEnvironment)
		name: self printString;
		yourself
]

{ #category : #executing }
RBTransformation >> openBrowserOn: anEnvironment [ 

	^ (self options at: #openBrowser)
		value: self value: anEnvironment
]

{ #category : #accessing }
RBTransformation >> options [

	^ options ifNil: [ options := self class refactoringOptions copy ]
]

{ #category : #accessing }
RBTransformation >> options: aDictionary [ 

	options := aDictionary
]

{ #category : #private }
RBTransformation >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #private }
RBTransformation >> parseTreeSearcherClass [
	^ RBParseTreeSearcher
]

{ #category : #preconditions }
RBTransformation >> preconditionChecking [

	^ preconditionChecking
]

{ #category : #preconditions }
RBTransformation >> preconditionChecking: aBoolean [

	preconditionChecking := aBoolean
]

{ #category : #preconditions }
RBTransformation >> preconditions [

	self subclassResponsibility
]

{ #category : #executing }
RBTransformation >> privateTransform [

	self subclassResponsibility
]

{ #category : #preconditions }
RBTransformation >> refactoringError: aString [

	^ RBRefactoringError signal: aString
]

{ #category : #preconditions }
RBTransformation >> refactoringError: aString with: aBlock [

	^ RBRefactoringError signal: aString with: aBlock
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
