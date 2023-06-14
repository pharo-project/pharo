Class {
	#name : #RBAbstractTransformation,
	#superclass : #Object,
	#instVars : [
		'model',
		'options'
	],
	#classVars : [
		'RefactoringOptions'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #displaying }
RBAbstractTransformation class >> basicMenuItemString [

	^ self subclassResponsibility
]

{ #category : #cleanup }
RBAbstractTransformation class >> cleanUp [
	"RefactoringOptions holds on to blocks, we should make sure to recreate them
	 so the block references the current method"
	self initializeRefactoringOptions
]

{ #category : #'class initialization' }
RBAbstractTransformation class >> initialize [
	self initializeRefactoringOptions
]

{ #category : #'private - initialization' }
RBAbstractTransformation class >> initializeRefactoringOptions [
	RefactoringOptions := IdentityDictionary new.
	RefactoringOptions
		at: #implementorToInline
		put: [ :ref :imps | self error: #implementorToInline ];

		at: #methodName
		put: [ :methodName :ref | self error: #methodName ];

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
		put: [ :ref :env |  self error: #openBrowser ];

		at: #searchInWholeHierarchy
		put: [ self error: #searchInWholeHierarchy ]
]

{ #category : #testing }
RBAbstractTransformation class >> isTransformation [

	^ false
]

{ #category : #displaying }
RBAbstractTransformation class >> menuItemString [

	^ (self isTransformation
		   ifTrue: [ '(T) ' ]
		   ifFalse: [ '' ]) , self basicMenuItemString
]

{ #category : #accessing }
RBAbstractTransformation class >> refactoringOptions [
	^ RefactoringOptions
]

{ #category : #accessing }
RBAbstractTransformation class >> setDefaultOption: aSymbol to: aBlock [
	self refactoringOptions at: aSymbol put: aBlock
]

{ #category : #accessing }
RBAbstractTransformation >> changes [

	^ self model changes
]

{ #category : #'condition definitions' }
RBAbstractTransformation >> checkInstanceVariableName: aName in: aClass [
	^RBCondition checkInstanceVariableName: aName in: aClass
]

{ #category : #'condition definitions' }
RBAbstractTransformation >> checkMethodName: aName in: aClass [
	^RBCondition checkMethodName: aName in: aClass
]

{ #category : #private }
RBAbstractTransformation >> convertMethod: selector for: aClass using: searchReplacer [
	"Convert the parse tree for selector using the searchReplacer. If a
	change is made then compile it into the changeBuilder."

	| parseTree |
	parseTree := aClass parseTreeForSelector: selector.
	parseTree ifNil: [ ^ self ].
	( searchReplacer executeTree: parseTree )
		ifTrue: [ aClass compileTree: searchReplacer tree ]
]

{ #category : #initialize }
RBAbstractTransformation >> defaultEnvironment [

	^ RBBrowserEnvironment new
]

{ #category : #'condition definitions' }
RBAbstractTransformation >> emptyCondition [
	^ RBCondition empty
]

{ #category : #accessing }
RBAbstractTransformation >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : #initialize }
RBAbstractTransformation >> newModel [

	^ (RBNamespace
			onEnvironment: self defaultEnvironment)
			name: self printString;
			yourself
]

{ #category : #'To be removed' }
RBAbstractTransformation >> openBrowserOn: anEnvironment [

	^ (self options at: #openBrowser)
		value: self value: anEnvironment
]

{ #category : #accessing }
RBAbstractTransformation >> options [

	^ options ifNil: [ options := self class refactoringOptions copy ]
]

{ #category : #accessing }
RBAbstractTransformation >> options: aDictionary [

	options := aDictionary
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeRewriter [
	^ self parseTreeRewriterClass new
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeRewriterClass [
	^ RBParseTreeRewriter
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeSearcher [
	^ self parseTreeSearcherClass new
]

{ #category : #parsing }
RBAbstractTransformation >> parseTreeSearcherClass [
	^ RBParseTreeSearcher
]

{ #category : #parsing }
RBAbstractTransformation >> parserClass [
	^ RBParser
]

{ #category : #accessing }
RBAbstractTransformation >> poolVariableNamesFor: aClass [
	| pools |
	pools := Set new.
	aClass withAllSuperclasses do:
			[:each |
			each allPoolDictionaryNames do:
					[:pool |
					pools addAll: (self poolVariableNamesIn: pool asSymbol) ] ].
	^pools
]

{ #category : #accessing }
RBAbstractTransformation >> poolVariableNamesIn: poolName [
	^(model classNamed: poolName) classPool keys
]

{ #category : #preconditions }
RBAbstractTransformation >> preconditions [

	self subclassResponsibility
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringConfirmWarning: aString [
	| ret |
	ret := self uiManager
		confirm: aString
		orCancel: [ ^ RBRefactoringError signal: 'aborted' ].
	^ ret
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringError: aString [
	^ RBRefactoringError signal: aString
]

{ #category : #accessing }
RBAbstractTransformation >> refactoringErrorClass [

	^ RBRefactoringError 
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringWarning: aString [
	^ RBRefactoringWarning signal: aString
]

{ #category : #exceptions }
RBAbstractTransformation >> refactoringWarning: aString with: aBlock [
	^ RBRefactoringWarning signal: aString with: aBlock
]

{ #category : #requests }
RBAbstractTransformation >> requestImplementorToInline: implementorsCollection [
	^(self options at: #implementorToInline) value: self
		value: implementorsCollection
]

{ #category : #requests }
RBAbstractTransformation >> requestMethodNameFor: aMethodName [
	^(self options at: #methodName) cull: aMethodName cull: self
]

{ #category : #requests }
RBAbstractTransformation >> requestSelfArgumentName [
	^(self options at: #selfArgumentName) value: self
]

{ #category : #requests }
RBAbstractTransformation >> selectVariableToMoveMethodTo: aSelector class: aClass [
	^(self options at: #selectVariableToMoveTo)
		value: self
		value: aClass
		value: aSelector
]

{ #category : #requests }
RBAbstractTransformation >> selectVariableTypesFrom: initialTypeCollection selected: selectedTypeCollection [
	^ (self options at: #variableTypes)
		value: self
		value: initialTypeCollection
		value: selectedTypeCollection
]

{ #category : #requests }
RBAbstractTransformation >> shouldExtractAssignmentTo: aString [
	^(self options at: #extractAssignment) value: self value: aString
]

{ #category : #requests }
RBAbstractTransformation >> shouldInlineExpression: aString [
	^(self options at: #inlineExpression) value: self value: aString
]

{ #category : #requests }
RBAbstractTransformation >> shouldOverride: aSelector in: aClass [
	^(self options at: #alreadyDefined)
		value: self
		value: aClass
		value: aSelector
]

{ #category : #requests }
RBAbstractTransformation >> shouldUseExistingMethod: aSelector [
	^(self options at: #useExistingMethod) value: self value: aSelector
]

{ #category : #transforming }
RBAbstractTransformation >> transform [
	"Do the actual operations."
	
	self subclassResponsibility
]
