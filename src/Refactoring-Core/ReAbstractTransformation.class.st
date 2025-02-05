Class {
	#name : 'ReAbstractTransformation',
	#superclass : 'Object',
	#instVars : [
		'model',
		'options'
	],
	#classVars : [
		'RefactoringOptions'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'displaying' }
ReAbstractTransformation class >> basicMenuItemString [

	^ self subclassResponsibility
]

{ #category : 'cleanup' }
ReAbstractTransformation class >> cleanUp [
	"RefactoringOptions holds on to blocks, we should make sure to recreate them
	 so the block references the current method"
	self initializeRefactoringOptions
]

{ #category : 'class initialization' }
ReAbstractTransformation class >> initialize [
	self initializeRefactoringOptions
]

{ #category : 'private - initialization' }
ReAbstractTransformation class >> initializeRefactoringOptions [
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

		at: #shouldNotCreateExtraBindings
		put: [ :ref :string | self error: #shouldNotCreateExtraBindings ];

		at: #alreadyDefined
		put: [ :ref :cls :selector |  self error: #alreadyDefined ];

		at: #openBrowser
		put: [ :ref :env |  self error: #openBrowser ];

		at: #searchInWholeHierarchy
		put: [ self error: #searchInWholeHierarchy ]
]

{ #category : 'testing' }
ReAbstractTransformation class >> isTransformation [

	^ false
]

{ #category : 'displaying' }
ReAbstractTransformation class >> menuItemString [

	^ (self isTransformation
		   ifTrue: [ '(T) ' ]
		   ifFalse: [ '' ]) , self basicMenuItemString
]

{ #category : 'accessing' }
ReAbstractTransformation class >> refactoringOptions [
	^ RefactoringOptions
]

{ #category : 'preconditions' }
ReAbstractTransformation >> applicabilityPreconditions [

	^ #(  )
]

{ #category : 'private' }
ReAbstractTransformation >> buildParametersForSelector: aSelector [
	"(self new buildParametersForSelector: #cratpoc:freeFloc:) >>> #('param1' 'param2')"
	
	| params |
	params := OrderedCollection new. 
	1 
		to: aSelector numArgs 
		do: [ :i | params add: 'param', i asString ].
	^ params asArray
]

{ #category : 'private' }
ReAbstractTransformation >> buildParamtersForSelector: aSelector [
	| params |
	params := OrderedCollection new. 
	1 
		to: aSelector numArgs 
		do: [ :i | params add: 'param', i asString ].
	^ params 
]

{ #category : 'private' }
ReAbstractTransformation >> buildSelectorString: aSelector [
	aSelector numArgs = 0 ifTrue: [^aSelector].
	^self buildSelectorString: aSelector
		withPermuteMap: (1 to: aSelector numArgs)
]

{ #category : 'private' }
ReAbstractTransformation >> buildSelectorString: aSelector withPermuteMap: aPermutationCollection [

	aSelector numArgs == 0 ifTrue: [^aSelector asString].
	^ self 
		buildSelectorString: aSelector
		withPermuteMap: aPermutationCollection 
		andNewArguments: #()
]

{ #category : 'private' }
ReAbstractTransformation >> buildSelectorString: aSelector withPermuteMap: aPermutationCollection andNewArguments: anArgumentsCollection [
	| stream keywords |
	aSelector numArgs == 0 ifTrue: [^aSelector asString].
	stream := WriteStream on: String new.
	keywords := aSelector keywords.
	keywords with: aPermutationCollection
		do:
			[:each :i |
			stream
				nextPutAll: each.
				i < 0
				ifTrue:
					[ stream
						nextPut: Character space;
						nextPut: $(;
						nextPutAll: (anArgumentsCollection at: i abs) argValue;
						nextPut: $)]
				ifFalse:
					[stream
						nextPutAll: ' ``@arg';
						nextPutAll: i asString ].
			stream nextPut: Character space.
			].
	^stream contents
]

{ #category : 'accessing' }
ReAbstractTransformation >> changes [

	^ self model changes
]

{ #category : 'scripting api - conditions' }
ReAbstractTransformation >> checkApplicabilityPreconditions [
	"Check a preconditions and raise an error on violations. This method is part of the scripting API since it raises an error."
	
	| failedPreconditions |
	failedPreconditions := self failedApplicabilityPreconditions.
	failedPreconditions ifEmpty: [ ^ self ].
	RBRefactoringError signalFor: failedPreconditions
]

{ #category : 'condition definitions' }
ReAbstractTransformation >> checkInstanceVariableName: aName in: aClass [
	^RBCondition checkInstanceVariableName: aName in: aClass
]

{ #category : 'scripting api - conditions' }
ReAbstractTransformation >> checkPreconditions [

	self checkApplicabilityPreconditions
]

{ #category : 'preconditions' }
ReAbstractTransformation >> classExist [

	| className |
	^ RBCondition
		  withBlock: [ self definingClass isNotNil ]
		  errorString: 'Class named ' , className , ' does not exist' 
]

{ #category : 'private' }
ReAbstractTransformation >> convertMethod: selector for: aClass using: searchReplacer [
	"Convert the parse tree for selector using the searchReplacer. If a
	change is made then compile it into the changeBuilder."

	| parseTree |
	parseTree := aClass parseTreeForSelector: selector.
	parseTree ifNil: [ ^ self ].
	( searchReplacer executeTree: parseTree )
		ifTrue: [ aClass compileTree: searchReplacer tree ]
]

{ #category : 'accessing' }
ReAbstractTransformation >> copyOptionsFrom: aDictionary [
	| dict |
	dict := self options.
	dict == self class refactoringOptions
		ifTrue: [^self options: aDictionary copy].
	dict keysAndValuesDo:
			[:key :value |
			value == (self class refactoringOptions at: key ifAbsent: [nil])
				ifTrue: [ dict at: key put: (aDictionary at: key) ]].
	(aDictionary keys difference: dict keys) do:
		[ :e | dict at: e put: (aDictionary at: e) ].
	self options: dict
]

{ #category : 'initialize' }
ReAbstractTransformation >> defaultEnvironment [

	^ RBBrowserEnvironment new
]

{ #category : 'preconditions' }
ReAbstractTransformation >> eagerlyCheckApplicabilityPreconditions [

	self applicabilityPreconditions do: [ :cond |
		cond check ifFalse: [ RBRefactoringError signalFor: { cond } ] ]
]

{ #category : 'condition definitions' }
ReAbstractTransformation >> emptyCondition [
	self deprecated: 'Use trueCondition' transformWith: '`@rec emptyCondition' -> '`@rec trueCondition'. 
	^ RBCondition true
]

{ #category : 'transforming' }
ReAbstractTransformation >> execute [
	"Check precondition, execute the transformation that produces changes and finally execute the changes. This method is part of the scripting API. It should not be called from the driver."
	
	self generateChanges.
	self performChanges
]

{ #category : 'preconditions' }
ReAbstractTransformation >> failedApplicabilityPreconditions [
	"Returne the failed preconditions without raising error. It should only be called by drivers."
	
	^ self applicabilityPreconditions reject: [ :cond | cond check ]
]

{ #category : 'scripting api - executing' }
ReAbstractTransformation >> generateChanges [
	"Prepare, check the preconditions, perform the actual transformation (i.e., creating a list of changes that can be accessible using the changes message), and return the changes. This method should not be invoked from drivers since drivers usually check in finer grained mode the preconditions - and that this stage they already checked the preconditions."
	
	self prepareForExecution.
	self checkPreconditions.
	self privateTransform.
	^ self changes
	
	
]

{ #category : 'transforming' }
ReAbstractTransformation >> generateChangesFor: aRefactoring [
	"I will generate changes and save them in the model, BUT I will not apply them!
	Use me when a refactorings is composed of multiple other refactorings"

	"Execute the argument but passing the receiver options to that refactoring"
	aRefactoring copyOptionsFrom: self options.
	aRefactoring model: self model.
	aRefactoring generateChanges
]

{ #category : 'testing' }
ReAbstractTransformation >> isValidSelector [
	"Check I could be a valid selector (name of method).
	 For checking if there is symbol like me used as a selector, see Symbol>>#isSelectorSymbol"

	^ OCScanner isSelector: self
]

{ #category : 'testing' }
ReAbstractTransformation >> isValidSelector: aString [
	"Returns the boolean true if aString is a valid selector (name of method).
	 For checking if there is symbol like me used as a selector, see Symbol>>#isSelectorSymbol"

	"Pay attention RBCondition >> #isValidSelector is returning an Condition object."
	^ OCScanner isSelector: aString
]

{ #category : 'accessing' }
ReAbstractTransformation >> model [

	^ model
		ifNil: [ model := (RBNamespace onEnvironment: self defaultEnvironment )
				name: 'Changes for ', self class name asString;
				yourself
			]
		ifNotNil: [ model ]
]

{ #category : 'accessing' }
ReAbstractTransformation >> model: aRBNamespace [

	model := aRBNamespace
]

{ #category : 'To be removed' }
ReAbstractTransformation >> openBrowserOn: anEnvironment [

	^ (self options at: #openBrowser)
		value: self value: anEnvironment
]

{ #category : 'accessing' }
ReAbstractTransformation >> options [

	^ options ifNil: [ options := self class refactoringOptions copy ]
]

{ #category : 'accessing' }
ReAbstractTransformation >> options: aDictionary [

	options := aDictionary
]

{ #category : 'parsing' }
ReAbstractTransformation >> parseTreeRewriter [
	^ self parseTreeRewriterClass new
]

{ #category : 'parsing' }
ReAbstractTransformation >> parseTreeRewriterClass [
	^ OCParseTreeRewriter
]

{ #category : 'parsing' }
ReAbstractTransformation >> parseTreeSearcher [
	^ self parseTreeSearcherClass new
]

{ #category : 'parsing' }
ReAbstractTransformation >> parseTreeSearcherClass [
	^ OCParseTreeSearcher
]

{ #category : 'parsing' }
ReAbstractTransformation >> parserClass [
	^ OCParser
]

{ #category : 'transforming' }
ReAbstractTransformation >> performChanges [
	
	self performChanges: self changes
]

{ #category : 'transforming' }
ReAbstractTransformation >> performChanges: aCompositeChange [
	"Perform the changes contained in a composite change"
	
	RBRefactoryChangeManager instance
		performCompositeChange: aCompositeChange
]

{ #category : 'accessing' }
ReAbstractTransformation >> poolVariableNamesFor: aClass [
	| pools |
	pools := Set new.
	aClass withAllSuperclasses do:
			[:each |
			each allPoolDictionaryNames do:
					[:pool |
					pools addAll: (self poolVariableNamesIn: pool asSymbol) ] ].
	^pools
]

{ #category : 'accessing' }
ReAbstractTransformation >> poolVariableNamesIn: poolName [
	^(self model classNamed: poolName) classPool keys
]

{ #category : 'preconditions' }
ReAbstractTransformation >> preconditions [
	"By default we define applicabilityPreconditions, if you need to check
	breakingChangePreconditions, you need to override me"
	^ self applicabilityPreconditions
]

{ #category : 'transforming' }
ReAbstractTransformation >> prepareForExecution [
	"I am empty by default. You can override me to initialize your instance variables just before execution.
	I am typicaly a place where names (of classes, methods, etc.) are converted to RB model entities."
]

{ #category : 'transforming' }
ReAbstractTransformation >> privateTransform [ 

	self subclassResponsibility 
]

{ #category : 'exceptions' }
ReAbstractTransformation >> refactoringConfirmWarning: aString [
	| ret |
	ret := self uiManager
		confirm: aString
		orCancel: [ ^ RBRefactoringError signal: 'aborted' ].
	^ ret
]

{ #category : 'exceptions' }
ReAbstractTransformation >> refactoringError: aString [
	^ RBRefactoringError signal: aString
]

{ #category : 'accessing' }
ReAbstractTransformation >> refactoringErrorClass [

	^ RBRefactoringError 
]

{ #category : 'exceptions' }
ReAbstractTransformation >> refactoringWarning: aString [
	^ RBRefactoringWarning signal: aString
]

{ #category : 'exceptions' }
ReAbstractTransformation >> refactoringWarning: aString with: aBlock [
	^ RBRefactoringWarning signal: aString with: aBlock
]

{ #category : 'requests' }
ReAbstractTransformation >> requestImplementorToInline: implementorsCollection [
	^(self options at: #implementorToInline) value: self
		value: implementorsCollection
]

{ #category : 'requests' }
ReAbstractTransformation >> requestMethodNameFor: aMethodName [
	^(self options at: #methodName) cull: aMethodName cull: self
]

{ #category : 'requests' }
ReAbstractTransformation >> requestSelfArgumentName [
	^(self options at: #selfArgumentName) value: self
]

{ #category : 'utilities' }
ReAbstractTransformation >> safeMethodNameFor: aClass basedOn: aString [
	"Creates an unused method name containing aString"

	| baseString newString hasParam i |
	baseString := aString copy.
	baseString at: 1 put: baseString first asLowercase.
	newString := baseString.
	hasParam := newString last = $:.
	hasParam ifTrue: [
		baseString := newString copyFrom: 1 to: newString size - 1 ].
	newString asSymbol isBinary ifTrue: [ 
		^ baseString asSymbol ].
	i := 0.
	[ aClass hierarchyDefinesMethod: newString asSymbol ] whileTrue: [
		i := i + 1.
		newString := baseString , i printString , (hasParam
			              ifTrue: [ ':' ]
			              ifFalse: [ '' ]) ].
	^ newString asSymbol
]

{ #category : 'requests' }
ReAbstractTransformation >> selectVariableToMoveMethodTo: aSelector class: aClass [
	^(self options at: #selectVariableToMoveTo)
		value: self
		value: aClass
		value: aSelector
]

{ #category : 'requests' }
ReAbstractTransformation >> selectVariableTypesFrom: initialTypeCollection selected: selectedTypeCollection [
	^ (self options at: #variableTypes)
		value: self
		value: initialTypeCollection
		value: selectedTypeCollection
]

{ #category : 'accessing' }
ReAbstractTransformation >> setOption: aSymbol toUse: aBlock [
	"Unshare on usage"
	
	| dict |
	dict := self options.
	dict == self class refactoringOptions ifTrue: [dict := dict copy].
	dict at: aSymbol put: aBlock.
	self options: dict
]

{ #category : 'requests' }
ReAbstractTransformation >> shouldExtractAssignmentTo: aString [
	^(self options at: #extractAssignment) value: self value: aString
]

{ #category : 'requests' }
ReAbstractTransformation >> shouldNotCreateExtraBindings: aString [
	^(self options at: #shouldNotCreateExtraBindings) value: self value: aString
]

{ #category : 'requests' }
ReAbstractTransformation >> shouldOverride: aSelector in: aClass [
	^(self options at: #alreadyDefined)
		value: self
		value: aClass
		value: aSelector
]

{ #category : 'transforming' }
ReAbstractTransformation >> transform [
	"Do the actual operations."
	self deprecated: 'Use generateChanges or privateTransform instead. Check subclasses for more details.'. 
	
	self subclassResponsibility
]

{ #category : 'condition definitions' }
ReAbstractTransformation >> trueCondition [
	^ RBCondition true
]

{ #category : 'private' }
ReAbstractTransformation >> uniqueMethodNameFor: anInteger [
	"anInteger is the number of arguments/parameters"
	| before after index name |
	before := 'a'.
	after := ''.
	anInteger timesRepeat: [after := after , 'z:'].
	index := 0.

	[name := before , index printString , after.
	(Symbol findInterned: name) isNotNil]
			whileTrue: [index := index + 1].
	^name asSymbol
]
