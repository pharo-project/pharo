Class {
	#name : #RBGenericInteractLayer,
	#superclass : #Object,
	#instVars : [
		'refactoring',
		'method',
		'sourceNode'
	],
	#category : #'Refactoring-Missing'
}

{ #category : #'instance creation' }
RBGenericInteractLayer class >> method: method [
	^ self new 
		method: method .
]

{ #category : #'instance creation' }
RBGenericInteractLayer class >> method: aMethod class: aClass interval: anInterval [
	"Initialize method and node of this class given:
	aMethod: a string of method name
	aClass: class that owns the given method"
	
	|instance|
	instance := self new.
	instance method:  (instance compiledMethodOf: aMethod from: aClass).
	instance sourceNode: (instance method astForStylingInCalypso bestNodeFor: anInterval).
	instance sourceNode ifNil: [ instance sourceNode: instance method astForStylingInCalypso ].
	^ instance
]

{ #category : #'instance creation' }
RBGenericInteractLayer class >> sourceNode: source method: method [
	^ self new 
		sourceNode: source; 
		method: method .
]

{ #category : #accessing }
RBGenericInteractLayer >> compiledMethodOf: aMethod from: aClass [
	"Return compiled method given a method name (string) and a class"
	
	| index |
	(aClass methods collect: #selector)
		withIndexDo: [ :each :i | 
			each = aMethod asSymbol
				ifTrue: [ index := i ] ].
	^ aClass methods at: index
]

{ #category : #execution }
RBGenericInteractLayer >> executeExtractMethod [
	self
		executeThis: [ self extractMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #'as yet unclassified' }
RBGenericInteractLayer >> executeFindAndReplace [
	self
		executeThis: [ 
			self findAndReplaceMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBGenericInteractLayer >> executeInlineMethod [
	self
		executeThis: [self inlineMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBGenericInteractLayer >> executeInlineTemp [
	self
		executeThis: [ self inlineTemp.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBGenericInteractLayer >> executePushDown [
	self
		executeThis: [ 
			self pushDownMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBGenericInteractLayer >> executePushUp [
	self
		executeThis: [ 
			self pushUpMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBGenericInteractLayer >> executeRefactoringWith: aSymbol [
	"Execute aSymbol refactoring method"
	
	[refactoring perform: aSymbol.
	^ refactoring ] on:RBRefactoringError do: [ :e | e signal]
]

{ #category : #execution }
RBGenericInteractLayer >> executeThis: aBlock [
	[
		self executionOfRequirements.
		aBlock value.
		self showChanges 
	] 
	on: RBRefactoringError 
	do: [ :e | UIManager default alert: e messageText ]
]

{ #category : #execution }
RBGenericInteractLayer >> executionOfRequirements [
	"If the refactoring needs some requirement like newName or other thing you must
	 do this here"
]

{ #category : #refactoring }
RBGenericInteractLayer >> extractMethod [
	"Return extract method refactoring"
	
	| selectedInterval dialog|
	selectedInterval := sourceNode sourceInterval.
	refactoring := RBExtractMethodRefactoring	
		extract: selectedInterval from: method selector in: method origin.
	self setUpOptionToUseExistingMethodDuring: refactoring.
	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	refactoring setOption: #methodName toUse: [ :ref :methodName |
		dialog := SycMethodNameEditor openOn: methodName withRefactoring: ref.
		dialog cancelled ifTrue: [  CmdCommandAborted signal ].
		methodName]
]

{ #category : #refactoring }
RBGenericInteractLayer >> extractTempWithName: newTempName [
	"Return extract temporary refactoring using method, node and newTempName"
	
	^ RBExtractToTemporaryRefactoring	
		extract: sourceNode sourceInterval
		to: newTempName
		from: method selector
		in: method origin
]

{ #category : #'as yet unclassified' }
RBGenericInteractLayer >> findAndReplaceMethod [
	|dialog|
	refactoring :=  RBFindAndReplaceMethodRefactoring 
		find: method .
	refactoring setOption: #listToSearch toUse: [ :met :ref |
		dialog := RBFindAndReplacePreview method: met refactoring: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		ref].
	^ refactoring 
]

{ #category : #test }
RBGenericInteractLayer >> inlineMethod [
	"Return inline method refactoring using method and node"
	
	refactoring := RBInlineMethodRefactoring	
		inline: sourceNode sourceInterval
		inMethod: method selector
		forClass: method origin.
		
	^ refactoring
]

{ #category : #refactoring }
RBGenericInteractLayer >> inlineTemp [
	"Return inline temporary refactoring using method and node"
	
	| assignment |
	assignment := sourceNode methodNode assignmentNodes
		detect: [ :each | each variable = sourceNode ].
	refactoring := RBInlineTemporaryRefactoring
		inline: assignment sourceInterval
		from: method selector
		in: method origin.
		
	^ refactoring
]

{ #category : #accessing }
RBGenericInteractLayer >> method [
	^ method
]

{ #category : #accessing }
RBGenericInteractLayer >> method: anObject [
	method := anObject
]

{ #category : #refactoring }
RBGenericInteractLayer >> pushDownMethod [
	"Return push down method refactoring using method"
	|dialog|
	refactoring :=  RBPushDownMethodRefactoring 
		pushDown: method  
		from: method first origin.
	refactoring setOption: #selectorsFromView toUse: [ :ref |
		dialog := RBPushDownPreview on: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ]].
	
	^ refactoring 
]

{ #category : #refactoring }
RBGenericInteractLayer >> pushUpMethod [
	"Return push up method refactoring using method"
	|dialog|
	refactoring :=  RBPullUpMethodRefactoring 
		pullUp: method  
		from: method first origin.
	refactoring setOption: #superClass toUse: [ :ref |
		dialog := RBPullUpPreview on: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ]].
	
	^ refactoring 
]

{ #category : #accessing }
RBGenericInteractLayer >> refactoring [
	^ refactoring
]

{ #category : #accessing }
RBGenericInteractLayer >> refactoring: anObject [
	refactoring := anObject
]

{ #category : #refactoring }
RBGenericInteractLayer >> renameTempWithName: newTempName [
	"Return rename temporary refactoring using method, node and newTempName"
	
	 ^ RBRenameTemporaryRefactoring	
		renameTemporaryFrom: sourceNode sourceInterval
		to: newTempName
		in: method origin
		selector: method selector
]

{ #category : #initialization }
RBGenericInteractLayer >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :aClass :selector | 
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', aClass name]. 
]

{ #category : #initialization }
RBGenericInteractLayer >> setUpOptionToUseExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #useExistingMethod toUse:  [ :ref :selector | 
		UIManager default 
			confirm: 'Do you want use existing method ', selector printString, '?'
			label: 'Warninig']. 	
	
]

{ #category : #initialization }
RBGenericInteractLayer >> setupMethodNameFor: aRefactoring toReturn: aSelector [ 
	| options |
	options := aRefactoring options copy.
	options at: #methodName
		put: 
			[:ref :aMethodName | 
			aMethodName
				selector: aSelector;
				yourself].
	aRefactoring options: options
]

{ #category : #preview }
RBGenericInteractLayer >> showChanges [
	(ChangesBrowserPresenter changes: {refactoring}) open
]

{ #category : #accessing }
RBGenericInteractLayer >> sourceNode [
	^ sourceNode
]

{ #category : #accessing }
RBGenericInteractLayer >> sourceNode: anObject [
	sourceNode := anObject
]
